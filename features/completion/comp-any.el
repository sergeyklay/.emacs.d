;;; comp-any.el --- Initialise company. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setting up company for GNU Emacs.

;;; Code:

(defconst my/default-company-backends
  '((company-dabbrev-code
     company-gtags
     company-etags
     company-keywords)
    company-files
    company-dabbrev)
  "The list of default company backends used by my Emacs.")

;; For more see URL `http://company-mode.github.io'
(use-package company
  :defer t
  :init
  (progn
    (setq company-idle-delay 0.2
          company-echo-delay 0
          company-async-timeout 5
          company-minimum-prefix-length 2
          ;; Some languages use camel case naming convention,
          ;; so company should be case sensitive.
          company-dabbrev-ignore-case nil
          ;; I don't like the downcase word in company-dabbrev
          company-dabbrev-downcase nil
          ;; See URL `https://github.com/company-mode/company-mode/issues/146'
          company-tooltip-align-annotations t
          company-tooltip-idle-delay t
          company-tooltip-limit 20
          ;; make previous/next selection in the popup cycles
          company-selection-wrap-around t
          company-show-numbers t)))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort))

;; Company-Statistics: Suggest most used completions first
(use-package company-statistics
  :after company
  :defer t
  :init
  (progn
    (setq company-statistics-file
          (concat user-cache-dir "company-statistics-cache.el"))
    (add-hook 'company-mode-hook 'company-statistics-mode)))

;; from (concat "https://github.com/syl20bnr/spacemacs"
;;              "/blob/develop/layers/%2Bcompletion"
;;              "/auto-completion/funcs.el")
(defmacro my|add-company-backends (&rest props)
  "Add and enable company backends.

Available PROPS:

`:backends BACKENDS'
   One or several symbols or lists representing a company backend
   or a list of company backends.

`:modes MODES'
    One or several modes where BACKENDS will be added.

:variables VAR VALUE'
    One or several VAR VALUE pairs (similar to layer variables).
    These variables are made buffer local so their values are set only for
    the given MODES.

`:from SYMBOL'
    Advanced property aimed at avoiding hook function name conflicts when
    `:variables' property is used in several calls to this macro for the same
    MODES.

`:append-hook BOOLEAN'
    Advanced property to control whether hooks functions are hooked or not,
    if non-nil hook functions are appended to modes hooks passed as `:modes'.

`:call-hooks BOOLEAN'
    if non-nil then hooked functions are called right away."
  (declare (indent 0))
  (let* ((backends (my/mplist-get-values props :backends))
         (modes (my/mplist-get-values props :modes))
         (variables (my/mplist-get-values props :variables))
         (from (my/mplist-get-value props :from))
         (hooks (if (memq :append-hooks props)
                    (my/mplist-get-value props :append-hooks)
                  t))
         (call-hooks (when (memq :call-hooks props)
                       (my/mplist-get-value props :call-hooks)))
         (result '(progn)))
    (dolist (mode modes)
      (let ((backends-var-name (intern (format "company-backends-%S" mode)))
            (raw-backends-var-name (intern (format "company-backends-%S-raw"
                                                   mode)))
            (init-func-name (intern (format "my//init-company-%S" mode)))
            (vars-func-name (intern
                             (format "my//init-company-vars-%S%s" mode
                                     (if from (format "-%S" from) ""))))
            (mode-hook-name (intern (format "%S-hook" mode))))
        ;; declare buffer local company-backends variable
        (push `(defvar ,raw-backends-var-name
                 my/default-company-backends
                 ,(format "Company backend list for %S." mode)) result)
        (push `(defvar ,backends-var-name ,raw-backends-var-name
                 ,(format "Company backend list for %S." mode)) result)
        ;; add backends
        (dolist (backend backends)
          (push `(add-to-list ',raw-backends-var-name ',backend) result))
        ;; define initialization hook function
        (push `(defun ,init-func-name ()
                ,(format "Initialize company for %S." mode)
                (setq ,backends-var-name ,raw-backends-var-name)
                (set (make-variable-buffer-local 'auto-completion-front-end)
                     'company)
                (set (make-variable-buffer-local 'company-backends)
                     ,backends-var-name)) result)
        (when call-hooks
          (push `(,init-func-name) result))
        (when hooks
          (push `(add-hook ',mode-hook-name ',init-func-name t) result))
        (when variables
          (let ((variables-copy variables)
                (vars-func `(defun ,vars-func-name ()
                              ,(format "Define company local variables for %S."
                                       mode)))
                vars)
            (while variables-copy
              (let* ((var (pop variables-copy))
                     (forms
                      (when (consp variables-copy)
                        `(set (make-variable-buffer-local ',var)
                              ,(eval (pop variables-copy))))))
                (when forms (push forms vars))))
            (push (append vars-func vars) result))
          (when call-hooks
            (push `(,vars-func-name) result))
          (when hooks
            (push `(add-hook ',mode-hook-name ',vars-func-name t) result)))
        (when hooks
          (push `(add-hook ',mode-hook-name 'company-mode t) result))))
    ;; return the expanded macro in correct order
    (reverse result)))

(provide 'comp-any)
;;; comp-any.el ends here
