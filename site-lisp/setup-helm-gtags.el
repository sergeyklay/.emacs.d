;;; setup-helm-gtags.el --- Setup helm-gtags. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setup helm-gtags.

;;; Code:

(defvar helm-gtags-prefix-key "\C-cg")

(defun ph-find-symbol-at-point ()
  "Find symbol at point.

If the symbol matches a generic type in
C++/C#/Kotlin/Java (e.g. List<TypeA<TypeB>>), then the function
returns the word at point."
  (let ((token (thing-at-point 'symbol 'no-text-props)))
    (if (string-match "\\([^<>]+\\)<\\(.+\\)>" token)
        (thing-at-point 'word 'no-text-props)
      token)))

(use-package helm-gtags
  :if (executable-find "global")
  :custom
  (helm-gtags-ignore-case t)
  (helm-gtags-auto-update t)
  (helm-gtags-use-input-at-cursor t)
  (helm-gtags-pulse-at-cursor t)
  (helm-gtags-prefix-key "\C-cg")
  (helm-gtags-suggested-key-mapping t)
  (helm-gtags-cache-max-result-size 1073741824) ; 1024 MB
  ;; See URL `https://github.com/emacsorphanage/helm-gtags/pull/187'.
  (helm-gtags-symbol-at-point-function 'ph-find-symbol-at-point)
  :hook (;; Enable `helm-gtags-mode' in Dired to be able jump
         ;; to any tag when navigate project tree with Dired
         (dired-mode . helm-gtags-mode)
         ;; Enable `helm-gtags-mode' in Eshell for the same
         ;; reason as above
         (eshell-mode . helm-gtags-mode)
         ;; Enable helm-gtags-mode in languages that GNU Global
         ;; supports
         (emacs-lisp-mode . helm-gtags-mode)
         (c-mode          . helm-gtags-mode)
         (c++-mode        . helm-gtags-mode)
         (java-mode       . helm-gtags-mode)
         (asm-mode        . helm-gtags-mode))
  :bind (:map helm-gtags-mode-map
              ("C-c g a" . helm-gtags-tags-in-this-function)
              ("C-c g c" . helm-gtags-create-tags)
              ("C-c g u" . helm-gtags-update-tags)
              ("C-j"     . helm-gtags-select)
              ("M-."     . helm-gtags-dwim)
              ("M-,"     . helm-gtags-pop-stack)
              ("C-c <"   . helm-gtags-previous-history)
              ("C-c >"   . helm-gtags-next-history)))

(provide 'setup-helm-gtags)
;;; setup-helm-gtags.el ends here
