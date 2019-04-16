;;; core-defuns.el --- Core defuns. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>
;; Copyright (c) 2019 Sylvain Benner <sylvain.benner@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Core defuns for GNU Emacs

;;; Code:

(defun my/buffer-revert-no-confirm ()
  "Revert buffer without confirmation."

  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun my/buffer-insert-filename ()
  "Insert file name of current buffer at current point."

  (interactive)
  (insert (buffer-file-name (current-buffer))))

;; from https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-util.el
(defun my/add-to-hooks (func hooks)
  "Add FUNC to HOOKS."
  (dolist (hook hooks)
    (add-hook hook func)))

;; from https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-util.el
;;
;; Usage:
;;
;;   (add-to-hooks 'enable-paredit-mode
;;    'emacs-lisp-mode-hook
;;    'scheme-mode-hook
;;    'lisp-mode-hook
;;    'clojure-mode-hook
;;    'eval-expression-minibuffer-setup-hook)
;;
;;   (add-to-hooks (lambda () (setq show-trailing-whitespace t))
;;    'prog-mode-hook
;;    'org-mode-hook
;;    'html-mode-hook)
;;
(defun my/add-all-to-hook (hook &rest funs)
  "Add FUNS to HOOK."
  (my/add-to-hook hook funs))

;; from https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-util.el
(defun my/add-to-hook (hook funs)
  "Add list of FUNS to HOOK."
  (dolist (fun funs)
    (add-hook hook fun)))

(defun my/mplist-get-values (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

;; from https://github.com/syl20bnr/spacemacs/blob/develop/core/core-funcs.el
(defun my/mplist-get-value (plist prop)
  "Get a single value associated to PROP in PLIST, a modified plist.

You should always use this function instead of builtin `plist-get'
in Spacemacs.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (car (my/mplist-get-values plist prop)))

(defun my/read-write-toggle ()
  "Toggle read-only in any relevant mode."
  (interactive)
  (if (equal major-mode 'ag-mode)
      ;; wgrep-ag can support ag-mode
      (wgrep-change-to-wgrep-mode)
    ;; dired-toggle-read-only has its own conditional:
    ;; if the mode is Dired, it will make the directory writable
    ;; if it is not, it will just toggle read only, as desired
    (dired-toggle-read-only)))

(global-set-key (kbd "C-x t r") #'my/read-write-toggle)

(provide 'core-defuns)
;;; core-defuns.el ends here
