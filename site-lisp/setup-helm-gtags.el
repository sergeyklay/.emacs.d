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

(use-package helm-gtags
  :if (executable-find "global")
  :custom
  (helm-gtags-ignore-case t)
  (helm-gtags-auto-update t)
  (helm-gtags-use-input-at-cursor t)
  (helm-gtags-pulse-at-cursor t)
  (helm-gtags-prefix-key "\C-cg")
  (helm-gtags-suggested-key-mapping t)
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
