;;; im.el --- Key maps, IMs and relevant configs. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Key maps, IMs and relevant configurations.

;;; Code:

;;;; Input Method and Encoding

(use-package mule-cmds
  :ensure nil
  :defer t
  :custom
  (default-input-method "russian-computer")
  :init
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)

  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))

  (setq locale-coding-system 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8)
  :config
  (prefer-coding-system 'utf-8)
  (add-to-list 'auto-coding-alist '("/#[^/]+#\\'" . utf-8)))

;;;; Disable keys in Emacs

;; Disable Arrow Keys.

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

;; Disable C-m.  I never used it.

(global-unset-key (kbd "C-m"))

;; C-h h runs the command `view-hello-file'.
;; I never used this feature and actually don't need it.
(global-unset-key (kbd "C-h h"))

(provide 'im)
;;; im.el ends here
