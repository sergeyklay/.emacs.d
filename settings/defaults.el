;;; defaults.el --- Sane defaults. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Sane defaults for GNU Emacs.

;;; Code:

(require 'directories)

(global-set-key (kbd "C-x t d") #'toggle-debug-on-error)

;;;; Encoding

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(add-to-list 'auto-coding-alist '("/#[^/]+#\\'" . utf-8))

;;;; Tutorial

(advice-add
 'tutorial--saved-dir
 :override (lambda ()
             (let ((tutorial-dir (concat user-cache-dir "tutorial/")))
               (unless (file-exists-p tutorial-dir)
                 (make-directory tutorial-dir t))
               tutorial-dir)))

(provide 'defaults)
;;; defaults.el ends here
