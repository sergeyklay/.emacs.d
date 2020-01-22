;;; jump.el --- GNU Emacs configuration for tags. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Tags realated features for GNU Emacs.

;;; Code:

(eval-when-compile
  (require 'etags)
  (require 'company))

(defconst rdm-executable-path (executable-find "rdm")
  "The rdm executable path on this system.")

;; Make Emacs reload the TAGS file automatically
(setq tags-revert-without-query 1)

;; t=case-insensitive, nil=case-sensitive
(setq tags-case-fold-search nil)

;; Never “Keep current list of tags tables also”
(setq tags-add-tables nil)

(use-package rtags
  :if rdm-executable-path
  :after company
  :defer 10
  :config
  (setq rtags-completions-enabled t
	rtags-path (directory-file-name
		    (file-name-directory rdm-executable-path)))
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-rtags))
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings))

(use-package company-rtags
  :if rdm-executable-path
  :after (rtags))

(provide 'jump)
;;; jump.el ends here
