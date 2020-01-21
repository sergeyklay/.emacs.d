;;; snippets.el --- Adds snippets support. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add snippets support for GNU Emacs.

;;; Code:

(eval-when-compile
  (require 'prelude))

;;;; YASnippet

(declare-function yas-reload-all "yasnippet")

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 20
  :custom
  (yas-indent-line 'fixed)
  :config
  (setq yas-verbosity (if emacs-debug-mode 3 0))
  (add-to-list
   'yas-snippet-dirs (concat user-emacs-directory "snippets"))
  (yas-reload-all)
  (yas-global-mode t))

(use-package ivy-yasnippet
  :defer 20
  :after (yasnippet ivy))

(provide 'snippets)
;;; snippets.el ends here
