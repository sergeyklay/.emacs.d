;;; snippets.el --- Adds snippets support. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add snippets support for GNU Emacs

;;; Code:

(require 'core-startup)
(require 'core-dirs)

;;; Yasnippet

;; It takes a few seconds to load and I don't need them immediately when
;; Emacs starts up, so we can defer loading yasnippet until there's some
;; idle time.

;; Setting `yas-indent-line' to 'fixed' fixes Python indentation
;; behavior when typing a templated snippet.

(use-package yasnippet
  :config
  (setq yas-verbosity (if emacs-debug-mode 3 0)
        yas-snippet-dirs (list (concat user-emacs-dir "snippets"))
        yas-indent-line 'fixed)
  (yas-global-mode))

(provide 'snippets)
;;; snippets.el ends here
