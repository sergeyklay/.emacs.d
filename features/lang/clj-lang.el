;;; clj-lang.el --- Add support for the Clojure. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;;; Code:

(use-package clojure-mode
  :defer t
  :preface
  (declare-function put-clojure-indent "clojure-mode" (sym indent))
  :config
  (define-clojure-indent
    (if-let-failed? 'defun)
    (if-let-ok? 'defun)
    (when-let-failed? 'defun)
    (when-let-ok? 'defun)
    (attempt-all 'defun)
    (alet 'defun)
    (alet 'defun)
    (mlet 'defun))
  :hook (clojure-mode . eldoc-mode))

(use-package clojure-snippets
  :defer t)

(use-package clojure-mode-extra-font-locking)

(use-package cider
  :custom
  (cider-repl-display-help-banner nil)
  :commands (cider cider-connect cider-jack-in))

(provide 'clj-lang)
;;; clj-lang.el ends here
