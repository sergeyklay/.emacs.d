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

(require 'core-dirs)
(require 'core-defuns)

;; key bindings and code colorization for Clojure
;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :defer t
  :preface
  (declare-function put-clojure-indent "clojure-mode" (sym indent))
  (declare-function clojure-mode-hook "clojure-mode")
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
  :init
  (my/add-to-hook
   #'clojure-mode-hook
   '(turn-on-eldoc-mode
     my|ggtags-mode-enable
     hs-minor-mode
     subword-mode)))

(use-package clojure-snippets
  :defer t
  :after clojure-mode)

;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :after clojure-mode)

;; integration with a Clojure REPL
;; https://github.com/clojure-emacs/cider
(use-package cider
  :after clojure-mode
  :init
  (setq
   ;; go right to the REPL buffer when it's finished connecting
   cider-repl-pop-to-buffer-on-connect t
   ;; when there's a cider error, show its buffer and switch to it
   cider-show-error-buffer t
   cider-auto-select-error-buffer t
   cider-repl-history-file (concat user-cache-dir "cider-history")
   cider-repl-wrap-history t)
  :hook
  ((cider-mode . eldoc-mode)
   (clojure-mode . cider-mode))
  :custom
  (cider-repl-display-help-banner nil)
  :commands
  (cider cider-connect cider-jack-in)
  :bind
  (:map clojure-mode-map
	([f7] . cider-jack-in)))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode))

(use-package flycheck-clojure
  :after (flycheck clojure-mode))

(provide 'clj-lang)
;;; clj-lang.el ends here
