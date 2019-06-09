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

;; key bindings and code colorization for Clojure
;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :after company
  :mode
  (("\\.clj\\'"    . clojure-mode)
   ("\\.cljc\\'"   . clojurec-mode)
   ("\\.edn\\'"    . clojure-mode)
   ("\\.boot\\'"   . clojure-mode)
   ("\\.cljs.*\\'" . clojure-mode))
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
    (mlet 'defun))
  :hook
  ((clojure-mode . hs-minor-mode)
   (clojure-mode . subword-mode)
   (clojure-mode . rainbow-delimiters-mode)))

(use-package clojure-snippets
  :after clojure-mode
  :defer t)

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
   cider-repl-wrap-history t
   ;; Do not prompt for a symbol when execute interactive commands
   cider-prompt-for-symbol nil
   nrepl-hide-special-buffers t)
  :hook
  ((cider-repl-mode . company-mode)
   (cider-repl-mode . rainbow-delimiters-mode)
   (cider-mode      . company-mode)
   (cider-mode      . eldoc-mode)
   (clojure-mode    . cider-mode))
  :custom
  (cider-repl-display-help-banner nil)
  :commands
  (cider cider-connect cider-jack-in)
  :bind (:map clojure-mode-map
	([f7] . cider-jack-in)
	:map cider-repl-mode-map
	("C-c M-o" . cider-repl-clear-buffer)
	("C-c M-l" . cider-repl-switch-to-other)))

(use-package clj-refactor
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode))

(use-package flycheck-clojure
  :after (flycheck clojure-mode))

(provide 'clj-lang)
;;; clj-lang.el ends here
