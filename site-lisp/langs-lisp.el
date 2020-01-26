;;; langs-lisp.el --- Lisp-family of languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for the Lisp-family of languages.

;;; Code:

(eval-when-compile
  (require 'company))

(defconst sbcl-executable-path (executable-find "sbcl")
  "The sbcl executable path on this system.")

;; The Superior Lisp Interaction Mode for Emacs.
(use-package slime
  :if sbcl-executable-path
  :commands slime-mode
  :hook
  ((lisp-mode . slime-mode))
  :custom
  (slime-complete-symbol*-fancy t)
  (slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)
  (slime-net-coding-system 'utf-8-unix)
  :init
  (setq inferior-lisp-program sbcl-executable-path)
  (slime-setup '(slime-asdf
		 slime-fancy
		 slime-indentation
		 slime-sbcl-exts
		 slime-scratch)))

(use-package slime-company
  :after (slime company)
  :defer t
  :hook
  ((slime-mode . company-mode))
  :init
  (slime-setup '(slime-fancy slime-company)))

(use-package ielm
  :ensure nil
  :hook
  ((ielm-mode . company-mode)
   (ielm-mode . turn-on-eldoc-mode)
   (ielm-mode . rainbow-delimiters-mode)))

(use-package lisp-mode
  :ensure nil
  :hook
  ((lisp-mode . company-mode) ;; TODO: setup TAGS
   (lisp-mode . turn-on-eldoc-mode)
   (lisp-mode . rainbow-delimiters-mode)))

(use-package elisp-mode
  :ensure nil
  :hook
  ((emacs-lisp-mode . company-mode) ; TODO: Setup tags
   (emacs-lisp-mode . turn-on-eldoc-mode)
   (emacs-lisp-mode . rainbow-delimiters-mode)

   (lisp-interaction-mode . company-mode) ; TODO: Setup tags
   (lisp-interaction-mode . turn-on-eldoc-mode)
   (lisp-interaction-mode . rainbow-delimiters-mode))
  :init
  (add-to-list 'company-backends
	       '(company-elisp
		 company-capf
		 company-dabbrev-code
		 company-yasnippet
		 company-files))
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-b" . #'eval-buffer)))

(use-package eros
  :defer t
  :hook
  ((ielm-mode       . eros-mode)
   (emacs-lisp-mode . eros-mode))
  :custom
  (eros-eval-result-prefix "∴ "))

(use-package cask-mode
  :mode "Cask")

(use-package flycheck-cask
  :after flycheck-mode
  :hook
  (flycheck-mode . flycheck-cask-setup))

(provide 'langs-lisp)
;;; langs-lisp.el ends here
