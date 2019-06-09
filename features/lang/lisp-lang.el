;;; lisp-lang.el --- Configure the Lisp-family of languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Configure the Lisp-family of languages for GNU Emacs.

;;; Code:

(defconst sbcl-executable-path (executable-find "sbcl")
  "The sbcl executable path on this system.")

(defvar slime-complete-symbol*-fancy)

;; The Superior Lisp Interaction Mode for Emacs.
(use-package slime
  :if sbcl-executable-path
  :commands slime-mode
  :hook
  ((lisp-mode . slime-mode))
  :custom
  (slime-complete-symbol*-fancy t)
  (slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)
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
  ((lisp-mode . company-mode)
   (lisp-mode . turn-on-eldoc-mode)
   (lisp-mode . rainbow-delimiters-mode)
   (lisp-mode . my|ggtags-mode-enable)))

(defun my|elisp-common-hook ()
  "Common Emacs Lisp hook."
  (setq-local mood-line-show-point t))

(use-package elisp-mode
  :ensure nil
  :hook
  ((emacs-lisp-mode . company-mode)
   (emacs-lisp-mode . turn-on-eldoc-mode)
   (emacs-lisp-mode . hs-minor-mode)
   (emacs-lisp-mode . rainbow-delimiters-mode)
   (emacs-lisp-mode . my|ggtags-mode-enable)
   (emacs-lisp-mode . my|elisp-common-hook)

   (lisp-interaction-mode . company-mode)
   (lisp-interaction-mode . turn-on-eldoc-mode)
   (lisp-interaction-mode . rainbow-delimiters-mode)
   (lisp-interaction-mode . my|ggtags-mode-enable))
  :init
  (add-to-list 'company-backends '(company-elisp company-capf))
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-b" . #'eval-buffer)))

(use-package cask-mode
  :mode "Cask")

(use-package flycheck-cask
  :after flycheck-mode
  :hook
  (flycheck-mode . flycheck-cask-setup))

(provide 'lisp-lang)
;;; lisp-lang.el ends here
