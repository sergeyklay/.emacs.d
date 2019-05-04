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

;; The Superior Lisp Interaction Mode for Emacs.
(use-package slime
  :if sbcl-executable-path
  :commands slime-mode
  :hook (lisp-mode . slime-mode)
  :init
  (progn
    (setq inferior-lisp-program sbcl-executable-path
          slime-complete-symbol*-fancy t
          slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)

    (slime-setup '(slime-asdf
                   slime-fancy
                   slime-indentation
                   slime-sbcl-exts
                   slime-scratch))

    (add-company-backends!!
      :backends (company-capf company-files)
      :modes slime-mode)))

(use-package slime-company
  :after (slime company)
  :defer t
  :init
  (slime-setup '(slime-fancy slime-company)))

(use-package ielm
  :ensure nil
  :init
  (my/add-to-hook
   #'ielm-mode-hook
   '(turn-on-eldoc-mode))
  (add-company-backends!!
    :backends (company-files company-capf) company-elisp
    :modes ielm-mode))

(use-package lisp-mode
  :ensure nil
  :init
  (my/add-to-hook
   #'lisp-mode-hook
   '(turn-on-eldoc-mode
     my|ggtags-mode-enable))
  :bind
  (:map lisp-mode-map
        ("C-<tab>" . #'company-complete)))

(use-package elisp-mode
  :ensure nil
  :init
  (require 'show-point-mode)

  (my/add-to-hook
   #'emacs-lisp-mode-hook
   '(turn-on-eldoc-mode
     my|ggtags-mode-enable
     show-point-mode
     hs-minor-mode))

  (my/add-to-hook
   #'lisp-interaction-mode-hook
   '(turn-on-eldoc-mode
     my|ggtags-mode-enable
     show-point-mode))

  (add-company-backends!!
    :backends company-capf company-elisp
    :modes emacs-lisp-mode lisp-interaction-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-b" . #'eval-buffer)
        ("C-<tab>" . #'company-complete)))

(use-package cask-mode
  :mode "Cask")

(use-package flycheck-cask
  :after flycheck-mode
  :hook
  (flycheck-mode . flycheck-cask-setup)
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

(provide 'lisp-lang)
;;; lisp-lang.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
