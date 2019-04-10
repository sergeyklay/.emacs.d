;;; lisp-lang.el --- Configure the Lisp-family of languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Configure the Lisp-family of languages for the GUN Emacs.

;;; Code:

(defconst sbcl-executable-path (executable-find "sbcl")
  "The sbcl executable path on this system.")

;; The Superior Lisp Interaction Mode for Emacs.
(use-package slime
  :commands slime-mode
  :init
  (progn
    (setq inferior-lisp-program sbcl-executable-path)

    ;; enable fuzzy matching in code buffer and SLIME REPL
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

    (slime-setup '(slime-asdf
                   slime-fancy
                   slime-indentation
                   slime-sbcl-exts
                   slime-scratch))

    (add-company-backends!! :backends (company-capf company-files)
                            :modes slime-mode)

    (add-hook 'lisp-mode-hook #'slime-mode)))

(use-package slime-company
  :after (slime company)
  :defer t
  :init
  (slime-setup '(slime-fancy slime-company)))

(use-package ielm
  :ensure nil
  :init
  (add-company-backends!! :backends (company-files company-capf)
                          :modes ielm-mode))

(use-package lisp-mode
  :ensure nil
  :init
  (progn
    (my/add-to-hooks #'my/ggtags-mode-enable '(lisp-mode-hook))
    (my/add-to-hooks #'turn-on-eldoc-mode '(lisp-mode-hook))))

(use-package elisp-mode
  :ensure nil
  :init
  (progn
    (my/add-to-hooks #'my/ggtags-mode-enable
                     '(emacs-lisp-mode-hook
                       lisp-interaction-mode-hook))

    (my/add-to-hooks #'turn-on-eldoc-mode
                     '(emacs-lisp-mode-hook
                       lisp-interaction-mode-hook))

    (my/add-to-hooks #'company-mode
                     '(emacs-lisp-mode-hook
                       lisp-interaction-mode-hook))

    (add-company-backends!! :backends company-capf
                            :modes emacs-lisp-mode)))

(provide 'lisp-lang)
;;; lisp-lang.el ends here
