;;; lisp-lang.el --- Configure the Lisp-family of languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Configure the Lisp-family of languages for the GUN Emacs.

;;; Code:

;;; SLIME

;; The Superior Lisp Interaction Mode for Emacs.
;; Note: You have to install sbcl.

(use-package slime
  :defer 10
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (add-to-list 'slime-contribs 'slime-fancy))

(use-package elisp-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'my/ggtags-mode-enable)
    (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)

    (add-hook 'lisp-interaction-mode-hook #'my/ggtags-mode-enable)
    (add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)))

(provide 'lisp-lang)
;;; lisp-lang.el ends here
