;;; modeline.el --- Modeline related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Modeline related configuration for the GNU Emacs.

;;; Code:

;;; Eldoc

;; This package displays function signatures in the mode line.

(use-package c-eldoc
  :commands c-turn-on-eldoc-mode
  :init
    (progn
      (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode)
      (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)))

(provide 'modeline)
;;; modeline.el ends here
