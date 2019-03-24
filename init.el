;;; init.el --- Initialization file. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; This file is used as a bootstrap for my Emacs.

;;; Code:

(require 'core (concat user-emacs-directory "features/core"))

(require 'packaging)
(require 'appearance)
(require 'p14n)

(setq custom-file (concat user-etc-dir "custom.el"))
(load custom-file t)

(require 'pm)
(require 'fcompany)
(require 'fivy)     ; Initialize ivy, counsel and swiper
(require 'tags)
(require 'vcs)
(require 'spellcheck)
(require 'forg)     ; Org related configuration
(require 'fphp)     ; PHP related configuration
(require 'fhs)      ; Haskell configuration

;; deprecated
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	'((org-plus-contrib . "org"))))

;; deprecated
(org-babel-load-file
 (expand-file-name "settings.org" user-emacs-directory))

;;; init.el ends here
