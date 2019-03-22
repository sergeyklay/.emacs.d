;;; init.el -- Initialization file. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d

;;; Commentary:

;; This file is used to set up the packages sources and then call
;; an org-babel tangle in order to load a literate configuration.

;;; Code:

(require 'core (concat user-emacs-directory "features/core"))

(setq user-full-name "Serghei Iakovlev"
      user-mail-address (concat "sadhooklay" "@" "gmail" ".com"))

(require 'appearance)

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	'((org-plus-contrib . "org"))))

;; See: https://emacs.stackexchange.com/a/3147/16592
(org-babel-load-file
 (expand-file-name "settings.org" user-emacs-directory))

;;; init.el ends here
