;;; packaging.el --- All packaging related features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Packaging related features for GNU Emacs.

;;; Code:

;;; Bootstrap packaging system

(require 'package)

(setq
 package--init-file-ensured t
 package-user-dir (concat user-local-dir "packages/" emacs-version "/elpa"))

;; Emacs >= 26.1
(when (boundp 'package-gnupghome-dir)
  (setq package-gnupghome-dir
        (expand-file-name "gnupg" user-local-dir)))

(setq package-archives
      '(("org"      . "http://orgmode.org/elpa/")
        ("melpa"    . "http://melpa.org/packages/")
        ("m-stable" . "http://stable.melpa.org/packages/")
        ("gnu"      . "https://elpa.gnu.org/packages/")))

;; Priorities. Default priority is 0.
(setq package-archive-priorities
      '(("m-stable" . 20)
        ("melpa" . 10)))

;; Initialize package manager.
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; All packages should be installed.
(setq use-package-always-ensure t)

;; Install diminish
(unless (package-installed-p 'diminish)
  (package-install 'diminish))

(require 'diminish)
(require 'bind-key)

;;; Utilities for =list-packages= menu

;; Add functions to filter the list by status (s new), or filter to see only
;; marked packages.

(defun my/package-menu-find-marks ()
  "Find packages marked for action in *Packages*."
  (interactive)
  (occur "^[A-Z]"))

(defun my/package-menu-filter-by-status (status)
  "Filter the *Packages* buffer by STATUS."
  (interactive
   (list (completing-read
          "Status : " '("new" "installed" "dependency" "obsolete"))))
  (package-menu-filter (concat "status:" status)))

(define-key package-menu-mode-map "s" #'my/package-menu-filter-by-status)
(define-key package-menu-mode-map "a" #'my/package-menu-find-marks)

(provide 'packaging)
;;; packaging.el ends here
