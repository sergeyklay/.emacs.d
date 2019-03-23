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

(setq package-archives
      '(("org"          . "http://orgmode.org/elpa/")
        ("melpa"        . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")))

(setq
 package--init-file-ensured t
 package-user-dir (concat user-local-dir "packages/" emacs-version "/elpa"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; Install diminish
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
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
