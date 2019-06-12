;;; core-packaging.el --- Package management. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Packaging related stuff for GNU Emacs.

;;; Code:

;;; Bootstrap packaging system

(require 'package)
(require 'core-dirs)
(require 'core-startup)

(setq package--init-file-ensured t)
(setq package-user-dir
      (concat
       (substitute-in-file-name "$HOME/.local/lib/emacs/")
       "packages/" emacs-version "/elpa"))

(unless (file-exists-p package-user-dir)
    (make-directory package-user-dir t))

(setq package-gnupghome-dir
      (expand-file-name "gnupg" user-local-dir))

(setq package-archives
      '(("org"      . "http://orgmode.org/elpa/")
        ("melpa"    . "http://melpa.org/packages/")
        ("m-stable" . "http://stable.melpa.org/packages/")
        ("elpa"     . "https://elpa.gnu.org/packages/")))

;; Priorities. Default priority is 0.
(setq package-archive-priorities
      '(("m-stable" . 10)
        ("melpa" . 20)))

;; Initialize package manager.
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; All packages should be installed.
(setq use-package-always-ensure t)
(setq use-package-verbose emacs-debug-mode)

(require 'bind-key)

;; Quelpa support
(defvar quelpa-dir)

(use-package quelpa
  :defer t
  :init
  (setq quelpa-dir
	(concat
	 (substitute-in-file-name "$HOME/.local/lib/emacs/")
	 "packages/" emacs-version "/quelpa"))
  (unless (file-exists-p quelpa-dir)
    (make-directory quelpa-dir t))
  :custom
  (quelpa-checkout-melpa-p nil)
  (quelpa-update-melpa-p nil "Don't update the MELPA git repo."))

(use-package quelpa-use-package)

;;; Utilities for `list-packages' menu

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

(provide 'core-packaging)
;;; core-packaging.el ends here
