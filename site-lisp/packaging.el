;;; packaging.el --- Package management. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Bootstrap packaging system.

;;; Code:

(require 'package)

(eval-when-compile
  (require 'prelude))

(when (eval-when-compile (version< emacs-version "27"))
  ;; WARNING: This broke `esup' command usage.
  ;; TODO(sergei): Fix me
  (load "~/.emacs.d/early-init.el")
  (require 'early-init))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 '(use-package-enable-imenu-support t) ; enable imenu support for `use-package'
 '(use-package-always-ensure t)        ; all packages should be installed
 '(use-package-verbose emacs-debug-mode))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

;;;; Utilities for `list-packages' menu

(defun my/package-menu-find-marks ()
  "Find packages marked for action in *Packages*."
  (interactive)
  (occur "^[A-Z]"))

(defun my/package-menu-filter-by-status (status)
  "Filter the *Packages* buffer by STATUS."
  (interactive
   (list (completing-read
          "Status : " '("new" "installed" "dependency" "obsolete"))))
  (if (version< emacs-version "27")
      (package-menu-filter (concat "status:" status))
    (package-menu-filter-by-keyword (concat "status:" status))))

(define-key package-menu-mode-map "s" #'my/package-menu-filter-by-status)
(define-key package-menu-mode-map "a" #'my/package-menu-find-marks)

(provide 'packaging)
;;; packaging.el ends here
