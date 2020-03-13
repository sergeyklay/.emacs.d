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
(require 'directories)

(custom-set-variables
 ;; Directory containing the user’s Emacs Lisp packages.
 '(package-user-dir
   (let ((dir (concat
	       (substitute-in-file-name "$HOME/.local/share/emacs/elpa/")
	       emacs-version)))
     (unless (file-exists-p dir)
       (make-directory dir t))
     dir))
 ;; Directory containing GnuPG keyring.
 ;; At my PC it is `~/.emacs.d/.local/gnupg'.
 '(package-gnupghome-dir (expand-file-name "gnupg" user-local-dir))
 ;; Setting up package archives.
 '(package-archives
   '(("melpa"    . "https://melpa.org/packages/")
     ("m-stable" . "https://stable.melpa.org/packages/")
     ("gnu"      . "https://elpa.gnu.org/packages/")))
 ;; Priorities. Default priority is 0.
 '(package-archive-priorities
   '(("m-stable" . 10)
     ("melpa"    . 20))))

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
;; TODO: Do I really need ALL packages on fisrt run
;; on a new PC?
(setq use-package-always-ensure t)

(when (boundp 'emacs-debug-mode)
  (setq use-package-verbose emacs-debug-mode))

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
