;;; security.el --- Security related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; GPG and security related features for GNU Emacs.

;;; Code:

(require 'directories)

(defconst gpg-executable-path (executable-find "gpg")
  "The gpg executable path on this system.")

(defconst pass-executable-path (executable-find "pass")
  "The pass executable path on this system.")

;;;; EasyPG

(use-package epg
  :ensure nil
  :if gpg-executable-path
  :custom
  (setq epg-gpg-program gpg-executable-path))

;;;; EasyPG Assistant

(use-package epa
  :ensure nil
  :if gpg-executable-path
  :after epg
  :init
  ;; For more see "man 1 gpg" for the option "--pinentry-mode"
  (unless (eq (window-system) 'w32)
    (setq epa-pinentry-mode 'loopback))
  :config
  ;; Enable automatic encryption/decryption of *.gpg files
  (unless (memq epa-file-handler file-name-handler-alist)
    (epa-file-enable)))

;;;; Pin Entry

;; For more see https://emacs.stackexchange.com/a/32882/16592
(use-package pinentry
  :init
  (setenv "INSIDE_EMACS" "t")
  :config
  (pinentry-start))

;;;; Auth Source

(use-package auth-source
  :if gpg-executable-path
  :init
  (add-to-list 'auth-sources (concat user-local-dir "etc/.authinfo.gpg")))

;;;; Password store

(use-package password-store
  :if (and gpg-executable-path pass-executable-path))

;; See https://www.passwordstore.org/
(use-package pass
  :after password-store
  :init
  (let ((passwd-dir (substitute-in-file-name "$HOME/.password-store")))
    (unless (file-exists-p passwd-dir)
      (make-directory passwd-dir t))))

(use-package auth-password-store
  :after pass
  :disabled t
  :config
  (auth-pass-enable))

(use-package ivy-pass
  :after password-store
  :requires ivy
  :commands (ivy-pass)
  :bind ("C-c p" . ivy-pass))

(provide 'security)
;;; security.el ends here
