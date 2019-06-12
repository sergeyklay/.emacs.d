;;; core-gpg.el --- GPG related setting. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; GPG and security related features for GNU Emacs.

;;; Code:

(require 'core-dirs)
(require 'epa-file)

;;;; EasyPG

(use-package epg
  :ensure nil
  :custom
  ;; GnuPG 2.2.x
  (setq epg-gpg-program "/usr/local/bin/gpg"))

;;;; EasyPG Assistant

(use-package epa
  :ensure nil
  :after epg
  :init
  ;; For more see "man 1 gpg2" for option "--pinentry-mode"
  (unless (eq (window-system) 'w32)
    (setq epa-pinentry-mode 'loopback))
  :config
  ;; Enable automatic encryption/decryption of *.gpg files
  (epa-file-enable))

;;;; Pin Entry

;; For more see https://emacs.stackexchange.com/a/32882/16592
(use-package pinentry
  :init
  (setenv "INSIDE_EMACS" "t")
  :config
  (pinentry-start))

;;;; Auth Source

(use-package auth-source
  :init
  (add-to-list 'auth-sources (concat user-local-dir "etc/.authinfo.gpg")))

;;;; Password store

(use-package password-store
  :if (executable-find "pass"))

;; See https://www.passwordstore.org/
(use-package pass
  :if (executable-find "pass")
  :after password-store
  :init
  (let ((passwd-dir (substitute-in-file-name "$HOME/.password-store")))
    (unless (file-exists-p passwd-dir)
      (make-directory passwd-dir t))))

(use-package auth-password-store
  :if (executable-find "pass")
  :disabled t
  :after (pass)
  :config
  (auth-pass-enable))

(use-package ivy-pass
  :if (executable-find "pass")
  :after password-store
  :requires ivy
  :commands (ivy-pass)
  :bind ("C-c p" . ivy-pass))

(use-package use-package-secrets
  :custom
  (use-package-secrets-default-directory user-etc-dir)
  :quelpa
  (use-package-secrets :repo "a13/use-package-secrets"
		       :fetcher github
		       :version original))

(provide 'core-gpg)
;;; core-gpg.el ends here
