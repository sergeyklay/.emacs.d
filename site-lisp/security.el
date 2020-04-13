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

;;;; EasyPG

(use-package epg
  :ensure nil
  :custom
  ;; GPG 2 actually
  (epg-gpg-program "gpg"))

;;;; EasyPG Assistant

(use-package epa
  :ensure nil
  :after epg
  :init
  ;; For more see "man 1 gpg" for the option "--pinentry-mode"
  (unless (eq (window-system) 'w32)
    (if (version< emacs-version "27")
	(custom-set-variables
	 '(epa-pinentry-mode 'loopback))
      (custom-set-variables
	 '(epg-pinentry-mode 'loopback))))
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
  :ensure nil
  :custom
  (auth-sources
   `(,(concat user-local-dir "etc/.authinfo.gpg")
     "~/.authinfo" "~/.authinfo.gpg")))

;;;; Password store

;; I use 'pass' on all my machines, thus I prefer avoid here
;; any check for speed Emacs load reasons.
(use-package password-store
  :defer 5
  :commands (password-store-insert
             password-store-copy
             password-store-get))

;; See https://www.passwordstore.org/
(use-package pass
  :commands (pass pass-view-mode)
  :mode ("\\.passwords/.*\\.gpg\\'" . pass-view-mode)
  :init
  (let ((passwd-dir "~/.password-store"))
    (unless (file-exists-p passwd-dir)
      (make-directory passwd-dir t))))

(use-package auth-password-store
  :after pass
  :disabled t
  :config
  (auth-pass-enable))

(use-package helm-pass
  :ensure helm
  :after password-store
  :commands helm-pass
  :bind ("C-c p p" . helm-pass))

(provide 'security)
;;; security.el ends here
