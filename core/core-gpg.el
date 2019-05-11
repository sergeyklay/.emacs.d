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

;;;; EasyPG Assistant

(use-package epa
  :ensure nil
  :init
  ;; non-GUI password dialog
  (setenv "GPG_AGENT_INFO" nil)
  ;; For more see "man 1 gpg2" for option "--pinentry-mode"
  (unless (eq (window-system) 'w32)
    (setq epa-pinentry-mode 'loopback))
  :config
  ;; Enable automatic encryption/decryption of *.gpg files
  (epa-file-enable))

;;;; Pin Entry

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

(use-package pass
  :if (executable-find "pass"))

(use-package password-store
  :if (executable-find "pass"))

(use-package auth-password-store
  :if (executable-find "pass")
  :disabled t
  :after (pass)
  :config
  (auth-pass-enable))

(provide 'core-gpg)
;;; core-gpg.el ends here
