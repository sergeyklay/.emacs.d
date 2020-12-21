;;; security.el --- Security related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d

;; This file is NOT part of Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; GPG and security related features for GNU Emacs.

;;; Code:

(require 'directories)

;;;; SSH / GnuPG

(defvar my/gpg-ssh-auth-sock-set nil
  "Non-nil if a SSH auth sock by the GPG agent is set.")

;; Copyright (c) 2018 Akira Komamura
(defun my|ensure-gpg-ssh-auth-sock-hook ()
  "Ensure a SSH auth sock by the GPG agent is set."
  (unless my/gpg-ssh-auth-sock-set
    (message "Ensure a SSH auth sock by the GPG agent is set...")
    (let ((agent-buffer (generate-new-buffer "*gpg-connect-agent*")))
      (unless (= 0 (call-process "gpg-connect-agent"
                                 nil
                                 agent-buffer
                                 nil
                                 "/bye"))
        (display-buffer agent-buffer)
        (error "Filed to run 'gpg-connect-agent /bye'"))
      (if-let (sock (car (process-lines "gpgconf" "--list-dirs"
                                        "agent-ssh-socket")))
          (if (file-exists-p sock)
              (setq my/gpg-ssh-auth-sock-set
                    (setenv "SSH_AUTH_SOCK" sock))
            (error "Invalid socket: %s" sock))
        (error "The gpgconf didn't return an agent socket")))))

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
    (custom-set-variables '(epg-pinentry-mode 'loopback)))
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

(provide 'security)
;;; security.el ends here
