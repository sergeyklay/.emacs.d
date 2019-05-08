;;; core-news.el --- News/Email related features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; News/Email related features for GNU Emacs.

;;; Code:

(require 'gnus-sum)
(require 'gnus-group)
(require 'gnus-start)
(require 'message)
(require 'smtpmail)
(require 'mml-sec)

(defun my/gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages."
  (interactive)
  (gnus-group-list-all-groups 5))

(defun my/gmail-archive ()
  "Archive the current or marked mails.
This moves them into the All Mail folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

(defun my/gmail-report-spam ()
  "Report the current or marked mails as spam.
This moves them into the Spam folder."
  (interactive)
  (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))

(defun my|gnus-summary-keys ()
  "Create two key bindings for my Gmail experience."
  (local-set-key "y" #'my/gmail-archive)
  (local-set-key "$" #'my/gmail-report-spam))

(defun my|common-message-hook ()
  "Common Gnus message hook."
  (setq fill-column 69)
  (turn-on-auto-fill))

(use-package gnus
  :ensure nil
  :init
  (setq gnus-select-method
        ;; This tells Gnus to get email from Gmail via IMAP.
        '(nnimap "gmail"
                 ;; it could also be imap.googlemail.com if that's your server.
                 (nnimap-address "imap.gmail.com")
                 (nnimap-server-port "imaps")
                 (nnimap-stream ssl))

        ;; This tells Gnus to use the Gmail SMTP server. This
        ;; automatically leaves a copy in the Gmail Sent folder.
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587

        ;; Tell message mode to use SMTP.
        message-send-mail-function 'smtpmail-send-it

        ;; Gmail system labels have the prefix [Gmail], which matches
        ;; the default value of gnus-ignored-newsgroups. That's why we
        ;; redefine it.
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"

        ;; I don't want local, unencrypted copies of emails I write.
        gnus-message-archive-group nil

        mml-secure-openpgp-signers '("DC898A5F")
        ;; I want to be able to read the emails I wrote.
        mml-secure-openpgp-encrypt-to-self t)
  :hook
  ((gnus-summary-mode . my|gnus-summary-keys)
   (message-mode . my|common-message-hook))
  :bind
  (:map gnus-group-mode-map
        ("o" . #'my/gnus-group-list-subscribed-groups)))

(use-package bbdb
  :after (gnus message)
  :pin melpa
  :init
  (setq bbdb-file (concat user-etc-dir "contacts.bbdb")
        bbdb-phone-style nil
        bbdb-pop-up-window-size 0.3
        bbdb-mua-pop-up-window-size 1.0
        bbdb-mua-update-interactive-p '(query . create)
        bbdb-message-all-addresses t
        bbdb-mua-summary-mark nil
        bbdb-completion-list t
        bbdb-complete-mail-allow-cycling t
        bbdb-layout 'multi-line
        bbdb-pop-up-layout 'multi-line
        bbdb-mua-pop-up nil
        bbdb-default-country "Ukraine")
  (bbdb-initialize))

(use-package bbdb-gnus
  :ensure bbdb
  :bind (:map gnus-summary-mode-map
         (":" . #'bbdb-mua-display-sender)
         (";" . #'bbdb-mua-edit-field)
         :map gnus-article-mode-map
         (":" . #'bbdb-mua-display-sender)
         (";" . #'bbdb-mua-edit-field)))

(use-package counsel-bbdb
  :after (bbdb counsel))

(provide 'core-news)
;;; core-news.el ends here
