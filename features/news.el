;;; news.el --- News/Email related features. -*- lexical-binding: t; -*-

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

(require 'core-news)
(require 'smtpmail)
(require 'nnir)

(setq gnus-secondary-select-methods
      '((nnimap "Personal"
                (nnimap-inbox "INBOX")
                (nnimap-user "sadhooklay@gmail.com")
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnimap-expunge t)
                (nnimap-split-methods default)
                (nnir-search-engine imap)
                (nnmail-expiry-wait 30)
                (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash"))

        (nntp "gmane"
              (nntp-address "news.gmane.org"))

        (nnml "")))

(setq nnmail-split-methods
      '(("mail.emacs" "^\\(To\\|From\\|Cc\\):.*emacs-devel@gnu\\.org.*")
        ("mail.forums" "^From:.*phosphorum@phalconphp\\.com.*")
        ("mail.misc" "")))

;; This tells Gnus to use the Gmail SMTP server. This
;; automatically leaves a copy in the Gmail Sent folder.
;; Also tell message mode to use SMTP.
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Gmail system labels have the prefix [Gmail], which matches
;; the default value of `gnus-ignored-newsgroups'.  That's why we
;; redefine it.
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq mml-secure-openpgp-signers '("1E0B5331219BEA88")
      ;; I want to be able to read the emails I wrote.
      mml-secure-openpgp-encrypt-to-self t)

;; Archive outgoing email in Sent folder on imap.gmail.com:
(setq gnus-message-archive-method '(nnimap "imap.gmail.com")
      gnus-message-archive-group "[Gmail]/Sent Mail")

;;
(defconst my-gmail-trash-newsgroup "nnimap+gmail:[Gmail]/Trash")

(defun my/gmail-move-to-trash ()
  "Move mails to trash using Google Mail."
  (interactive)
  (gnus-summary-move-article nil my-gmail-trash-newsgroup))

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
  (local-set-key "$" #'my/gmail-report-spam)
  (local-set-key "D" #'my/gmail-move-to-trash))

(add-hook 'gnus-summary-mode-hook #'my|gnus-summary-keys)

(provide 'news)
;;; news.el ends here
