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
(require 'gnus-msg)

;; I don't use (nnimap-user "xxx@gmail.com") here.
;; In a new enough Gnus version, this is solved by replacing "imap.gmail.com"
;; in .authinfo.gpg with the account name "persoanl", "work", etc.
(setq gnus-secondary-select-methods
      `((nnimap "personal"
                (nnimap-inbox "INBOX")
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stresm ssl)
                (nnimap-expunge t)
                (nnmail-expiry-target "nnimap+personal:[Gmail]/Trash")
                (nnmail-expiry-wait 30)
                (nnir-search-engine imap)
                (nnimap-authinfo-file
                 ,(concat user-local-dir "etc/.authinfo.gpg")))
        (nnimap "work"
                (nnimap-inbox "INBOX")
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stresm ssl)
                (nnimap-expunge t)
                (nnmail-expiry-target "nnimap+work:[Gmail]/Trash")
                (nnmail-expiry-wait 30)
                (nnir-search-engine imap)
                (nnimap-authinfo-file
                 ,(concat user-local-dir "etc/.authinfo.gpg")))))

;; TODO: unbreak (assumes that my personal email is set up)
;; Use gnus-cloud to sync private config, setup over IMAP
(setq gnus-cloud-method "personal")
(setq gnus-cloud-synced-files
      `(,gnus-init-file
        ,gnus-startup-file
        ,(concat user-local-dir "etc/.authinfo.gpg")
        ,(concat user-etc-dir "contacts.bbdb")
        (:directory "~/News" :match ".*.SCORE\\'")))

(setq mml-secure-openpgp-signers '("1E0B5331219BEA88")
      ;; I want to be able to read the emails I wrote.
      mml-secure-openpgp-encrypt-to-self t)

(setq gnus-posting-styles
      '(("nnimap\\+personal:.*"
         (name "Serghei Iakovlev")
         (address ,(my/get-gmail-imap-user "personal"))
         (signature-file ,(concat user-etc-dir "signature"))
         ;; Gmail does not require any handling for sent messages.  The server
         ;; will automatically save them to the Sent folder and that folder will
         ;; get synced locally through my email setup.
         (gcc nil)
         ("X-Message-SMTP-Method"
          (concat "smtp smtp.gmail.com 587 " (my/get-gmail-imap-user "personal"))))
        ("nnimap\\+work:.*"
         (name "Serghei Iakovlev")
         (address ,(my/get-gmail-imap-user "work"))
         (signature-file ,(concat user-etc-dir "signature"))
         (gcc nil)
         ("X-Message-SMTP-Method"
          (concat "smtp smtp.gmail.com 587 " (my/get-gmail-imap-user "work"))))))

(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
     (setq gnus-server-alist
           '(("archive" nnfolder "archive"
              (nnfolder-get-new-mail nil)
              (nnfolder-inhibit-expiry t))))

     (setq gnus-topic-topology '(("Gnus" visible)
                                 (("Personal" visible nil nil))
                                 (("Work" visible nil nil))
                                 (("News" visible nil nil))
                                 (("Misc" visible nil nil))))

     ;; TODO: Make each email account's groups auto categorize under
     ;; it's corresponding topic
     (setq gnus-topic-alist
           '(("Personal"
              "nnimap+personal:INBOX"
              "nnimap+personal:[Gmail]/Important"
              "nnimap+personal:[Gmail]/Sent Mail"
              "nnimap+personal:[Gmail]/All Mail"
              "nnimap+personal:[Gmail]/Starred"
              "nnimap+personal:[Gmail]/Spam"
              "nnimap+personal:[Gmail]/Trash"
              "nnimap+personal:CICD"
              "nnimap+personal:Invoices"
              "nnimap+personal:Lists"
              "nnimap+personal:Meetings")
             ("Work"
              "nnimap+work:INBOX"
              "nnimap+work:[Gmail]/Important"
              "nnimap+work:[Gmail]/Sent Mail"
              "nnimap+work:[Gmail]/All Mail"
              "nnimap+work:[Gmail]/Starred"
              "nnimap+work:[Gmail]/Spam"
              "nnimap+work:[Gmail]/Trash"
              "nnimap+work:CICD"
              "nnimap+work:Digests"
              "nnimap+work:Meetings"
              "nnimap+work:Notes")
             ("Misc"
              "nnfolder+archive:sent.2019"
              "nndraft:drafts")
             ("News")
             ("Phalcon")
             ("Gnus")))

     (setq gnus-killed-list '("nnimap+personal:[Gmail]"
                              "nnimap+work:[Gmail]"))))

(defun my/gmail-move-to-trash ()
  "Move mails to trash using Google Mail.
The moving is definitely necessary for Gmail if you want to
actually delete mail and not just remove it from a a label."
  (interactive)
  (cond ((string-match "nnimap\\+personal" gnus-newsgroup-name)
         (gnus-summary-move-article nil "nnimap+personal:[Gmail]/Trash"))
        ((string-match "nnimap\\+work" gnus-newsgroup-name)
         (gnus-summary-move-article nil "nnimap+work:[Gmail]/Trash"))
        ((string-match "\\(drafts\\|queue\\|delayed\\)" gnus-newsgroup-name)
         (gnus-summary-move-article nil "mail.trash"))
        ;; just do a normal delete instead of a move since items in these
        ;; newsgroups aren't being tracked by Gmail, so we don't have to signal
        ;; to Gmail to REALLY delete them from all labels.  If we normal deleted
        ;; from Gmail labels, that would only "remove" the label, but keep it in
        ;; the "All Mail" folder in Gmail, thus for Gmail folders, you need to
        ;; "move" the message to the trash folder which signals to REALLY delete
        ;; them email to Gmail.
        (t (gnus-summary-delete-article nil))))

(defun my/gmail-archive ()
  "Archive the current or marked mails."
  (interactive)
  (cond ((string-match "nnimap\\+personal" gnus-newsgroup-name)
         (gnus-summary-move-article nil "nnimap+personal:[Gmail]/All Mail"))
        ((string-match "nnimap\\+work" gnus-newsgroup-name)
         (gnus-summary-move-article nil "nnimap+work:[Gmail]/All Mail"))
        (t (gnus-summary-move-article nil "mail.archive"))))

(defun my/gmail-report-spam ()
  "Report the current or marked mails as spam."
  (interactive)
    (cond ((string-match "nnimap\\+personal" gnus-newsgroup-name)
           (gnus-summary-move-article nil "nnimap+personal:[Gmail]/Spam"))
          ((string-match "nnimap\\+work" gnus-newsgroup-name)
           (gnus-summary-move-article nil "nnimap+work:[Gmail]/Spam"))
          (t (gnus-summary-move-article nil "mail.spam"))))

(defun my|gnus-summary-keys ()
  "Create two key bindings for my Gmail experience."
  (local-set-key "y" #'my/gmail-archive)
  (local-set-key "$" #'my/gmail-report-spam)
  (local-set-key "D" #'my/gmail-move-to-trash))

(add-hook 'gnus-summary-mode-hook #'my|gnus-summary-keys)

(provide 'news)
;;; news.el ends here
