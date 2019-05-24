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
;;
;; Currently used `gnus-topic-topology':
;;
;; Gnus
;;   Personal
;;   Phalcon
;;   Work
;;   News
;;   Misc

;;; Code:

(require 'core-news)
(require 'gnus-msg)

(setq gnus-secondary-select-methods
      (mapcar #'my-gmail-user-to-nnimap
              '("personal")))

;; TODO: unbreak (assumes that my personal email is set up)
(setq gnus-cloud-method "personal")

(setq mml-secure-openpgp-signers '("1E0B5331219BEA88")
      ;; I want to be able to read the emails I wrote.
      mml-secure-openpgp-encrypt-to-self t)

;; By default archive outgoing email in Sent Mail folder on imap.gmail.com
;; using personal mailbox.  This might be redefined for a particular
;; use case if needed arises
(setq gnus-message-archive-group
      '((".*" "nnimap+personal:[Gmail]/Sent Mail")))

(setq gnus-posting-styles
      '(("nnimap\\+personal:.*"
         (name (concat user-full-name))
         (address user-mail-address)
         (signature "Serghei")
         ;; Gmail does not require any handling for sent messages.  The server
         ;; will automatically save them to the Sent folder and that folder will
         ;; get synced locally through my email setup.
         (gcc nil)
         ("X-Message-SMTP-Method"
          (concat "smtp smtp.gmail.com 587 " user-mail-address)))))

(defun my/gmail-move-to-trash ()
  "Move mails to trash using Google Mail.
The moving is definitely necessary for Gmail if you want to
actually delete mail and not just remove it from a a label."
  (interactive)
  (cond ((string-match "nnimap\\+personal" gnus-newsgroup-name)
         (gnus-summary-move-article nil "nnimap+personal:[Gmail]/Trash"))
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
        (t (gnus-summary-move-article nil "mail.archive"))))

(defun my/gmail-report-spam ()
  "Report the current or marked mails as spam."
  (interactive)
    (cond ((string-match "nnimap\\+personal" gnus-newsgroup-name)
           (gnus-summary-move-article nil "nnimap+personal:[Gmail]/Spam"))
          (t (gnus-summary-move-article nil "mail.spam"))))

(defun my|gnus-summary-keys ()
  "Create two key bindings for my Gmail experience."
  (local-set-key "y" #'my/gmail-archive)
  (local-set-key "$" #'my/gmail-report-spam)
  (local-set-key "D" #'my/gmail-move-to-trash))

(add-hook 'gnus-summary-mode-hook #'my|gnus-summary-keys)

(provide 'news)
;;; news.el ends here
