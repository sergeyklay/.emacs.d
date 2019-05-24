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

;; Used `gnus-topic-topology':
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

;; Fallback group.
(setq gnus-message-archive-group
      '((".*" "nnimap+personal:[Gmail]/Sent Mail")))

(setq gnus-posting-styles
      '(("nnimap\\+personal:.*"
         (name (concat user-full-name))
         (address user-mail-address)
         (signature "Serghei")
         ;; Archive outgoing email in Sent folder on imap.gmail.com
         (gcc "nnimap+personal:[Gmail]/Sent Mail")
         ("X-Message-SMTP-Method"
          (concat "smtp smtp.gmail.com 587 " user-mail-address)))))

;; ;; (defconst my-gmail-spam-folder "nnimap+gmail:[Gmail]/Spam")
;; ;; (defconst my-gmail-all-mail-folder "nnimap+gmail:[Gmail]/All Mail")

;; ;; (defun my/gmail-move-to-trash ()
;; ;;   "Move mails to trash using Google Mail."
;; ;;   (interactive)
;; ;;   (gnus-summary-move-article nil my-gmail-trash-folder))

;; ;; (defun my/gmail-archive ()
;; ;;   "Archive the current or marked mails.
;; ;; This moves them into the All Mail folder."
;; ;;   (interactive)
;; ;;   (gnus-summary-move-article nil my-gmail-all-mail-folder))

;; ;; (defun my/gmail-report-spam ()
;; ;;   "Report the current or marked mails as spam.
;; ;; This moves them into the Spam folder."
;; ;;   (interactive)
;; ;;   (gnus-summary-move-article nil my-gmail-spam-folder))

;; ;; (defun my|gnus-summary-keys ()
;; ;;   "Create two key bindings for my Gmail experience."
;; ;;   (local-set-key "y" #'my/gmail-archive)
;; ;;   (local-set-key "$" #'my/gmail-report-spam)
;; ;;   (local-set-key "D" #'my/gmail-move-to-trash))

;; ;; (add-hook 'gnus-summary-mode-hook #'my|gnus-summary-keys)

(provide 'news)
;;; news.el ends here
