;;; core-news.el --- News/Email configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Core News/Email configuration for GNU Emacs.

;;; Code:

(require 'core-dirs)
(require 'message)
(require 'gnus-start)
(require 'gnus-group)

(defconst my--gmail-group-name-map
  '(("\\(?:nnimap\\+\\w+:\\)INBOX" . "Inbox")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/All Mail" . "Archive")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Important" . "Important")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Notes" . "Notes")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Starred" . "Starred")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Sent Mail" . "Sent")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Spam" . "Spam")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Trash" . "Trash")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Important" . "Important")))

(defun my/gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages."
  (interactive)
  (gnus-group-list-all-groups 5))

(use-package gnus
  :ensure nil
  :defer t
  :commands gnus
  :hook
  (;; Use topics per default
   (gnus-group-mode . gnus-topic-mode))
  :custom
  (gnus-init-file (concat user-etc-dir "gnus.el"))
  (gnus-startup-file (concat user-etc-dir "newsrc"))
  ;; No primary server
  (gnus-select-method '(nnnil ""))

  ;; Archive outgoing email in Sent folder on imap.gmail.com
  ;; TODO: Use per account sent mail dir
  (gnus-message-archive-method '(nnimap "imap.gmail.com"))
  (gnus-message-archive-group "[Gmail]/Sent Mail")

  ;; M-x `gnus-find-new-newsgroups' to check for new newsgroups
  (gnus-check-new-newsgroups nil)
  ;; Use the cache to the full extent of the law.
  (gnus-use-cache t)

  ;; respect my--gmail-group-name-map
  (gnus-group-line-format "%M\ %S\ %p\ %P\ %5y:%B%(%uG%)\n")

  (gnus-always-read-dribble-file t)
  (gnus-save-newsrc-file nil)
  (gnus-read-newsrc-file nil)

  ;; Gmail system labels have the prefix [Gmail], which matches
  ;; the default value of `gnus-ignored-newsgroups'.  That's why we
  ;; redefine it.
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  :config
  (setq-default
   gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
   gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
   gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"

   gnus-article-browse-delete-temp t
   gnus-treat-strip-trailing-blank-lines 'last
   gnus-keep-backlog 'nil

   ;; Show more MIME-stuff
   gnus-mime-display-multipart-related-as-mixed t
   ;; Don't get the first article automatically
   gnus-auto-select-first nil
   smiley-style 'medium
   gnus-keep-backlog '0)

  (when window-system
    (setq-default
     gnus-sum-thread-tree-indent "  "
     gnus-sum-thread-tree-root "● "
     gnus-sum-thread-tree-false-root "◯ "
     gnus-sum-thread-tree-single-indent "◎ "
     gnus-sum-thread-tree-vertical "│"
     gnus-sum-thread-tree-leaf-with-other "├─► "
     gnus-sum-thread-tree-single-leaf "╰─► "))

  (defun gnus-user-format-function-G (arg)
    (let ((mapped-name
           (assoc-default
            gnus-tmp-group
            my--gmail-group-name-map
            'string-match)))
      (if (null mapped-name)
          gnus-tmp-group
        mapped-name)))
  :bind
  (:map gnus-group-mode-map
        ("o" . #'my/gnus-group-list-subscribed-groups)))

(use-package smtpmail
  :ensure nil
  :commands smtpmail-send-it
  :init
  ;; This tells Gnus to use the Gmail SMTP server.  This
  ;; automatically leaves a copy in the Gmail Sent folder.
  ;; Also tell message mode to use SMTP.
  (setq send-mail-function #'smtpmail-send-it)
  (setq message-send-mail-function #'smtpmail-send-it)
  :custom
  (smtpmail-starttls-credentials '(("smptmail.gmail.com" 587 nil nil)))
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587))

(defun my-gmail-user-to-nnimap (mailbox)
  "Return nnimap select method for specified MAILBOX."
  `(nnimap
     ,mailbox
     (nnimap-inbox "INBOX")
     (nnimap-address "imap.gmail.com")
     (nnimap-server-port 993)
     (nnimap-stresm ssl)
     (nnimap-expunge t)
     (nnmail-expiry-target
      (concat "nnimap+"
              ,mailbox
              ":[Gmail]/Trash"))
     (nnmail-expiry-wait 30)
     (nnir-search-engine imap)
     (nnimap-authinfo-file (concat user-local-dir "etc/.authinfo.gpg"))))

;; (defun my|common-message-hook ()
;;   "Common Gnus message hook."
;;   (unless (fboundp 'bbdb-com) (require 'bbdb-com))
;;   (setq fill-column 69)

;;   (auto-fill-mode t)
;;   (font-lock-mode t)
;;   (abbrev-mode t)
;;   (flyspell-mode 1)
;;   (local-set-key [(tab)] #'bbdb-complete-mail))

;; (defun my|common-article-hook ()
;;   "Common Gnus article hook."
;;   (setq gnus-visible-headers
;;           (concat "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:"
;;                   "\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus"))

;;   ;; Show the article headers in this order.
;;   (setq gnus-sorted-header-list
;;         '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
;;           "^Subject:" "^Date:" "^Gnus"))

;;   (gnus-article-highlight)
;;   (gnus-article-hide-headers-if-wanted)
;;   (article-emphasize))

;; (use-package gnus
;;   :hook
;;   ((message-mode . my|common-message-hook)
;;    (gnus-article-display . my|common-article-hook)
;;    ))

;; I'd like Gnus NOT to render HTML-mails
;; but show me the text part if it's available.
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext")
  (setq mm-text-html-renderer 'gnus-w3m))

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
  :config
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'message)
  :hook
  ((gnus-startup . bbdb-insinuate-gnus)
   (mail-setup . bbdb-define-all-aliases)))

(use-package counsel-bbdb
  :after (bbdb counsel))

(provide 'core-news)
;;; core-news.el ends here
