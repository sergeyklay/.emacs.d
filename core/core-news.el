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
(require 'gnus-cloud)
(require 'gnus-art)
(require 'nnfolder)
(require 'nndraft)

(defconst my--gmail-group-name-map
  '(("\\(?:nnimap\\+\\w+:\\)INBOX" . "Inbox")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/All Mail" . "Archive")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Important" . "Important")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Notes" . "Notes")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Starred" . "Starred")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Sent Mail" . "Sent")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Spam" . "Spam")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Trash" . "Trash")
    ("\\(?:nnimap\\+\\w+:\\)\\[Gmail\\]/Important" . "Important")
    ("\\(?:nnimap\\+\\w+:\\)Meetings" . "Forums")
    ("\\(?:nnimap\\+\\w+:\\)Lists" . "Lists")
    ("\\(?:nnimap\\+\\w+:\\)Invoices" . "Invoices")
    ("\\(?:nnimap\\+\\w+:\\)CI & CD" . "CI/CD")
    ("nndraft:drafts" . "Drafts")))

(defun my/gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages."
  (interactive)
  (gnus-group-list-all-groups 5))

(defun my|common-message-hook ()
  "Common Gnus message hook."
  (setq fill-column 69)

  (auto-fill-mode t)
  (font-lock-mode t)
  (abbrev-mode t)
  (flyspell-mode 1))

(defun my|bbdb-message-hook ()
  "Auto-complete Emacs address using bbdb UI."
  (unless (fboundp 'bbdb-com) (require 'bbdb-com))

  (bbdb-initialize 'message)
  (bbdb-initialize 'gnus)

  (local-set-key [(tab)] #'bbdb-complete-mail))

(defun my|common-article-hook ()
  "Common Gnus article hook."
  (setq gnus-visible-headers
        (concat "^From:\\|^Reply-To\\|^Organization:\\|"
                "^To:\\|^Cc:\\|^[BGF]?Cc:\\|"
                "^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus"))

  ;; Show the article headers in this order.
  (setq gnus-sorted-header-list
        '("^From:" "^Reply-To" "^Organization:"
          "^To:" "^Cc:" "^[BGF]?Cc:"
          "^Newsgroups:" "^Subject:" "^Date:" "^Gnus"))

  (gnus-article-highlight)
  (gnus-article-hide-headers-if-wanted)
  (article-emphasize))

(defun my|exit-gnus-on-exit ()
  "Exit Gnus on exit Emacs."
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))

(use-package gnus
  :ensure nil
  :defer t
  :commands gnus
  :hook
  ((gnus-group-mode . gnus-topic-mode) ; Use topics per default
   (message-mode . my|common-message-hook)
   (gnus-article-display . my|common-article-hook)
   (kill-emacs . my|exit-gnus-on-exit))
  :custom
  (gnus-init-file (concat user-etc-dir "gnus.el"))
  (gnus-startup-file (concat user-etc-dir "newsrc"))

  ;; Directories
  (gnus-home-directory user-local-dir)
  (gnus-directory (concat user-local-dir "news/"))
  (gnus-article-save-directory (concat user-local-dir "news/"))
  (gnus-kill-files-directory (concat user-local-dir "news/"))
  (gnus-cache-directory (concat user-cache-dir "gnus/"))
  (gnus-dribble-directory (concat user-cache-dir "gnus/"))

  ;; Update any active summary buffers automatically
  ;; first before exiting
  (gnus-interactive-exit 'quiet)

  ;; No primary server
  (gnus-select-method '(nnnil ""))

  ;; Fetch old headers to build complete thread
  (gnus-fetch-old-headers 'some)

  (gnus-gcc-mark-as-read t)

  ;; M-x `gnus-find-new-newsgroups' to check for new newsgroups
  (gnus-check-new-newsgroups nil)
  ;; Use the cache to the full extent of the law.
  (gnus-use-cache t)

  ;; respect my--gmail-group-name-map
  (gnus-group-line-format "%M\ %S\ %p\ %P\ %5y:%B%(%uG%)\n")

  (gnus-always-read-dribble-file t)
  (gnus-save-newsrc-file nil)
  (gnus-read-newsrc-file nil)

  ;; Fetch articles while reading other articles
  (gnus-asynchronous t)

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

  (setq nnfolder-directory (concat user-local-dir "mail/archive/"))
  (setq nndraft-directory (concat user-local-dir "mail/drafts/"))

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

;; Create all necessary directories
(defconst my--gnus-directories
  '(gnus-directory
    gnus-article-save-directory
    gnus-kill-files-directory
    gnus-cache-directory
    gnus-dribble-directory
    nnfolder-directory
    nndraft-directory))

(dolist (directory my--gnus-directories)
  (let ((dir-value (symbol-value directory)))
    (unless (file-exists-p dir-value)
      (make-directory dir-value t))))

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
  (smtpmail-starttls-credentials '(("smpt.gmail.com" 587 nil nil)))
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
    (nnmail-expiry-target (concat "nnimap+" ,mailbox ":[Gmail]/Trash"))
    (nnmail-expiry-wait 30)
    (nnir-search-engine imap)
    (nnimap-authinfo-file (concat user-local-dir "etc/.authinfo.gpg"))))

;; I'd like Gnus NOT to render HTML-mails
;; but show me the text part if it's available.
(with-eval-after-load "mm-decode"
  (setq mm-discouraged-alternatives
        '("application/msword"
          "text/richtext"
          "text/html"))

  (setq mm-text-html-renderer 'gnus-w3m))

(add-hook 'gnus-summary-exit-hook #'gnus-summary-bubble-group)

;; BBDB: Address list
(use-package bbdb
  :after (gnus message)
  :init
  (setq
   ;; File where things will be saved
   bbdb-file (concat user-etc-dir "contacts.bbdb")
   bbdb-phone-style nil
   ;; Size of the bbdb popup
   bbdb-pop-up-window-size 0.3
   bbdb-mua-pop-up-window-size 1.0
   ;; What do we do when invoking bbdb interactively
   bbdb-mua-update-interactive-p '(query . create)
   bbdb-message-all-addresses t
   bbdb-mua-summary-mark nil
   bbdb-completion-list t
   bbdb-complete-mail-allow-cycling t
   bbdb-layout 'multi-line
   bbdb-pop-up-layout 'multi-line
   bbdb-mua-pop-up nil)
  :config
  (bbdb-initialize 'gnus 'message)
  (bbdb-mua-auto-update-init 'gnus 'message)
  :hook
  ((gnus-startup . bbdb-insinuate-gnus)
   (mail-setup . bbdb-define-all-aliases)
   (message-mode . my|bbdb-message-hook)))

(use-package counsel-bbdb
  :after (bbdb counsel))

(provide 'core-news)
;;; core-news.el ends here
