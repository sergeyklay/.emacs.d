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
(require 'gnus-start)
(require 'gnus-art)
(require 'gnus-group)
(require 'nnrss)

(defun my/gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages."
  (interactive)
  (gnus-group-list-all-groups 5))

(defun my|common-message-hook ()
  "Common Gnus message hook."
  (unless (fboundp 'bbdb-com) (require 'bbdb-com))
  (setq fill-column 69)

  (auto-fill-mode t)
  (font-lock-mode t)
  (abbrev-mode t)
  (flyspell-mode 1)
  (local-set-key [(tab)] #'bbdb-complete-mail))

(defun my|common-article-hook ()
  "Common Gnus article hook."
  (setq gnus-visible-headers
          (concat "^From:\\|^Reply-To\\|^Organization:\\|^To:\\|^Cc:"
                  "\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Gnus"))

  ;; Show the article headers in this order.
  (setq gnus-sorted-header-list
        '("^From:" "^Reply-To" "^Organization:" "^To:" "^Cc:" "^Newsgroups:"
          "^Subject:" "^Date:" "^Gnus"))
  (gnus-article-highlight)
  (gnus-article-hide-headers-if-wanted)
  (article-emphasize))

(use-package gnus
  :ensure nil
  :defer t
  :commands gnus
  :config
  (progn
    ;; No primary server
    (setq gnus-select-method '(nnnil ""))
    (setq gnus-startup-file (concat user-etc-dir "newsrc"))
    ;; Do not store local, unencrypted copies of emails.
    (setq gnus-message-archive-group nil)

    (setq-default
       gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B (%c) %s%)\n"
       gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
       gnus-group-line-format "%M%S%p%P%5y:%B %G\n"
       gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
       gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
       gnus-sum-thread-tree-false-root ""
       gnus-sum-thread-tree-indent " "
       gnus-sum-thread-tree-leaf-with-other "├─ "
       gnus-sum-thread-tree-root ""
       gnus-sum-thread-tree-single-leaf "└─ "
       gnus-sum-thread-tree-vertical "│"
       gnus-article-browse-delete-temp t
       gnus-treat-strip-trailing-blank-lines 'last
       gnus-keep-backlog 'nil
       gnus-summary-display-arrow nil ; Don't show that annoying arrow:
       gnus-mime-display-multipart-related-as-mixed t ; Show more MIME-stuff:
       gnus-auto-select-first nil ; Don't get the first article automatically:
       smiley-style 'medium
       gnus-keep-backlog '0))
  :hook
  ((message-mode . my|common-message-hook)
   (gnus-group-mode . gnus-topic-mode)
   (gnus-article-display . my|common-article-hook))
  :bind
  (:map gnus-group-mode-map
        ("o" . #'my/gnus-group-list-subscribed-groups)))


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
  (bbdb-initialize)
  :hook
  ((gnus-startup . bbdb-insinuate-gnus)
   (mail-setup . bbdb-define-all-aliases)))

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
