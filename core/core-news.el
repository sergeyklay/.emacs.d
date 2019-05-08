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

(use-package bbdb
  :after (gnus message)
  :pin melpa
  :init
  (setq bbdb-file (concat user-etc-dir "contacts.bbdb/")
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
  (message "Initialized"))

(use-package bbdb-gnus
  :ensure bbdb
  :config
  (defun eh-bbdb-insinuate-gnus ()
    "BBDB setting for gnus, See `bbdb-insinuate-gnus' for details."
    (define-key gnus-summary-mode-map ":" 'bbdb-mua-display-sender)
    (define-key gnus-article-mode-map ":" 'bbdb-mua-display-sender)
    (define-key gnus-summary-mode-map ";" 'bbdb-mua-edit-field)
    (define-key gnus-article-mode-map ";" 'bbdb-mua-edit-field))

  (add-hook 'gnus-startup-hook 'eh-bbdb-insinuate-gnus))

(use-package counsel-bbdb
  :after (bbdb counsel))

(provide 'news)
;;; news.el ends here
