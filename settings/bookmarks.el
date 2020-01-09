;;; bookmarks.el --- Setting for bookmarks, recentf, etc. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setting for bookmarks, recentf, etc.

;;; Code:

(require 'directories)

(defun klay|common-recentf-hook ()
  "Common hook for function `recentf-mode'."
  (unless recentf-mode
    (recentf-mode)
    (recentf-track-opened-file)))

(use-package recentf
  :ensure nil
  ;; lazy load recentf
  :hook (find-file . klay|common-recentf-hook)
  :init
  (setq recentf-save-file (concat user-cache-dir "recentf")
        recentf-max-saved-items 200)
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude ".cache")
  (add-to-list 'recentf-exclude "[/\\]elpa/")
  (add-to-list 'recentf-exclude ".cask")
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

(provide 'bookmarks)
;;; bookmarks.el ends here
