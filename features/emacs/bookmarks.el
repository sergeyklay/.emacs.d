;;; bookmarks.el --- Setting for bookmarks, recentf, etc. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setting for bookmarks, recentf, etc.

;;; Code:

(defun my|common-recentf-hook ()
  "Common hook for function `recentf-mode'."
  (unless recentf-mode
    (recentf-mode)
    (recentf-track-opened-file)))

(use-package recentf
  :ensure nil
  ;; lazy load recentf
  :hook (find-file . my|common-recentf-mode)
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

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
