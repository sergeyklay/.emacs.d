;;; setup-tags.el --- Setup tags. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setup tags.

;;; Code:

;;;; Constants

(defconst global-executable-path (executable-find "global")
  "The global executable path on this system.")

(defconst gtags-executable-path (executable-find "gtags")
  "The gtags executable path on this system.")

;; Warn if GNU GLOBAL is not installed, we'll still compile but
;; these functions won't work.
(unless (and global-executable-path gtags-executable-path)
  (warn "Cannot find necessary installation of GNU GLOBAL"))

;;;; Etags

(use-package etags
  :ensure nil
  :defer t
  :custom
  ;; Make Emacs reload the TAGS file automatically.
  (tags-revert-without-query 1)
  ;; Whether tags operations should be case-sensitive.
  (tags-case-fold-search nil)
  ;; Never “Keep current list of tags tables also”.
  (tags-add-tables nil)
  :bind (("M-," . pop-tag-mark)))

;;;; Ggtags

;; A front-end for accessing the gtags-generated tags.
;; For more see URL `https://github.com/leoliu/ggtags'

(use-package ggtags
  :if global-executable-path
  :commands (ggtags-mode
	     ggtags-build-imenu-index
	     ggtags-eldoc-function
	     pop-tag-mark)
  :custom
  ;; Auto-pdate GTAGS on each save.
  (ggtags-update-on-save t)
  ;; Auto-highlight tag at point
  (ggtags-highlight-tag t)
  ;; Enabling nearness requires global 6.5.+
  (ggtags-sort-by-nearness t)
  ;; The over size limit for the  GTAGS file.
  (ggtags-oversize-limit (* 100 1024 1024))
  ;; Generate the idutils DB.
  (ggtags-use-idutils t)
  :bind (("M-]"     . ggtags-idutils-query)

	 :map ggtags-mode-map
	 ("C-c g s" . ggtags-find-other-symbol)
	 ("C-c g h" . ggtags-view-tag-history)
	 ("C-c g r" . ggtags-find-reference)
	 ("C-c g f" . ggtags-find-file)
	 ("C-c g c" . ggtags-create-tags)
	 ("C-c g u" . ggtags-update-tags)
	 ("M-."     . ggtags-find-tag-dwim)
	 ("C-c <"   . ggtags-prev-mark)
	 ("C-c >"   . ggtags-next-mark)

	 :map ggtags-navigation-map
	 ("M-o"     . ggtags-navigation-next-file)
	 ("M-l"     . ggtags-navigation-visible-mode)))

(provide 'setup-tags)
;;; setup-tags.el ends here
