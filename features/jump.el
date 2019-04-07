;;; jump.el --- Emacs configuration for tags. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Tags realated features GNU Emacs.

;;; Code:

(require 'config)

(defvar gtags-enable-by-default t
  "Whether or not to enable ggtags-mode.")

;; A front-end for accessing the gtags-generated tags.
;; For more see URL `https://github.com/leoliu/ggtags'
(use-package ggtags
    :if (and global-found gtags-enable-by-default)
    :bind
    (:map ggtags-mode-map
          ("C-c g s" . 'ggtags-find-other-symbol)
          ("C-c g h" . 'ggtags-view-tag-history)
          ("C-c g r" . 'ggtags-find-reference)
          ("C-c g f" . 'ggtags-find-file)
          ("C-c g c" . 'ggtags-create-tags)
          ("C-c g u" . 'ggtags-update-tags)
          ("M-."     . 'ggtags-find-tag-dwim)
          ("M-,"     . 'pop-tag-mark)
          ("M-]"     . nil)
          ("C-c <"   . 'ggtags-prev-mark)
          ("C-c >"   . 'ggtags-next-mark)))

;; Make Emacs reload the TAGS file automatically
(setq tags-revert-without-query 1)

;; t=case-insensitive, nil=case-sensitive
(setq tags-case-fold-search nil)

;; Never “Keep current list of tags tables also”
(setq tags-add-tables nil)

(defun my/ggtags-mode-enable ()
  "Enable ggtags and eldoc mode.

For eldoc, ggtags advises the eldoc function at the lowest priority
so that if the major mode has better support it will use it first."
  (when (and global-found gtags-enable-by-default)
    (ggtags-mode 1)
    (eldoc-mode 1)))

(provide 'jump)
;;; jump.el ends here
