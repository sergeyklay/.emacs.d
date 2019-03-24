;;; fivy.el --- Initialize ivy. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; This frature adds Ivy, a completion backend for GNU Emacs.

;;; Code:

;;; Ivy

(use-package ivy
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  (setq
   ;; Number of lines for the minibuffer window
   ivy-height 12
   ;; do not set `completion-in-region-function'
   ivy-do-completion-in-region nil
   ;; wrap around after the first and the last candidate
   ivy-wrap t
   ;; fix the height of the minibuffer during ivy completion
   ivy-fixed-height-minibuffer t
   ;; add recent files and bookmarks to `ivy-switch-buffer'
   ivy-use-virtual-buffers t
   ;; Don't use ^ as initial input
   ivy-initial-inputs-alist nil
   ;; highlight til EOL
   ivy-format-function #'ivy-format-function-line
   ;; allow minibuffer commands while in the minibuffer
   enable-recursive-minibuffers t
   ;; disable magic slash on non-match
   ivy-magic-slash-non-match-action nil))

(global-set-key (kbd "C-c C-r") 'ivy-resume)

(provide 'fivy)
;;; fivy.el ends here
