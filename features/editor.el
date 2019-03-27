;;; editor.el --- Editor related editor. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Features related to the behavior of the editor.

;;; Code:

(setq indent-tabs-mode nil)

;; Make sure that there is one newline at the end of the file while saving,
;; also removes all spaces at the end of lines.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Increase the warning threshold for big files
(setq large-file-warning-threshold (* 50 1024 1024))

;;; Undo Tree

(use-package undo-tree
  :ensure t)

;;; Multiple cursors

;; We'll also need to 'require 'multiple-cusors' because of
;; `https://github.com/magnars/multiple-cursors.el/issues/105'
(use-package multiple-cursors
  :ensure t
  :config
  (setq mc/list-file (concat user-etc-dir "mc-lists.el"))
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-!"         . mc/mark-next-symbol-like-this)
         ("C-c d"       . mc/mark-all-dwim)))

;;; Edit With Emacs

;; Editing input boxes from Chrome/Firefox with Emacs. Pretty useful to keep all
;; significant text-writing on the web within Emacs.  I typically use this
;; with posts on GitHub, which has a post editor that overrides normal
;; Emacs key bindings with other functions.  As such, ~markdown-mode~ is used.
;;
;; For more see URL
;; `http://psung.blogspot.com.es/2009/05/using-itsalltext-with-emacsemacsclient.html'

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t)

  :config
  (add-to-list 'edit-server-url-major-mode-alist '("^stackoverflow" . markdown-mode))
  (add-to-list 'edit-server-url-major-mode-alist '("^github.com" . markdown-mode))
  (add-to-list 'edit-server-url-major-mode-alist '("^emacs\\.stackexchange" . markdown-mode))
  (add-to-list 'edit-server-url-major-mode-alist '("^unix\\.stackexchange" . markdown-mode))

  (setq edit-server-default-major-mode 'markdown-mode)
  (setq edit-server-new-frame nil))

;;; Editorconfig

;; Editorconfig is a configuration format for controlling the
;; text attributes for text files.  It is good to use with version
;; control, especially when contributors develop on different platforms.
;; For more see URL `https://editorconfig.org'

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;;; Whitespace mode

(use-package whitespace
  :bind ("<f10>" . whitespace-mode))

(provide 'editor)
;;; editor.el ends here
