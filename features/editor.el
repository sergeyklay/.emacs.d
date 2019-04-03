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

;; Show Line Numbers
(setq-default display-line-numbers-type 'visual
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)

(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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

;;; Folding

(use-package fold-this
  :ensure t
  :bind ("C-c C-f" . fold-this))

(defun my--move-text-internal (arg)
  "Move text ARG lines up or down."
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))

(defun my/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (my--move-text-internal arg))

(defun my/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (my--move-text-internal (- arg)))

(defun my/duplicate-line (arg)
  "Duplicate current line, ARG times leaving point in lower line.
This function is for interactive use only;"

  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol
         (save-excursion
           (beginning-of-line)
           (point)))
        eol)

    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count))))

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))

  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key [M-S-up] #'my/move-text-up)
(global-set-key [M-S-down] #'my/move-text-down)
(global-set-key [C-c d] #'my/duplicate-line)

(provide 'editor)
;;; editor.el ends here
