;;; editor.el --- Editor related editor. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
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

;; Set the default width of fill mode to 80
(setq-default fill-column 80)

;; Visually indicate empty lines after the buffer end
(setq-default indicate-empty-lines t)

;; Enable line highlight mode everywhere
(global-hl-line-mode 1)

;; When Visual Line mode is enabled, ‘word-wrap’ is turned on in
;; this buffer, and simple editing commands are redefined to act on
;; visual lines, not logical lines.
(global-visual-line-mode)

(electric-pair-mode t)

;; Show Line Numbers
(use-package display-line-numbers
  :ensure nil
  :defer t
  :bind ("C-x t l" . #'display-line-numbers-mode)
  :init
  (progn
    (setq display-line-numbers-type t
          display-line-numbers-width 4)

    (my/add-to-hooks
     #'display-line-numbers-mode
     '(conf-mode-hook
       text-mode-hook
       prog-mode-hook))))

;; Undo Tree
(use-package undo-tree)

;; Editorconfig
;;
;; Editorconfig is a configuration format for controlling the
;; text attributes for text files.  It is good to use with version
;; control, especially when contributors develop on different platforms.
;; For more see URL `https://editorconfig.org'
(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; Whitespace mode
(use-package whitespace
  :bind ("C-x t w" . #'whitespace-mode))

;; Folding
(use-package hideshow
  :diminish hs-minor-mode
  :preface
  (defun my/toggle-fold ()
    "Toggle hiding/showing of a block."
    (interactive)
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding)))
  :bind ("C-x t f" . my/toggle-fold))

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
          (when (< arg 0)
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
  (forward-line arg))

(global-set-key [M-S-up] #'my/move-text-up)
(global-set-key [M-S-down] #'my/move-text-down)
(global-set-key (kbd "C-c d") #'my/duplicate-line)

(provide 'editor)
;;; editor.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
