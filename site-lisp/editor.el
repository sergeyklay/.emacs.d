;;; editor.el --- Editor related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021, 2022 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d

;; This file is NOT part of Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Features realted to the text editing.

;;; Code:

(require 'directories)

(setq indent-tabs-mode nil)

;; Remove all spaces at the end of lines.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Increase the warning threshold for big files
(setq large-file-warning-threshold (* 50 1024 1024))

;; Set the default width of fill mode to 80
(setq-default fill-column 80)

;; Visually indicate empty lines after the buffer end
(setq-default indicate-empty-lines t)

;; When Visual Line mode is enabled, ‘word-wrap’ is turned on in
;; this buffer, and simple editing commands are redefined to act on
;; visual lines, not logical lines.
(global-visual-line-mode)

(electric-pair-mode t)

;; Right margin bar.
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(use-package rainbow-delimiters)

;;;; Show Line Numbers

(use-package display-line-numbers
  :ensure nil
  :bind ("C-x t l" . display-line-numbers-mode)
  :init
  (setq display-line-numbers-type t
	display-line-numbers-width 4))

;;;; Save point position between sessions

(use-package saveplace
  :ensure nil
  :custom
  (save-place-file (concat user-cache-dir "places"))
  :config
  ;; Automatically save place in each file.
  (save-place-mode t))

;;;; Whitespace mode

(use-package whitespace
  :ensure nil
  :bind ("C-x t w" . #'whitespace-mode))

;;;; Refactor

;; Edit multiple regions in the same way simultaneously.
(use-package iedit
  :defer t
  :commands (iedit-mode iedit-rectangle-mode)
  :bind (("C-c r" . iedit-mode)))

;;;; Folding

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

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-mode-text nil)
  :config
  ;; Turn the delay on auto-reloading from 5 seconds down to 1 second.  We have
  ;; to do this before turning on `auto-revert-mode' for the change to take
  ;; effect, unless we do it through `customize-set-variable' (which is slow
  ;; enough to show up in startup profiling).
  (setq auto-revert-interval 1)
  (global-auto-revert-mode 1))

(provide 'editor)
;;; editor.el ends here
