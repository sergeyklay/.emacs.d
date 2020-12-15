;;; setup-buffer.el --- Buffer related configuration and utils.. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Buffer related configuration and utils.

;;; Code:

(eval-when-compile
  (require 'recentf))

(defun my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun my/kill-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode
                    (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun my/undo-kill-buffer (arg)
  "Re-open the last buffer killed.
With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
	 (buffer-files-list
	  (delq nil (mapcar (lambda (buf)
			      (when (buffer-file-name buf)
				(expand-file-name (buffer-file-name buf))))
			    (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
	     (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun my/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm t))

(provide 'setup-buffer)
;;; setup-buffer.el ends here
