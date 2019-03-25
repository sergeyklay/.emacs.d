;;; windows.el --- Windows management features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Windows management features for the GNU Emacs.

;;; Code:

;; Convenient keybindings to resize windows.
;; For more see URL `https://www.emacswiki.org/emacs/WindowResize'

(bind-key "S-C-<left>"  'shrink-window-horizontally)
(bind-key "S-C-<right>" 'enlarge-window-horizontally)
(bind-key "S-C-<down>"  'shrink-window)
(bind-key "S-C-<up>"    'enlarge-window)

;; Whenever I split windows, I usually do so and also switch to the other
;; window as well, so might as well rebind the splitting key bindings to
;; do just that to reduce the repetition.

(defun my/vsplit-other-window ()
  "Split the window vertically and switch to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))

(defun my/hsplit-other-window ()
  "Split the window horizontally and switch to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

(bind-key "C-x 2" 'my/vsplit-other-window)
(bind-key "C-x 3" 'my/hsplit-other-window)

;;; Winner mode

;; Winner mode allows you to undo/redo changes to window changes
;; in GNU Emacs and allows you.

(use-package winner
  :config
  (winner-mode t)
  :bind (("M-s-<left>" . winner-undo)
         ("M-s-<right>" . winner-redo)))

;;; Transpose frame

;; Provides some interactive functions which allows users to transpose
;; windows arrangement in currently selected frame.

(use-package transpose-frame
  :bind ("C-c t" . transpose-frame))

;;; Winum

;; Navigate your windows and frames using numbers.
;; For more see URL `https://github.com/deb0ch/emacs-winum'

(use-package winum
  :config
  (progn
    (setq winum-auto-assign-0-to-minibuffer nil
          winum-ignored-buffers '(" *which-key*"))

    (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
    (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
    (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
    (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
    (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
    (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
    (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
    (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
    (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
    (define-key winum-keymap (kbd "M-9") 'winum-select-window-9)

    (winum-mode)

    (set-face-attribute 'winum-face nil :foreground "DeepPink" :weight 'bold)))

(provide 'windows)
;;; windows.el ends here
