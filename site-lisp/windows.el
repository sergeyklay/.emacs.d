;;; windows.el --- Windows and frames related stuff. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Serghei Iakovlev <egrep@protonmail.ch>

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

;; Windows and frames related features for GNU Emacs.

;;; Code:

;; I prefer not to have any of the GUI elements.
;; This keeps the window clean. And a bit speed up loading.
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Set frame title.
(setq frame-title-format
          '(buffer-file-name "%f"
			     (dired-directory dired-directory "%b")))

(defun my/monitor-resolution (monitor)
  "Get current MONITOR resolution in pixels.
For non-window systems will return frame dimesion."
  (let* ((monitor-attrs (display-monitor-attributes-list))
         (num-displays (length monitor-attrs))
         (geometry (assq 'geometry (nth monitor monitor-attrs))))
    (if (< monitor num-displays)
        (list (nth 3 geometry)
              (nth 4 geometry))
      (error "Invalid monitor number: %d" monitor))))

(defun my/monitor-pixel-width (monitor)
  "Get current MONITOR width in pixels."
  (car (my/monitor-resolution monitor)))

(defun my/monitor-pixel-height (monitor)
  "Get current MONITOR height in pixels."
  (nth 1 (my/monitor-resolution monitor)))

(defun my/calibrate-frame-geometry()
  "Set the size and position of the main Emacs frame."
  (interactive)
  (let ((frame (selected-frame))
        width-in-pixels
        height-in-pixels
        top-in-pixels
        left-in-pixels
        (width-in-chars 158)
        (height-in-chars 50)
        (total-width (my/monitor-pixel-width 0))
        (total-height (my/monitor-pixel-height 0)))

    (cond ((<= total-height 640)
           (setq height-in-chars 34))
          ((<= total-height 800)
           (setq height-in-chars 40))
          ((<= total-height 900)
           (setq height-in-chars 50))
          ((<= total-height 1440)
           (setq height-in-chars 60))
          ((>= total-height 1692)
           (setq height-in-chars 80)))

    (when (<= total-width 1024)
      (setq width-in-chars 79))

    (setq width-in-pixels (* width-in-chars (frame-char-width)))
    (setq height-in-pixels (* height-in-chars (frame-char-height)))

    ;; Center frame.  22 here is a top menubar in Gnome, macOs and Ubuntu.
    (setq top-in-pixels (round (- (/ (- total-height height-in-pixels) 2) 22)))
    (setq left-in-pixels (round (/ (- total-width width-in-pixels) 2)))

    (set-frame-size frame width-in-chars height-in-chars)
    (set-frame-position frame left-in-pixels top-in-pixels)))

(defun my|frame-setup-hook()
  "Hook to set the size and position of the main Emacs frame."
  (let ((frame (selected-frame)))
    ;; For terminal frame the height in pixels is always 1.
    ;; Do now change geometry for terminal frames.
    (when (> (frame-char-height) 1)
      (my/calibrate-frame-geometry))
    (select-frame-set-input-focus frame)))

(add-hook 'window-setup-hook #'my|frame-setup-hook)

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

(use-package ace-window
  :defer t
  :hook
  ((after-init . winner-mode))
  :bind
  (("M-o" . ace-window)))

(provide 'windows)
;;; windows.el ends here
