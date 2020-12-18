;;; windows.el --- Windows and frames related stuff. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

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
        width height top left
        width-factor
        height-factor
        (total-width (my/monitor-pixel-width 0))
        (total-height (my/monitor-pixel-height 0)))

    (cond ((= total-width 1024)       ; 1024x640
           (setq width-factor 1.82)
           (setq height-factor 1.43))
          ((= total-width 1280)       ; 1280x800
           (setq width-factor 1.12)
           (setq height-factor 1.43))
          ((= total-width 1440)       ; 1440x900
           (setq width-factor 1.26)
           (setq height-factor 1.43))
          ((= total-width 1680)       ; 1680x1050
           (setq width-factor 1.47)
           (setq height-factor 1.43))
          ((= total-width 1920)       ; 1920x1080
           (setq width-factor 1.52)
           (setq height-factor 1.43))
          (t                          ; Retina
           (setq width-factor 1.30)
           (setq height-factor 1.43)))

    (setq width (round (/ total-width width-factor)))
    (setq height (round (/ total-height height-factor)))

    ;; Center frame.  22 here is a top menubar in Gnome, macOs and Ubuntu.
    (setq top (round (- (/ (- total-height height) 2) 22)))
    (setq left (round (/ (- total-width width) 2)))

    (set-frame-size frame width height t)
    (set-frame-position frame left top)))

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
