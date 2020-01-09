;;; appearance.el --- All appearance related setting. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; This file should contain all appearance related settings.

;;; Code:

;; Throw away the mouse when typing.
;; Move the mouse to the corner only if the cursor gets too close,
;; and allow it to return once the cursor is out of the way.
(mouse-avoidance-mode 'exile)

;; Don't beep at me.
(setq visible-bell t)

;; Just blink the modeline on errors.
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; The variable `redisplay-dont-pause', when set to non-nil,
;; will cause Emacs to fully redraw the display before it
;; processes queued input events.  For more see
;; URL: https://lists.gnu.org/archive/html/emacs-devel/2011-09/msg00350.html
(setq redisplay-dont-pause t)

;; Pretify page breaks.
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; For more see URL `http://ergoemacs.org/emacs/emacs_list_and_set_font.html'
(cond
 ((string-equal system-type "gnu/linux")
  (when (member "Source Code Pro" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Source Code Pro-10"))
    (add-to-list 'default-frame-alist '(font . "Source Code Pro-10"))))
 ((string-equal system-type "darwin")
  (when (member "Source Code Pro" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Source Code Pro-13"))
    (add-to-list 'default-frame-alist '(font . "Source Code Pro-13")))))

(use-package unicode-fonts)

(add-to-list 'face-font-rescale-alist '(".*icons.*" . 0.9))
(add-to-list 'face-font-rescale-alist '(".*FontAwesome.*" . 0.9))

;; Change default line spacing
(setq-default line-spacing 0.24)

(defun klay/toggle-line-spacing ()
  "Toggle `line-spacing' value between 0 and 0.24."
  (interactive)
  (cond ((eq line-spacing nil)
	 (setq line-spacing 0.24))
	((= line-spacing 0)
	 (setq line-spacing 0.24))
	(t
	 (setq line-spacing 0))))

;;; Theme

;; I tend to switch themes more often than normal.  For example,
;; switching to a lighter theme (such as the default) or to a
;; different theme depending on the time of day or my mood.  Normally,
;; switching themes is a multi-step process with `disable-theme' and
;; `load-theme'.  The `switch-theme' function will do that in one
;; swoop.  I just choose which theme I want to go to.
(defun klay/switch-theme (theme)
  "Disable any currently active themes and load the THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
			     (mapc 'symbol-name
				   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun klay/disable-active-themes ()
  "Disable any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(bind-key "<f12>" 'klay/switch-theme)
(bind-key "<f11>" 'klay/disable-active-themes)

;; Steve Purcell's Tomorrow theme

(use-package nord-theme
  :config
  (when (not (display-graphic-p))
    (klay/switch-theme 'nord)))

;; One Dark Theme

(use-package one-themes
  :config
  (when (display-graphic-p)
    (klay/switch-theme 'one-dark)))

;;; Modeline

;; Activate column and line number in the modeline.
(column-number-mode t)
(line-number-mode t)

(provide 'appearance)
;;; appearance.el ends here
