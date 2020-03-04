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

(use-package leuven-theme
  :custom
  (leuven-scale-outline-headlines nil)
  (leuven-scale-org-agenda-structure nil))

(defconst my/font-mono-linux "Fira Code"
  "The default monospace typeface to use in Linux.")

(defconst my/font-mono-darwin "Source Code Pro"
  "The default monospace typeface to use in macOS.")

(defconst my/font-mono-params ":hintstyle=hintfull"
  "Fontconfig parameters for the monospaced  typeface.")

(defun my/font-family-size (family size)
  "Set frame font to FAMILY at SIZE."
  (when (member family (font-family-list))
    (set-frame-font
     (concat family "-" (number-to-string size) my/font-mono-params) t t)))

(defun my/fonts-linux ()
  "Pass desired argument to `my/font-family-size' for use on Linux."
  (interactive)
  (when window-system
    (my/font-family-size my/font-mono-linux 11)))

(defun my/fonts-darwin ()
  "Pass desired argument to `my/font-family-size' for use on macOS."
  (interactive)
  (when window-system
    (my/font-family-size my/font-mono-darwin 13)))

(defun my/fonts-setup ()
  "Choose between `my/fonts-linux' and `my/fonts-darwin' based on the OS."
  (interactive)
  (when window-system
    (cond
     ((string-equal system-type "gnu/linux")
      (my/fonts-linux))
     ((string-equal system-type "darwin")
      (my/fonts-darwin)))
    (add-to-list 'face-font-rescale-alist '(".*icons.*" . 0.9))
    (add-to-list 'face-font-rescale-alist '(".*FontAwesome.*" . 0.9))))

;; I tend to switch themes more often than normal.  For example,
;; switching to a lighter theme (such as the default) or to a
;; different theme depending on the time of day or my mood.  Normally,
;; switching themes is a multi-step process with `disable-theme' and
;; `load-theme'.  The `my/select-theme' function will do that in one
;; swoop.  I just choose which theme I want to go to.

(defun my/enable-theme (theme)
  "Disable any currently active themes and load the THEME."
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme enabled-themes)
    (load-theme theme t)))

(defun my/default-theme ()
  "Load the default theme."
  (my/enable-theme 'leuven))

(defun my/toggle-theme ()
  "Simplistic toggle for my used themes.
All it does is check if `leuven' (light version) is active and if so switch to
`wombat' (dark version).  Else it switches to the light theme."
  (interactive)
  (if (eq (car custom-enabled-themes) 'leuven)
      (my/enable-theme 'wombat)
    (my/enable-theme 'leuven)))

(defun my/select-theme (theme)
  "Provide a way to select and enable custom THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
			     (mapc 'symbol-name
				   (custom-available-themes))))))
  (my/enable-theme theme))


(use-package emacs
  :ensure nil
  :custom
  (x-underline-at-descent-line t)
  (underline-minimum-offset 1)
  ;; Don't beep at me.
  (visible-bell t)
  :bind (([C-f11] . #'my/select-theme)
	 ([C-f12] . #'my/toggle-theme))
  :config
  ;; Call these defuns here to make UI changes transparently.
  (my/default-theme)
  (my/fonts-setup))

;; Throw away the mouse when typing.
;; Move the mouse to the corner only if the cursor gets too close,
;; and allow it to return once the cursor is out of the way.
(mouse-avoidance-mode 'exile)

;; Just blink the modeline on errors.
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Pretify page breaks.
(use-package page-break-lines
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(provide 'appearance)
;;; appearance.el ends here
