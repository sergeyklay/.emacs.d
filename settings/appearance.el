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


;; Pretify page breaks.
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; For more see URL `http://ergoemacs.org/emacs/emacs_list_and_set_font.html'
(cond
 ((string-equal system-type "gnu/linux")
  (when (member "Source Code Pro" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Source Code Pro-13"))
    (add-to-list 'default-frame-alist '(font . "Source Code Pro-13"))))
 ((string-equal system-type "darwin")
  (when (member "Source Code Pro" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Source Code Pro-13"))
    (add-to-list 'default-frame-alist '(font . "Source Code Pro-13")))))

(use-package unicode-fonts
  :ensure t)

(add-to-list 'face-font-rescale-alist '(".*icons.*" . 0.9))
(add-to-list 'face-font-rescale-alist '(".*FontAwesome.*" . 0.9))

;;; Theme

;; I tend to switch themes more often than normal.
;; Thus there are convenient theme functions to manipulate
;; them intercatively.

(defun klay/switch-theme (theme)
  "Disables any currently active themes and loads THEME."
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
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(bind-key "M-<f12>" 'klay/switch-theme)
(bind-key "M-<f11>" 'klay/disable-active-themes)

;; Steve Purcell's Tomorrow theme

(use-package color-theme-sanityinc-tomorrow
  :if (not (window-system))
  :ensure t
  :config
  (klay/switch-theme 'sanityinc-tomorrow-night))

;; One Dark Theme

(use-package one-themes
  :if (window-system)
  :ensure t
  :config
  (klay/switch-theme 'one-dark))

(provide 'appearance)
;;; appearance.el ends here
