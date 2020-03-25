;;; appearance.el --- Appearance related settings. -*- lexical-binding: t; -*-

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
  (leuven-scale-org-agenda-structure nil)
  :init
  (load-theme 'leuven t))

(defconst my/font-mono-linux "Source Code Pro"
  "The default monospaced typeface to use in Linux.")

(defconst my/font-mono-darwin "Source Code Pro"
  "The default monospaced typeface to use in macOS.")

(defconst my/font-mono-params ":hintstyle=hintfull"
  "Fontconfig parameters for the monospaced typeface.")

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

(use-package emacs
  :ensure nil
  :custom
  (x-underline-at-descent-line t)
  (underline-minimum-offset 1)
  ;; Don't beep at me.
  (visible-bell t)
  :config
  ;; Call these defuns here to make UI changes transparently.
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

;; Highlight matching parentheses when the point is on them.
(add-hook 'after-init-hook 'show-paren-mode)

(provide 'appearance)
;;; appearance.el ends here
