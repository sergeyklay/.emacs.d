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

(use-package kaolin-themes
  :config
  (use-package all-the-icons)
  (load-theme 'kaolin-light t)
  (kaolin-treemacs-theme))

(defvar font-face nil)

(cond ((member "JetBrains Mono" (font-family-list))
       (setq font-face "JetBrains Mono-11:hintstyle=hintfull"))
      ((member "Source Code Pro" (font-family-list))
       (setq font-face "Source Code Pro-11:hintstyle=hintfull")))

(when font-face
  (add-to-list 'initial-frame-alist `(font . ,font-face))
  (add-to-list 'default-frame-alist `(font . ,font-face)))

(use-package emacs
  :ensure nil
  :custom
  (x-underline-at-descent-line t)
  (underline-minimum-offset 1)
  ;; Don't beep at me.
  (visible-bell t))

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

;; Get my initial frame to be the desired size
(when window-system
  (let ((tpos (floor (/ (x-display-pixel-height) 4)))
        (lpos (floor (/ (x-display-pixel-width) 4)))
        fheight fwidth
        (height-gain 0.5)
        (width-gain 0.5))

    (setq fwidth (floor (/ (* (x-display-pixel-width) width-gain)
                           (frame-char-width))))
    (setq fheight (floor (/ (* (x-display-pixel-height) height-gain)
                            (frame-char-height))))

    (set-frame-size (selected-frame) fwidth fheight)
    (set-frame-position (selected-frame) lpos tpos)))

(provide 'appearance)
;;; appearance.el ends here
