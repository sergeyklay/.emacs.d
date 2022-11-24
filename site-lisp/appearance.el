;;; appearance.el --- Appearance related settings. -*- lexical-binding: t; -*-

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

;; This file should contain all appearance related settings.

;;; Code:

; TODO: fixme
;(use-package kaolin-themes
;  :config
;  (load-theme 'kaolin-dark t))

(defvar font-face nil)
(defvar font-size 11)

;; Increase font size on retina displays
(when (and (display-graphic-p) (<= (frame-char-height) 15))
  (setq font-size 13))

(cond ((member "JetBrains Mono" (font-family-list))
       (setq font-face (concat "JetBrains Mono-"
                               (number-to-string font-size)
                               ":hintstyle=hintfull")))
      ((member "Source Code Pro" (font-family-list))
       (setq font-face (concat "Source Code Pro-"
                               (number-to-string font-size)
                               ":hintstyle=hintfull"))))

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

(provide 'appearance)
;;; appearance.el ends here
