;;; appearance.el -- All appearance related setting. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d

;;; Commentary:

;; This file should contain all appearance related settings

;;; Code:


;;; Bell configuration

;; Don't beep at me
(setq visible-bell t)

;; Just blink the modeline on errors
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.05 nil 'invert-face 'mode-line)))


;;; Modeline tweaks

;; Activate column and line number in the modeline
(column-number-mode t)
(line-number-mode t)

(global-visual-line-mode)


;;; Misc

;; Enable line highlight mode everywhere
(global-hl-line-mode 1)

(setq-default
 fill-column 80         ; Set the default width of fill mode to 80
 indicate-empty-lines t ; Visually indicate empty lines after the buffer end
 )

(when window-system
  ;; More useful frame title, that show either a file or a
  ;; buffer name (if the buffer isn't visiting a file).
  (setq frame-title-format
        '("" invocation-name " Emacs - "
          (:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))))
  (tooltip-mode -1)
  ;; Turn off the blinking cursor
  (blink-cursor-mode -1))

;; Pretify page breaks
(use-package page-break-lines
  :diminish page-break-lines-mode
  :ensure t
  :config
  (global-page-break-lines-mode))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)


;;; Theme & font

;; For more see URL `http://ergoemacs.org/emacs/emacs_list_and_set_font.html'

(cond
 ((string-equal system-type "gnu/linux")
  (when (member "Fira Code" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Fira Code-12"))
    (add-to-list 'default-frame-alist '(font . "Fira Code-12"))))
 ((string-equal system-type "darwin")
    (when (member "Fira Code" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Fira Code-14"))
    (add-to-list 'default-frame-alist '(font . "Fira Code-14")))))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))


;;; Scrolling

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; This makes it so C-n and C-p  won't make the buffer
;; jump around so much.

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode))

(provide 'appearance)
;;; appearance.el ends here
