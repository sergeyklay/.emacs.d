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

;; Highlight current line
(global-hl-line-mode 1)

;; More useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file).
(setq frame-title-format
      '("" invocation-name " Emacs - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


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

(provide 'appearance)
;;; appearance.el ends here
