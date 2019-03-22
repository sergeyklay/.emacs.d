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

(add-to-list 'default-frame-alist
             (cond
              ((string-equal system-type "darwin")    '(font . "Fira Code-14"))
              ((string-equal system-type "gnu/linux") '(font . "Fira Code-12"))))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(provide 'appearance)
;;; appearance.el ends here
