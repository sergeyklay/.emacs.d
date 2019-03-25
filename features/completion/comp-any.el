;;; comp-any.el --- Initialise company. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setting up company for GNU Emacs.

;;; Code:

;; For more see URL `http://company-mode.github.io'
(use-package company
  :init
  (global-company-mode)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous))
  :config
  (setq
   ;; remove annoying blinking
   company-echo-delay 0
   ;; decrease delay before autocompletion popup shows
   company-idle-delay .3
   company-selection-wrap-around t
   company-show-numbers t
   company-tooltip-align-annotations t
   company-tooltip-idle-delay t
   ;; bigger popup window
   company-tooltip-limit 20))

;; For more see URL `https://github.com/randomphrase/company-c-headers'
(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))

;; Company-Statistics: Suggest most used completions first
(use-package company-statistics
  :hook (company-mode . company-statistics-mode)
  :config
  (setq company-statistics-file
        (concat user-cache-dir "company-statistics-cache.el")))

(provide 'comp-any)
;;; comp-any.el ends here
