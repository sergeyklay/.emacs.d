;;; intellisense.el --- Initialise company. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setting up company

;;; Code:

;; For more see URL `http://company-mode.github.io'
(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-echo-delay 0
        company-idle-delay .3
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-idle-delay t
        company-tooltip-limit 20))

;; For more see URL `https://github.com/randomphrase/company-c-headers'
(use-package company-c-headers
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends 'company-c-headers))

(provide 'intellisense)
;;; intellisense.el ends here
