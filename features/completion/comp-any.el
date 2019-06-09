;;; comp-any.el --- Initialise company. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>
;; Copyright (c) 2019 Sylvain Benner <sylvain.benner@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; This feature adds auto-completion support to all used languages.

;;; Code:

(require 'core-dirs)
(require 'core-defuns)

(eval-when-compile
  (require 'company-dabbrev))

(use-package company
  :defer t
  :init
  (setq company-idle-delay nil ; never start completions automatically
        company-echo-delay 0
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-tooltip-align-annotations t
        company-tooltip-limit 10
        company-selection-wrap-around t
        company-show-numbers t)
  :bind
  (:map company-active-map
        ("SPC" . company-abort)))

(eval-after-load 'company
  '(progn
     ;; Map tab to cycle through the completion options
     (when (fboundp 'company-complete-common-or-cycle)
       (global-set-key (kbd "M-TAB") #'company-complete-common-or-cycle))

     ;; make TAB complete, without losing the ability to manually indent
     (when (fboundp 'company-indent-or-complete-common)
       (global-set-key (kbd "TAB") #'company-indent-or-complete-common))))

(use-package company-statistics
  :after company
  :defer t
  :hook
  (company-mode . company-statistics-mode)
  :config
  (setq company-statistics-file
        (concat user-cache-dir "company-statistics-cache.el")))

(use-package company-box
  :if (display-graphic-p)
  :after all-the-icons
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-alist 'company-box-icons-all-the-icons))

(provide 'comp-any)
;;; comp-any.el ends here
