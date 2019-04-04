;;; php-lang.el --- PHP related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; PHP related configuration for the GNU Emacs.

;;; Code:

(defun my/php-locate-executable ()
  "Search for the PHP executable using ’phpenv’.

This function will try to find the PHP executable by calling ’phpenv’.
If it is not available, the function will utilize `executable-find'.
The function will set `php-executable' to the actual PHP if found
or nil otherwise."
  (let ((phpenv (executable-find "phpenv")))
    (if phpenv
        (replace-regexp-in-string
         "\n\\'" ""
         (shell-command-to-string (concat phpenv " which php")))
      (executable-find "php"))))

(defun my/php-hook ()
  "The hook to configure `php-mode', `ac-php'  as well as `company-php'."
  (let ((php-path (my/php-locate-executable)))
    (progn
      (setq
       ;; Setting up actual path to the executable
       php-executable php-path
       ac-php-php-executable php-path

       ;; Store all the caches in the common place
       ac-php-tags-path (concat user-cache-dir "ac-php/")

       ;; Currently I'm involved to develop this package
       ac-php-debug-flag t

       ;; My development version
       ac-php-ctags-executable (expand-file-name "~/work/phpctags/phpctags"))

      (flycheck-mode)
      (subword-mode)
      (company-mode)
      (yas-minor-mode)

      ;; Enable ElDoc support
      (ac-php-core-eldoc-setup)

      (make-local-variable 'company-backends)
      (add-to-list 'company-backends 'company-ac-php-backend)

      ;; Jump to definition (optional)
      (define-key php-mode-map (kbd "M-]")
        'ac-php-find-symbol-at-point)

      ;; Return back (optional)
      (define-key php-mode-map (kbd "M-[")
        'ac-php-location-stack-back))))

(use-package php-mode
  :mode "\\.php[ts354]?\\'"
  :after (company flycheck)
  :custom
  (php-mode-coding-style 'psr2)
  (php-manual-path "/usr/local/share/php/doc/html")
  :init
  (add-hook 'php-mode-hook #'my/php-hook)
  :bind
  (:map php-mode-map
        ("C-<tab>" . #'counsel-company)
        ("C-c /"   . #'comment-or-uncomment-region)
        ("C-c C--" . #'php-current-class)
        ("C-c C-=" . #'php-current-namespace)))

(use-package ac-php
  :after php-mode)

(use-package company-php
  :after ac-php
  :pin melpa)

(provide 'php-lang)
;;; php-lang.el ends here
