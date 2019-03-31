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

(defun my--locate-php-executable ()
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

(use-package php-mode
  :mode "\\.php[ts354]?\\'"
  :after (company flycheck)
  :init
  (progn
    (use-package ac-php
      :after php-mode
      :config
      (validate-setq
       ;; Currently I'm involved to develop this package
       ac-php-debug-flag t
       ;; My development version
       ac-php-ctags-executable (expand-file-name "~/work/phpctags/phpctags"))
      (auto-complete-mode -1)
      (ac-php-core-eldoc-setup))

    (use-package company-php
      :after ac-php
      :pin melpa
      :defer t)

    (defun php-hook ()
      (let ((php-path (my--locate-php-executable)))
        (progn
          (require 'ac-php-core)
          (validate-setq php-mode-coding-style 'psr2
                         php-manual-path "/usr/local/share/php/doc/html"
                         php-executable php-path
                         ac-php-php-executable php-path
                         ac-php-tags-path (concat user-cache-dir "ac-php/"))

          (flycheck-mode)
          (subword-mode)
          (company-mode)
          (yas-minor-mode)

          (ac-php-core-eldoc-setup)

          (make-local-variable 'company-backends)
          (add-to-list 'company-backends 'company-ac-php-backend))))

    (add-hook 'php-mode-hook 'php-hook))
  :bind
  (:map php-mode-map
        ("C-<tab>" . #'counsel-company)
        ("C-c /"   . #'comment-or-uncomment-region)
        ("C-c C--" . #'php-current-class)
        ("C-c C-=" . #'php-current-namespace)))

(provide 'php-lang)
;;; php-lang.el ends here
