;;; fphp.el --- PHP related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; PHP related configuration for the GNU Emacs.

(defun my//locate-php-executable ()
  "Search for the PHP executable using ’phpenv’.

This function will try to find the PHP executable by calling ’phpenv’.
If it is not available, the function will utilize `executable-find'.
The function will set `php-executable' to the actual PHP if found
or nil otherwise."
  (message "We're going to set php bin")
  (let ((phpenv (executable-find "phpenv")))
    (if phpenv
        (setq php-executable
              (replace-regexp-in-string
               "\n\\'" ""
               (shell-command-to-string (concat phpenv " which php"))))
      (setq php-executable (executable-find "php")))))

(use-package php-mode
  :mode (("\\.php[ts354]?\\'" . php-mode)
         ("\\.inc\\'" . php-mode))
  :hook ((php-mode . my//locate-php-executable)
         (php-mode . subword-mode ))
  :config
  (setq-default php-mode-coding-style 'psr2)
  :bind
  (:map php-mode-map
        ("C-<tab>" . #'counsel-company)
        ("C-c /"   . #'comment-or-uncomment-region)
        ("C-c C--" . #'php-current-class)
        ("C-c C-=" . #'php-current-namespace)))

;;; Code:

(provide 'fphp)
;;; fphp.el ends here
