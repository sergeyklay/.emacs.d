;;; langs-php.el --- PHP related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; PHP related configuration for GNU Emacs.

;;; Code:

(defun my|common-php-hook ()
  "The hook to configure `php-mode' as well as `company-php'."
  (eval-when-compile (require 'flycheck nil t))
  (when (file-exists-p "~/.composer/vendor/bin/phpcs")
    (setq-local flycheck-php-phpcs-executable "~/.composer/vendor/bin/phpcs"))

  (setq-local fill-column 120)
  (setq-local indent-tabs-mode nil))

(use-package php-mode
  :defer t
  :hook
  ((php-mode . subword-mode)
   (php-mode . flycheck-mode)
   (php-mode . yas-minor-mode)
   (php-mode . my|common-php-hook))
  :config
  (setq php-mode-coding-style 'psr2)
  (when (file-directory-p "/usr/local/share/php/doc/html")
    (setq php-manual-path "/usr/local/share/php/doc/html"))
  :bind
  (:map php-mode-map
        ("C-c C--" . #'php-current-class)
        ("C-c C-=" . #'php-current-namespace)))

(provide 'langs-php)
;;; langs-php.el ends here
