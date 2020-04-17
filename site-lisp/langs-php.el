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
  "The hook to configure `php-mode'."
  (eval-when-compile (require 'flycheck nil t))
  (when (file-exists-p "~/.local/bin/phpcs")
    (setq-local flycheck-php-phpcs-executable "~/.local/bin/phpcs"))

  ;; Coding style
  (setq-local flycheck-phpcs-standard "PSR2")
  (setq-local php-project-coding-style 'psr2)

  ;; Fill, indents
  (setq-local fill-column 120)
  (setq-local indent-tabs-mode nil)

  (when (eq 0 (buffer-size))
    (insert "<?php\n\n")))

(use-package phpt-mode
  :defer t
  :init
  (add-hook 'phpt-mode-hook #'(lambda () (flycheck-mode -1))))

(use-package php-mode
  :defer t
  :custom
  (php-mode-coding-style 'psr2)
  (php-manual-path
   (if (file-directory-p "/usr/local/share/php/doc/html")
       "/usr/local/share/php/doc/html"
     ""))
  :hook
  ((php-mode . subword-mode)
   (php-mode . flycheck-mode)
   (php-mode . yas-minor-mode)
   (php-mode . my|common-php-hook)
   (php-mode . php-enable-symfony2-coding-style))
  :bind
  (:map php-mode-map
        ("C-c C--" . #'php-current-class)
        ("C-c C-=" . #'php-current-namespace)))

(provide 'langs-php)
;;; langs-php.el ends here
