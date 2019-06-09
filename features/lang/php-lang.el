;;; php-lang.el --- PHP related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; PHP related configuration for GNU Emacs.

;;; Code:

(require 'rx)

(eval-when-compile
  (require 'flycheck))

(defconst my--php-imports-start-regexp
  (rx (group (and bol "use"))))

(defconst my--php-imports-end-regexp
  (rx (or (and bol "use") (and bol (* space) eol))))

(defun my--php--search-beg-point (&optional end)
  "Search the first import line until reach the END point."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward my--php-imports-start-regexp end t)
         (match-beginning 1))))

(defun my--php-search-end-point (begin)
  "Search the last import line starting from BEGIN point."
  (let (end)
    (save-excursion
      (goto-char begin)
      (goto-char (point-at-bol))
      (catch 'eof
        (while (re-search-forward my--php-imports-end-regexp (point-at-eol) t)
          (when (eobp)
            (throw 'eof "End of file."))
          (setq end (point-at-eol))
          (forward-line 1))))
    end))

(defun my/php-optimize-imports ()
  "Sort PHP imports from current buffer."
  (interactive)
  (let* ((begin (my--php--search-beg-point))
         (end (and begin (my--php-search-end-point begin))))
    (when (and begin end)
      (sort-lines nil begin end))))

(defun my--php-locate-executable ()
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

(defun my|common-php-hook ()
  "The hook to configure `php-mode' as well as `company-php'."
  (let ((php-path (my--php-locate-executable)))
    ;; Setting up actual path to the executable
    (when (boundp 'php-executable)
      (setq-local php-executable php-path))

    (when (boundp 'ac-php-php-executable)
      (setq-local ac-php-php-executable php-path))

    (setq-local flycheck-php-executable php-path)
    (setq-local flycheck-php-phpcs-executable "~/.composer/vendor/bin/phpcs")

    (setq-local fill-column 120)
    (setq-local indent-tabs-mode nil)))

(use-package php-mode
  :defer t
  :hook
  ((php-mode . subword-mode)
   (php-mode . flycheck-mode)
   (php-mode . yas-minor-mode)
   (php-mode . my|common-php-hook))
  :config
  (setq php-mode-coding-style 'psr2
        php-manual-path "/usr/local/share/php/doc/html")
  :bind
  (:map php-mode-map
        ("C-c C--" . #'php-current-class)
        ("C-c C-=" . #'php-current-namespace)))

(use-package company-php
  :defer t
  :pin melpa
  :init
  (setq ac-php-tags-path (concat user-cache-dir "ac-php/"))

  (my/add-to-hook
   #'php-mode-hook
   '(company-mode turn-on-eldoc-mode ac-php-core-eldoc-setup))

  (add-to-list 'company-backends '(company-ac-php-backend company-capf))
  :bind
  (:map php-mode-map
        ("C-<tab>" . #'company-complete)
	("M-["     . #'ac-php-location-stack-back)
	("M-]"     . #'ac-php-find-symbol-at-point)))

(provide 'php-lang)
;;; php-lang.el ends here
