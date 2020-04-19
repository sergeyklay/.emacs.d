;;; langs-conf.el --- Configuration like languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for the configuration like languages for GNU Emacs.

;;; Code:

(require 'utils)
(eval-when-compile
  (if (>= emacs-major-version 27)
      (require 's)))

;;;; JSON

(use-package json-mode
  :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)\\'"
  :config
  (bind-keys :map json-mode-map
	     ("C-c <tab>" . json-mode-beautify)))

;;;; Ini

(use-package ini-mode
  :mode "\\.ini\\'")

;;;; Dotenv

(use-package dotenv-mode
  :mode "\\.env\\'"
  :mode "\\.env\\..*\\'")

;;;; Vim

(use-package vimrc-mode
  :mode "/\\.?g?vimrc\\'"
  :mode "\\.vim\\'"
  :mode "\\.?vimperatorrc\\'"
  :mode "\\.vimp\\'")

;;;; Yaml

(if (< emacs-major-version 27)
    (defun s-count-matches (regexp s &optional start end)
      "Count occurrences of REGEXP in S.

START, inclusive, and END, exclusive, delimit the part of S to
match.  START and END are both indexed starting at 1; the initial
character in `s' is index 1.

Backported from GNU Emacs 27."
      (declare (side-effect-free t))
      (save-match-data
        (with-temp-buffer
          (insert s)
          (goto-char (point-min))
          (count-matches regexp (or start 1) (or end (point-max)))))))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :config
  (defun yaml-outline-level ()
    "Return the outline level based on the indentation, hardcoded at 2 spaces."
    (s-count-matches "[ ]\\{2\\}" (match-string 0)))

  (defun yaml-mode-outline-hook ()
    (outline-minor-mode)
    (setq-local outline-regexp
	  (concat "^\\([ ]\\{2\\}\\)*\\([-] \\)?\\([\"][^\"]*[\"]\\|"
		  "[a-zA-Z0-9_-]*\\): *\\([>|]\\|&[a-zA-Z0-9_-]*\\)?$"))
    (setq-local outline-level 'yaml-outline-level)
    (message "Outline level is %s" outline-level))
  :hook (yaml-mode . yaml-mode-outline-hook)
  :interpreter ("yml" . yml-mode))

;;;; Autoconf

(use-package autoconf-mode
  :ensure nil
  :mode "/config\\.\\(ac\\|in\\|m4\\)\\'")

;;;; Nginx

(use-package nginx-mode
  :mode (("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
         ("nginx.conf" . nginx-mode)))

(use-package company-nginx
  :after (company nginx-mode)
  :hook
  ((nginx-mode . company-mode)
   (nginx-mode . company-nginx-keywords)))


;;;; conf-mode

;; https://github.com/jrockway/emacs/blob/master/lisp/textmodes/conf-mode.el
(use-package conf-mode
  :ensure nil
  :mode (("\\.conf\\'"    . conf-space-mode)
         ("\\.setup.*\\'" . conf-space-mode)
         ("/\\(Cargo.lock\\|\\.cargo/config\\)\\'" . conf-toml-mode)))

;;;; Dotenv


;; https://github.com/preetpalS/emacs-dotenv-mode/tree/master
(use-package dotenv-mode
  :mode (("\\.env\\..*\\'" . dotenv-mode)))

;;;; Systemd

;; Major mode for editing systemd units
;; https://github.com/holomorph/systemd-mode
(use-package systemd
  :defer t)

(provide 'langs-conf)
;;; langs-conf.el ends here
