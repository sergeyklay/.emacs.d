;;; langs-cc.el --- Add support for the C-family of languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for the C-family of languages for GNU Emacs.

;;; Code:

(eval-when-compile
  (require 'company))

(defconst cc-standard-include-dirs-cmd
  (concat
   "echo | cpp -x c++ -Wp,-v 2>&1 | "
   "ag --nocolor '^[ \t]*/([^/ ]+(/)?)+$' | sed 's/^ //g'")
  "Command used to retrieve the standard C/C++ include directories.")

(defun cc-get-standard-include-dirs ()
  "Retrieve the standard C/C++ include directories."
  (mapcar (lambda (include-path)
	    (concat "-I" include-path))
	  (split-string
	   (shell-command-to-string cc-standard-include-dirs-cmd) "\n" t)))

;; Displays function signatures in the mode line.
(use-package c-eldoc
  :hook
  ((c-mode c++-mode) . c-turn-on-eldoc-mode)
  :config
  (setq c-eldoc-buffer-regenerate-time 60
	c-eldoc-includes (append '("-I./" "-I../")
				 (cc-get-standard-include-dirs))))

(use-package company-c-headers
  :after company
  :defer t
  :hook
  ((c-mode-common . company-mode))
  :init
  (add-to-list 'company-backends 'company-c-headers))

(defun klay|cc-common-hook ()
  "Common CC hook."
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label 4))

(add-hook 'c-mode-common-hook #'klay|cc-common-hook)
(add-hook 'c-mode-common-hook #'hs-minor-mode)

(provide 'langs-cc)
;;; langs-cc.el ends here
