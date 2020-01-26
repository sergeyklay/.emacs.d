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
  (require 'company)
  (require 'jump)
  (require 'utils))

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

(defun my|cc-common-hook ()
  "Common CC hook."
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label 4))

(my/add-to-hooks
 #'my|cc-common-hook
 '(c-mode-common-hook
   c++-mode-common-hook))

(my/add-to-hooks
 #'hs-minor-mode
 '(c-mode-common-hook
   c++-mode-common-hook))

(when rdm-executable-path
  (my/add-to-hooks
   #'rtags-start-process-unless-running
   '(c-mode-common-hook
     c++-mode-common-hook)))

(use-package cmake-ide
  :config
  (progn
    (cmake-ide-setup)
    (setq cmake-ide-header-search-other-file nil
          cmake-ide-header-search-first-including nil
          cmake-ide-try-unique-compiler-flags-for-headers nil)))

(defun cmake-ide/c-c++-hook ()
  "A common hook for the `cmake-ide' mode."
  (with-eval-after-load 'projectile
    (setq cmake-ide-project-dir (projectile-project-root))
    (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
  (cmake-ide-load-db))

(add-hook 'c++-mode-hook #'cmake-ide/c-c++-hook)

;; If `cmake-ide' cannot find correct build dir, provide function to solve issue.
(defun set-cmake-ide-build-dir()
  "Set build dir with compile_commands.jso file."
  (interactive)
  (let ((dir (read-directory-name "Build dir:")))
    (setq cmake-ide-build-dir dir)))

(provide 'langs-cc)
;;; langs-cc.el ends here
