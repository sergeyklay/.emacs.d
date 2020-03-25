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

(defun my|cc-common-hook ()
  "Common CC hook."
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label 4))

(add-hook 'c-mode-hook #'my|cc-common-hook)
(add-hook 'c++-mode-hook #'my|cc-common-hook)

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
  :defer 2
  :config
  (setq c-eldoc-buffer-regenerate-time 60
	c-eldoc-includes (append '("-I./" "-I../")
				 (cc-get-standard-include-dirs))))

(use-package company-c-headers
  :after company
  :defer t
  :hook
  ((c-mode-common . company-mode)))

(defun company-c-headers-setup ()
  "Add `company-c-headers' to `company-mode'."
  (add-to-list 'company-backends 'company-c-headers))

(use-package cmake-ide
  :after (projectile rtags)
  :custom
  (cmake-ide-build-dir "build")
  (cmake-ide-header-search-other-file nil)
  (cmake-ide-header-search-first-including nil))

(defun cmake-ide/c-c++-hook ()
  "A common hook for the `cmake-ide' mode."
  (with-eval-after-load 'projectile
    (setq cmake-ide-project-dir (projectile-project-root))
    (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
  (cmake-ide-load-db))

(add-hook 'c++-mode-hook #'cmake-ide/c-c++-hook)

;; If `cmake-ide' cannot find correct build dir, provide function to solve issue.
(defun set-cmake-ide-build-dir()
  "Set build dir with compile_commands.json file."
  (interactive)
  (let ((dir (read-directory-name "Build dir:")))
    (setq cmake-ide-build-dir dir)))

(defun company-semantic-setup ()
  "Configure `company-backends' for `company-semantic' and `company-yasnippet'."
  (delete 'company-irony company-backends)
  (push '(company-semantic :with company-yasnippet) company-backends))

(defconst ede-custom-file (concat user-etc-dir "cc-mode-projects.el")
  "The default EDE project configuration file.")

(use-package ede
  :ensure nil
  :defer t
  :custom
  (ede-project-placeholder-cache-file
   (concat user-cache-dir "ede-projects.el"))
  :config
  ;; In EDE project configuration file, user can configure a project in this
  ;; form:
  ;;
  ;; (ede-cpp-root-project "project_name"
  ;;                       :file "dir/to/project/project_root/Makefile"
  ;;                       :include-path '("user_include1"
  ;;                                       "user_include2")
  ;;                       :system-include-path '("sys_include1"
  ;;                                              "sys_include2"))
  ;;
  ;; `:include-path' specifies local directories of the project relative to the
  ;; project root specified by `:file' that EDE should search
  ;; first. `:system-include-path' specifies system header files path that are
  ;; not belong to the project.
  (when (file-exists-p ede-custom-file)
    (load ede-custom-file)))

(defun semantic-include-hook ()
  "Modifies a mode-local version of `semantic-dependency-system-include-path'."
  (let ((include-dirs (cc-get-standard-include-dirs)))
    (mapc (lambda (dir)
	    ;; Trim “-I” from beginning.
	    (let ((ldir (substring dir 2)))
	      (progn
		(semantic-add-system-include ldir 'c++-mode)
		(semantic-add-system-include ldir 'c-mode))))
	  include-dirs)))

(use-package semantic
  :ensure nil
  :custom
  (semanticdb-default-save-directory (concat user-cache-dir "semanticdb/"))
  :hook
  ((semantic-init . semantic-include-hook))
  :config
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)

  (when (boundp 'semanticdb-default-save-directory)
    (unless (file-exists-p semanticdb-default-save-directory)
      (make-directory semanticdb-default-save-directory t))))

(defun semantic-enable ()
  "Enable semantic."
  (global-ede-mode 1)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)

  (semantic-mode 1)

  (when global-executable-path
    (message "Enable the use of the GNU Global SemanticDB...")
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode)))

(defun semantic-disable ()
  "Disable semantic."
  (global-ede-mode -1)
  (global-semanticdb-minor-mode nil)
  (global-semantic-idle-scheduler-mode nil)

  (semantic-mode -1))

(defun cedet-enable ()
  "Start CEDET."
  (interactive)
  (remove-hook 'c++-mode-hook 'company-rtags-setup)
  (remove-hook 'c-mode-hook 'company-rtags-setup)

  (remove-hook 'c++-mode-hook 'flycheck-rtags-setup)
  (remove-hook 'c-mode-hook 'flycheck-rtags-setup)

  (remove-hook 'c-mode-hook 'cide--mode-hook)
  (remove-hook 'c++-mode-hook 'cide--mode-hook)
  (remove-hook 'before-save-hook 'cide--before-save)

  (semantic-enable)

  (add-hook 'c++-mode-hook #'company-c-headers-setup)
  (add-hook 'c-mode-hook #'company-c-headers-setup)

  (add-hook 'c++-mode-hook #'company-semantic-setup)
  (add-hook 'c-mode-hook #'company-semantic-setup)

  (c-turn-on-eldoc-mode))

(defun irony-enable ()
  "Start irony mode."
  (interactive)
  (semantic-disable)

  (remove-hook 'c++-mode-hook 'company-c-headers-setup)
  (remove-hook 'c-mode-hook 'company-c-headers-setup)

  (remove-hook 'c++-mode-hook 'company-semantic-setup)
  (remove-hook 'c-mode-hook 'company-semantic-setup)

  (rtags-setup)

  (add-hook 'c++-mode-hook #'company-rtags-setup)
  (add-hook 'c-mode-hook #'company-rtags-setup)

  (add-hook 'c-mode-hook #'flycheck-rtags-setup)
  (add-hook 'c++-mode-hook #'flycheck-rtags-setup)

  (cmake-ide-setup))

(provide 'langs-cc)
;;; langs-cc.el ends here
