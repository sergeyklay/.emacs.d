;;; init.el --- Initialization file. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;; Keywords: configuration, misc

;; This file is NOT part of Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;   This file bootstraps the Emacs configuration.
;;
;; I started this project on 4 March 2019 from this commit:
;; eb11ce25b0866508e023db4b8be6cca536cd3044

;;; Code:

;;;; Begin initialization.

(defconst emacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all Emacs will be verbose.
Set DEBUG=1 in the command line or use --debug-init to enable this.")

;; Measure the current start up time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;;; Package management

;; Package management in Emacs can be done in several ways. I personally like
;; `use-package' together with package.el. Some will prefer straight.el, but I
;; haven't found the need for it yet.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

;; Package loading optimization.
(custom-set-variables '(package-quickstart t))

;; No need to activate all the packages so early.
(when (or (daemonp) noninteractive)
  (package-initialize))

(custom-set-variables
 '(use-package-enable-imenu-support t)
 '(use-package-verbose emacs-debug-mode))

;; For the actual package configuration, I use `use-package'.
(eval-when-compile
  (unless (ignore-errors (require 'use-package))
    ;; This is a seldomly-run part of my configuration, as `use-package' is
    ;; installed on Emacs' first run.
    (require 'package)
    (package-refresh-contents)
    (package-install 'use-package)
    ;; Only in the first run all packages configured within this file will get
    ;; ensured. Speeds up other startups quite nicely.
    (setq use-package-always-ensure t)
    (require 'use-package)))

;;;; Backup

;; Delete excess backup versions silently.
(setq delete-old-versions t)

;; Make numeric backup versions unconditionally.
(setq version-control t)

;; Make backup files even in version controlled directories.
(setq vc-make-backup-files t)

;; Keep all backup in one directory.
(let ((my-backup-dir (concat user-emacs-directory "backup/")))
  (setq backup-directory-alist
        `(("." . ,(file-name-as-directory my-backup-dir))))
  (unless (file-exists-p my-backup-dir)
    (make-directory my-backup-dir t)))

;;;; Auto-Saving

(let ((my-auto-save-dir (concat user-emacs-directory "autosave/")))
  (setq
   auto-save-file-name-transforms
   `((".*" ,(expand-file-name "\\2" my-auto-save-dir) t))

   auto-save-list-file-name
   (concat my-auto-save-dir
           (format ".saves-%d-%s~" (emacs-pid) (system-name))))

  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir t)))

(setq auto-save-default t
      auto-save-timeout 10
      auto-save-interval 200)

;;;; History

(setq-default
 history-length 1000
 history-delete-duplicates t)

(savehist-mode 1)

;;;; Sane defaults

(setq default-directory (concat (getenv "HOME") "/"))

(global-set-key (kbd "C-x t d") #'toggle-debug-on-error)

(setq-default
 vc-follow-symlinks t ; Don't ask for confirmation when opening symlinks
 debug-on-error (and (not noninteractive) emacs-debug-mode))

(custom-set-variables
 '(initial-scratch-message "")    ; No scratch message
 '(inhibit-startup-screen t)      ; Disable start-up screen
 '(initial-major-mode 'text-mode) ; Configure the Scratch Buffer's Mode
 '(cursor-type '(bar . 2))        ; Vertical cursor width
 `(custom-file ,(concat user-emacs-directory "custom.el")))

;; I use C source to understand and debug built-in functions.
(let ((src "~/src/emacs.git"))
  (when (or (file-directory-p src)
            (file-symlink-p src))
    (setq source-directory
          (expand-file-name (substitute-in-file-name src)))))

;;;; Emacs Server

;; Declare the function `server-running-p' from the "server" module.
;;
;; This declaration informs the Emacs Lisp compiler that the function
;; `server-running-p' exists, but is defined in an external module ("server").
;; This prevents compile-time warnings and helps in generating better bytecode.
;; The `declare-function' acts as a forward declaration, essentially saying:
;; "Trust me, this function will be available at runtime, even if you can't see
;; it now at compile time."
;;
;; Note: This is not a substitute for `(require 'server)` or `(load "server")`,
;; it is only for the compiler's benefit.
(declare-function server-running-p "server")

(add-hook 'after-init-hook
          #'(lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start))))

;;;; Appearance

(load-theme 'modus-vivendi)

(defun my-terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

;; Just blink the modeline on errors.
(setq ring-bell-function #'my-terminal-visible-bell)

;; Highlight matching parentheses when the point is on them.
(add-hook 'after-init-hook 'show-paren-mode)

;; I prefer not to have any of the GUI elements.
;; This keeps the window clean. And a bit speed up loading.
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;;;; Project management

(use-package project
  :defer t
  :commands (project-root
             project-find-file
             project-switch-to-buffer
             project-switch-project
             project-switch-project-open-file
             project-prompt-project-dir
             project-forget-zombie-projects)
  :custom
  (project-vc-ignores
   '(;; Ignore these suffixes
     "*.elc" "*.pyc" "*.o" "*.lo" "*.la" "*.out" "*.sock" "*.zwc"
     ;; Ignore these files
     ".DS_Store" "Icon" "GRTAGS" "GTAGS" "GPATH"
     ;; Ignore project dependency directories
     "node_modules"))
  :config
  ;; Auto clean up zombie projects from `project-list-file'
  (run-at-time "07:00pm" (* 24 60 60) 'project-forget-zombie-projects)
  ;; Use ripgrep if installed
  (when (shell-command-to-string "command rg --version")
    (setq xref-search-program 'ripgrep)))

;;;; Language support

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'"
  :config
  :interpreter ("yml" . yml-mode))

(use-package python
  :defer t
  :custom
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "-i --simple-prompt --pprint"))

(use-package anaconda-mode
  :ensure t
  :after python
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; eval: (display-fill-column-indicator-mode)
;; coding: utf-8-unix
;; End:

;;; init.el ends here
