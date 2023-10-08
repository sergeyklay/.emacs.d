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

;; Welcome to my GNU Emacs haven.  This configuration is a reflection of my
;; desire for a minimalist yet powerful editing environment.  Here are the
;; guiding principles that shape this setup:
;;
;; 1. Maintain a lean line count: The entire configuration, including comments
;;    and the early-init.el file, shall remain under 512 lines.  The
;;    `fill-column’ is firmly set at 80 to ensure readability and maintain the
;;    compact nature.
;; 2. Modularity is key: Configurations are organized into self-contained
;;    sections, making it a breeze to lift and shift any part of the setup into
;;    another configuration if need be.
;; 3. Embrace outlines: The structure adheres to `outline-minor-mode' standards,
;;    allowing for a clean and organized view of the configuration's skeleton.
;; 4. No line-count gimmicks: There's a ban on line-count reduction tricks like
;;    squishing multiple `setq' arguments into a single line.  However, `dolist’
;;    and similar constructs are fair game.
;; 5. A delightful Emacs experience: The setup is crafted to provide a pleasant
;;    and efficient Emacs experience, tailored to my preferences and workflow.
;;
;; Each section of the configuration is meticulously crafted to adhere to these
;; principles while ensuring that Emacs remains a powerful and efficient tool
;; for all my text editing needs.
;;
;; I started this project on 4 March 2019 from this commit:
;; eb11ce25b0866508e023db4b8be6cca536cd3044

;;; Code:

;;;; Profiling and Debug
(defconst emacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, Emacs will be verbose.
Set DEBUG=1 in the command line or use --debug-init to enable this.")

;; Set the `debug-on-error' variable as per the runtime context:
;; - Enable debugging on error if Emacs is running in interactive mode,
;;   and the custom variable `emacs-debug-mode' is true.
;; - Do not enable debugging on error in non-interactive mode,
;;   regardless of the `emacs-debug-mode' value.
(setq-default debug-on-error (and (not noninteractive) emacs-debug-mode))

;; Measure the current start up time.
(add-hook
 'emacs-startup-hook
 #'(lambda ()
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
(setq package-quickstart t)

;; No need to activate all packages so early in either server or
;; non-interactive mode.
(when (or (daemonp) noninteractive)
  (package-initialize))

;; For the actual package configuration, I use `use-package'.
(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (unless (ignore-errors (require 'use-package))
    ;; This is a seldomly-run part of my configuration, as `use-package' is
    ;; installed on Emacs' first run.
    (require 'package)
    (package-refresh-contents)
    (package-install 'use-package)
    ;; Only on the first run will all packages configured within this file be
    ;; ensured. This speeds up subsequent startups quite nicely.
    (setq use-package-always-ensure t)
    (require 'use-package)))

(require 'bind-key)

;;;; Backup
;; Silently deletes excess backup versions.
(setq delete-old-versions t)

;; Make numeric backup versions unconditionally.
(setq version-control t)

;; Make backup files even in version controlled directories.
(setq vc-make-backup-files t)

;; Keep all backups in one directory.
(let ((my-backup-dir (concat user-emacs-directory "backup/")))
  (setq backup-directory-alist
        `(("." . ,(file-name-as-directory my-backup-dir))))
  (unless (file-exists-p my-backup-dir)
    (make-directory my-backup-dir t)))

;;;; Auto-Saving
(let ((save-dir (concat user-emacs-directory "auto-save-list/")))
  (setq
   auto-save-file-name-transforms `((".*" ,(expand-file-name "\\2" save-dir) t))
   auto-save-list-file-name
   (concat save-dir (format ".saves-%d-%s~" (emacs-pid) (system-name))))
  (unless (file-exists-p save-dir) (make-directory save-dir t)))

;; Save point position between sessions
(use-package saveplace
  ;; Do not enable on daemon or batch mode.
  :if (and (not noninteractive) (not (daemonp)))
  :demand 2
  :init
  ;; Automatically save place in each file.
  ;; Utilizing `save-place-mode' within the :init section is preferable
  ;; over setting the analogous variable in :custom due to performance reasons.
  ;; It ensures that the mode is activated before the package is fully loaded,
  ;; which can lead to a quicker startup.
  (save-place-mode t))

;;;; History
(use-package savehist
  ;; Do not enable on daemon or batch mode.
  :if (and (not noninteractive) (not (daemonp)))
  :demand 2
  :custom
  ;; The default value is typically lower, but increasing it allows for a more
  ;; comprehensive history, which can be beneficial when needing to recall or
  ;; reuse previous inputs across sessions.
  (history-length 1000)
  ;; Enable the automatic deletion of duplicate entries from history.  By
  ;; default, this is disabled, but enabling it helps in keeping the history
  ;; clean and more manageable, especially when frequently reusing the same
  ;; inputs.
  (history-delete-duplicates t)
  :init
  ;; Save minibuffer history.  Utilizing `savehist-mode' within the :init
  ;; section is preferable over setting the analogous variable in :custom due to
  ;; performance reasons.  It ensures that the mode is activated before the
  ;; package is fully loaded, which can lead to a quicker startup.
  (savehist-mode t))

(use-package recentf
  :if (not noninteractive)
  :defer 2
  :custom
  (recentf-max-saved-items 100)
  (recentf-keep '(recentf-keep-default-predicate
		  file-remote-p file-readable-p))
  :config
  (recentf-mode)
  (when (fboundp 'recentf-save-list)
    (run-with-idle-timer (* 3 60) t #'recentf-save-list)))

;;;; Sane defaults
(use-package emacs
  :custom
  ;; Use tab key as completion option.
  (tab-always-indent 'complete)
  ;; No scratch message.
  (initial-scratch-message "")
  ;; Disable start-up screen.
  (inhibit-startup-screen t)
  ;; Configure the Scratch Buffer's Mode.
  (initial-major-mode 'text-mode)
  ;; Save custom variables in custom.el.
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  ;; Disable lockfiles on Windows, as they are a hassle.
  (create-lockfiles (not (member system-type '(windows-nt))))
  ;; No tabs - except for specific files, which Emacs can identify.
  (indent-tabs-mode nil)
  ;; Visually indicate empty lines after the buffer's end.
  (indicate-empty-lines t)
  ;; Redefine line and column format. It will looks like " 278:59 ".
  (mode-line-position '((line-number-mode ("%l" (column-number-mode ":%c")))))
  :hook
  ((text-mode prog-mode) . (lambda () (setq show-trailing-whitespace t)))
  :init
  ;; Show column number next to line number in mode line.
  ;; Emacs displays line numbers by default.
  (column-number-mode t)
  ;; Load cutomization, if any
  (when (or (file-exists-p custom-file) (file-symlink-p custom-file))
    (load custom-file t t)))

;; I use C source to understand and debug built-in functions.
(let ((src "~/src/emacs.git"))
  (when (or (file-directory-p src)(file-symlink-p src))
    (setq source-directory (expand-file-name (substitute-in-file-name src)))))

;;;; Emacs Server
(declare-function server-running-p "server")
(add-hook 'after-init-hook
          #'(lambda ()
              (require 'server)
              (unless (server-running-p)(server-start))))

;;;; Organization
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  ;; Resize images to 300px, unless there's an attribute
  (org-image-actual-width '(300))
  ;; Enable shift+arrow for text selection
  (org-support-shift-select t)
  ;; Also include diary on org-agenda
  (org-agenda-include-diary t)
  ;; Don' clutter the actual entry with notes
  (org-log-into-drawer t)
  :config
  (declare-function org-agenda-prepare-buffers "org" files)
  (declare-function org-agenda-files "org" (&optional unrestricted archives))
  (defun my|org-agenda-refresh-on-idle-hook ()
    "Set up a timer to refresh Org Agenda buffers after 60s of idle time."
    (run-with-idle-timer 60 nil
                         (apply-partially #'org-agenda-prepare-buffers
                                          (org-agenda-files t t))))
  (unless (file-exists-p org-directory)
    (make-directory org-directory))
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . org-display-inline-images)
         (org-agenda-mode . #'my/org-agenda-refresh-on-idle))
  :bind (("C-c c" . #'org-capture)
         ("C-c a" . #'org-agenda)
         ("C-c l" . #'org-store-link)))

;;;; Appearance
(load-theme 'modus-vivendi)

;; I prefer the thin cursor.
(setq-default cursor-type '(bar . 2))

(defun my/terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

;; Just blink the modeline on errors.
(setq ring-bell-function #'my/terminal-visible-bell)

;; Highlight matching parentheses when the point is on them.
(add-hook 'after-init-hook #'show-paren-mode)

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; I prefer not to have any of the GUI elements.
;; This keeps the window clean and speeds up loading a bit.
(setq-default use-file-dialog nil)
(setq-default use-dialog-box nil)

;; Show line numbers
(use-package display-line-numbers
  :custom (display-line-numbers-width 4)
  :hook (prog-mode . display-line-numbers-mode))

(use-package elec-pair
  :hook
  ;; Activate electric-pair-mode automatically after Emacs initialization.
  (after-init . electric-pair-mode)
  ;; Disable electric-pair-local-mode in the minibuffer.
  (minibuffer-setup . (lambda () (electric-pair-local-mode 0))))

;;;; Editing
(use-package outline
  :custom
  ;; Have a blank line before a heading.
  (outline-blank-line t)
  :commands (outline-mode outline-minor-mode)
  :diminish outline-minor-mode
  :hook (prog-mode . outline-minor-mode))

;; This hook is set globally to delete trailing whitespace on save in all modes.
;; If there are modes or projects where trailing whitespace should be retained,
;; it is suggested to override this setting on a per-mode or per-project basis
;; by removing `delete-trailing-whitespace' from `before-save-hook' locally or
;; by using directory-local variables to change the behavior as needed.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; This setting is applied globally to ensure a final newline on save in all
;; modes.  If there are modes or projects where a final newline should not be
;; added, it is suggested to override this setting on a per-mode or per-project
;; basis by removing the local setting for `require-final-newline' or by using
;; directory-local variables to change the behavior as needed.
(setq-default require-final-newline t)

;;;; Project management
(use-package project
  :defer t
  :commands (project-find-file
             project-switch-to-buffer
             project-switch-project
             project-forget-zombie-projects)
  :custom
  (project-vc-ignores '("#*#" ".#*" "*~" ".*~" "*.*~" "*.elc" "*.pyc" "*.o"
   "*.lo" "*.la" "*.sock" "*.zwc" ".DS_Store" "Icon" "GRTAGS" "GTAGS" "GPATH"
   "__pycache__" "node_modules"))
  :config
  ;; Auto clean up zombie projects from `project-list-file'
  (run-at-time "07:00pm" (* 24 60 60) 'project-forget-zombie-projects)
  ;; Use ripgrep if installed
  (when (shell-command-to-string "command rg --version")
    (setq xref-search-program 'ripgrep)))

;;;; VCS
(use-package git-modes
  :ensure t
  :mode (("/\\.gitattributes\\'" . gitattributes-mode)
         ("/\\.gitconfig\\'"     . gitconfig-mode)
         ("/\\.gitmodules\\'"    . gitconfig-mode)
         ("/\\.gitignore\\'"     . gitignore-mode)
         ("/\\.dockerignore\\'"  . gitignore-mode)
         ("/\\.elpaignore\\'"    . gitignore-mode)))

(use-package magit
  :ensure t
  :after transient
  :commands (magit magit-status)
  :bind (("C-x g" . magit-status)))

;;;; Programming Languages, Markup and Configurations
(use-package css-mode
  :mode "\\.css$"
  :custom
  (css-indent-offset 2))

(use-package js
  :mode (("\\.js\\'" . js-mode))
  :custom
  (js-indent-level 2))

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook css-mode)

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package rst
  :mode ("\\.rst\\'" . rst-mode)
  :custom
  ;; This setting is used to automate the creation of section adornments in
  ;; reStructuredText files.  When a new section is created,
  ;; `rst-new-adornment-down' ensures the adornment level is one level lower
  ;; than the previous section, rather than keeping the same level.
  (rst-new-adornment-down t)
  :bind (:map rst-mode-map ("C-c . t" . rst-toc))
  :hook ((rst-mode . visual-line-mode)
         ;; This hook is utilized to automatically update the inline
         ;; table-of-contents whenever a section title is adjusted.
         (rst-adjust . rst-toc-update)))

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
