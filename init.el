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
(let ((my-auto-save-dir (concat user-emacs-directory "auto-save-list/")))
  (setq
   auto-save-file-name-transforms
   `((".*" ,(expand-file-name "\\2" my-auto-save-dir) t))

   auto-save-list-file-name
   (concat my-auto-save-dir
           (format ".saves-%d-%s~" (emacs-pid) (system-name))))

  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir t)))

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
  ;; Set the default history length to 1000 entries.  The default value is
  ;; typically lower, but increasing it allows for a more comprehensive history,
  ;; which can be beneficial when needing to recall or reuse previous inputs
  ;; across sessions.
  (history-length 1000)
  ;; Enable the automatic deletion of duplicate entries from history.  By
  ;; default, this is disabled, but enabling it helps in keeping the history
  ;; clean and more manageable, especially when frequently reusing the same
  ;; inputs.
  (history-delete-duplicates t)
  :init
  ;; Save minibuffer history.
  ;; Utilizing `savehist-mode' within the :init section is preferable
  ;; over setting the analogous variable in :custom due to performance reasons.
  ;; It ensures that the mode is activated before the package is fully loaded,
  ;; which can lead to a quicker startup.
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
  :config
  ;; Use tab key as completion option.
  (setq tab-always-indent 'complete)

  ;; No scratch message.
  (setq-default initial-scratch-message "")

  ;; Disable start-up screen
  (setq inhibit-startup-screen t)

  ;; Configure the Scratch Buffer's Mode
  (setq initial-major-mode 'text-mode)

  ;; Show possible whitespace problems in code and text files.
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook (lambda () (setq show-trailing-whitespace t))))

  ;; Save custom variables in custom.el
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; I use C source to understand and debug built-in functions.
(let ((src "~/src/emacs.git"))
  (when (or (file-directory-p src)
            (file-symlink-p src))
    (setq source-directory
          (expand-file-name (substitute-in-file-name src)))))

;;;; Emacs Server
(declare-function server-running-p "server")
(add-hook 'after-init-hook
          #'(lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start))))

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
(add-hook 'after-init-hook 'show-paren-mode)

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; I prefer not to have any of the GUI elements.
;; This keeps the window clean and speeds up loading a bit.
(setq-default use-file-dialog nil)
(setq-default use-dialog-box nil)

;;;; Windows and frames
(defun my/monitor-resolution (monitor)
  "Get current MONITOR resolution in pixels.
For non-window systems, this will return frame dimesion."
  (let* ((monitor-attrs (display-monitor-attributes-list))
         (num-displays (length monitor-attrs))
         (geometry (assq 'geometry (nth monitor monitor-attrs))))
    (if (< monitor num-displays)
        (list (nth 3 geometry)
              (nth 4 geometry))
      (error "Invalid monitor number: %d" monitor))))

(defun my/monitor-pixel-width (monitor)
  "Get current MONITOR width in pixels."
  (car (my/monitor-resolution monitor)))

(defun my/monitor-pixel-height (monitor)
  "Get current MONITOR height in pixels."
  (nth 1 (my/monitor-resolution monitor)))

(defun my/calibrate-frame-geometry()
  "Set the size and position of the main Emacs frame."
  (interactive)
  (let ((frame (selected-frame))
        width-in-pixels
        height-in-pixels
        top-in-pixels
        left-in-pixels
        (width-in-chars 175)
        (height-in-chars 50)
        (total-width (my/monitor-pixel-width 0))
        (total-height (my/monitor-pixel-height 0)))

    ;; Adjust frame height based on monitor height
    (cond ((<= total-height 640)
           (setq height-in-chars 34))
          ((<= total-height 800)
           (setq height-in-chars 40))
          ((<= total-height 900)
           (setq height-in-chars 45))
          ((<= total-height 1440)
           (setq height-in-chars 50))
          ((>= total-height 1692)
           (setq height-in-chars 60)))

    ;; Adjust frame width for small monitors
    (when (<= total-width 1024)
      (setq width-in-chars 86))

    (setq width-in-pixels (* width-in-chars (frame-char-width)))
    (setq height-in-pixels (* height-in-chars (frame-char-height)))

    ;; Center frame.  22 here is a top menubar in Gnome, macOs and Ubuntu.
    (setq top-in-pixels (round (- (/ (- total-height height-in-pixels) 2) 22)))
    (setq left-in-pixels (round (/ (- total-width width-in-pixels) 2)))

    (set-frame-size frame width-in-chars height-in-chars)
    (set-frame-position frame left-in-pixels top-in-pixels)))

(defun my|frame-setup-hook()
  "Hook to set the size and position of the main Emacs frame."
  (let ((frame (selected-frame)))
    ;; For terminal frame the height in pixels is always 1.
    ;; Do not change geometry for terminal frames.
    (when (> (frame-char-height) 1)
      (my/calibrate-frame-geometry))
    (select-frame-set-input-focus frame)))

(add-hook 'window-setup-hook #'my|frame-setup-hook)

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
   '("#*#" ".#*" "*~" ".*~" "*.*~"
     "*.elc" "*.pyc" "*.o" "*.lo" "*.la" "*.sock" "*.zwc"
     ".DS_Store" "Icon" "GRTAGS" "GTAGS" "GPATH"
     "node_modules"))
  :config
  ;; Auto clean up zombie projects from `project-list-file'
  (run-at-time "07:00pm" (* 24 60 60) 'project-forget-zombie-projects)
  ;; Use ripgrep if installed
  (when (shell-command-to-string "command rg --version")
    (setq xref-search-program 'ripgrep)))

;;;; Editor
;; No tabs - except for specific files, which Emacs can identify.
(setq-default indent-tabs-mode nil)

;; Increase the warning threshold for big files
(setq large-file-warning-threshold (* 50 1024 1024))

;; Set the default width of fill mode to 80
(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Visually indicate empty lines after the buffer's end.
(setq-default indicate-empty-lines t)

;; Show column number next to line number in mode line.
;; Emacs displays line numbers by default.
(setq-default column-number-mode t)

;; Redefine line and column format. It will looks like " 278:59 ".
(setq-default mode-line-position-column-line-format '(" %l:%c "))

;; Show Line Numbers
(use-package display-line-numbers
  :custom (display-line-numbers-width 4)
  :hook (prog-mode . display-line-numbers-mode))

(use-package elec-pair
  :hook
  ;; Activate electric-pair-mode automatically after Emacs initialization.
  (after-init . electric-pair-mode)
  ;; Disable electric-pair-local-mode in the minibuffer.
  (minibuffer-setup . (lambda () (electric-pair-local-mode 0))))

;;;; Human Languages
(use-package outline
  :commands (outline-mode outline-minor-mode)
  :hook (prog-mode . outline-minor-mode))

;;;; Programming Languages, Markup and Configurations
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

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

;;; init.el ends here
