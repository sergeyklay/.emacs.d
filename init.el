;;; init.el --- Initialization file. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026 Serghei Iakovlev <gnu@serghei.pl>

;; Author: Serghei Iakovlev <gnu@serghei.pl>
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
;; desire for a "minimalist" yet powerful editing environment.
;;
;; I started this project on 4 March 2019 from this commit:
;; eb11ce25b0866508e023db4b8be6cca536cd3044

;;; Code:

;;;; Profiling and Debug for Emacs
(defconst emacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, Emacs will be verbose.
Set DEBUG=1 in the command line or use --debug-init to enable this.")

;; Set the `debug-on-error' variable as per the runtime context:
;; - Enable debugging on error if Emacs is running in interactive mode, and the
;;   custom variable `emacs-debug-mode' is true.
;; - Do not enable debugging on error in non-interactive mode, regardless of the
;;   `emacs-debug-mode' value.
(setq-default debug-on-error (and (not noninteractive) emacs-debug-mode))

;; Measure the current start up time.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs ready in %s with %d garbage collections."
            (format "%.2f seconds"
                    (float-time
                     (time-subtract after-init-time before-init-time)))
            gcs-done)))

;;;; Personal Info
;; Used for `gnus', `erc', and so non.
(setopt user-full-name "Serghei Iakovlev")

;;;; Packages management
;; Package management in Emacs can be done in several ways.  I personally like
;; classic one with `package.el'.  Some will prefer straight.el, use-package,
;; and so on, but I haven't found the need for them.
(require 'package)

;; Setting up package archives.
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)

;; Priorities.  Default priority is 0.
(setq package-archive-priorities
      '(("gnu"      . 100)
        ("nongnu"   . 50)
        ("melpa"    . 10)))

;; Precompute activation actions to speed up startup.
(setq package-quickstart t)

;; Enable native compilation of packages upon their first load,
;; leveraging Emacs's native compilation feature for performance.
(setq package-native-compile t)

(package-initialize)

(defun ensure-package-installed (package)
  "Ensure that PACKAGE is installed."
  (unless (package-installed-p package)
      (condition-case _err
          (package-install package)
        (error
         (package-refresh-contents)
         (package-install package))))
  (package-installed-p package))

;; package.el updates the saved version of `package-selected-packages' correctly
;; only after `custom-file' has been loaded, which is a bug.  We work around
;; this by adding the required packages to `package-selected-packages' after
;; startup is complete.  This code borrowed from Steve Purcell configuration.

(defvar my-required-packages nil
  "List of packages that were successfully installed.")

(defun my-note-selected-package (oldfun package &rest args)
  "If OLDFUN reports PACKAGE was successfully installed, note that fact.
The package name is noted by adding it to
`my-required-packages'.  This function is used as an
advice for `require-package', to which ARGS are passed."
  (let ((available (apply oldfun package args)))
    (prog1
        available
      (when available
        (add-to-list 'my-required-packages package)))))

(advice-add 'ensure-package-installed :around 'my-note-selected-package)

(defconst my-selected-packages
  '(anzu                  ; Show current match and total matches information
    avy                   ; Jump to things in Emacs tree-style
    benchmark-init        ; Benchmarks for require and load calls
    cask-mode             ; Major mode for editing Cask files
    company               ; Code completion framework
    consult               ; Incremental narrowing framework
    consult-lsp           ; `lsp-mode' and `consult' helping each other
    csv-mode              ; CSV file editing mode
    delight               ; Customise the mode names displayed in the mode line
    embark                ; Contextual actions in buffers
    embark-consult        ; Embark integration with `consult'
    flycheck              ; Syntax checking for used languages
    git-modes             ; Modes for Git-related files
    json-mode             ; Major mode for editing JSON files
    lsp-mode              ; Language Server Protocol support
    lsp-pyright           ; LSP client leveraging pyright Python Language Server
    lsp-ui                ; UI enhancements for `lsp-mode'
    magit                 ; The Git porcelain inside Emacs
    marginalia            ; Annotations for minibufer completions
    markdown-mode         ; Major mode for Markdown files
    orderless             ; Flexible completion style for minibufers
    prescient             ; Predictive sorting and filtering
    rainbow-delimiters    ; Color nested parentheses
    rainbow-mode          ; Colorize color values in buffers
    vertico               ; Vertical interactive completion for minibuffers
    vertico-prescient     ; Prescient integration with `vertico'
    yaml-mode             ; Major mode for YAML files
    web-mode)             ; Web template editing mode for Emacs
   "A list of packages that are selected for installation.")

;; Install packages as soon as possible.
(dolist (package my-selected-packages)
  (ensure-package-installed package))

;;;; Benchmark
(require 'benchmark-init)
(add-hook 'after-init-hook #'benchmark-init/deactivate)

;;;; Setup keymap
;; Create and setup my own keyboard map.
;; For details see:
;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs
(defvar my-keyboard-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "-") #'text-scale-decrease)
    (define-key map (kbd "+") #'text-scale-increase)
    ;; because "+" needs "S-=" and I might forget to press shift
    (define-key map (kbd "=") #'text-scale-increase)
    map)
  "My own keyboard map.")

;; As stated in the manual:
;;
;; A small number of keys are reserved for user-defined bindings, and should not
;; be used by modes, so key bindings using those keys are safer in this regard.
;; The reserved key sequences are those consisting of C-c followed by a letter
;; (either upper or lower case), and function keys F5 through F9 without
;; modifiers
(keymap-global-set "C-c s" my-keyboard-map)

;; `which-key' is built-in since Emacs 30.
(add-hook 'after-init-hook #'which-key-mode)

;; Increase the delay for which-key buffer to popup.
(setq-default which-key-idle-delay 1.5)

;;;; Common functions
(defun my-ensure-directory-exists (dir)
  "Ensure that the directory DIR exists, create it if it doesn't."
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun my/switch-to-scratch ()
  "Get a scratch buffer."
  (interactive)
  (let* ((name "*scratch*")
         (buf (get-buffer name)))
    (pop-to-buffer
     (if (bufferp buf)
         buf  ; Existing scratch buffer
       ;; New scratch buffer
       (with-current-buffer (get-buffer-create name)
         (current-buffer))))))

(define-key my-keyboard-map (kbd "s") #'my/switch-to-scratch)

(defun my/insert-current-date-iso-8601 ()
  "Insert the current date and time in ISO 8601 format (YYYY-MM-DDTHH:MM:SSZ)."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))

(define-key my-keyboard-map (kbd "D") #'my/insert-current-date-iso-8601)

(defun my/reload-current-mode ()
  "Reload the current major mode.

This function temporarily switches the current buffer's major
mode to `fundamental-mode' and then back to the original major
mode.  This is useful when you've re-evaluated a major mode's
definition (e.g., using `eval-buffer' on the mode's source file)
and want to apply the changes to the current buffer.

After modifying and evaluating your mode's source code, call this
function to refresh the mode in the mode related buffer and see
the changes take effect."
  (interactive)
  (let ((current-mode major-mode)
        (load-prefer-newer t))
    (fundamental-mode)
    (when (locate-library (concat (symbol-name current-mode) ".el"))
      (unload-feature current-mode)
      (load (concat (symbol-name current-mode) ".el")))
    (funcall current-mode)))

(global-set-key (kbd "C-<f5>") #'my/reload-current-mode)

;;;; Backup
;; Silently deletes excess backup versions.
(setq delete-old-versions t)

;; Make numeric backup versions unconditionally.
(setq version-control t)

;; Make backup files even in version controlled directories.
(setq vc-make-backup-files t)

;; Keep all backups in one directory.
(let ((my-backup-dir (expand-file-name "backup" user-emacs-directory)))
  (setopt backup-directory-alist
        `(("." . ,(file-name-as-directory my-backup-dir))))
  (my-ensure-directory-exists my-backup-dir))

;;;; Auto-Saving
(let ((save-dir (expand-file-name "auto-save-list" user-emacs-directory)))
  (setopt auto-save-file-name-transforms
          `((".*" ,(concat (file-name-as-directory save-dir) "\\2") t)))
  (setopt auto-save-list-file-name
          (expand-file-name (format ".saves-%d-%s~" (emacs-pid) (system-name))
                            save-dir))
  (my-ensure-directory-exists save-dir))

;;;; History
;; Set the maximum number of entries to save in the history.  A larger value
;; allows for a more extensive history, which can be handy if you often need
;; to recall older entries.
(setopt history-length 1000)

;; Enable the removal of duplicate entries in the history.
(setopt history-delete-duplicates t)

;; Enable `save-place-mode' to save point position in files across sessions.  It
;; ensures that you return to the exact spot where you left off, enhancing
;; workflow efficiency.  Only enable in interactive mode.
(when (not noninteractive)
  (add-hook 'after-init-hook #'save-place-mode))

;; Enable `savehist-mode', which automatically saves the minibuffer history
;; to a file and loads it on startup, preserving your history between sessions.
;; Only enable in interactive mode.
(when (not noninteractive)
  (add-hook 'after-init-hook #'savehist-mode))

;; Enable `recentf-mode' to keep track of recently opened files.
;; Only enable in interactive mode.
(when (not noninteractive)
  (add-hook 'after-init-hook #'recentf-mode))

(with-eval-after-load 'recentf
  ;; Set the maximum number of recent files to save.
  (setopt recentf-max-saved-items 1000)

  ;; Update `recentf-exclude' to exclude specific paths and files
  ;; I din't need in the list.
  (setopt recentf-exclude
          `(,(temporary-file-directory)
            ,(concat (file-name-as-directory package-user-dir)
                     ".*-autoloads\\.el\\'")))

  ;; Set up an idle timer to save the recent list every 3 minutes
  (run-with-idle-timer (* 3 60) t #'recentf-save-list))

;;;; Sane defaults
;; Disable the startup screen, allowing for a quicker start directly into
;; your working environment.
(setq-default inhibit-startup-screen t)


;; Visually indicate empty lines after the buffer's end.
(setq-default indicate-empty-lines t)

;; Customize the format of the mode line to show line and column numbers
;; in the format "278:59".
(setq-default mode-line-position
              '((line-number-mode ("%l" (column-number-mode ":%c")))))

;; Enable column number mode to display column numbers in the mode line.
(column-number-mode t)

;; Highlight trailing whitespace in text-mode and prog-mode.
(add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Set `source-directory' to the local Emacs source directory.
;; Used to understand and debug built-in Emacs functions.
(let ((src (expand-file-name "~/src/emacs.git")))
  ;; Check if the directory exists and is
  ;; not remote before setting `source-directory`.
  (when (and (file-directory-p src)
             (not (file-remote-p src)))
    (setopt source-directory src)))

;;;; Search
(defun my-isearch-exit-and-run (function)
  "Exit `isearch' and run FUNCTION with the current `isearch' string."
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (isearch-exit)
    (funcall function query)))

(defun my-occur-from-isearch ()
  "Invoke `occur' using the current `isearch' string."
  (interactive)
  (my-isearch-exit-and-run #'occur))

(defun my-project-search-from-isearch ()
  "Invoke `project-find-regexp' using the current `isearch' string."
  (interactive)
  (my-isearch-exit-and-run #'project-find-regexp))

;; Invoke `occur' using the current `isearch' string.
(define-key isearch-mode-map (kbd "C-o") #'my-occur-from-isearch)

;; Invoke `project-find-regexp' using the current `isearch' string.
(define-key isearch-mode-map (kbd "C-f") #'my-project-search-from-isearch)

;; Do incremental search forward for a symbol found near point.
(define-key isearch-mode-map (kbd "C-d") #'isearch-forward-symbol-at-point)

;; Jump to search results instead of having to do multiples C-s.
(define-key isearch-mode-map (kbd "C-j") #'avy-isearch)

;; Interactive replacement:
;; - M-% to call `anzu-isearch-query-replace'
;; - C-M-% to call `anzu-isearch-query-replace-regexp'
(define-key isearch-mode-map [remap isearch-query-replace]
            #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp]
            #'anzu-isearch-query-replace-regexp)

(defun my-isearch-start-from-region (orig-fn &rest args)
  "Advice to start `isearch' with the active region or symbol at point.

This advice function enhances `isearch' commands by using the
current active region as the initial search string when
available.  If the region is active and `transient-mark-mode' is
enabled, the region's content is used.  Otherwise, the original
search behavior is preserved.

ORIG-FN is the original isearch function being advised.
ARGS are the arguments passed to the original isearch function."
  (let ((search-string
         (when (and transient-mark-mode mark-active (not (eq (mark) (point))))
           (buffer-substring-no-properties (mark) (point)))))
    ;; Deactivate mark if a region was used for search initialization.
    (when search-string
      (isearch-update-ring search-string)
      (deactivate-mark))
    ;; Call the original isearch function.
    (apply orig-fn args)
    ;; Yank the search string if set.
    (when search-string
      (isearch-yank-string
       (if isearch-regexp
           search-string
         (regexp-quote search-string))))))

;; Add the advice to all relevant isearch functions.
(dolist (fn '(isearch-forward
              isearch-forward-regexp
              isearch-backward
              isearch-backward-regexp))
  (advice-add fn :around #'my-isearch-start-from-region))

(defun my-occur-visit-result-and-close ()
  "Visit the occurrence at point and close the Occur window."
  (interactive)
  (let ((pos (occur-mode-find-occurrence)))
    (when pos
      (quit-window)
      (goto-char pos))))

;; Bind M-<return> to visit the occurrence and close the window.
(define-key occur-mode-map (kbd "M-<return>") #'my-occur-visit-result-and-close)

;; Enable `hl-line-mode` in `occur-mode` buffers.
(add-hook 'occur-mode-hook (lambda () (hl-line-mode 1)))

;; Use ripgrep if installed
(when (executable-find "rg")
  (setopt xref-search-program 'ripgrep))

(defun my|xref-setup-environment ()
  "Set up a tailored environment for `xref--xref-buffer-mode' buffers."
  ;; Enable `hl-line-mode' in `xref' buffers.
  (hl-line-mode 1)

  ;; Bind M-<return> to visit the occurrence and close the window.
  (define-key xref--xref-buffer-mode-map (kbd "M-<return>")
              #'xref-quit-and-goto-xref))

(add-hook 'xref--xref-buffer-mode-hook #'my|xref-setup-environment)

;;;; Security
(defconst my-gpg-program
  (cond
   ((executable-find "gpg") "gpg")
   ((executable-find "gpg2") "gpg2")
   (t nil))
  "Path to the GPG program to use.
If neither gpg nor gpg2 is found, this is set to nil.")

;; Check if GPG is available, then set `epg-gpg-program',
;; otherwise display a warning.
(if my-gpg-program
    (setq epg-gpg-program my-gpg-program)
  (warn (concat "Neither gpg nor gpg2 is available. "
                "Encryption and decryption features will not be available.")))

;; Initialize epa settings
;; Set pinentry mode to loopback.
;; For more see "man 1 gpg" for the option "--pinentry-mode".
(setopt epg-pinentry-mode 'loopback)

;; GnuPG ID of your default identity.
(setq epg-user-id "1E0B5331219BEA88")

;; Specify the key to use for file encryption
;; Specify the key to use for file encryption.
(setq epa-file-encrypt-to `(,epg-user-id))

;; Enable automatic encryption/decryption of *.gpg files
(require 'epa-file)

;; Only enable `epa-file' if it's not already enabled
(unless (memq epa-file-handler file-name-handler-alist)
  (epa-file-enable))

;;;; Window Handling
;; `winner-mode' allows you to undo and redo window configurations.
(add-hook 'after-init-hook #'winner-mode)

(with-eval-after-load 'winner
  ;; Define a list of buffer names that Winner mode will ignore when restoring
  ;; window configurations.
  (setq winner-boring-buffers
        '("*Apropos*"
          "*Buffer List*"
          "*Compile-Log*"
          "*Completions*"
          "*Help*"
          "*Ibuffer*"
          "*inferior-lisp*"
          "*Messages*")))

;; `windmove-mode' enables navigation between windows using user-defined keys.
(add-hook 'after-init-hook #'windmove-mode)

(with-eval-after-load 'windmove
  (global-set-key (kbd "C-x w k") 'windmove-up)
  (global-set-key (kbd "C-x w j") 'windmove-down)
  (global-set-key (kbd "C-x w l") 'windmove-right)
  (global-set-key (kbd "C-x w h") 'windmove-left))

(defun my-select-window (window &rest _)
  "Select WINDOW as the active window in `display-buffer-alist'."
  (select-window window))

;; Configure display behavior for search result buffers (`occur' and `xref').
;; This ensures consistent placement and appearance of these buffers.
(setq display-buffer-alist
      '(((or
          (derived-mode . occur-mode)
          (major-mode . xref--xref-buffer-mode))
         ;; Reuse an existing Occur window, or display
         ;; it at the bottom if not found.
         (display-buffer-reuse-window display-buffer-at-bottom)
         ;; Automatically select the window when it is displayed.
         (body-function . my-select-window)
         ;; Set the window height to 26% of the available space.
         (window-height . 0.26)
         ;; Mark the window as dedicated to its buffer, preventing
         ;; other buffers from being displayed in this window.
         (dedicated . t)
         ;; Preserve the window's size when redisplaying or
         ;; resizing other windows.
         (preserve-size . (t . t))
         ;; Remove the mode line to reduce visual clutter, providing
         ;; a cleaner view focused on the search results.
         (window-parameters . ((mode-line-format . none))))))

;;;; Appearance
;; Set cursor to use when this buffer is in the selected window.
(setopt cursor-type 'bar)

;; Keep cursor outside of any `cursor-intangible' text property.
(setopt minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Theme
(load-theme 'modus-vivendi t)

;; Modeline
(delight 'auto-revert-mode nil "autorevert")
(delight 'eldoc-mode nil "eldoc")
(delight 'outline-minor-mode nil "outline")

(defun my/terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

;; Just blink the modeline on errors.
(setq ring-bell-function #'my/terminal-visible-bell)

;; Highlight matching parentheses when the point is on them.
(add-hook 'after-init-hook #'show-paren-mode)

;; Highlight brackets according to their depth.
;; Add `rainbow-delimiters-mode' to `prog-mode-hook'.
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Can be activated with `display-line-numbers-mode'
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)

;; Enable `electric-pair-mode', which automatically inserts the closing
;; parenthesis, bracket, or quote when you type an opening one.
(add-hook 'after-init-hook #'electric-pair-mode)

;; Disable `electric-pair-mode' in minibuffer.
(add-hook 'minibuffer-setup-hook (lambda () (electric-pair-local-mode -1)))

;;;; Editing
;; TAB behaviour (indentation and completion)

;; Use the tab key for both indentation and completion.
(setq-default tab-always-indent 'complete)

;; Set the preferred, conservative completion behavior when pressing tab
;; for various contexts, prioritizing safe and predictable actions.
(setq-default tab-first-completion 'word-or-paren-or-punct)

;; Use spaces instead of tabs for indentation, except in cases where
;; Emacs specifically recognizes that tabs are required.
(setq-default indent-tabs-mode nil)

;; Hide all body lines in buffers with `outline-minor-mode' enabled,
;; leaving all headings visible.
(add-hook 'outline-minor-mode-hook #'outline-hide-body)

;; Delete trailing whitespace on save in all modes.  If there are modes or
;; projects where trailing whitespace should be retained, it is suggested to
;; override this setting on a per-mode or per-project basis by removing
;; `delete-trailing-whitespace' from `before-save-hook' locally or by using
;; directory-local variables to change the behavior as needed.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Ensure a final newline on save in all modes.  If there are modes or projects
;; where a final newline should not be added, it is suggested to override this
;; setting on a per-mode or per-project basis by removing the local setting for
;; `require-final-newline' or by using directory-local variables to change the
;; behavior as needed.
(setq-default require-final-newline t)

;; When duplicating line/region, move the point to the first line of the
;; duplicate line/region.  Used for `duplicate-dwim' bellow.
(setq duplicate-line-final-position 1)

;; C-c d will duplicate line (see `duplicate-line') if the region is inactive,
;; otherwise will duplicate region.  With C-u N C-c d will duplicate N times.
(define-key my-keyboard-map (kbd "d") #'duplicate-dwim)

;;;; Project management
(require 'project)

;; Auto clean up zombie projects from `project-list-file'
(run-at-time "07:00pm" (* 24 60 60) 'project-forget-zombie-projects)

(defun my/project-switch-project (dir)
  "Switch to another project by running an Emacs command.
Directly use `project-find-file' instead of getting prompted.

When called in a program, it will use the project corresponding
to directory DIR."
  (interactive (list (project-prompt-project-dir)))
  (let ((project-current-directory-override dir))
    (project-find-file)))

(defun my/switch-project-and-kill-buffers ()
  "Kill all buffers of the current project, then switch to a new project."
  (interactive)
  (let ((current-project (project-current)))
    (if current-project
        (progn
          ;; Kill all buffers belonging to the current project without
          ;; confirmation.
          (project-kill-buffers t)
          ;; Interactively switch to another project.
          (call-interactively #'project-switch-project))
      ;; If there's no current project, inform the user.
      (message "No current project found."))))

(defun my/project-list-buffers ()
  "Display a list of buffers associated with the current project.

This function filters the list to show only file-visiting buffers.
After generating the buffer list, it pops up the `*Buffer List*` buffer
in a new window, allowing you to easily navigate through the buffers
related to your current project."
  (interactive)
  (project-list-buffers t)
  (pop-to-buffer "*Buffer List*"))

;; Bind keys to commands within the `project-prefix-map'
(define-key project-prefix-map (kbd "p") #'my/project-switch-project)
(define-key project-prefix-map (kbd "s") #'my/switch-project-and-kill-buffers)
(define-key project-prefix-map (kbd "C-b") #'my/project-list-buffers)
(define-key project-prefix-map (kbd "R") #'project-remember-projects-under)
(define-key project-prefix-map (kbd "K") #'project-kill-buffers)

;;;; VCS
;; Associate files with the appropriate git modes.
(add-to-list 'auto-mode-alist
             '("/\\.gitattributes\\'" . gitattributes-mode))

(add-to-list 'auto-mode-alist
             '("/\\.git\\(config\\|modules\\)\\'" . gitconfig-mode))

(add-to-list 'auto-mode-alist
             '("/\\.\\(git\\|docker\\|elpa\\)ignore\\'" . gitignore-mode))

;; Load `magit' after `transient' to ensure dependency order.
(with-eval-after-load 'transient
  (require 'magit)

  (global-set-key (kbd "C-x g") 'magit-status))

;;;; Setup completion
;; Provide a nicer `completing-read'.
(add-hook 'after-init-hook #'vertico-mode)

;; Show more candidates in the minibuffer.
(setq vertico-count 12)

;; Enable cycling through candidates.
(setq vertico-cycle t)

;; Enable Consult for enhanced command completion.
(global-set-key [remap switch-to-buffer]   #'consult-buffer)
(global-set-key [remap goto-line]          #'consult-goto-line)
(global-set-key [remap locate]             #'consult-locate)
(global-set-key [remap man]                #'consult-man)
(global-set-key [remap load-theme]         #'consult-theme)
(global-set-key [remap recentf-open-files] #'consult-recent-file)

(global-set-key [remap switch-to-buffer-other-window]
                #'consult-buffer-other-window)

(global-set-key [remap switch-to-buffer-other-frame]
                #'consult-buffer-other-frame)

(with-eval-after-load 'consult
  (define-key minibuffer-local-map (kbd "C-r") #'consult-history))

(defun my/consult-jump-in-buffer ()
  "Jump in buffer with `consult-imenu'."
  (interactive)
  (call-interactively #'consult-imenu))

(define-key my-keyboard-map (kbd "j") #'my/consult-jump-in-buffer)

(defun my-isearch-consult-line-from-isearch ()
  "Invoke `consult-line' from isearch."
  (interactive)
  (my-isearch-exit-and-run #'consult-line))

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-l")
              #'my-isearch-consult-line-from-isearch))

;; Add additional context and annotations to completion UI.  Marginalia enriches
;; the completion interface with more information, such as documentation
;; strings, file sizes, etc.
(add-hook 'after-init-hook #'marginalia-mode)

(with-eval-after-load 'vertico
  ;; Enable Orderless for flexible matching style.  Orderless provides advanced
  ;; matching capabilities that allow you to combine multiple search patterns.
  (require 'orderless)

  ;; Set up Orderless as the default completion style.
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)

  ;; Adjust completion for specific contexts, e.g., file paths
  (setq completion-category-overrides
        '((file (styles basic partial-completion))))

  ;; Ensure case-insensitive matching for all completions.
  (setq completion-ignore-case t)

  ;; Enable Embark for additional actions within completion UI.  Embark adds the
  ;; ability to perform actions directly from completion buffers (e.g.,
  ;; minibuffer, `completing-read').

  ;; Create a type-specific buffer to manage current candidates.
  (define-key vertico-map (kbd "C-c C-e") #'embark-export)

  ;; Collect current candidates into a live, interactive buffer for further
  ;; processing.
  (define-key vertico-map (kbd "C-c C-l") #'embark-collect)

  ;; Prompt the user for an action and perform it.
  (define-key vertico-map (kbd "C-c .") #'embark-act))

;; Explore current command key bindings with `completing-read'.
(global-set-key [remap describe-bindings] #'embark-bindings)

;; Prompt the user for an action and perform it
(global-set-key (kbd "C-.") 'embark-act)

;; Do-What-I-Mean (e.g., open or describe)
(global-set-key (kbd "C-;") 'embark-dwim)

(with-eval-after-load 'embark
  (require 'embark-consult)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(with-eval-after-load 'vertico
  (require 'prescient)
  (require 'vertico-prescient)

  ;; The default settings seem a little forgetful to me.  Let's try this out.
  (setq prescient-history-length 200)

  ;; Common sense.
  (setq prescient-sort-full-matches-first t)

  ;; Disable filtering in Prescient, leaving only sorting.  I use `orderless'
  ;; for filtering (see above).
  (setq vertico-prescient-enable-filtering nil)

  ;; Prescient for improved sorting and filtering.  Prescient enhances sorting
  ;; and filtering of candidates based on your history and frequently used
  ;; items.
  (prescient-persist-mode 1)

  ;; Enable Vertico Prescient integration.  Integrate Prescient with Vertico to
  ;; combine powerful sorting and filtering with the Vertico completion system.
  (vertico-prescient-mode 1))

;;;; Shells
;; Setting up the switch used to have the shell execute its command line
;; argument.
;;
;; -i is for interactive (I don't use it)
;; -c tells bash to read whatever commands follow
;; -l means invoke login shells, so that .profile or .bash_profile is read
;;
;; For more see:
;;
;; - `https://github.com/bbatsov/projectile/issues/1097'
;; - `https://emacs.stackexchange.com/q/3447/16592'
(setq shell-command-switch "-lc")

;; The comint prompt is read only.
(setq comint-prompt-read-only t)

;; Make colors display correctly in M-x shell.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;;; Eshell
;; The default settings seem a little forgetful to me.  Let's try this out.
(setq eshell-history-size 1000)

;; Prevent duplicate entries in the Eshell history.
(setq eshell-hist-ignoredups t)

;; Always scroll the window to the end when entering a new command.
(setq eshell-scroll-to-bottom-on-input t)

;; Scroll the window to the end when new output appears.
;; The value all makes sure it scrolls for any kind of output.
(setq eshell-scroll-to-bottom-on-output 'all)

(defun my-eshell-shorten-path (path)
  "Replace the home directory in PATH with ~."
  (let ((home (getenv "HOME")))
    (if (string-prefix-p home path)
        (concat "~" (substring path (length home)))
      path)))

;; Making the eshell prompt behave more like my Zsh prompt.
(setq eshell-prompt-regexp "^[^@\n]+@[^ ]+ [^\n]+[\n][#%] ")
(setq eshell-prompt-function
      (lambda nil
        (concat
	 (user-login-name) "@" (system-name) " "
	 (if (string= (eshell/pwd) (getenv "HOME"))
	     "~"
           (my-eshell-shorten-path (eshell/pwd)))
	 "\n"
	 (if (= (user-uid) 0) "# " "% "))))

;; Keybinding to quickly launch Eshell.
;; Pressing "M-s e" will open Eshell in the current buffer.
(global-set-key (kbd "M-s e") 'eshell)

;;;; IDE Like Features
;;;;; Setup Completion
(require 'company)

;; The idle delay in seconds until completion starts automatically.
(setopt company-idle-delay 0.1)

;; Aligns annotations to the right side of the tooltip.
(setopt company-tooltip-align-annotations t)

(delight 'company-mode nil "company")

(defmacro company-backend-for-hook (hook backends)
  "Add a HOOK to set `company-backends' dynamically.

HOOK is the name of the hook to which the configuration function
is added. BACKENDS is the list of backends to set for
`company-backends' in the local buffer when the hook is run.

Example usage:

  (company-backend-for-hook 'prog-mode-hook
                            '(company-capf company-dabbrev-code))

This will configure `company-backends' for all `prog-mode'
buffers to include `company-capf' and `company-dabbrev-code'."
  `(add-hook ,hook (lambda()
                     (set (make-local-variable 'company-backends)
                          ,backends))))

;;;;; Diagnostics
(delight 'flycheck-mode nil 'flycheck)

;;;;; Setup LSP
;; Change the LSP keymap prefix.
(setopt lsp-keymap-prefix "C-c l")

;; Shut down LSP server after close all buffers associated with the server.
(setopt lsp-keep-workspace-alive nil)

;; Segments used in breadcrumb text on headerline.
(setopt lsp-headerline-breadcrumb-segments
        '(path-up-to-project  ; Include the directories up to project
          file                ; Include the open file name
          symbols))           ; Include document symbols if server supports it

;; Delay before showing the doc.
(setopt lsp-ui-doc-delay 0.5)

;; Prefer `flycheck-mode' as the checker backend provider.
(setopt lsp-diagnostics-provider :flycheck)

(with-eval-after-load 'lsp-ui
  ;; Remap `xref-find-definitions' (bound to M-. by default)
  (define-key lsp-ui-mode-map
              [remap xref-find-definitions]
              #'lsp-ui-peek-find-definitions)

  ;; Remap `xref-find-references' (bound to M-? by default)
  (define-key lsp-ui-mode-map
              [remap xref-find-references]
              #'lsp-ui-peek-find-references))

;; Configure LSP mode for enhanced experience.
(with-eval-after-load 'lsp-mode
  ;; Remap `lsp-treemacs-errors-list' (bound to C-c l g e).
  ;;
  ;; I find `consult-lsp-diagnostics' more convenient than
  ;; `lsp-treemacs-errors-list' for querying `lsp-mode' diagnostics.  Therefore,
  ;; I prefer to replace the default command used with the standard binding for
  ;; displaying the diagnostics window.  The `lsp-treemacs-errors-list' command
  ;; will remain available for manual invocation if needed.
  (define-key lsp-mode-map
              [remap lsp-treemacs-errors-list]
              #'consult-lsp-diagnostics)

  ;; Remap `xref-find-apropos' (bound to C-c l g a).
  ;;
  ;; As a `consult' user, I find it more natural and comfortable to use
  ;; `consult-lsp-symbols' instead of `xref-find-apropos'.  The output of
  ;; `consult-lsp-symbols' feels more intuitive and user-friendly to me.
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)

  ;; Enable `which-key-mode' integration for LSP.
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;;;; Language Support
;;;;; JSON
;; Associate `json-mode' with .json and .jsonc files.
(add-to-list 'auto-mode-alist '("\\.jsonc?\\'" . json-mode))
(add-hook 'json-mode-hook (lambda () (hs-minor-mode 1)))

;;;;; HTML
;; Associate `web-mode' with .htm and .html files.  `web-mode' is an autonomous
;; emacs major-mode for editing web templates.  HTML documents can embed parts
;; (CSS / JavaScript) and blocks (client / server side).
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my|setup-web-mode-environment ()
  "Custom configurations for web-mode."
  ;; Enable rainbow-mode for color highlighting.
  (rainbow-mode 1)

  ;; Set indentation offsets.
  (setopt web-mode-markup-indent-offset 2)
  (setopt web-mode-css-indent-offset 2)
  (setopt web-mode-code-indent-offset 2)

  ;; Highlight current element and column.
  (setopt web-mode-enable-current-element-highlight t)
  (setopt web-mode-enable-current-column-highlight t))

(add-hook 'web-mode-hook #'my|setup-web-mode-environment)

;;;;; CSS
;; Associate `css-mode' with .css files.
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;; Set the indentation level for CSS files to 2 spaces.
(setopt css-indent-offset 2)

;; Defer loading `rainbow-mode' until `css-mode' is activated.
(add-hook 'css-mode-hook #'rainbow-mode)

;;;;; JS
;; Associate `js-mode' with javascript files.
(add-to-list 'auto-mode-alist '("\\.\\(?:c\\|m\\)?js\\'" . js-mode))

;; Set the indentation level for JavaScript files to 2 spaces.
(setopt js-indent-level 2)

;;;;; XML
;; Associate `xml-mode' with .plist files.
(add-to-list 'auto-mode-alist '("\\.plist\\'" . xml-mode))

;;;;; Yaml
;; Associate `yaml-mode' whith .yml and .yaml files.
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))


;;;;; Markdown
(defconst pandoc-executable-path (executable-find "pandoc")
  "The pandoc executable path on this system.")

(defconst gfm-patterns
  (rx (or "README" "CONTRIBUTTING" "BACKERS"
          (: "CHANGELOG" (? "-" (+ (in "." digit))))
          "CODE_OF_CONDUCT"
          "PULL_REQUEST_TEMPLATE"
          "pull_request_template"
          "bug_report"
          "feature_request")
      "."
      (or "markdown" "md")
      string-end)
  "Regexp to match files with GFM format.")

;; Associate '.markdown' and '.md' files with `markdown-mode'.
(add-to-list 'auto-mode-alist
             '("\\.\\(?:m\\(?:arkdown\\|d\\)\\)\\'" . markdown-mode))

;; Associate GitHub Flavored Markdown patterns with `gfm-mode'.
(add-to-list 'auto-mode-alist `(,gfm-patterns . gfm-mode))

;; Configure markdown command to use Pandoc.
(setq markdown-command pandoc-executable-path)

;; Enable wiki links in Markdown.
(setq markdown-enable-wiki-links t)

;; Enable LaTeX math support in Markdown.
(setq markdown-enable-math t)

;; Allow underscores for italic text.
(setq markdown-italic-underscore t)

;; Use asymmetric headers for different levels.
(setq markdown-asymmetric-header t)

;; Enable GitHub-style checkboxes as buttons.
(setq markdown-make-gfm-checkboxes-buttons t)

;; Use uppercase checkboxes in GitHub Flavored Markdown.
(setq markdown-gfm-uppercase-checkbox t)

;; Fontify code blocks natively.
(setq markdown-fontify-code-blocks-natively t)

;; Show full URLs instead of hiding them.
(setq markdown-hide-urls nil)

;; Add hooks for `markdown-mode'.
(add-hook 'markdown-mode-hook #'auto-fill-mode)
(add-hook 'markdown-mode-hook #'visual-line-mode)

;;;;; SQL
;; Associate .sql files with `sql-mode'.
(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))

(with-eval-after-load 'sql
  (define-key sql-mode-map (kbd "M-s s") #'sql-sqlite))

(require 'sqlite-mode)
(with-eval-after-load 'sqlite-mode
  (define-key sqlite-mode-map (kbd "M-s q") #'sqlite-mode-open-file)
  (define-key sqlite-mode-map (kbd "M-s t") #'sqlite-mode-list-tables)
  (define-key sqlite-mode-map (kbd "<tab>") #'sqlite-mode-list-data))

;;;;; CSV
;; Associate `.csv` files with `csv-mode'.
(add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))

;; Enable `csv-align-mode' automatically when `csv-mode is activated.
(add-hook 'csv-mode-hook #'csv-align-mode)

;;;;; Python
;; Ensure `python' mode is loaded when opening Python files.
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; Let Emacs guess the Python offset but prevent telling me about guessing.
;; Simple, I find the "Can’t guess python-indent-offset, using defaults: 4"
;; warning to be harmless and unnecessary spam.
(setopt python-indent-guess-indent-offset-verbose nil)

(defun flycheck-chain-lsp-python-checkers ()
  "Chain Python-specific Flycheck checkers after the LSP checker."
  (if (and (derived-mode-p 'python-mode)
             flycheck-python-flake8-executable)
      (flycheck-add-next-checker 'lsp 'python-flake8 t)
    (flycheck-remove-next-checker 'lsp 'python-flake8)))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-managed-mode-hook #'flycheck-chain-lsp-python-checkers))

(defun setup-python-environment ()
  "Setup a Python development environment in the current buffer."
  ;; Initialize Python-specific Flycheck checkers.
  (setq-local flycheck-python-flake8-executable (executable-find "flake8"))
  (setq-local flycheck-python-pylint-executable (executable-find "pylint"))
  (setq-local flycheck-python-mypy-executable (executable-find "mypy"))

  ;; Setup active backends for `python-mode'.
  (company-backend-for-hook 'lsp-completion-mode-hook
                            '(company-capf company-dabbrev-code))

  ;; Prevent `lsp-pyright' start in multi-root mode.
  ;; This must be set before the package is loaded.
  (setq-local lsp-pyright-multi-root nil)

  ;; Enable LSP support in Python buffers.
  (require 'lsp-pyright)

  ;; The difference between `lsp' and `lsp-deferred' is that
  ;; `lsp-deferred' will start the server when you switch to that
  ;; buffer (on demand).
  (lsp-deferred))

;; Configure hooks after `python-mode' is loaded.
(add-hook 'python-mode-hook #'setup-python-environment)

;;;;; Lisp and company
;; Associate `cask-mode' with Cask files.
(add-to-list 'auto-mode-alist '("Cask'" . cask-mode))

(defun my-elisp-outline-level ()
  "Calculate the outline level based on the number of leading semicolons.

Three semicolons represent level 1, four semicolons represent
level 2, and so on."
  (save-excursion
    (beginning-of-line)
    ;; Count the number of leading semicolons.
    (let ((count (length (match-string 0 (or (thing-at-point 'line t) "")))))
      ;; Adjust the count to match the desired outline levels.
      (- count 2))))

(defun my|lisp-modes-setup ()
  "Custom configurations for Lisp modes."
  (turn-on-eldoc-mode)

  ;; Make parenthesis depth easier to distinguish at a glance.
  (rainbow-delimiters-mode 1)

  ;; Comments that start with three semicolons, ";;;", are considered top-level
  ;; headings by `outline-minor-mode'.  Four or more semicolons can be used as
  ;; subheadings in hierarchical fashion.
  (setq-local outline-regexp "^;;;+[\t ]+")

  ;; Use `my-elisp-outline-level' to compute a header’s nesting level in an
  ;; outline.
  (setq-local outline-level #'my-elisp-outline-level)

  ;; Automatically rescan imenu on each call
  (setq-local imenu-auto-rescan t)

  ;; Disable optimization for finding previous index positions.
  ;; Ensures custom imenu expressions work correctly.
  (setq-local imenu-prev-index-position-function nil)

  ;; Disable custom index name extraction.
  ;; Allows using the full heading text in imenu without modification.
  (setq-local imenu-extract-index-name-function nil)

  ;; Set the default function for creating the imenu index.
  ;; Ensures our customizations integrate with the standard index creation process.
  (setq-local imenu-create-index-function 'imenu-default-create-index-function)

  ;; Disable skipping of comments and strings when creating the index.
  ;; Ensures that headings in comments are included in imenu.
  (setq-local imenu-generic-skip-comments-and-strings nil)

  (add-to-list
   'imenu-generic-expression
   '("Sections" "^\\(;;[;]\\{1,8\\}[\s\t]+\\)\\(.*\\)$" 2) t))

(add-hook 'emacs-lisp-mode-hook #'my|lisp-modes-setup)
(add-hook 'lisp-interaction-mode-hook #'my|lisp-modes-setup)

(define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)

;;;; Customizations.
;; Save custom variables in a separate file named custom.el
;; `user-emacs-directory'.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load `custom-file' if it exists or is a symbolic link.  This should be done
;; at the very end of the configuration to ensure that it does not override any
;; settings made earlier in this file.
(when (or (file-exists-p custom-file) (file-symlink-p custom-file))
  (load custom-file t t))

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; eval: (display-fill-column-indicator-mode)
;; coding: utf-8-unix
;; End:

;;; init.el ends here
