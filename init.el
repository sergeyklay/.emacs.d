;;; init.el --- Initialization file. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024 Serghei Iakovlev <gnu@serghei.pl>

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
    consult-flyspell      ; Flyspell integration with `consult'
    csv-mode              ; CSV file editing mode
    embark                ; Contextual actions in buffers
    embark-consult        ; Embark integration with `consult'
    envrc                 ; Environment variable manager for shell
    erc-hl-nicks          ; Nick highlighting in `erc' (IRC client)
    flyspell-correct      ; Correct spelling with popup menus
    git-modes             ; Modes for Git-related files
    htmlize               ; Convert buffer text to HTML
    json-mode             ; Major mode for editing JSON files
    lsp-mode              ; Language Server Protocol support
    lsp-pyright           ; LSP client leveraging pyright Python Language Server
    lsp-ui                ; UI enhancements for `lsp-mode'
    magit                 ; The Git porcelain inside Emacs
    marginalia            ; Annotations for minibufer completions
    markdown-mode         ; Major mode for Markdown files
    orderless             ; Flexible completion style for minibufers
    org-cliplink          ; Capture clipboard URLs to `org-mode'
    ox-hugo               ; Blogging using `org-mode'
    pass                  ; Interface to the Password Store
    password-store        ; Emacs interface for password-store
    prescient             ; Predictive sorting and filtering
    rainbow-delimiters    ; Color nested parentheses
    rainbow-mode          ; Colorize color values in buffers
    sql-indent            ; Indentation support for SQL
    vertico               ; Vertical interactive completion for minibuffers
    vertico-prescient     ; Prescient integration with `vertico'
    which-key             ; Display available keybindings
    writegood-mode        ; Improve writing style
    yaml-mode             ; Major mode for YAML files
    yasnippet             ; A template system for Emacs
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
    (define-key map (kbd ";") #'comment-or-uncomment-region)
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

;; Load `which-key' and enable `which-key-mode'.
(add-hook 'after-init-hook #'which-key-mode)

;; Increase the delay for which-key buffer to popup.
(setq-default which-key-idle-delay 1.5)


;;;; System Path
(cond
 ((eq system-type 'windows-nt)
  (let ((chocolatey-bin "C:/ProgramData/chocolatey/bin"))
    ;; Only add the directory to `exec-path' if it exists
    ;; and is not already present
    (when (and (file-directory-p chocolatey-bin)
               (not (member chocolatey-bin exec-path)))
      (add-to-list 'exec-path chocolatey-bin)))))


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

;; Disable the creation of lockfiles on Windows, as they can cause issues
;; with file systems on that platform.
(setq-default create-lockfiles (not (member system-type '(windows-nt))))

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
(let ((src (if (eq system-type 'windows-nt)
               "D:/src/emacs.git"
             (expand-file-name "~/src/emacs.git"))))
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


;;;; Spell
(require 'ispell)

;; Automatically save the personal dictionary without asking for confirmation.
(setopt ispell-silently-savep t)

(when (eq system-type 'windows-nt)
  (let ((dic-dir (or (getenv "DICPATH") "C:/Hunspell"))
        (hunspell-bin (executable-find "hunspell")))
    (when (and hunspell-bin (file-exists-p dic-dir))
      ;; Set environment variables for Hunspell
      (unless (getenv "LANG") (setenv "LANG" "en_US"))
      (unless (getenv "DICPATH") (setenv "DICPATH" dic-dir))

      (defun my--build-dict-path (lang ext)
        "Build the full path to the dictionary file for LANG and EXT."
        (expand-file-name (concat lang ext) (file-name-as-directory dic-dir)))

      ;; Find all available .aff files in the dictionary directory
      (let ((dict-files (directory-files dic-dir t "\\.aff$"))
            dict-paths)
        (dolist (dict-file dict-files)
          ;; Extract language code from file name
          ;; (e.g., "en_US" from "en_US.aff")
          (let ((lang (file-name-base dict-file)))
            ;; Add dictionary to the alist if both .aff and .dic files exist
            (when (file-exists-p (my--build-dict-path lang ".dic"))
              (push (list lang (my--build-dict-path lang ".aff")) dict-paths))))

        ;; Set `ispell-hunspell-dict-paths-alist' dynamically
        ;; if there are valid dictionaries
        (when dict-paths
          (setq ispell-hunspell-dict-paths-alist dict-paths))

        ;; Use Hunspell as the spell checker.  This should be set after
        ;; `ispell-hunspell-dict-paths-alist', "LANG" and "DICPATH" environment
        ;; variables due to `setopt' nature.
        (setopt ispell-program-name hunspell-bin)))))

;; Configure the list of my dictionaries.
;; Note: Homebrew on macOS, and Chocolatey on Windows does not provide
;; dictionaries by default.  You will need to install them manually.
(let ((valid-dicts (ispell-valid-dictionary-list))
      (dicts-alist nil))

  ;; Add English dictionary
  (when (member "en_US" valid-dicts)
    ;; Define the default dictionary to be used.
    (setopt ispell-local-dictionary "en_US")
    (push '("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" t
            ("-d" "en_US") nil utf-8)
          dicts-alist))

  ;; Add Polish dictionary
  (let ((pl-dict (cond
                  ((member "pl" valid-dicts) "pl")
                  ((member "pl_PL" valid-dicts) "pl_PL")
                  (t nil))))
    (when pl-dict
      (push `("polish" "[[:alpha:]]" "[^[:alpha:]]" "[']" t
              ("-d" ,pl-dict) nil utf-8)
            dicts-alist)))

  ;; Add Russian dictionary
  (let ((ru-dict (cond
                  ((member "ru-yeyo" valid-dicts) "ru-yeyo")
                  ((member "russian-aot-ieyo" valid-dicts) "russian-aot-ieyo")
                  ((member "ru_RU" valid-dicts) "ru_RU")
                  (t nil))))
    (when ru-dict
      (push `("russian" "[А-Яа-я]" "[^А-Яа-я]" "[-']" nil
              ("-d" ,ru-dict) nil utf-8)
            dicts-alist)))

  (setopt ispell-local-dictionary-alist dicts-alist))

;; Be silent when checking words to avoid unnecessary messages.
(setopt flyspell-issue-message-flag nil)

(with-eval-after-load 'flyspell
  ;; These keys are intended for `embark'
  (dolist (key '("C-;" "C-," "C-."))
    (unbind-key key flyspell-mode-map)))

;; Load `writegood-mode' and set up hooks.
(with-eval-after-load 'writegood-mode
  (add-hook 'text-mode-hook #'writegood-mode)
  (add-hook 'latex-mode-hook #'writegood-mode))


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
(unless (eq (window-system) 'w32)
  ;; Set pinentry mode to loopback for all systems except Windows.
  ;; For more see "man 1 gpg" for the option "--pinentry-mode".
  (setopt epg-pinentry-mode 'loopback))

;; GnuPG ID of your default identity.
(setq epg-user-id "1E0B5331219BEA88")

;; Specify the key to use for file encryption
;; Also used for `org-crypt-key' (see bellow).
(setq epa-file-encrypt-to `(,epg-user-id))

;; Enable automatic encryption/decryption of *.gpg files
(require 'epa-file)

;; Only enable `epa-file' if it's not already enabled
(unless (memq epa-file-handler file-name-handler-alist)
  (epa-file-enable))

;; Setup `pass-view-mode' iff pass executable is available.
(when (executable-find "pass")
  (my-ensure-directory-exists (expand-file-name "~/.password-store"))

  (require 'pass)
  (with-eval-after-load 'pass
    ;; Associate .gpg files in the pass directory with `pass-view-mode'.
    (add-to-list
     'auto-mode-alist (cons "~/.password-store/.*\\.gpg\\'" 'pass-view-mode))))


;;;; Calendar
;; Disable not used holidays
(setq-default holiday-hebrew-holidays nil)
(setq-default holiday-islamic-holidays nil)
(setq-default holiday-bahai-holidays nil)
(setq-default holiday-oriental-holidays nil)
(setq-default holiday-solar-holidays nil)
(setq-default holiday-christian-holidays nil)

(defconst my-polish-holidays
  '((holiday-fixed 1 1 "Nowy Rok (Poland)")
    (holiday-fixed 5 1 "Święto Pracy (Poland)")
    (holiday-fixed 5 3 "Święto Konstytucji 3 Maja (Poland)")
    (holiday-fixed 8 15 "Wniebowzięcie Najświętszej Maryi Panny (Poland)")
    (holiday-fixed 11 1 "Wszystkich Świętych (Poland)")
    (holiday-fixed 11 11 "Święto Niepodległości (Poland)")
    (holiday-fixed 12 25 "Boże Narodzenie (Poland)")
    (holiday-fixed 12 26 "Drugi dzień Bożego Narodzenia (Poland)"))
  "Polish public holidays.")

(defvar my-ukrainian-holidays
  '((holiday-fixed 1 1 "Новий рік (Ukraine)")
    (holiday-fixed 1 7 "Різдво Христове (Ukraine)")
    (holiday-fixed 3 8 "Міжнародний жіночий день (Ukraine)")
    (holiday-fixed 4 1 "День гумору (Ukraine)")
    (holiday-fixed 5 1 "День праці (Ukraine)")
    (holiday-fixed 5 9 "День перемоги (Ukraine)")
    (holiday-fixed 6 28 "День Конституції України (Ukraine)")
    (holiday-fixed 8 24 "День Незалежності України (Ukraine)")
    (holiday-fixed 10 1 "День захисників і захисниць України (Ukraine)")
    (holiday-fixed 12 25 "Різдво Христове (Ukraine)"))
  "Ukrainian public holidays.")

(defvar my-canadian-holidays
  '((holiday-fixed 1 1 "New Year's Day (Canada)")
    (holiday-fixed 7 1 "Canada Day (Canada)")
    (holiday-float 9 1 1 "Labour Day (Canada)")
    (holiday-float 10 1 2 "Thanksgiving Day (Canada)")
    (holiday-fixed 12 25 "Christmas (Canada)")
    (holiday-fixed 12 26 "Boxing Day (Canada)"))
  "Canadian public holidays.")

(defvar my-us-holidays
  '((holiday-fixed 1 1 "New Year's Day (US)")
    (holiday-float 1 1 3 "Martin Luther King Jr. Day (US)")
    (holiday-float 2 1 3 "Presidents' Day (US)")
    (holiday-fixed 7 4 "Independence Day (US)")
    (holiday-float 11 4 4 "Thanksgiving (US)")
    (holiday-fixed 12 25 "Christmas (US)"))
  "US public holidays.")

;; Define local holidays to be tracked.
(setq holiday-local-holidays
      '((holiday-fixed 10 24 "Programmers' Day")
        (holiday-fixed 3 8  "Women's Day")
        (holiday-fixed 6 1  "Children's Day")
        (holiday-fixed 9 10 "Teachers' Day")
        (holiday-fixed 3 12 "Arbor Day")))

;; Define other holidays to be tracked.
(setq-default holiday-other-holidays
              '((holiday-fixed 4 23 "World Book Day")
                (holiday-fixed 2 14 "Valentine's Day")
                (holiday-fixed 4 1 "April Fools' Day")
                (holiday-fixed 10 31 "Halloween")
                (holiday-sexp '(if (or (zerop (% year 400))
                                       (and (% year 100) (zerop (% year 4))))
                                   (list 9 12 year)
                                 (list 9 13 year))
                              "World Programmers' Day")))

;; Customize `calendar-holidays' to include public holidays from various
;; countries.  I work in a multinational company with teams based in different
;; regions, so it's useful to know when my colleagues have days off.
(setq-default calendar-holidays
              `(,@my-polish-holidays
                ,@my-ukrainian-holidays
                ,@my-canadian-holidays
                ,@my-us-holidays
                ,@holiday-local-holidays
                ,@holiday-other-holidays))

;; Week starts on Monday.
(setq-default calendar-week-start-day 1)

;; I prefer read dates in 'year/month/day' format.
(setq-default calendar-date-style 'iso)

;; Prefer +0800 over CST.
(setq-default calendar-time-zone-style 'numeric)

;; Mark dates of holidays in the calendar window.
(setq-default calendar-mark-holidays-flag t)

;; Mark dates with diary entries, in the calendar window.
(setq-default calendar-mark-diary-entries-flag t)

;; Set the default latitude and longitude for calendar calculations.  These
;; values are used to determine local sunrise/sunset times, moon phases, and
;; other location-dependent calendar features in Emacs.
(setq-default calendar-latitude 51.1)
(setq-default calendar-longitude 17.0)

;; Alist of time zones and places for `world-clock' to display for the same
;; reason as for `calendar-holidays' above.
(setq-default world-clock-list '(("Canada/Pacific" "Vancouver")
                                 ("America/New_York" "New York")
                                 ("Europe/Warsaw" "Warsaw")
                                 ("Europe/Kyiv" "Kyiv")))


;;;; Organization
(require 'org)

(with-eval-after-load 'org
  ;; Ensure the directory for Org files is exist.
  (my-ensure-directory-exists org-directory)

  ;; Although I don't use `diary-file', Org-agenda still checks for its
  ;; existence even when `org-agenda-include-diary' is set to nil.  This happens
  ;; due to internal checks tied to `org-agenda-diary-file'.  Since I handle
  ;; recurring tasks differently within my Org files and don't need an
  ;; additional diary, I don't want to set `org-agenda-diary-file' to anything
  ;; meaningful.  As a simple workaround, I just create an empty `diary-file' to
  ;; bypass these checks and keep Org-agenda running smoothly.
  (unless (file-exists-p diary-file)
    (make-empty-file diary-file)))

;; Associate .org, .org_archive and .txt files with `org-mode'.
(add-to-list 'auto-mode-alist
             '("\\.\\(org\\|org_archive\\|txt\\)\\'" . org-mode))

;; When opening an Org file, start with all top-level headers collapsed.
(setq org-startup-folded t)

;; Automatically log the timestamp when a TODO is marked as DONE.
(setq org-log-done 'time)

;; Store notes and state changes in a drawer instead of cluttering the entry.
(setq org-log-into-drawer t)

;; Hide emphasis markers (e.g., bold, italics) to see styled text only.
(setq org-hide-emphasis-markers t)

;; Set the default width for inline images to 600 pixels.
(setq org-image-actual-width '(600))

;; Enforce TODO dependencies, blocking parent tasks from being marked DONE
;; if child tasks are still not completed.
(setq org-enforce-todo-dependencies t)

;; Modify only the agenda context in `org-fold-show-context-detail' without
;; altering the other default values.
(setq org-fold-show-context-detail
      (cons '(agenda . lineage)
            (assq-delete-all 'agenda org-fold-show-context-detail)))

;; Allow setting single tags without bringing up the tag selection menu.
(setq org-fast-tag-selection-single-key t)

;; Configure the languages supported in org-babel code blocks.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (emacs-lisp . t)
   (haskell . t)
   (js . t)
   (latex . t)
   (lisp . t)
   (makefile . t)
   (org . t)
   (python . t)
   (scheme . t)
   (shell . t)
   (sql . t)
   (calc . t)))

(defun my|org-setup-environment ()
  "Set up a tailored environment for editing Org-mode buffers.

This function customizes the appearance and behavior of `org-mode' by
enabling modes and features that enhance the editing experience:

- Enables `visual-line-mode' for soft line wrapping, providing a more
  natural reading and writing flow for long lines of text.
- Activates `org-indent-mode' to visually align text according to the
  structure of the outline, improving readability.
- Displays inline images directly within the buffer when they are linked
  in Org files, providing immediate visual feedback."
  (visual-line-mode 1)
  (org-indent-mode 1)
  (org-display-inline-images 1))

;; Attach the environment setup to Org-mode.
(add-hook 'org-mode-hook #'my|org-setup-environment)

(defun my/switch-to-org ()
  "Get a scratch buffer for the `org-mode'."
  (interactive)
  (let* ((name "*scratch-org*")
         (buf (get-buffer name)))
    (pop-to-buffer
     (if (bufferp buf)
         buf  ; Existing scratch buffer
       ;; New scratch buffer
       (with-current-buffer (get-buffer-create name)
         (org-mode)
         (goto-char (point-min))
         ;; Initial documentation displayed in *scratch-org* buffer.
         (insert "# This buffer is for notes and Org ")
         (insert "structure editing that are not saved.\n")
         (insert "# To create a file, visit it with ")
         (insert (format "%s and enter text in its buffer.\n\n"
                         (substitute-command-keys "\\[find-file]")))
         (current-buffer))))))

(define-key my-keyboard-map (kbd "o") #'my/switch-to-org)

;; Standard actions
(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o b") #'org-switchb)

;;;;; Helpers
(defun my-org-get-created-date ()
  "Retrieve the CREATED property if it exists, otherwise return nil.

This function utilizes Org-mode's built-in date parsing
capabilities by using `org-parse-time-string' to handle various
date formats.  If no valid date is found, it returns nil."
  (let* ((created (org-entry-get nil "CREATED" t))
         (parsed-date (and created (org-parse-time-string created))))
    (when (and parsed-date
               ;; Ensure that year, month, and day are all present
               (nth 4 parsed-date)
               (nth 5 parsed-date)
               (nth 3 parsed-date))
      (format "%04d-%02d-%02d"
              (nth 5 parsed-date)  ; Year
              (nth 4 parsed-date)  ; Month
              (nth 3 parsed-date)))))

(defun my-org-sanitize-heading-for-id (heading)
  "Sanitize the HEADING text to create a slug suitable for use as an ID.

Removes progress indicators, priority markers, links, timestamps,
non-alphanumeric characters and replaces spaces with hyphens."
  (let ((slug (substring-no-properties heading)))
    ;; Replacements for common symbols
    (setq slug (replace-regexp-in-string "&" "and" slug))
    (setq slug (replace-regexp-in-string "\\." "dot" slug))
    (setq slug (replace-regexp-in-string "\\+" "plus" slug))
    ;; Remove progress indicators like "[25%]" or "[2/7]"
    (setq slug (replace-regexp-in-string "\\(\\[[0-9]+%\\]\\)" "" slug))
    (setq slug (replace-regexp-in-string "\\(\\[[0-9]+/[0-9]+\\]\\)" "" slug))
    ;; Remove priority indicators like "[#A]"
    (setq slug (replace-regexp-in-string "\\(\\[#[ABC]\\]\\)" "" slug))
    ;; Remove links but keep their descriptions
    (setq slug (replace-regexp-in-string "\\[\\[\\(.+?\\)\\]\\[" "" slug t))
    ;; Remove timestamps (active and inactive)
    (setq slug (replace-regexp-in-string
                "<[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)?>"
                "" slug t))
    (setq slug (replace-regexp-in-string
                "\\[[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)\\]"
                "" slug t))
    ;; Replace spaces with hyphens and remove non-alphanumeric characters
    (setq slug (replace-regexp-in-string "[^[:alnum:][:space:]-]" "" slug))
    (setq slug (replace-regexp-in-string " +" "-" slug))
    ;; Remove trailing hyphens, downcase and trim length to 60 chars
    (string-limit (downcase (replace-regexp-in-string "-+$" "" slug)) 60)))

(defun my-org-find-property (property)
  "Find the position of the first occurrence of PROPERTY's value.

Return the position of the property's value, or nil if not found.
If narrowing is in effect, only search the visible part of the
buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
	  (re (org-re-property property nil t nil)))
      (catch 'exit
	(while (re-search-forward re nil t)
	  (when (org-entry-get (point) property nil t)
	    (throw 'exit (match-beginning 3))))))))

(defun my/org-goto-id-property ()
  "Move cursor to the start of the ID property value in Org heading."
  (interactive)
  (let ((drawer-pos nil)
        (id-pos nil))
    (save-excursion
      (org-back-to-heading t)
      (let ((case-fold-search t)
            (start (point)))
        (outline-next-visible-heading 1)
        (save-restriction
          (narrow-to-region start (point))
          (setq id-pos (my-org-find-property "ID"))
          (when id-pos
            (goto-char id-pos)
            (setq drawer-pos
                  (re-search-backward "^\\s-*:PROPERTIES:" nil t))))))
    (when (and id-pos drawer-pos)
      (org-show-entry)
      (goto-char drawer-pos)
      (org-flag-drawer nil)
      (goto-char id-pos))))

(define-key my-keyboard-map (kbd "M-i") #'my/org-goto-id-property)

(defun my/org-generate-id ()
  "Create a human-readable ID for the current entry and return it.

If the entry already has an ID, just return it.  The function
generates a new ID by performing the following steps:

1. Retrieves the heading text of the current Org heading.
2. Sanitizes the heading text to form a slug suitable for an ID.
   The sanitization process removes or replaces special
   characters to create a clean, URL-friendly string.
3. Prepends the slug with a date prefix.  The date is derived
   from the CREATED property if it exists; otherwise, today's
   date is used in the format YYYY-MM-DD.
4. Sets the generated ID as the ID property of the current
   heading.
5. Updates the Org database of ID locations with the new ID and
   the file location to ensure the ID can be tracked and
   retrieved in the future.

The function returns the generated ID, or if an existing ID is
found, it returns the existing ID."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This function is intended to be used in Org-mode only"))
  (org-with-point-at nil  ; with nil refer to the entry at point
    (let ((id (org-id-get)))
      (cond
       ;; Check if the ID property already exists
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       ;; If not - retrieve the heading text and create a new ID
       (t
        (let* ((heading (nth 4 (org-heading-components)))
               (clean-heading (my-org-sanitize-heading-for-id heading))
               ;; Use CREATED property if available, otherwise today's date
               (date-prefix (or (my-org-get-created-date)
                                (format-time-string "%Y-%m-%d")))
               (id (concat date-prefix "-" clean-heading)))
          ;; Set the ID property
          (org-set-property "ID" id)
          ;; Add the ID with location FILE to the database of ID locations.
          (org-id-add-location id (or org-id-overriding-file-name
				      (buffer-file-name (buffer-base-buffer))))
          id))))))

(defun my/org-id-advice (&rest args)
  "The advice to update Org ID locations.

This function accepts ARGS but does not use them directly.
It generates an Org ID if needed and updates the ID locations."
  (my/org-generate-id)
  (org-id-update-id-locations)
  args)

(advice-add 'org-store-link :before #'my/org-id-advice)

(define-key my-keyboard-map (kbd "I") #'my/org-generate-id)

(defun my/org-mark-as-project ()
  "Mark the current Org heading as a project.

This function ensures that the current heading:
1. Has the tag project
2. Has the property COOKIE_DATA set to todo recursive
3. Has a TODO keyword
4. Starts with a progress indicator [/]

This setup helps in organizing projects within Org Mode
and ensures that projects are easily identifiable and managed.

I find this approach particularly appealing, and I first learned
about it from an article by Karl Voit.  The original function was
created by Karl, and this version is simply a refined and
optimized adaptation tailored to my specific needs.  For origin
see: https://karl-voit.at/2019/11/03/org-projects/"
  (interactive)
  ;; Try to move to a heading if not already there
  (unless (org-at-heading-p)
    (org-back-to-heading t)
    (unless (org-at-heading-p)
      (user-error
       "No Org heading found.  Please place the cursor at or near a heading")))

  ;; Ensure the :project: tag is added
  (org-toggle-tag "project" 'on)

  ;; Set the COOKIE_DATA property to 'todo recursive'
  (org-set-property "COOKIE_DATA" "todo recursive")

  ;; Ensure the :ID: property is added
  (my/org-generate-id)

  (let* ((title (nth 4 (org-heading-components)))
         (keyword (nth 2 (org-heading-components))))

    ;; Add TODO keyword if missing
    (unless keyword (org-todo "TODO"))

    ;; Ensure that the heading starts with a progress indicator '[/]'
    (unless (string-match-p "\\[.*\\]" title)
      (org-edit-headline (concat "[/] " title))))

  ;; Update statistics cookies
  (org-back-to-heading t)
  (org-update-statistics-cookies nil)

  ;; Save changes
  (save-buffer)

  ;; Inform the user of success
  (message "Heading marked as a project."))

(define-key my-keyboard-map (kbd "P") #'my/org-mark-as-project)

;;;;; Org Crypt
;; Set my encrypt key from `epa-file-encrypt-to' (see above).
(setopt org-crypt-key epg-user-id)

;; Do not ask for disabling `auto-save-mode'.
(setopt org-crypt-disable-auto-save nil)

;; Check if GPG is available, then require `org-crypt'.
;; Otherwise, display a warning.
(if my-gpg-program
    (progn
      (require 'org-crypt)
      ;; Encrypt all entries before saving.
      (org-crypt-use-before-save-magic))
  (warn "GPG is not available. `org-crypt' could not be loaded."))

;; Projects are tagged with :project: and :crypt: is used to mark headings
;; to be encrypted.  I don't want all subitems to pop up in the corresponding
;; agenda view.
(setopt org-tags-exclude-from-inheritance '("project" "crypt"))

;;;;; Org Contib
(with-eval-after-load 'org
  ;; Take a URL from the clipboard and inserts an org-mode link.
  (require 'org-cliplink)
  (global-set-key (kbd "C-x p i") #'org-cliplink))

(with-eval-after-load 'ox
  (require 'ox-hugo))

;;;;; Org TODO
;; Define my default keywords as workflow states.
;; The command C-c C-t cycles an entry from TODO to CANCELED.
;; For details see: https://orgmode.org/manual/Workflow-states.html
(setq org-done-keywords '("DONE" "CANCELED"))
(setq org-todo-keywords
      '((sequence
         ;; Need action
         "TODO(t)" "STARTED(s)" "WAITING(w@/!)" "|"
         ;; Done
         "DONE(d!/!)" "CANCELED(c@/!)")))

;; Define the style of the keywords
(setq org-todo-keyword-faces
      '(("TODO"     :foreground "firebrick1"   :weight bold)
        ("STARTED"  :foreground "red3"         :weight bold)
        ("WAITING"  :foreground "orange"       :weight bold)
        ("DONE"     :foreground "forest green" :weight bold)
        ("CANCELED" :foreground "lime green"   :weight bold)))

;;;;; Org Capture
(defconst my-org-capture-template-simple
  "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
  "A template for a simple heading with keyword TODO in Org files.")

(defconst my-org-capture-template-blog
  ;; Quick template expansion explanation:
  ;;
  ;; %?         - position point here
  ;; %^g        - prompt for tags
  ;; %U         - date
  ;; %^{PROMPT} - prompt for a string
  ;;
  ;; For the full documentation see:
  ;; https://orgmode.org/org.html#Template-expansion-1
  (concat
   "* TODO %?        :blog:%^g\n"
   ":PROPERTIES:\n:CREATED: %U\n:ID: %^{PROMPT}\n:END:\n\n"
   "-----------------------\n"
   "- [ ] Link on older/similar articles?\n"
   "- [ ] Link on tag pages?\n\n")
  "A template for capturing blog-related ideas.")

;; Default target for capturing notes.  Also used for mobile sync.
;; See `org-mobile-inbox-for-pull' bellow.
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

;; Define custom Org Capture templates
(setq org-capture-templates
      `(("s" "Shorts" entry (file+headline "misc.org" "Shorts")
         ,my-org-capture-template-simple :empty-lines 1)
        ("e" "Event" entry (file+headline "misc.org" "Events")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
        ("t" "Trip Checklist" checkitem
         (file+headline "misc.org" "Trip Checklist"))
        ("b" "Bookmark" entry (file+headline "notes.org" "Bookmarks")
         "* %x%?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
        ("i" "Blog Idea" entry (file+headline "blog.org" "Captured Blog Ideas")
         ,my-org-capture-template-blog :empty-lines 1)
        ("n" "Thought or Note" entry (file+headline "notes.org" "Random Notes")
          ,my-org-capture-template-simple :empty-lines 1)
        ("I" "Inbox, refile later" entry (file "")
          "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 0)
        ("r" "To-Read" entry (file+headline "misc.org" "To-Read List")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%x\n\n" :empty-lines 1)
        ("w" "To-Watch" entry (file+headline "misc.org" "To-Watch List")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%x\n\n" :empty-lines 1)
        ("B" "Business")
        ("Bs" "Shorts" entry (file+headline "business.org" "Shorts")
         ,my-org-capture-template-simple :empty-lines 1)
        ("Bm" "Meeting Note" entry (file+headline "airslate.org" "Meeting Notes")
         ,my-org-capture-template-simple :empty-lines 1)
        ("Be" "Event" entry (file+headline "business.org" "Events")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)))

(global-set-key (kbd "C-c c") #'org-capture)

;;;;; Org Mobile
;; Set up for syncing Org files between current workstation and mobile app.
;;
;; This configuration is compatible with several mobile applications that handle
;; Org files, such as Beorg, Plain Org, and potentially MobileOrg.  The
;; `org-mobile-directory' is set to the user's "Notes" folder, which must be
;; properly configured in the mobile application for syncing.
;;
;; On iOS, applications like Beorg and Plain Org can access the synced files by
;; navigating to the specified directory via iCloud.  This setup would also
;; theoretically work with Dropbox if it supported symbolic links in the 21st
;; century, but Dropbox's current limitations in background syncing with
;; Org-related apps make it unreliable - you'll forced to put it in ~/Dropbox/*
;; directory.  Moreover, I have observed an unpleasant behavior with Dropbox:
;; changes made to a file on the computer are not reflected in the mobile
;; application that works with Org files.  My guess is that there’s something
;; missing from Dropbox’s mobile background syncing process, unrelated to
;; Org-related apps specifically.
;;
;; Beorg Setup:
;; 1. Under Files Synchronization setting in the Beorg mobile app choose Sync
;;    method, and select "Choose Folder"
;; 2. Next, tap "Link Folder"
;; 3. Select up syncing with the "Notes" directory.
;;
;; Plain Org Setup:
;; 1. Open the menu in the Plain Org mobile app.
;; 2. Select "Open file."
;; 3. Navigate to and open a file located in your "Notes" directory.
;;
;; For MobileOrg on macOS with iCloud, the path would typically need to be
;; set to something like
;; "~/Library/Mobile Documents/iCloud~com~mobileorg~mobileorg/Documents".
;; However, I find the performance of the MobileOrg app on my phone
;; unsatisfactory by my quality standards; the way this app behaves and the
;; issues it creates in my daily workflow are unacceptable to me.
(setq org-mobile-directory (expand-file-name "~/Documents/Notes"))

(with-eval-after-load 'org
  ;; Ensure directories for export/import Org files are exist.
  (my-ensure-directory-exists org-mobile-directory)
  (my-ensure-directory-exists
   (expand-file-name "Reports" org-mobile-directory)))

;; Do not generate IDs for all headings.
(setq org-mobile-force-id-on-agenda-items nil)

;; Append captured notes and flags to `org-default-notes-file' file.
(setq org-mobile-inbox-for-pull org-default-notes-file)

(defun my/mobile-org-pull ()
  "Import content from `org-mobile-capture-file' to `org-mobile-inbox-for-pull'.

This function performs the following steps:
1. Executes `org-mobile-pull' to synchronize mobile Org data.
2. Opens the `org-mobile-inbox-for-pull' file.
3. Removes trailing whitespace from the buffer.

It is intended to be used as part of the mobile Org synchronization
workflow, ensuring that captured tasks and notes are properly imported
and cleaned up for further processing."
  (interactive)
  (org-mobile-pull)
  (find-file org-mobile-inbox-for-pull)
  (delete-trailing-whitespace))

(define-key my-keyboard-map (kbd "C-p") #'my/mobile-org-pull)

(defun my/mobile-org-push ()
  "Push a custom agenda setup to MobileOrg directory.

This function temporarily modifies `org-agenda-custom-commands'
to set up an agenda suitable for MobileOrg, including a 1-month
agenda view and a filtered list of standalone tasks (excluding
projects and completed tasks).  After pushing the agenda to
MobileOrg, the original `org-agenda-custom-commands' is restored."
  (interactive)
  (let* ((original-org-agenda-custom-commands org-agenda-custom-commands)
         (org-agenda-custom-commands
          '(("m" "Monthly Overview"
             ((agenda "1 month"
                      ((org-agenda-span 31)
                       (org-agenda-time-grid nil)
                       (org-agenda-entry-types '(:timestamp :sexp))))))
            ("s" "Standalone Tasks"
             ((tags-todo "-project-DONE-CANCELED"
                         ((org-agenda-skip-function
                           '(my-org-agenda-skip-if-parent-has-tag
                             "project")))))))))
    ;; Generate MobileOrg export.
    (org-mobile-push)))

(define-key my-keyboard-map (kbd "M-p") #'my/mobile-org-push)

;;;;; Org Agenda
(defconst my-org-agenda-files-work
  `(,(expand-file-name "business.org" org-directory)
    ,(expand-file-name "airslate.org" org-directory))
  "The list of my work agenda files.")

(defconst my-org-agenda-files-life
  `(,(expand-file-name "blog.org" org-directory)
    ,(expand-file-name "contacts.org" org-directory)
    ;; Finances / Legal / Authorities / Insure / Regulate
    ,(expand-file-name "flair.org" org-directory)
    ,(expand-file-name "hardware.org" org-directory)
    ,(expand-file-name "inbox.org" org-directory)
    ,(expand-file-name "misc.org" org-directory)
    ,(expand-file-name "notes.org" org-directory))
  "The list of my non-work agenda files.")

;; I maintain two categories of agenda files: work and non-work files.
(setq org-agenda-files
      (append my-org-agenda-files-work my-org-agenda-files-life))

(defun my|create-missing-org-files ()
  "Create missing files listed in `org-agenda-files'."
  (dolist (file org-agenda-files)
    (unless (file-exists-p file)
      (with-temp-buffer
        (write-file file))
      (message "Created missing org file: %s" file))))

(add-hook 'emacs-startup-hook #'my|create-missing-org-files)

(defun my|org-agenda-refresh-on-idle ()
    "Set up a timer to refresh Org Agenda buffers after 60s of idle time."
    (run-with-idle-timer 60 nil
                         (apply-partially #'org-agenda-prepare-buffers
                                          (org-agenda-files t t))))

(with-eval-after-load 'org-agenda
  ;; Always highlight the current agenda line.
  (add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))

  ;; Refresh Org Agenda buffers on idle.
  (add-hook 'org-agenda-mode-hook #'my|org-agenda-refresh-on-idle)

  ;; Re-align tags when window shape changes
  (add-hook 'window-configuration-change-hook #'org-agenda-align-tags nil t))

(defun my/org-search-agenda ()
  "Search (i.e. ripgrep) stuff in my agenda files."
  (interactive)
  (consult-ripgrep org-directory))

(define-key my-keyboard-map (kbd "C-s") #'my/org-search-agenda)

;; Show diaries in my agenda.
(setopt org-agenda-include-diary t)

;; Do not initialize agenda Org files when generating (only) agenda.
(setq org-agenda-inhibit-startup t)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Number of days to include in overview display.
(setq org-agenda-span 1)

;; Restore windows layout after quit agenda.
(setq org-agenda-restore-windows-after-quit t)

(defun my-org-agenda-skip-non-stuck-projects ()
  "Skip projects that are not stuck."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (or (org-entry-is-done-p)  ; Skip if project is done
            (save-excursion
              (re-search-forward "^\\*+ \\(TODO\\)" subtree-end t))
            (save-excursion
              (re-search-forward
               "^\\*+.* SCHEDULED:.*\\|^[[:space:]]*SCHEDULED:.*"
               subtree-end t)))
        subtree-end  ; Skip if project has TODO or SCHEDULED descendants
      nil)))         ; Do not skip if the project is stuck

(defun my-org-agenda-skip-if-parent-has-tag (tag)
  "Skip an entry if any of its parents have the specified TAG.

TAG should be a string without the colons, e.g., project."
  (let ((parent-skipped nil))
    (save-excursion
      (while (and (not parent-skipped) (org-up-heading-safe))
        (when (member tag (org-get-tags))
          (setq parent-skipped t))))
    (when parent-skipped
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun my-org-agenda-skip-parent-if-has-scheduled-childs ()
  "Skip a parent heading if it has uncompleted children that are scheduled.

This function is intended to be used with
`org-agenda-skip-function' to refine the agenda view.  It checks
if the current heading is a TODO item and skips it if it has any
children that are not yet completed but have a scheduled time."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (save-excursion
      (org-back-to-heading t)
      (when (org-entry-is-todo-p)  ; Only interested in TODO entries
        (let ((has-scheduled-childs nil)
              (has-deadline (org-entry-get (point) "DEADLINE")))
          (save-excursion
            (org-map-entries
             (lambda ()
               (when (and (org-entry-is-todo-p)
                          (not (org-entry-is-done-p))
                          ;; Look for SCHEDULED or DEADLINE
                          (or (org-get-scheduled-time (point))
                              (org-entry-get (point) "DEADLINE")))
                 (setq has-scheduled-childs t)))
             nil 'tree))
          ;; If this function returns nil, the current match should not be
          ;; skipped.  Otherwise, the function must return a position from where
          ;; the search should be continued.
          (when (or has-scheduled-childs has-deadline)
            ;; In the context of `org-agenda-skip-function', returning
            ;; `subtree-end' tells the agenda to skip over the entire subtree of
            ;; the current heading.
            (goto-char subtree-end)))))))

(defun my-org-cmp-closed (a b)
  "Compare Org agenda entries A and B by their CLOSED timestamp.
Return +1 if A is closed more recently than B, -1 if B is closed
more recently than A, and nil if they have the same CLOSED time."
  (let* ((a-marker (get-text-property 0 'org-marker a))
         (b-marker (get-text-property 0 'org-marker b))
         (now (current-time))
         (a-closed-ts (org-timestamp-from-string
                       (org-entry-get a-marker "CLOSED")))
         (b-closed-ts (org-timestamp-from-string
                       (org-entry-get b-marker "CLOSED")))
         (a-closed-time (or (and a-closed-ts
                                 (org-timestamp-to-time a-closed-ts))
                            now))
         (b-closed-time (or (and b-closed-ts
                                 (org-timestamp-to-time b-closed-ts))
                            now)))
    (cond ((time-less-p b-closed-time a-closed-time) +1)
          ((time-less-p a-closed-time b-closed-time) -1)
          (t nil))))

(defun my-get-week-start-date ()
  "Return the date and time of the start of the current week."
  (let* ((current-time (current-time))
         ;; %u gives the day of the week where Monday is 1 and Sunday is 7
         (day-of-week (string-to-number (format-time-string "%u" current-time)))
         ;; Calculate the offset from the current day to the start of the week.
         ;; If `day-of-week' is before `calendar-week-start-day', `mod' ensures
         ;; that the result is always non-negative.
         (days-from-start (mod (- day-of-week calendar-week-start-day) 7))
         (week-start-time (time-subtract current-time
                                         (days-to-time days-from-start))))
    (format-time-string "%Y-%m-%d 00:00" week-start-time)))

(defun my-get-month-start-date ()
  "Return the date and time of the start of the current month."
  (format-time-string "%Y-%m-01 00:00"))

(defun my-weekly-agenda-export-range ()
  "Return the start and end dates of the current week as a list.

The start date is determined by `my-get-week-start-date', which is
assumed to return the current week's Monday.  The end date is calculated
by adding six days to the start date, yielding the upcoming Sunday.
Dates are formatted as YYYY.MM.DD."
  (let* ((week-start (my-get-week-start-date))
         (week-end (time-add (date-to-time week-start) (days-to-time 6))))
    `(,(format-time-string "%Y.%m.%d" (date-to-time week-start))
      ,(format-time-string "%Y.%m.%d" week-end))))

(defun my-weekly-agenda-export-name ()
  "Generate an export file name for the weekly agenda review.

The file name format is '<USER>_work_review_<START>-<END>.txt',
where <USER> is the current variable `user-login-name', <START>
is the date of the current week's start (Monday), and <END> is
the date of the week's end (Sunday)."
  (let* ((date (string-join (my-weekly-agenda-export-range) "-")))
    (concat user-login-name "_work_review_" date ".txt")))

;; For details see: https://orgmode.org/manual/Special-Agenda-Views.html
(setq org-agenda-custom-commands
      `(("g" "Agenda" agenda "")
        ;; List of tasks to add missed tags
        ("u" "Untagged Headings" tags "-{.*}"
         ((org-agenda-overriding-header "Untagged Headings")))
        ;; Probably need to add :someday: tag
        ("o" "Unfinished, but not scheduled tasks"
         ((tags-todo "-someday-project"
                     ((org-agenda-skip-function
                       'my-org-agenda-skip-parent-if-has-scheduled-childs)
                      (org-agenda-overriding-header "Unscheduled Tasks")))))
        ;; Probably need to remove the scheduled date
        ("O" "Scheduled, but with :someday: tag"
         ((tags "+someday"
                ((org-agenda-skip-function
                  '(or (org-agenda-skip-entry-if 'notscheduled)
                       (org-agenda-skip-entry-if 'todo 'done)))
                 (org-agenda-overriding-header "Scheduled Someday Tasks")))))
        ;; Unscheduled with :someday: tag
        ("p" "Pick a task from unscheduled list"
         ((tags "+someday"
                ((org-agenda-skip-function
                  '(or (org-agenda-skip-entry-if 'scheduled)
                       (org-agenda-skip-entry-if 'todo 'done)))
                 (org-agenda-overriding-header "Backlog")))))
        ;; Non-business tasks that currenly in my focus
        ("N", "Non-business: Open focus projects"
         ((tags "+focus+project-CATEGORY={airslate\\|business}"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header
                  "Personal Focus: Open Projects")))))
        ;; Business tasks that currenly in my focus
        ("B" "Business: Open focus projects"
         ((tags "+focus+project+CATEGORY={airslate\\|business}"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header
                  "Business Focus: Open Projects")))))
        ;; Something went wrong with these projects
        ("P" "Stuck Projects"
         ((tags "+project-someday-DONE-CANCELED"
                ((org-agenda-skip-function
                  'my-org-agenda-skip-non-stuck-projects)
                 (org-agenda-overriding-header
                  "Stuck Projects with open but not scheduled sub-tasks")))))
        ;; All tasks marked as DONE within the current week for weekly reviews
        ;; or status updates, including recurring TODO tasks that were acted
        ;; upon during the same period.
        ("w" "Tasks done this week" ;;
         ((tags (format "TODO=\"DONE\"&CLOSED>=\"<%s>\""
                        (my-get-week-start-date))
                ((org-agenda-overriding-header
                  "Tasks done this week")))
          ;; Show recurring TODO tasks that were acted upon this week,
          ;; identified by their LAST_REPEAT property being within the current
          ;; week.
          (tags (format "TODO=\"TODO\"&LAST_REPEAT>=\"<%s>\""
                        (my-get-week-start-date))
                ((org-agenda-overriding-header
                  "Recurrent tasks acted upon this week"))))
         ((org-agenda-cmp-user-defined 'my-org-cmp-closed)
          (org-agenda-sorting-strategy '(user-defined-down))))
        ;; All tasks marked as DONE within the current month for montly reviews
        ;; or status updates, including recurring TODO tasks that were acted
        ;; upon during the same period.
        ("W" "Tasks done this month"
         ((tags (format "TODO=\"DONE\"&CLOSED>=\"<%s>\""
                        (my-get-month-start-date))
                ((org-agenda-overriding-header
                  "Tasks done this month")))
          ;; Show recurring TODO tasks that were acted upon this month,
          ;; identified by their LAST_REPEAT property being within the current
          ;; month.
          (tags (format "TODO=\"TODO\"&LAST_REPEAT>=\"<%s>\""
                        (my-get-month-start-date))
                ((org-agenda-overriding-header
                  "Recurrent tasks acted upon this month"))))
         ((org-agenda-cmp-user-defined 'my-org-cmp-closed)
          (org-agenda-sorting-strategy '(user-defined-down))))
        ;; Filtered tasks excluding those with :project: tag, their children,
        ;; and DONE/CANCELED tasks
        ("-" "Standalone Tasks"
         ((tags-todo "-project-DONE-CANCELED"
                     ((org-agenda-skip-function
                       '(my-org-agenda-skip-if-parent-has-tag "project"))
                      (org-agenda-overriding-header
                       "Standalone Tasks: No Projects or DONE Items")))))
        ("R" . "Reports")
        ;; The next 180 days, excluding all TODO items.  Will used to export
        ;; HTML file.  See `org-agenda-exporter-settings' comment bellow.
        ("Rn" "No TODO events +180d"
         ((agenda "No TODO events +180d"
                  ((org-agenda-span 180)
                   (org-agenda-time-grid nil)
                   (org-agenda-entry-types '(:timestamp :sexp))
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'any)))))
         nil
         (,(expand-file-name
            "agenda_180d_filtered.html"
            (expand-file-name "Reports" org-mobile-directory))))
        ;; Full agenda for the next 31 days.  Will used to export HTML file.
        ;; See `org-agenda-exporter-settings' comment bellow.
        ("Ra", "Detail agenda +31d"
         ((agenda "Detail agenda"
                  ((org-agenda-span 31)
                   (org-agenda-time-grid nil)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'done)))))
         nil
         (,(expand-file-name
            "agenda_details_raw.html"
            (expand-file-name "Reports" org-mobile-directory))))
        ("Rw", "Company work done this week"
         ((tags (format "TODO=\"DONE\"&CLOSED>=\"<%s>\"|TODO=\"STARTED\""
                        (my-get-week-start-date))
                ((org-agenda-overriding-header
                  (concat
                   "Tasks I worked on this week ("
                   (string-join (my-weekly-agenda-export-range) " - ") ")"))))
          ;; Show recurring TODO tasks that were acted upon this week,
          ;; identified by their LAST_REPEAT property being within the current
          ;; week.
          (tags (format "TODO=\"TODO\"&LAST_REPEAT>=\"<%s>\""
                        (my-get-week-start-date))
                ((org-agenda-overriding-header
                  "Recurrent tasks acted upon this week"))))
         ;; Files in `org-agenda-files' can be relative to `org-directory'.
         ((org-agenda-files '("airslate.org"))
          (org-agenda-cmp-user-defined 'my-org-cmp-closed)
          (org-agenda-sorting-strategy '(user-defined-down))
          ;; Hide tags in this agenda view.  This is a report after all.
          (org-agenda-hide-tags-regexp ".")
          ;; I don't need category/filename in this agenda view.
          (org-agenda-prefix-format "  %?-12t% s")
          ;; All TODO keywords occupy a fixed space in the agenda display.
          (org-agenda-todo-keyword-format "%-10s"))
         ;; This can be a list of files, for example ("~/a.pdf" "~/b.txt").
         ;; Reworked to get plain txt format due to this issue:
         ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2024-09/msg00058.html
         (,@(expand-file-name
             (my-weekly-agenda-export-name)
             (expand-file-name "Reports" org-mobile-directory))))))

;; Export agenda settings.  To save the result as an HTML file, you have to call
;; the `org-store-agenda-view' which generates and saves HTML reports directly
;; based on the commands defined in `org-agenda-custom-commands', without
;; needing to first display the agenda view.  The resulting file will be saved
;; in the directory specified by custom agenda commands.
;;
;; I export my agendas using a daily cronjob with command that looks like:
;;
;;   emacsclient --no-wait --eval '(org-store-agenda-views)'
;;
;; Note: In my cron job, I don't override `org-agenda-files' as it is already
;; set up correctly and does not include any unnecessary files.  However, your
;; setup may vary, and you might need to adjust your cron job to customize
;; `org-agenda-files' to suit your own needs.  Be mindful of your specific file
;; organization and how it might impact your agenda generation.
;;
;; For details see: https://orgmode.org/manual/Exporting-Agenda-Views.html
(setq org-agenda-exporter-settings
      '((htmlize-output-type 'css)))

;; Standard key binding
(global-set-key (kbd "C-c a") #'org-agenda)

;;;;; Org Bookmarks
(defun my-org-switch-to-buffer (filename)
  "Switch to the buffer associated with the given Org FILENAME.

If a buffer visiting FILENAME is already open, switch to it.  If
the buffer does not exist, open the FILENAME from the `org-directory'
directory and switch to the newly created buffer.

FILENAME should be the name of the Org file without any directory
path.  The file is expected to reside in the `org-directory' directory."
  (switch-to-buffer
   (or (get-buffer filename)
       (find-file-noselect (expand-file-name filename org-directory)))))

(defun my/org-move-bookmark-to-notes()
  "Move and format mobile bookmark heading.

This function is designed to integrate with workflows where you
use a mobile app for capturing notes.  These notes are
synchronized into a file specified by `org-mobile-inbox-for-pull'
using `org-mobile-pull'.  The primary purpose of this function is
to move bookmarks, which you add on your mobile device and which
end up in the `org-mobile-inbox-for-pull' file, into notes.org
while formatting them according to my preferred style.  I have
accounted for the specific formats used by MobileOrg, Beorg and
Plain Org in this implementation, but if you encounter issues,
please let me know so I can address them.

Steps performed by this function:

1. Remove unwanted keywords from beggining of the heading:
   - TODO Bookmark
   - Bookmark
   - BOOKMARK
   - TODO
2. Convert the heading to a second-level heading if needed.
3. Insert a creation date property under the heading.
4. Move the cleaned and formatted bookmark to the end of notes.org.
5. Reapply tags to the previous heading in notes.org.

This function is adapted from an implementation by Karl Voit, and
has been reworked to enhance its readability, maintainability,
and alignment with my specific workflow.  For origin see:
https://karl-voit.at/2014/08/10/bookmarks-with-orgmode/"
  (interactive)
  (save-excursion
    ;; Use `org-back-to-heading' instead `beginning-of-line' here
    ;; just because the cursor may not be on the heading, but below it,
    ;; in a block.
    (org-back-to-heading t)
    ;; Save the starting point and narrow to the current heading
    (let ((start (point)))
      (outline-next-visible-heading 1)
      (save-restriction
        (narrow-to-region start (point))

        ;; Step 1: Clean up heading by removing unwanted keywords
        (goto-char (point-min))
        (when (re-search-forward
               (concat "\\*\\s-+\\(?:"
                       "\\(?:TODO\\s-+\\)?Bookmark\\|BOOKMARK\\|TODO"
                       "\\)\\s-+")
               nil t)
          (replace-match "* " nil nil))

        ;; Step 2: Ensure the heading is at the second level
        (goto-char (point-min))
        (when (looking-at "\\*+")
          (let ((current-level (- (match-end 0) (match-beginning 0))))
            (cond
             ; if level 1, add one more "*"
             ((= current-level 1) (insert "*"))
             ; if level > 2, remove excess "*"
             ((> current-level 2) (delete-char (- current-level 2))))))

        ;; Step 3: Insert creation date property
        (unless (org-entry-get nil "CREATED")
          (goto-char (point-min))
          ;; Check if the date already persist
          (if (re-search-forward org-ts-regexp-inactive nil t)
              (let ((timestamp (match-string-no-properties 0)))
                (org-back-to-heading)
                (org-entry-put nil "CREATED" timestamp)
                ;; Move the cursor to the end of the metadata,
                ;; including the end of the :PROPERTIES block and remove old
                ;; date stamp
                (org-end-of-meta-data t)
                (when (re-search-forward (regexp-quote timestamp) nil t)
                  (replace-match "")))
            ;; Otherwise use current date
            (org-entry-put nil "CREATED"
                           (format-time-string (org-time-stamp-format t t)))))

        ;; Step 4: Cut the current heading and move it to the notes.org file
        (org-cut-subtree)
        (my-org-switch-to-buffer "notes.org")
        (goto-char (point-max))
        (newline)
        (org-paste-subtree)  ; Paste the heading into the notes.org file

        ;; Step 5: Reapply tags to the previous heading in the notes.org file
        (org-back-to-heading t)
        (org-set-tags-command)))))

(define-key my-keyboard-map (kbd "b") #'my/org-move-bookmark-to-notes)

;;;;; Org Refile
;; Refile targets include this file and any file contributing
;; to the agenda - up to 4 levels deep.
(setq org-refile-targets
      '((nil :maxlevel . 4)  ; nil means current buffer
        (org-agenda-files :maxlevel . 4)))

;; Targets start with the file name allows creating level 1 tasks.
(setq org-refile-use-outline-path 'file)

;; Speed up refiling by caching target locations, a must for large Org files.
(setq org-refile-use-cache t)

;; Make `org-refile' outline working IDO/consult/helm/ivy.
(setq org-outline-path-complete-in-steps nil)

;; Allow the creation of new nodes as refile targets.
;; The new node creation must be confirmed by the user.
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Set a timer to regenerate refile cache automatically every time
;; my Emacs has been idled for 10 minutes.
(run-with-idle-timer 600 t (lambda ()
                             (require 'org-refile)
                             (org-refile-cache-clear)
                             (org-refile-get-targets)))

(defun my|org-update-statistics-cookies-after-refile ()
  "Update statistics cookies after refiling a task."
  (org-update-statistics-cookies nil))

(add-hook 'org-after-refile-insert-hook
          #'my|org-update-statistics-cookies-after-refile)

;; Local keys
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c r") #'org-refile )
  (define-key org-mode-map (kbd "C-c C-x C-a") #'org-archive-subtree))

(defun my-org-refile-target-verify ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; Exclude DONE state tasks from refile targets
(setq org-refile-target-verify-function 'my-org-refile-target-verify)


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

;; The blinking cursor is distracting and interferes with cursor setting in some
;; minor modes that try to change buffer-locally.  Additionally, it can cause
;; freezing, especially on macOS, for users with customized and colored cursors.
(blink-cursor-mode -1)

;; Keep cursor outside of any `cursor-intangible' text property.
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(defun my-theme-dark-p ()
  "Return t if the current Emacs theme is dark."
  (let* ((bg-color (face-background 'default nil 'default))
         (bg-rgb (color-values bg-color))
         (brightness (apply '+ (mapcar (lambda (x) (/ x 65535.0)) bg-rgb))))
    (< brightness 1.5)))

(defun my-set-cursor-color-based-on-theme ()
  "Set cursor color based on whether the theme is dark or light."
  (if (my-theme-dark-p)
      (custom-set-faces
       '(cursor ((t (:background "#F6CB47")))))  ; Orange
    (custom-set-faces
     '(cursor ((t (:background "#000000")))))))  ; Black

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(add-hook 'after-load-theme-hook #'my-set-cursor-color-based-on-theme)

;; Modus themes
(load-theme 'modus-vivendi t)
(my-set-cursor-color-based-on-theme)

(global-set-key (kbd "<f5>") #'modus-themes-toggle)

;; Fonts
(defun my-scale-factor (&optional frame)
  "Return the scale factor for the given FRAME.
If FRAME is nil, use the currently selected frame. If GDK scale
factor is unavailable, return 1.0 as the default."
  (let* ((attrs (frame-monitor-attributes frame))
         (scale (cdr (assq 'scale-factor attrs))))
    (or scale 1.0)))

(defun my-calculate-dpi (&optional frame)
  "Calculate and return the normalized DPI for the given FRAME.
If FRAME is nil, use the currently selected frame.

This function computes DPI using the monitor geometry and
physical dimensions reported by `frame-monitor-attributes' and
adjusts it using the scale factor for better accuracy."
  (let* ((attrs (frame-monitor-attributes frame))
         (geometry (cdr (assq 'geometry attrs)))
         (mm-size (cdr (assq 'mm-size attrs)))
         (pixel-width (nth 2 geometry))
         (pixel-height (nth 3 geometry))
         (mm-width (car mm-size))
         (mm-height (cadr mm-size))
         (dpi-x (/ (* pixel-width 25.4) mm-width))
         (dpi-y (/ (* pixel-height 25.4) mm-height))
         (raw-dpi (/ (+ dpi-x dpi-y) 2.0))
         (scale-factor (my-scale-factor frame)))
    (/ raw-dpi scale-factor)))

(defun my-adjust-font-height (dpi)
  "Adjust and return the font height based on DPI.

The function uses predefined DPI ranges to determine an appropriate
font height. Users can customize these ranges and values as needed.

- DPI > 200: Font height 140 (very high DPI, e.g., Retina or 4K laptops)
- DPI > 145: Font height 130 (high DPI, e.g., high-res displays)
- DPI > 100: Font height 120 (medium DPI, e.g., standard monitors)
- DPI <= 100: Font height 110 (low DPI, e.g., older displays)."
  (cond
   ((> dpi 200) 150)
   ((> dpi 145) 130)
   ((> dpi 100) 110)
   (t 100)))

(defun my-dynamic-font-size (&optional frame)
  "Return the optimal font height for the given FRAME.
If FRAME is nil, use the currently selected frame.

This function combines `calculate-dpi' and `adjust-font-height'
to determine the appropriate font size dynamically."
  (let ((dpi (my-calculate-dpi frame)))
    (my-adjust-font-height dpi)))

(defun my-set-dynamic-font (&rest args)
  "Set dynamic fonts for 'default, 'fixed-pitch, and 'variable-pitch faces.

ARGS is a plist of key-value pairs specifying font settings. The
following keys are supported:

- :frame FRAME                The frame for which to set fonts
                              (default: current frame).
- :default FONT-FAMILY        The font family for 'default face.
- :default-size SIZE          The font height for 'default face (default: auto).
- :fixed-pitch FONT-FAMILY    The font family for 'fixed-pitch face.
- :fixed-pitch-size SIZE      The font height for 'fixed-pitch face
                              (default: auto).
- :variable-pitch FONT-FAMILY The font family for 'variable-pitch face.
- :variable-pitch-size SIZE   The font height for 'variable-pitch face
                              (default: auto).
- :sync-fixed-pitch t         Should this function use the same font for
                              'fixed-pitch as for 'default.

If a specified font is unavailable, the current settings for that
face are retained. Sizes default to dynamic calculation based on DPI
unless explicitly provided."
  (let* ((frame (plist-get args :frame))
         (default-font (or (plist-get args :default)
                           (face-attribute 'default :family)))
         (default-size (or (plist-get args :default-size)
                           (my-dynamic-font-size frame)))
         (fixed-pitch-font (if (plist-get args :sync-fixed-pitch)
                               default-font
                             (or (plist-get args :fixed-pitch)
                                 (face-attribute 'fixed-pitch :family))))
         (fixed-pitch-size (or (plist-get args :fixed-pitch-size)
                               default-size))
         (variable-pitch-font (or (plist-get args :variable-pitch)
                                  (face-attribute 'variable-pitch :family)))
         (variable-pitch-size (or (plist-get args :variable-pitch-size)
                                  default-size)))

    ;; Set 'default face
    (set-face-attribute 'default frame
                        :family
                        (and default-font
                             (find-font (font-spec :name default-font)))
                        :height default-size
                        :weight 'regular)


    ;; Set 'fixed-pitch face
    (set-face-attribute 'fixed-pitch frame
                        :family
                        (and fixed-pitch-font
                             (find-font (font-spec :name fixed-pitch-font)))
                        :height fixed-pitch-size
                        :weight 'normal)

    ;; Set 'variable-pitch face
    (set-face-attribute 'variable-pitch frame
                          :family
                          (and variable-pitch-font
                               (find-font (font-spec :name variable-pitch-font)))
                          :height variable-pitch-size
                          :weight 'normal)))

;; Setup fonts for initial frame
(add-hook 'after-init-hook
          (lambda ()
            (when (display-graphic-p)
              (my-set-dynamic-font
               :default "JetBrains Mono"
               :variable-pitch "Cantarell"
               :sync-fixed-pitch t))))

;; Setup fonts for all subsequent frames
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (my-set-dynamic-font :frame frame
                                   :default "JetBrains Mono"
                                   :variable-pitch "Cantarell"
                                   :sync-fixed-pitch t))))

(when (display-graphic-p)
  ;; Handle Apple, Windows and Linux by setting proper Emoji font.
  (cond
   ((member "Apple Color Emoji" (font-family-list))
    (set-fontset-font t 'emoji "Apple Color Emoji"))
   ((member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font t 'emoji "Segoe UI Emoji"))
   ((member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'emoji "Noto Color Emoji")))

  ;; Nicer scrolling
  (pixel-scroll-precision-mode 1))

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

;; I prefer not to have any of the GUI elements.  This keeps the window clean
;; and speeds up loading a bit.
(setq-default use-file-dialog nil)
(setq-default use-dialog-box nil)

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
  "Jump in buffer with `consult-imenu' or `consult-org-heading'."
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'consult-org-heading)
    (t 'consult-imenu))))

(define-key my-keyboard-map (kbd "j") #'my/consult-jump-in-buffer)

(defun my-isearch-consult-line-from-isearch ()
  "Invoke `consult-line' from isearch."
  (interactive)
  (my-isearch-exit-and-run #'consult-line))

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-l")
              #'my-isearch-consult-line-from-isearch))

;; Completion of misspelled words in buffer.
(with-eval-after-load 'flyspell
  (require 'flyspell-correct)

  (define-key flyspell-mode-map (kbd "C-c f s") #'consult-flyspell)

  ;; Set custom function for selecting corrections.
  (setq consult-flyspell-select-function #'flyspell-correct-at-point))

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


;;;; IRC and other communication
(require 'erc)
(require 'erc-log)
(require 'erc-join)

; Autojoin specific channels on Libera.Chat
(setopt erc-autojoin-channels-alist
      '(("Libera.Chat" "#emacs" "#org-mode" "#re2c" "#systemcrafters")))

;; Set autojoin timing to wait for ident response
(setopt erc-autojoin-timing 'ident)

;; Set the user's full name in ERC.  You can set `user-full-name' by setting
;; NAME environment variable in your system.
(setopt erc-user-full-name user-full-name)

;; Hide certain messages (JOIN, PART, QUIT, NICK) in chat buffers
(setopt erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
(setopt erc-lurker-hide-list '("JOIN" "PART" "QUIT"))

;; Set the lurker threshold time to 12 hours (43200 seconds)
(setopt erc-lurker-threshold-time 43200)

;; Disable prompting for password when connecting to servers
(setopt erc-prompt-for-password nil)

(require 'erc-fill)

;; Set the fill function to erc-fill-static and center lines
(setopt erc-fill-function 'erc-fill-static)
(setopt erc-fill-static-center 22)

(defun my|setup-erc-modules ()
  "Set up additional modules for ERC."
  (add-to-list 'erc-modules 'log)
  (erc-update-modules))

(add-hook 'erc-mode-hook #'my|setup-erc-modules)

;; Set up logging for ERC
(defconst my-erc-base-log-directory
  (concat (expand-file-name "~") "/logs/erc/")
  "The base directory where ERC logs will be stored.

This directory serves as the root for all ERC logs, with further
subdirectories being created for specific periods.  The log files
are organized by year and month to ensure easy access and
management of chat history.")

(defun my-erc-monthly-log-directory (&rest _ignore)
  "Return the directory path for storing ERC logs for the current month.

This function generates a directory path based on the current
year and month, ensuring that logs are organized chronologically.
If the directory does not already exist, it will be created.
The resulting path is of the form: ~/logs/erc/YYYY/MM/."
  (let ((directory (concat my-erc-base-log-directory (format-time-string "%Y/%m"))))
    (unless (file-directory-p directory)
      (make-directory directory t))
    directory))

(defun my-erc-log-file-name-short (_buffer target _nick server _port)
  "Computes a log file name from the TARGET and SERVER only.

This results in a filename of the form #channel@server.txt, for example:
#emacs@irc.libera.chat.txt."
  (let ((file (concat target "@" server ".txt")))
    (convert-standard-filename file)))

;; Organize chat history with timestamped directories.
(setopt erc-log-channels-directory #'my-erc-monthly-log-directory)

;; Keep logs succinct and identifiable by channel and server.
(setopt erc-generate-log-file-name-function #'my-erc-log-file-name-short)

;; Seamlessly integrate past conversations into current session.
(setopt erc-log-insert-log-on-open t)

;; Leave the chat without baggage—no auto-saving on exit.
(setopt erc-save-buffer-on-part nil)

;; Exit private chats without leaving a paper trail.
(setopt erc-save-queries-on-quit nil)

;; Capture every word as it’s typed—no lag in logging.
(setopt erc-log-write-after-insert t)

;; Ensure every message you send is instantly preserved.
(setopt erc-log-write-after-send t)

;;;;; ERC Services Configuration
(require 'erc-services)

;; Skip the redundant password prompt—use stored credentials instead.
(setopt erc-prompt-for-nickserv-password nil)

;; Let auth-source handle your NickServ credentials with discretion.
(setopt erc-use-auth-source-for-nickserv-password t)

;; Enable services to handle NickServ and other IRC services.
(erc-services-mode 1)

;;;;; ERC Spelling Configuration
(require 'erc-spelling)

;; Ensure your messages are typo-free, because every word counts.
(erc-spelling-mode 1)

;;;;; ERC Track Configuration
(require 'erc-track)

;; Ignore the noise—focus only on the messages that matter.
(setopt erc-track-exclude-types
      '("JOIN" "MODE" "NICK" "PART" "QUIT"
        "301"   ; away notice
        "305"   ; return from awayness
        "306"   ; set awayness
        "324"   ; modes
        "329"   ; channel creation date
        "332"   ; topic notice
        "333"   ; who set the channel topic
        "353"   ; listing of users on the current channel
        "477")) ; ignore the noise—focus only on the messages that matter.

;; Load `erc-hl-nicks' after `erc' is loaded.
(with-eval-after-load 'erc
  (require 'erc-hl-nicks))

(defun my/erc-buffer-connected-p (buffer)
  "Check if ERC BUFFER is connected."
  (with-current-buffer buffer
    (and (erc-server-process-alive)
         erc-server-connected)))

(defun my/erc-start-or-switch ()
  "Connects to ERC, or switch to last active buffer.

This function serves multiple purposes:

1. Check Active Buffers: It iterates through a predefined list of
   ERC buffers to determine if any of them are actively connected
   to an IRC server.

2. Verify Connection Status: For each buffer, it checks whether
   the associated ERC process is alive and whether there is an
   established network connection to the server.  This is done
   using the `erc-server-process-alive' function and the
   `erc-server-connected' variable.

3. Switch to Active Buffer: If any buffer is found to be actively
   connected, the function switches to that buffer using
   `erc-track-switch-buffer'.

4. Reconnect if Disconnected: If none of the checked buffers are
   connected, the function prompts the user to reconnect to the
   IRC server.  If the user confirms, a new connection is
   initiated using the `erc' command with the server and port
   specified (`irc.libera.chat` on port 6667)."
  (interactive)
  (let ((erc-buffers '("Libera.Chat" "irc.libera.chat" "irc.libera.chat:6667"))
        (connected nil))
    (dolist (buffer erc-buffers)
      (when (and (get-buffer buffer)
                 (my/erc-buffer-connected-p buffer))
        (setq connected t)))
    (if connected
        (erc-track-switch-buffer 1)
      (when (y-or-n-p "Start ERC? ")
        ;; No need to manually call `password-store-get' here if you
        ;; properly setup `auth-source' above.
        (erc :server "irc.libera.chat" :port 6667)))))

(define-key my-keyboard-map (kbd "i") #'my/erc-start-or-switch)


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


;;;; News, Feeds and Mail

(defun my|mutt-mail-mode-detect ()
  "Enable `mail-mode' when editing Mutt temporary files.

This function checks if the current buffer is associated with a
file created by Mutt for composing emails (like /tmp/mutt-XXXXXXX)."
  (when (and buffer-file-name
             (let* ((temp-dir (file-name-as-directory
                               (file-truename temporary-file-directory)))
                    (mutt-file-prefix (concat temp-dir "mutt-")))
               (string-prefix-p mutt-file-prefix
                                (file-truename buffer-file-name))))
    (mail-mode)))

(add-hook 'find-file-hook #'my|mutt-mail-mode-detect)

(defun my|mail-setup-environment ()
  "Configure the environment for composing messages in Mutt.

This hook sets up various editing modes and formatting options
commonly used when composing emails in Mutt.  Specifically:

- `fill-column' is set to 69, aligning with traditional email
  formatting standards.
- `show-trailing-whitespace' is set to nil, to disable trailing
  whitespace highlighting in mail composition mode, as it is not
  needed here.
- `auto-fill-mode' is enabled to automatically wrap lines at the
  fill column.
- `font-lock-mode' ensures syntax highlighting is active.
- `abbrev-mode' expands abbreviations during text entry.
- `flyspell-mode' is activated for on-the-fly spell checking.
- `writegood-mode' helps improve writing quality with stylistic
  suggestions.

Add this function to `mail-mode-hook' to apply these settings
when composing new messages."
  (setq-local fill-column 69)
  (setq-local show-trailing-whitespace nil)

  (auto-fill-mode 1)
  (font-lock-mode 1)
  (abbrev-mode 1)
  (flyspell-mode 1)
  (writegood-mode 1))

;; Attach the environment setup to mail-mode.
(add-hook 'mail-mode-hook #'my|mail-setup-environment)


;;;; IDE Like Features
;;;;; Setup Completion
(require 'company)

;; The idle delay in seconds until completion starts automatically.
(setopt company-idle-delay 0.1)

;; Show quick-access hints beside the candidates.
(setopt company-show-quick-access t)

;;;;; Snippets and Expansions
(setopt yas-verbosity (if emacs-debug-mode 3 0))

(with-eval-after-load 'yasnippet
  (yas-reload-all))

;;;;; Setup LSP
;; Set the LSP keymap prefix.
(setopt lsp-keymap-prefix "C-c l")

;; Segments used in breadcrumb text on headerline.
(setopt lsp-headerline-breadcrumb-segments
        '(path-up-to-project  ; Include the directories up to project
          file                ; Include the open file name
          symbols))           ; Include document symbols if server supports it

;; Label symbols with numbers on the breadcrumb.
(setopt lsp-headerline-breadcrumb-enable-symbol-numbers t)

;; Display diagnostics on the sideline.
(setopt lsp-ui-sideline-show-diagnostsics t)

;; Display documentation of the symbol at point on hover.
(setopt lsp-ui-doc-enable t)

;; Show doc near the cursor.
(setopt lsp-ui-doc-position 'at-point)

;; Delay before showing the doc.
(setopt lsp-ui-doc-delay 0.5)

;; Configure LSP mode for enhanced experience.
(with-eval-after-load 'lsp-mode
  ;; Enable feedback on headerline of the symbols at point, current file, etc.
  (add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)
  ;; Enable `which-key-mode' integration for LSP
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))


;;;; Language Support
;;;;; Language grammars
;; Since I’m the author and maintainer of `bnf-mode', I use my local version
;; instead of the installed package.  This lets me tweak the mode however I
;; want, experiment with it, and instantly apply changes in buffers thanks to
;; `my/reload-current-mode'.
(when (file-exists-p "~/work/bnf-mode/bnf-mode.el")
  (add-to-list 'load-path "~/work/bnf-mode")
  (load "bnf-mode.el")

  ;; Associate `bnf-mode' with .bnf files.
  (add-to-list 'auto-mode-alist '("\\.bnf\\'" . bnf-mode)))

;;;;; Configuration like languages
(when (file-exists-p "~/src/muttrc-mode-el/muttrc-mode.el")
  (add-to-list 'load-path "~/src/muttrc-mode-el")

  (autoload 'muttrc-mode "muttrc-mode.el" "Major mode to edit muttrc files" t)
  ;; Associate `muttrc-mode' with muttrc files.
  (add-to-list 'auto-mode-alist '("muttrc\\'" . muttrc-mode)))

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

;;;;; reStructuredText

;; Associate `rst-mode' whith .rst files.
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))

(with-eval-after-load 'rst
  ;; Customize `rst-new-adornment-down' to automatically lower the adornment
  ;; level when creating new sections in reStructuredText files.  This setting
  ;; makes it easier to maintain a clear structure in your documentation.
  (setq rst-new-adornment-down t)

  ;; Enable `visual-line-mode' automatically in `rst-mode' to handle long lines
  ;; more gracefully by soft-wrapping them at word boundaries, which is often
  ;; preferred in text-heavy files like reStructuredText.
  (add-hook 'rst-mode-hook #'visual-line-mode))

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

;; Configure `sql-indent' to activate `sqlind-minor-mode' in `sql-mode'.
(with-eval-after-load 'sql
  (add-hook 'sql-mode-hook #'sqlind-minor-mode)
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

;; Set custom Python shell interpreter arguments.
(setopt python-shell-interpreter-args "-i --simple-prompt --pprint")

;; Let Emacs guess the Python offset but prevent telling me about guessing.
;; Simple, I find the "Can’t guess python-indent-offset, using defaults: 4"
;; warning to be harmless and unnecessary spam.
(setopt python-indent-guess-indent-offset-verbose nil)

(defun my-locate-python-virtualenv ()
  "Find the Python executable based on the VIRTUAL_ENV environment variable."
  (when-let ((venv (getenv "VIRTUAL_ENV")))
    (let ((python-path (expand-file-name "bin/python" venv)))
      (lsp--info "VIRTUAL_ENV is set to %s" venv)
      (when (file-executable-p python-path)
        (lsp--info "Found python executable at %s" python-path)
        python-path))))

(with-eval-after-load 'lsp-pyright
  (add-to-list 'lsp-pyright-python-search-functions
               #'my-locate-python-virtualenv))

(defun my|setup-python-environment ()
  "Custom configurations for Pyton mode."
  ;; Enable YASnippet mode
  (yas-minor-mode 1)

  ;; Set the `python-shell-interpreter' to the python in PATH.
  ;; At this moment `envrc' should successfully configure environment.
  (setq-local python-shell-interpreter (executable-find "python"))

  ;; Setup active backends for python mode
  (setq-local company-backends
              '((company-capf :with company-yasnippet)
                company-dabbrev-code))

  ;; Enable LSP support in Python buffers.
  (require 'lsp-pyright)
  (lsp))

;; Configure hooks after `python-mode' is loaded.
(with-eval-after-load 'python
  (add-hook 'python-mode-hook #'my|setup-python-environment))

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
;; Setup buffer-local direnv integration for Emacs.
(when (executable-find "direnv")
  ;; `envrc-global-mode' should be enabled after other global minor modes,
  ;; since each prepends itself to various hooks.
  (add-hook 'after-init-hook #'envrc-global-mode))

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
