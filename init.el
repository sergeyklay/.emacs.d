;;; init.el --- Initialization file. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d

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

;;   The guiding philosophy behind this Emacs configuration is that it should
;; be a direct extension of my own memory.  This project serves as a
;; representation of what I can actively recall and understand.
;;
;; As a result, you won't find overly complex settings or esoteric designs here.
;; The aim is to keep everything as straightforward and transparent as possible.
;; This ensures that the configuration can be easily read, understood, and held
;; mentally.
;;
;; If you come across this project, bear in mind that it's molded to fit what I
;; can keep "in my head" rather than what Emacs is capable of doing.  The
;; simplicity is intentional and serves as a mnemonic aid for me.
;;
;; I started this project on 4 March 2019 from this commit:
;; eb11ce25b0866508e023db4b8be6cca536cd3044

;;; Code:

;;;; Begin initialization.

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(setq file-name-handler-alist nil)

;; One less file to load at startup
(setq site-run-file nil)

;; Actually this project is my personal configuration
;; so I use GNU Emacs 29.1 now.
(eval-when-compile
  (and (version< emacs-version "29.1")
       (error
        (concat
         "Detected Emacs %s. "
         "This configuration is designed to work "
         "only with Emacs 29.1 and higher. I'm sorry.")
        emacs-version)))

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

;;;; Packaging

(require 'package)

(defun my/package-initialize ()
  "Actviate all packages (in particular autoloads)."
  ;; Package loading optimization.
  ;; No need to activate all the packages so early.
  (setq package-quickstart t)
  (when (or (daemonp)
            noninteractive)
    (package-initialize)))

(my/package-initialize)

(custom-set-variables
 ;; Setting up package archives.
 '(package-archives
   '(("melpa"    . "https://melpa.org/packages/")
     ("m-stable" . "https://stable.melpa.org/packages/")
     ("gnu"      . "https://elpa.gnu.org/packages/")))
 ;; Priorities. Default priority is 0.
 '(package-archive-priorities
   '(("m-stable" . 10)
     ("melpa"    . 20))))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 '(use-package-enable-imenu-support t) ; enable imenu support for `use-package'
 '(use-package-always-ensure t)        ; all packages should be installed
 '(use-package-verbose emacs-debug-mode))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

;;;; Sane defaults

(setq default-directory (concat (getenv "HOME") "/"))

(global-set-key (kbd "C-x t d") #'toggle-debug-on-error)

;; Modern API for working with files and directories in Emacs.
(use-package f
  :defer t)

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :custom
  ;; Idle time to wait in seconds before triggering GC.
  (gcmh-idle-delay 'auto)
  ;; Factor to compute the idle delay when in idle-delay auto mode.
  (gcmh-auto-idle-delay-factor 10)
  ;; High cons GC threshold.
  (gcmh-high-cons-threshold #x1000000))

(setq-default
 vc-follow-symlinks t ; Don't ask for confirmation when opening symlinks
 debug-on-error (and (not noninteractive) emacs-debug-mode))

(custom-set-variables
 '(initial-scratch-message "")     ; No scratch message
 '(inhibit-startup-screen t)       ; Disable start-up screen
 '(initial-major-mode 'text-mode)) ; Configure the Scratch Buffer's Mode

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

;;;; Language support

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :config
  :interpreter ("yml" . yml-mode))

;;;; Custom Variables and Faces

;; The `custom-set-variables' and `custom-set-faces' blocks are placed at the
;; end of this file for several reasons:
;;
;; 1. Dependency Handling: Some custom variables might depend on packages that
;; are initialized earlier in this file. Placing the custom blocks at the end
;; ensures all required packages are already loaded.
;;
;; 2. Readability: These blocks are often auto-generated by the Emacs customize
;; interface and can become quite large. Keeping them at the end improves the
;; readability of the config file.
;;
;; 3. Overriding: If there are manual settings scattered through the init file
;; that conflict with custom settings, placing custom blocks at the end allows
;; them to take precedence.
;;
;; If you're an aspiring Emacs hacker, consider these points when deciding
;; where to place your own custom variables and faces.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(leuven))
 '(global-visual-line-mode t)
 '(ring-bell-function 'ignore))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
