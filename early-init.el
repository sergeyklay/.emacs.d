;; early-init.el --- Early init file to use for Emacs >= 27.x -*- lexical-binding: t; -*-

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

;; Handle only package initialization and early initialization tasks.

;;; Code:

;; Increasing the GC threshold is a common way to speed up Emacs.
;; `gc-cons-threshold' sets at what point Emacs should invoke its garbage
;; collector.  When set it temporarily to a large number, we only garbage
;; collect once on startup.  We'll reset it later, in this file.  Not
;; resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; Garbage Collector Optimization Hack
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024) ; 16mb
                  gc-cons-percentage 0.1)))

;; The command-line option ‘-batch’ makes Emacs to run noninteractively.
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code.  Otherwise, skipping the mtime checks
;; on every *.elc file saves a bit of IO time.
(setq load-prefer-newer noninteractive)

;; Contrary to common configurations, this is all that's needed to set UTF-8
;; as the default coding system:
(set-language-environment "UTF-8")

;; `set-language-enviornment' sets `default-input-method', which is unwanted.
(setq default-input-method nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq tool-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font.  By inhibiting this, the startup time is significantly reduced,
;; especially with fonts larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Remove command line options that aren't relevant to the current OS; this
;; results in slightly less processing at startup.
(unless (eq system-type 'darwin)
  (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux)
  (setq command-line-x-option-alist nil))

;; Prevent Emacs from automatically initializing packages at startup.  This
;; allows the main init file to handle package initialization manually,
;; providing more control over when and how packages are loaded.
(setq package-enable-at-startup nil)

(provide 'early-init)

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; eval: (display-fill-column-indicator-mode)
;; coding: utf-8-unix
;; End:

;;; early-init.el ends here
