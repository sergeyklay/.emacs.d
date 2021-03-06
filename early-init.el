;; early-init.el --- Early init file to use for Emacs >= 27.x -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Serghei Iakovlev <egrep@protonmail.ch>

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

;; Deal with packages only and early init stuff.

;;; Code:

;; Defined in Emacs 27 and above.
(defvar package-quickstart)

;; Increasing GC is a common way to speed up Emacs.
;; `gc-cons-threshold' sets at what point Emacs should
;; invoke its garbage collector.  When set it temporarily
;; to a large number, we only garbage collect once on startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq tool-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Currently I use menubar on graphical mode.
(when (and (not (display-graphic-p)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless (eq system-type 'darwin)
  (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux)
  (setq command-line-x-option-alist nil))

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

;; Activate all packages (in particular autoloads).  Use `package-quickstart'
;; feature in Emacs 27 so we only need to `package-initialize' if on Emacs 26
;; and below.  Take a look at $EMACS_CODEBASE/lisp/startup.el to refresh your
;; memory.  The gist is that `package-activate-all' is called in Emacs 27 which
;; reads `package-quickstart'.
(if (>= emacs-major-version 27)
    (setq package-quickstart t)
  (package-initialize))

(provide 'early-init)
;;; early-init.el ends here
