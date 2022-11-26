;;; packaging.el --- Package management. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021, 2022 Serghei Iakovlev <egrep@protonmail.ch>

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

;; Bootstrap packaging system.

;;; Code:

(require 'package)

(eval-when-compile
  (require 'prelude))

(defun my/package-initialize ()
  "Actviate all packages (in particular autoloads)."
  ;; Package loading optimization.
  ;; No need to activate all the packages so early.
  (when (>= emacs-major-version 27)
    (setq package-quickstart t))
  ;; `esup' need call `package-initialize'
  ;; For more see URL `https://github.com/jschaf/esup/issues/84'
  (when (or (featurep 'esup-child)
            (daemonp)
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

;;;; Utilities for `list-packages' menu

(defun my/package-menu-find-marks ()
  "Find packages marked for action in *Packages*."
  (interactive)
  (occur "^[A-Z]"))

(defun my/package-menu-filter-by-status (status)
  "Filter the *Packages* buffer by STATUS."
  (interactive
   (list (completing-read
          "Status : " '("new" "installed" "dependency" "obsolete"))))
  (package-menu-filter-by-keyword (concat "status:" status)))

(define-key package-menu-mode-map "s" #'my/package-menu-filter-by-status)
(define-key package-menu-mode-map "a" #'my/package-menu-find-marks)

(provide 'packaging)
;;; packaging.el ends here
