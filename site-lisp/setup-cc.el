;;; setup-cc.el --- Support for the C-family of languages. -*- lexical-binding: t; -*-

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

;; Support for the C-family of languages.

;;; Code:

(eval-when-compile
  (require 'compile)
  (require 'company))

(use-package company-c-headers
  :after company
  :defer t
  :init
  (add-to-list 'company-backends 'company-c-headers)
  :hook
  ((c-mode-common . company-mode)))

(defun my/custom-compile-command ()
  "Custom compile command to use for C buffers."
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(defun c-binds-hook ()
  "Setup keybindings to use for C buffers."
  (local-set-key (kbd "<f5>") #'my/custom-compile-command))

(use-package cc-mode
  :ensure nil
  :hook ((c-mode . c-binds-hook)))

(provide 'setup-cc)
;;; setup-cc.el ends here
