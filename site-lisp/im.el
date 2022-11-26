;;; im.el --- Key maps, IMs and relevant configs. -*- lexical-binding: t; -*-

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

;; Key maps, IMs and relevant configurations.

;;; Code:

;;;; Input Method and Encoding

(use-package mule-cmds
  :ensure nil
  :defer t
  :custom
  (default-input-method "russian-computer")
  :init
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)

  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))

  (setq locale-coding-system 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8)
  :config
  (prefer-coding-system 'utf-8)
  (add-to-list 'auto-coding-alist '("/#[^/]+#\\'" . utf-8)))

;;;; `which-key'

(use-package which-key
  :custom
  (which-key-separator " ")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-idle-delay 0.4)
  (which-key-max-description-length 32)
  :config
  (which-key-mode 1))

;;;; `command-log-mode'

(use-package command-log-mode
  :commands (command-log-mode global-command-log-mode)
  :custom
  ;; Does opening the command log turn on the mode?
  (command-log-mode-open-log-turns-on-mode t)
  ;; Global activation.
  (command-log-mode-is-global t)
  ;; Show the command-log window or frame automatically.
  (command-log-mode-auto-show))

;;;; Disable keys in Emacs

;; Disable Arrow Keys.

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

;; C-h h runs the command `view-hello-file'.
;; I never used this feature and actually don't need it.
(global-unset-key (kbd "C-h h"))

(provide 'im)
;;; im.el ends here
