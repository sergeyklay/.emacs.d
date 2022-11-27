;;; completion.el --- Setup completion. -*- lexical-binding: t; -*-

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

;; Initialize ivy, counsel, and swiper.

;;; Code:

(eval-when-compile
  (require 'directories))

;;;; Ivy

(use-package ivy
  :defer 0.1
  :diminish ivy-mode
  :custom
  ;; Don't show "./" and "../"
  (ivy-extra-directories '())
  ;; Show candidate index and total count
  (ivy-count-format "(%d/%d) ")
  ;; Number of lines for the minibuffer window
  (ivy-height 12)
  ;; do not set `completion-in-region-function'
  (ivy-do-completion-in-region nil)
  ;; Wrap around after the first and the last candidate
  (ivy-wrap t)
  ;; Fix the height of the minibuffer during ivy completion
  (ivy-fixed-height-minibuffer t)
  ;; Add recent files and bookmarks to `ivy-switch-buffer'
  (ivy-use-virtual-buffers t)
  ;; Don't use ^ as initial input
  (ivy-initial-inputs-alist nil)
  ;; Disable magic slash on non-match
  (ivy-magic-slash-non-match-action nil)
  (ivy-ignore-buffers '("\\` " "\\`\\*tramp/" "\\*Messages\\*" "TAGS"))
  ;; Allow minibuffer commands while in the minibuffer
  (enable-recursive-minibuffers t)
  :bind (([remap switch-to-buffer] . ivy-switch-buffer)
         ("C-x B"                  . ivy-switch-buffer-other-window)
         ("C-c C-r"                . ivy-resume)
         :map ivy-minibuffer-map
         ("<tab>"                  . ivy-alt-done)
         ("C-i"                    . ivy-partial-or-done)
         :map ivy-switch-buffer-map
         ("C-k"                    . ivy-switch-buffer-kill))
  :config
  (ivy-mode 1)
  (when (fboundp 'ivy-format-function-line)
    ;; Highlight til EOL
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)))

;;;; Smex

;; With smex installed, `counsel-M-x' lists all available commands but places
;; the most recently used ones on top of the list ordered by frequency.
(use-package smex
  :commands smex
  :custom
  (smex-history-length 20)
  (smex-save-file (concat user-cache-dir "smex-items.el"))
  :config
  (smex-initialize))

;;;; Counsel

(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :after exec-path-from-shell ; Due to `exec-path-from-shell-copy-env'
  :diminish counsel-mode
  :custom
  (counsel-find-file-ignore-regexp
   (concat
    ;; file names beginning with #
    "\\(?:^[#]\\)"
    ;; file names ending with # or ~
    "\\|\\(?:[#~]$\\)"
    ;; file names beginning with Icon
    "\\|\\(?:^Icon?\\)"
    ;; macOs' custom attributes of its containing folder
    "\\|\\(?:^\\.DS_Store$\\)"
    ;; zsh compiled functions
    "\\|\\(?:\\.zwc$\\)"
    ;; Python bytecode
    "\\|\\(?:^__pycache__/?$\\)"))
  ;; Let counsel-find-file-at-point choose the file under cursor
  (counsel-find-file-at-point t)
  (counsel-rg-base-command
   `("rg"
     "--max-columns" "240" ; Don't print the lines longer than 240 bytes
     "--with-filename"     ; Display the file path for matches
     "--no-heading"        ; Don't group matches by each file
     "--line-number"       ; Show line numbers (1-based)
     "--color" "never"     ; Colors  will never be used
     "--hidden"            ; Search hidden files and directories
     "%s"))
  :bind (("C-x C-i"                   . counsel-imenu)
         ("C-x l"                     . counsel-locate)
         ("C-h u"                     . counsel-unicode-char)
         ("C-h C-l"                   . counsel-find-library)
         ("C-h C-o"                   . counsel-info-lookup-symbol)
         ("C-c k"                     . counsel-rg)
         ("C-c f"                     . counsel-recentf)
         ("C-c C-h"                   . counsel-command-history)
         ([remap list-buffers]        . counsel-ibuffer)
         ([remap eshell-list-history] . counsel-esh-history)
         ([remap describe-symbol]     . counsel-describe-symbol)
         :map ivy-minibuffer-map
         ("C-r"                       . counsel-minibuffer-history)
         :map minibuffer-local-map
         ("C-r"                       . counsel-minibuffer-history)))

;;;; Swiper

(use-package swiper
  :after ivy
  :commands (swiper swiper-all)
  :bind(([remap isearch-forward] . swiper)
        :map swiper-map
        ("M-%" . swiper-query-replace)))

(provide 'completion)
;;; completion.el ends here
