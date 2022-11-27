;;; spelling.el --- Spell checking on the fly. -*- lexical-binding: t; -*-

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

;; Spell related features for GNU Emacs..

;;; Code:

;; Note: On macOs brew doesn't provide dictionaries.  So you have to install
;; them.  For more info on installing dictionaries see
;; URL `https://passingcuriosity.com/2017/emacs-hunspell-and-dictionaries'
(use-package ispell
  :if (executable-find "hunspell")
  :ensure nil
  :custom
  ;; Save personal dictionary without asking for confirmation.
  (ispell-silently-savep t)
  ;; Full path to the hunspell executable
  (ispell-program-name (executable-find "hunspell"))
  ;; The default dictionary I use.  To see available dictionaries
  ;; use 'hunspell -D'.
  (ispell-local-dictionary "british")
  ;; Setting up dictionary definitions
  (ispell-local-dictionary-alist
   '(("british" "[[:alpha:]]" "[^[:alpha]]" "[’']" t
      ("-d" "en_GB") nil utf-8)
     ("english" "[[:alpha:]]" "[^[:alpha]]" "[’']" t
      ("-d" "en_US") nil utf-8)
     ("russian" "[А-Яа-я]" "[^А-Яа-я]" "[-']" nil
      ("-d" "ru_RU") nil utf-8)))
  :config
  (setq ispell-really-aspell nil)
  (setq ispell-really-hunspell t)
  ;; 2022/11/27: commented because I'm not sure if it's still needed
  ;;
  ;; (when (string-equal system-type "darwin")
  ;;   ;; Set dictionary file name.  Without this variable you'll see on macOs:
  ;;   ;; 'Can't open affix or dictionary files for dictionary named "XXX"'
  ;;   ;; TODO(serghei): Still does not work
  ;;   (setenv "DICTIONARY" "en_GB"))
  )

(use-package flyspell
  :if (executable-find "hunspell")
  :ensure nil
  :custom
  ;; Be silent when checking words.
  (flyspell-issue-message-flag nil)
  :bind ("C-x t s" . flyspell-mode))

(provide 'spelling)
;;; spelling.el ends here
