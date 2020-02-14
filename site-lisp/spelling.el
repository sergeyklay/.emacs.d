;;; spelling.el --- Spell checking on the fly. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Spell related features for GNU Emacs..

;;; Code:

(require 'directories)

(defconst hunspell-executable-path (executable-find "hunspell")
  "The hunspell executable path on this system.")

;; Note: On macOs brew doesn't provide dictionaries.  So you have to install
;; them.  For more info on installing dictionaries see
;; URL `https://passingcuriosity.com/2017/emacs-hunspell-and-dictionaries'
(use-package ispell
  :if hunspell-executable-path
  :ensure nil
  :custom
  ;; Save personal dictionary without asking for confirmation.
  (ispell-silently-savep t)
  ;; Full path to the hunspell executable
  (ispell-program-name hunspell-executable-path)
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
  (when (string-equal system-type "darwin")
    ;; Set dictionary file name.  Without this variable you'll see on macOs:
    ;; 'Can't open affix or dictionary files for dictionary named "XXX"'
    (setenv "DICTIONARY" "en_GB")))

(use-package flyspell
  :if hunspell-executable-path
  :custom
  ;; Be silent when checking words.
  (flyspell-issue-message-flag nil)
  :defer t
  :hook ((org-mode        . flyspell-mode)
	 (message-mode    . flyspell-mode)
	 (text-mode       . flyspell-mode)
	 (texinfo-mode    . flyspell-mode)
	 (change-log-mode . flyspell-mode)
	 (log-edit-mode   . flyspell-mode)
	 (markdown-mode   . flyspell-mode)
	 (latex-mode      . flyspell-mode)))

(use-package flyspell-correct-ivy
  :after flyspell
  :commands (flyspell-correct-ivy)
  :init
  (require 'flyspell-correct)
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  :bind (:map flyspell-mode-map
	      ("C-;" . flyspell-correct-wrapper)))

(provide 'spelling)
;;; spelling.el ends here
