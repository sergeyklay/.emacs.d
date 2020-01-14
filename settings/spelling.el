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

(use-package ispell
  :if hunspell-executable-path
  :ensure nil
  :custom
  ;; Save personal dictionary without asking for confirmation.
  (ispell-silently-savep t)
  (ispell-program-name "hunspell")
  (ispell-extra-args '("-a" "-i" "utf-8"))
  (ispell-dictionary "en_US")
  (ispell-alternate-dictionary (concat user-etc-dir "alt.dic"))
  :config
  (setq ispell-really-hunspell t))

(use-package flyspell
  :if hunspell-executable-path
  :defer t
  ;; TODO: Use per feature configuration
  :hook ((org-mode        . flyspell-mode)
	 (text-mode       . flyspell-mode)
	 (change-log-mode . flyspell-mode)
	 (log-edit-mode   . flyspell-mode)
	 (markdown-mode   . flyspell-mode)
	 (latex-mode      . flyspell-mode))
  :config
  ;; Be silent when checking words.
  (setq flyspell-issue-message-flag nil))

(use-package auto-correct
  :after flyspell
  :config
  (auto-correct-mode t))

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
