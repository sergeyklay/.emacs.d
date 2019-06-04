;;; spelling.el --- Spell checking on the fly. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Spell related features for GNU Emacs.

;;; Code:

(defun flyspell-can-operate-p ()
  "Check if flyspell is available."
  (or (executable-find "ispell")
      (executable-find "aspell")
      (executable-find "hunspell")))

(when (flyspell-can-operate-p)
  (use-package ispell
    :custom
    (ispell-silently-savep t))

  (use-package flyspell
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
    (setq flyspell-issue-message-flag nil)

    (cond
     ((executable-find "hunspell")
      (setq
       ispell-program-name "hunspell"
       ispell-really-hunspell t
       ispell-extra-args '("-a" "-i" "utf-8")))
     ((executable-find "aspell")
      (progn
        (setq
         ispell-program-name "aspell"
         ispell-really-aspell t
         ;; Improve performance by reducing suggestions.
         ispell-extra-args '("--sug-mode=ultra" "--dont-suggest"))
        (when (boundp 'flyspell-list-command)
          (setq
           flyspell-list-command "--list"))))))

  (use-package auto-correct)

  (use-package flyspell-correct-ivy
    :after (flyspell ivy)
    :commands (flyspell-correct-ivy)
    :init
    (require 'flyspell-correct)
    (setq flyspell-correct-interface #'flyspell-correct-ivy)
    :bind (:map flyspell-mode-map
                ("C-;" . flyspell-correct-wrapper))))

(provide 'spelling)
;;; spelling.el ends here
