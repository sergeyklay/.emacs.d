;;; spelling.el --- Spell checking on the fly. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Spell related features for the GNU Emacs.

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
    :delight
    :hook ((org-mode        . flyspell-mode)
           (text-mode       . flyspell-mode)
           (change-log-mode . flyspell-mode)
           (log-edit-mode   . flyspell-mode)
           (markdown-mode   . flyspell-mode)
           (latex-mode      . flyspell-mode))
    :config
    (validate-setq
     ;; Be silent when checking words.
     flyspell-issue-message-flag nil)

    (cond
     ((executable-find "hunspell")
      (validate-setq
       ispell-program-name "hunspell"
       ispell-really-hunspell t
       ispell-extra-args '("-a" "-i" "utf-8")))
     ((executable-find "aspell")
      (progn
        (validate-setq
         ispell-program-name "aspell"
         ispell-really-aspell t
         ;; Improve performance by reducing suggestions.
         ispell-extra-args '("--sug-mode=ultra" "--dont-suggest"))
        (when (boundp 'flyspell-list-command)
          (validate-setq
           flyspell-list-command "--list"))))))

  (use-package auto-correct
    :diminish auto-correct-mode)

  (use-package flyspell-correct-ivy
    :after (flyspell ivy)
    :commands (flyspell-correct-ivy)
    :init
    (require 'flyspell-correct)
    (validate-setq flyspell-correct-interface #'flyspell-correct-ivy)
    :bind (:map flyspell-mode-map
                ("C-;" . flyspell-correct-wrapper))))

(provide 'spelling)
;;; spelling.el ends here
