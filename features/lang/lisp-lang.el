;;; lisp-lang.el --- Configure the Lisp-family of languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Configure the Lisp-family of languages for the GUN Emacs.

;;; Code:

(defconst sbcl-executable-path (executable-find "sbcl")
  "The sbcl executable path on this system.")

;; The Superior Lisp Interaction Mode for Emacs.
(use-package slime
  :if sbcl-executable-path
  :commands slime-mode
  :hook (lisp-mode . slime-mode)
  :init
  (progn
    (setq inferior-lisp-program sbcl-executable-path
          slime-complete-symbol*-fancy t
          slime-completion-at-point-functions 'slime-fuzzy-complete-symbol)

    (slime-setup '(slime-asdf
                   slime-fancy
                   slime-indentation
                   slime-sbcl-exts
                   slime-scratch))

    (add-company-backends!! :backends (company-capf company-files)
                            :modes slime-mode)))

(use-package slime-company
  :after (slime company)
  :defer t
  :init
  (slime-setup '(slime-fancy slime-company)))

(defun my|common-lisp-hook ()
  "A common hook for Lisp modes."
  (enable-paredit-mode)

  (turn-on-eldoc-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)

  (local-set-key (kbd "RET") #'my/electrify-return-if-match)
  (eldoc-add-command #'my/electrify-return-if-match))

(use-package ielm
  :ensure nil
  :init
  (add-company-backends!! :backends (company-files company-capf)
                          :modes ielm-mode)
  :hook
  (ielm-mode . my|common-lisp-hook))

(use-package lisp-mode
  :ensure nil
  :hook
  ((lisp-mode . my|common-lisp-hook)
   (lisp-mode . my|ggtags-mode-enable)))

(use-package elisp-mode
  :ensure nil
  :hook
  ((emacs-lisp-mode . my|common-lisp-hook)
   (emacs-lisp-mode . my|ggtags-mode-enable)
   (lisp-interaction-mode . my|common-lisp-hook)
   (lisp-interaction-mode . my|ggtags-mode-enable))
  :init
  (add-company-backends!!
    :backends company-capf
    :modes emacs-lisp-mode lisp-interaction-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-b" . #'eval-buffer)
        ("C-?"     . #'comment-or-uncomment-region)))

(provide 'lisp-lang)
;;; lisp-lang.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
