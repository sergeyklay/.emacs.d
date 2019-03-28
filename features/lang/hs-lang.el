;;; hs-lang.el --- Haskell related features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Haskell configuration for the GUN Emacs.

;;; Code:

(with-eval-after-load 'company
  (add-hook 'haskell-mode-hook #'company-mode))

(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :init
  (progn
    (setq
     haskell-interactive-popup-errors nil
     haskell-process-log t
     haskell-process-suggest-remove-import-lines t
     haskell-process-auto-import-loaded-modules t
     haskell-stylish-on-save t)

    (use-package company-ghci :defer t)

    (use-package flycheck-haskell
      :after flycheck
      :init (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

    (use-package intero
      :defer t
      :init
      (progn
        (setq intero-extra-ghci-options '("-fno-defer-type-errors"))))

    (use-package hindent
      :commands (hindent-mode)
      :bind (:map hindent-mode-map
                  ("C-c R" . hindent-reformat-buffer)))

    (defun haskell-hook ()
      (semantic-mode)
      (flycheck-mode)
      (intero-global-mode)
      (hindent-mode)
      (add-to-list (make-local-variable 'company-backends)
                   '(company-intero company-ghci company-dabbrev-code company-yasnippet)))

    (add-hook 'haskell-mode-hook 'haskell-hook)))

(provide 'hs-lang)
;;; hs-lang.el ends here
