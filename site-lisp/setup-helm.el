;;; setup-helm.el --- Setup Helm. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setup Helm.

;;; Code:

(use-package helm
  :bind
  (("M-x"     . helm-M-x)
   ("C-s"     . helm-occur)
   ("C-x b"   . helm-mini)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ([help ?a] . helm-apropos)
   (:map helm-map
	 ("S-SPC" . helm-toggle-visible-mark)))
  :config
  (helm-mode 1))

(define-key minibuffer-local-map (kbd "C-r") #'helm-minibuffer-history)

(use-package helm-projectile
  :after projectile
  :commands
  (projectile-switch-project-action
   helm-projectile
   helm-projectile-switch-project
   helm-projectile-find-file)
  :custom
  (projectile-completion-system 'helm)
  (projectile-switch-project-action 'helm-projectile)
  :bind (("M-s k" . helm-projectile-ag)
	 ("M-s d" . helm-projectile-find-dir)
	 ("M-s f" . helm-projectile-find-file)
	 ([f12]   . helm-projectile))
  :config (helm-projectile-on))

(use-package helm-descbinds
  :commands (helm-descbinds)
  :bind
  ([help ?b] . helm-descbinds)
  :custom
  (helm-descbinds-window-style 'split "Use pop-up style window for descbinds"))

(use-package helm-ag
  :if (executable-find "ag")
  :commands (helm-ag helm-projectile-ag)
  :bind (("C-c k" . helm-ag-project-root)))

(use-package helm-pass
  :after password-store
  :commands helm-pass
  :bind ("C-c p" . helm-pass))

(use-package helm-flyspell
  :after flyspell
  :commands helm-flyspell-correct
  :init
  (require 'flyspell-correct)
  :bind (:map flyspell-mode-map
              ("C-;" . helm-flyspell-correct)))

(use-package helm-rtags
  :ensure nil ; installed from source code
  :if (executable-find "rdm")
  :after rtags
  :custom
  (rtags-display-result-backend 'helm))

(provide 'setup-helm)
;;; setup-helm.el ends here
