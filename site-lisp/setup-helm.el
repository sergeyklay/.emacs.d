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
  :init
  (require 'helm-config)
  :custom
  (helm-scroll-amount 4)
  ;; Search for library in `require' and `declare-function' sexp.
  (helm-ff-search-library-in-sexp t)
  (helm-split-window-in-side-p t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-buffer-skip-remote-checking t)
  (helm-mode-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-semantic-fuzzy-match t)
  (helm-imenu-fuzzy-match t)
  (helm-lisp-fuzzy-completion t)
  (helm-locate-fuzzy-match t)
  (helm-display-header-line nil)
  :bind (([remap find-tag]     . helm-etags-select)
         ([remap list-buffers] . helm-buffers-list)

         ("M-x"     . helm-M-x)
         ("C-c M-x" . execute-extended-command)
         ("C-c h o" . helm-occur)
         ("M-/"     . helm-dabbrev)
         ("C-x b"   . helm-mini)
         ("C-x r"   . helm-recentf)
         ("C-x C-f" . helm-find-files)
         ("C-h C-l" . helm-locate-library)
         ("C-h SPC" . helm-all-mark-rings)
         ([help ?a] . helm-apropos)
         ([help ?r] . helm-register)

         :map helm-map
         ("S-SPC" . helm-toggle-visible-mark)
         ("<tab>" . helm-execute-persistent-action)
         ("C-i"   . helm-execute-persistent-action)
         ("C-z"   . helm-select-action)

         :map minibuffer-local-map
         ("M-p"   . helm-minibuffer-history)
         ("M-n"   . helm-minibuffer-history))
  :hook
  ((helm-goto-line-before . helm-save-current-pos-to-mark-ring))
  :config
  (helm-mode 1))

(define-key minibuffer-local-map (kbd "C-r") #'helm-minibuffer-history)

(use-package helm-swoop
  :custom
  ;; Save buffer when helm-multi-swoop-edit complete
  (helm-multi-swoop-edit-save t)
  ;; If this value is t, split window inside the current window
  (helm-swoop-split-with-multiple-windows t)
  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (helm-swoop-split-direction 'split-window-vertically)
  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (helm-swoop-speed-or-color t)
  :bind (("C-c h o"  . helm-swoop)
         ("C-c s"    . helm-multi-swoop-all)

         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch)

         :map helm-swoop-map
         ("M-i" . helm-multi-swoop-all-from-helm-swoop)))

(use-package helm-info
  :ensure nil
  :commands (helm-info-emacs)
  :bind ("C-h TAB" . helm-info-emacs))

(use-package helm-descbinds
  :commands (helm-descbinds)
  :bind
  ([help ?b] . helm-descbinds)
  :custom
  (helm-descbinds-window-style 'split "Use pop-up style window for descbinds"))

(use-package helm-projectile
  :commands
  (helm-projectile-ag
   helm-projectile-find-dir
   helm-projectile-find-file
   helm-projectile
   helm-projectile-switch-project
   helm-projectile-switch-to-buffer
   helm-projectile-grep
   helm-projectile-recentf)
  :custom
  (projectile-completion-system 'helm)
  (projectile-switch-project-action 'helm-projectile)
  ;; “C-c p p” is reserved for `helm-pass'
  :bind (("C-c p h" . helm-projectile-switch-project)
         ("C-c p a" . helm-projectile-ag)
         ("C-c p g" . helm-projectile-grep)
         ("C-c p d" . helm-projectile-find-dir)
         ("C-c p f" . helm-projectile-find-file))
  :config (helm-projectile-on))

(use-package helm-ag
  :ensure helm
  :if (executable-find "ag")
  :commands (helm-ag helm-projectile-ag)
  :bind (("C-c k" . helm-ag-project-root)))

(use-package helm-grep
  :ensure helm
  :bind (:map helm-grep-mode-map
              ("<return>" . helm-grep-mode-jump-other-window)
              ("n"        . helm-grep-mode-jump-other-window-forward)
              ("p"        . helm-grep-mode-jump-other-window-backward)))

(provide 'setup-helm)
;;; setup-helm.el ends here
