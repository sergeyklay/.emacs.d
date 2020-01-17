;;; completion.el --- Setup completion. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Initialize ivy, counsel, swiper and company.

;;; Code:

(require 'directories)

;;;; Ivy

(use-package ivy
  :defer 0.1
  :delight
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-x B"   . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("<tab>"   . ivy-alt-done)
         ("C-i"     . ivy-partial-or-done)
         :map ivy-switch-buffer-map
         ("C-k"     . ivy-switch-buffer-kill))
  :config
  (ivy-mode 1)
  (setq
   ;; Show candidate index and total count
   ivy-count-format "(%d/%d) "
   ;; Number of lines for the minibuffer window
   ivy-height 12
   ;; do not set `completion-in-region-function'
   ivy-do-completion-in-region nil
   ;; Wrap around after the first and the last candidate
   ivy-wrap t
   ;; Fix the height of the minibuffer during ivy completion
   ivy-fixed-height-minibuffer t
   ;; Add recent files and bookmarks to `ivy-switch-buffer'
   ivy-use-virtual-buffers t
   ;; Don't use ^ as initial input
   ivy-initial-inputs-alist nil
   ;; Allow minibuffer commands while in the minibuffer
   enable-recursive-minibuffers t
   ;; Disable magic slash on non-match
   ivy-magic-slash-non-match-action nil)
  (when (and (boundp 'ivy-format-function)
	     (fboundp 'ivy-format-function-line))
    ;; Highlight til EOL
    (setq ivy-format-function #'ivy-format-function-line)))

(with-eval-after-load 'ivy
  (add-to-list 'ivy-ignore-buffers "\\*Messages\\*")
  (add-to-list 'ivy-ignore-buffers "TAGS"))

;;;; Counsel

(use-package counsel
  :requires ivy
  :hook (ivy-mode . counsel-mode)
  :custom
  (counsel-find-file-ignore-regexp
        "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  :config
  ;; Use faster search tools: ripgrep
  (let ((command
	 (cond
	  ((executable-find "rg")
	   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
	  ((executable-find "pt")
	   "pt -zS --nocolor --nogroup -e %s")
	  (t counsel-grep-base-command))))
    (setq counsel-grep-base-command command))

  (when (executable-find "rg")
    (setq counsel-git-cmd "rg --files")
    (setq counsel-rg-base-command
	  "rg -i -M 120 --no-heading --line-number --color never %s .")))

;; Replace standard keybindings
(global-set-key (kbd "C-x C-r") #'counsel-recentf)
(global-set-key (kbd "C-c k")   #'counsel-ag)
(global-set-key (kbd "C-h a")   #'counsel-apropos)
(global-set-key (kbd "C-h v")   #'counsel-describe-variable)
(global-set-key (kbd "C-h f")   #'counsel-describe-function)
(global-set-key (kbd "C-h b")   #'counsel-descbinds)

(global-set-key (kbd "C-x C-i") #'counsel-imenu)
(global-set-key (kbd "M-x")     #'counsel-M-x)

(define-key minibuffer-local-map (kbd "C-r") #'counsel-minibuffer-history)

;;;; Swiper

(use-package swiper
  :after ivy
  :commands (swiper swiper-all)
  :bind(("C-s" . swiper)
        :map swiper-map
        ("M-%" . swiper-query-replace)))

;;;; Company

(use-package company
  :defer t
  :delight
  :init
  (setq company-idle-delay nil ; never start completions automatically
        company-echo-delay 0
        company-tooltip-align-annotations t
        company-tooltip-limit 10
        company-selection-wrap-around t
        company-show-numbers t)
  :custom
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  :bind
  (:map company-active-map
        ("SPC" . company-abort)))

(eval-after-load 'company
  '(progn
     ;; Map tab to cycle through the completion options
     (when (fboundp 'company-complete-common-or-cycle)
       (global-set-key (kbd "M-TAB") #'company-complete-common-or-cycle))

     ;; make TAB complete, without losing the ability to manually indent
     (when (fboundp 'company-indent-or-complete-common)
       (global-set-key (kbd "TAB") #'company-indent-or-complete-common))))

(use-package company-statistics
  :after company
  :defer t
  :hook
  (company-mode . company-statistics-mode)
  :config
  (setq company-statistics-file
        (concat user-cache-dir "company-statistics-cache.el")))

(provide 'completion)
;;; completion.el ends here
