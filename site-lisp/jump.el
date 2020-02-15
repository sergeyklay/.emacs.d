;;; jump.el --- GNU Emacs configuration for tags. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Tags realated features for GNU Emacs.

;;; Code:

(require 'directories)
(require 'utils)

(defconst global-executable-path (executable-find "global")
  "The global executable path on this system.")

(defconst rdm-executable-path (executable-find "rdm")
  "The rdm executable path on this system.")

;;;; Bookmark

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-default-file
   (concat user-etc-dir "bookmarks.el")))

;;;; Etags

(use-package etags
  :ensure nil
  :defer 4
  :custom
  ;; Make Emacs reload the TAGS file automatically.
  (tags-revert-without-query 1)
  ;; Whether tags operations should be case-sensitive.
  (tags-case-fold-search nil)
  ;; Never “Keep current list of tags tables also”.
  (tags-add-tables nil))

;;;; Ggtags

;; A front-end for accessing the gtags-generated tags.
;; For more see URL `https://github.com/leoliu/ggtags'
(use-package ggtags
  :if global-executable-path
  :defer 2
  :custom
  ;; Don't try to update GTAGS on each save;
  ;; makes the system sluggish for huge projects.
  (ggtags-update-on-save nil)
  ;; Don't auto-highlight tag at point;
  ;; makes the system sluggish for huge projects.
  (ggtags-highlight-tag nil)
  ;; Enabling nearness requires global 6.5.+
  (ggtags-sort-by-nearness nil)
  (ggtags-oversize-limit (* 30 1024 1024)) ; 30MB
  :config
  ;; Remove the default binding for M-] in `ggtags-mode-map'
  (unbind-key "M-]" ggtags-mode-map)
  ;; Remove the default binding for M-o in `ggtags-navigation-map'
  (unbind-key "M-o" ggtags-navigation-map)
  :bind (:map ggtags-mode-map
	 ("C-c g s" . 'ggtags-find-other-symbol)
	 ("C-c g h" . 'ggtags-view-tag-history)
	 ("C-c g r" . 'ggtags-find-reference)
	 ("C-c g f" . 'ggtags-find-file)
	 ("C-c g c" . 'ggtags-create-tags)
	 ("C-c g u" . 'ggtags-update-tags)
	 ("M-."     . 'ggtags-find-tag-dwim)
	 ("M-,"     . 'pop-tag-mark)
	 ("C-c <"   . 'ggtags-prev-mark)
	 ("C-c >"   . 'ggtags-next-mark)
	 :map ggtags-navigation-map
	 ("M-l"     . 'ggtags-navigation-visible-mode)))

;;;; RTags

;; Do not install the followinng packages using MELPA.
;; These packages should be installed by hand using `make install' from
;; rtags source directory.
;;
;; For more see URL `https://github.com/Andersbakken/rtags/issues/1318'.

(defun rtags-eldoc-function ()
  "Eldoc documentation function to use for `c-mode' as well as `c++-mode'."
  (let ((summary (rtags-get-summary-text)))
    (and summary
         (fontify-string
          (replace-regexp-in-string
           "{[^}]*$" ""
           (mapconcat
            (lambda (str) (if (= 0 (length str)) "//" (string-trim str)))
            (split-string summary "\r?\n")
            " "))
          major-mode))))

(use-package rtags
  :ensure nil
  :if rdm-executable-path
  :defer 3
  :custom
  ;; Whether RTags automatically will restart diagnostics.
  (rtags-autostart-diagnostics t)
  ;; Whether completions are enabled.
  (rtags-completions-enabled t)
  ;; Path to RTags executables.
  (rtags-path (directory-file-name
	       (file-name-directory rdm-executable-path)))
  :config
  (defun my|rtags-common-hook ()
    "Common hook to setup `rtags'."
    (setq-local eldoc-documentation-function #'rtags-eldoc-function)
    (rtags-start-process-unless-running))

  (rtags-enable-standard-keybindings)
  :hook ((c-mode c++-mode) . my|rtags-common-hook))

(use-package company-rtags
  :ensure nil
  :if rdm-executable-path
  :after company rtags
  :config (push 'company-rtags company-backends))

(use-package flycheck-rtags
  :ensure nil
  :if rdm-executable-path
  :after flycheck rtags
  :config
  (defun my|flychack-rtags-common-hook ()
    "Common hook to setup `flycheck-rtags'."
    (flycheck-select-checker 'rtags)
    ;; RTags creates more accurate overlays.
    (setq-local flycheck-highlighting-mode nil
		flycheck-check-syntax-automatically nil))
  :hook ((c-mode c++-mode) . my|flychack-rtags-common-hook))

(use-package ivy-rtags
  :ensure nil
  :if rdm-executable-path
  :after ivy rtags
  :custom
  (rtags-display-result-backend 'ivy))

(provide 'jump)
;;; jump.el ends here
