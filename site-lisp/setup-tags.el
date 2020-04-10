;;; setup-tags.el --- Setup tags. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setting up tags and code navigation

;;; Code:

(require 'utils)
(require 'emacscfg)
(require 'directories)

;;;; Constants

(defconst global-executable-path (executable-find "global")
  "The global executable path on this system.")

(defconst gtags-executable-path (executable-find "gtags")
  "The gtags executable path on this system.")

(defconst rdm-executable-path (executable-find "rdm")
  "The rdm executable path on this system.")

(unless (and global-executable-path gtags-executable-path)
  (warn "Cannot find necessary installation of GNU GLOBAL"))

(unless rdm-executable-path
  (warn "Cannot find necessary installation of RTags"))

;;;; Etags

(use-package etags
  :ensure nil
  :defer t
  :custom
  ;; Make Emacs reload the TAGS file automatically.
  (tags-revert-without-query 1)
  ;; Whether tags operations should be case-sensitive.
  (tags-case-fold-search nil)
  ;; Never “Keep current list of tags tables also”.
  (tags-add-tables nil)
  :bind (("M-," . pop-tag-mark)))

;;;; Ggtags

;; A front-end for accessing the gtags-generated tags.
;; For more see URL `https://github.com/leoliu/ggtags'

(use-package ggtags
  :if (and global-executable-path gtags-executable-path)
  :commands (ggtags-mode
             ggtags-build-imenu-index
             ggtags-eldoc-function
             pop-tag-mark)
  :custom
  ;; Auto-pdate GTAGS on each save.
  (ggtags-update-on-save t)
  ;; Do not auto-highlight tag at point
  (ggtags-highlight-tag nil)
  ;; Enabling nearness requires global 6.5.+
  (ggtags-sort-by-nearness t)
  ;; The over size limit for the  GTAGS file.
  (ggtags-oversize-limit (* 100 1024 1024))
  ;; Generate the idutils DB.
  (ggtags-use-idutils t)
  ;; The directory to search GNU GLOBAL executables.
  (ggtags-executable-directory
   (directory-file-name (file-name-directory global-executable-path)))
  :bind (("M-]"     . ggtags-idutils-query)

         :map ggtags-mode-map
         ("C-c g s" . ggtags-find-other-symbol)
         ("C-c g h" . ggtags-view-tag-history)
         ("C-c g r" . ggtags-find-reference)
         ("C-c g f" . ggtags-find-file)
         ("C-c g c" . ggtags-create-tags)
         ("C-c g u" . ggtags-update-tags)
         ("M-."     . ggtags-find-tag-dwim)
         ("C-c <"   . ggtags-prev-mark)
         ("C-c >"   . ggtags-next-mark)

         :map ggtags-navigation-map
         ("M-o"     . ggtags-navigation-next-file)
         ("M-l"     . ggtags-navigation-visible-mode)))

;;;; Rtags

;; NOTE: Do not install the following packages using MELPA.
;; These packages should be installed by hand using `make install' from
;; rtags source directory.
;;
;; For more see URL `https://github.com/Andersbakken/rtags/issues/1318'.

(use-package rtags
  :ensure nil
  :if rdm-executable-path
  :commands (rtags-mode)
  :custom
  ;; Whether RTags automatically will restart diagnostics.
  (rtags-autostart-diagnostics t)
  ;; Path to RTags executables.
  (rtags-path (directory-file-name
               (file-name-directory rdm-executable-path))))

(use-package company-rtags
  :ensure nil
  :if rdm-executable-path
  :after company rtags)

(use-package flycheck-rtags
  :ensure nil
  :if rdm-executable-path
  :after flycheck rtags)

(use-package helm-rtags
  :ensure nil
  :if rdm-executable-path
  :after rtags
  :custom
  (rtags-display-result-backend 'helm))

(defun rtags--eldoc-function ()
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

(defun company-rtags-setup ()
  "Configure `company-backends' for `company-rtags'."
  (delete 'company-semantic company-backends)
  ;; Whether completions are enabled.
  (setq rtags-completions-enabled t)
  (push '(company-rtags :with company-yasnippet) company-backends))

(defun flycheck-rtags-setup ()
  "Configure `flycheck-rtags'."
  ;; Do not enable `eldoc' here (it is enabled in separated configuration).
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

(defun tags-enable-rtags ()
  "Common function to enable and configure `rtags'."
  (rtags-mode 1)

  ;; TODO(serghei): Move outside
  (company-rtags-setup)
  (flycheck-rtags-setup)

  (setq-local eldoc-documentation-function
              #'rtags--eldoc-function)

  (rtags-enable-standard-keybindings)
  (rtags-start-process-unless-running))

(defun tags-enable-ggtags ()
  "Common function to enable and configure `ggtags'."
  (ggtags-mode 1)

  (setq-local imenu-create-index-function
              #'ggtags-build-imenu-index)

  (setq-local eldoc-documentation-function
              #'ggtags-eldoc-function))

(defun setup-tags-fronted ()
  "Common hook to enable tags fronted."
  (let ((cfg (ecfg-read-project-config)))
    (pcase (gethash "tags-frontend" cfg nil)
      ;; symbol
      ('ggtags     (tags-enable-ggtags))
      ('rtgas      (tags-enable-rtags))
      ;; nil
      ((pred null) nil)
      ;; unknown
      (f           (message "Unknown tags fronted `%S'" f)))))

(defun tags-enable-project-wide (frontend)
  "Setup project wide tags FRONTEND."
  (interactive
   (list (completing-read
          "Tags frontend: " '("ggtags" "rtags" "disable"))))
  (let* ((cfg (ecfg-read-project-config))
         (tags-frontend (if (equal frontend "disable") nil (make-symbol frontend)))
         (project-root (projectile-project-root)))
    (progn
      (defun apply-to-project-buffers (buf)
        (with-current-buffer buf
          (when (and tags-frontend
                     (equal (projectile-project-root) project-root))
            (setup-tags-fronted))))

      (puthash "tags-frontend" tags-frontend cfg)
      (ecfg-save-project-config cfg)
      (mapcar 'apply-to-project-buffers (buffer-list)))))

(provide 'setup-tags)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; setup-tags.el ends here
