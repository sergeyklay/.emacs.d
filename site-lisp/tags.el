;;; tags.el --- Setup tags. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021, 2022 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d

;; This file is NOT part of Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Configuration for tags and code navigation.

;;; Code:

(require 'cl-macs)
(require 'directories)

;;;; Constants

(defconst global-executable-path (executable-find "global")
  "The global executable path on this system.")

(defconst gtags-executable-path (executable-find "gtags")
  "The gtags executable path on this system.")

(defconst lid-executable-path (executable-find "lid")
  "The lid executable path on this system.")

(defconst mkid-executable-path (executable-find "mkid")
  "The mid executable path on this system.")

(defconst rdm-executable-path (executable-find "rdm")
  "The rdm executable path on this system.")

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
  (tags-add-tables nil))

;;;; Ggtags

;; A front-end for accessing the gtags-generated tags.
;; For more see URL `https://github.com/leoliu/ggtags'.
;;
;; To be efficient, the host system tool “gtags” (called “global”) should be
;; compiled from source with “exuberant-ctags”.  Also for proper parsing, the
;; Python package “pygments” should be installed.  For better and faster lookup
;; support, “id-utils” should be inastalled so a database instead of a text
;; file is used.
;;
;; The full requirements list to use “gtags” is:
;;   - exuberant-ctags
;;   - pygment
;;   - id-utils
;;   - global (6.5 or later)
;;
;; Note: There's an Emacs tool called “ctags” that is not the same as
;; exuberant-ctags' even though the executable is named the same.  Be careful
;; that's not in your path first.  The one possible solution to resolve name
;; collision is to pass the following option to Emacs' confige script:
;;
;;   --program-transform-name='s/^ctags$/ctags.emacs/'

(use-package ggtags
  :if (and global-executable-path gtags-executable-path)
  :commands (ggtags-mode)
  :custom
  ;; Auto-pdate GTAGS on each save.
  (ggtags-update-on-save t)
  ;; Do not auto-highlight tag at point.
  (ggtags-highlight-tag nil)
  ;; Enabling nearness requires global 6.5.+
  (ggtags-sort-by-nearness t)
  ;; The over size limit for the  GTAGS file.
  (ggtags-oversize-limit (* 100 1024 1024))
  ;; Generate the idutils DB.
  (ggtags-use-idutils (not (null mkid-executable-path)))
  ;; The directory to search GNU GLOBAL executables.
  (ggtags-executable-directory
   (directory-file-name (file-name-directory global-executable-path)))
  :bind (:map ggtags-mode-map
         ("C-c g s" . ggtags-find-other-symbol)
         ("C-c g h" . ggtags-view-tag-history)
         ("C-c g r" . ggtags-find-reference)
         ("C-c g f" . ggtags-find-file)
         ("C-c g c" . ggtags-create-tags)
         ("C-c g u" . ggtags-update-tags)
         ("C-c <"   . ggtags-prev-mark)
         ("C-c >"   . ggtags-next-mark)))

(add-hook 'prog-mode-hook
          (lambda ()
            (when (and (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode
                                       'emacs-lisp-mode)
                       (and global-executable-path gtags-executable-path))
              (ggtags-mode 1))))

(provide 'tags)
;;; tags.el ends here
