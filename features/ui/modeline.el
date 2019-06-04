;;; modeline.el --- Modeline related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Modeline related configuration for GNU Emacs.

;;; Code:

;; Activate column and line number in the modeline
(column-number-mode t)
(line-number-mode t)

;; Much simpler version of doom-modeline.
;; Absence of ‘atom-like’ features in the mode line much appreciated.
(use-package mood-line
  :preface
  ;; Until this bug https://gitlab.com/jessieh/mood-line/issues/2
  ;; is resolved
  (advice-add 'mood-line-segment-multiple-cursors :override 'string)
  (advice-add 'mood-line-segment-anzu :override 'string)
  :hook
  (after-init . mood-line-mode))

(provide 'modeline)
;;; modeline.el ends here
