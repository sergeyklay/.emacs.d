;;; langs-human.el --- Settings for Human Languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Configuration of Human Languages.

;;; Code:

(use-package outline
  :ensure nil
  :commands (outline-mode outline-minor-mode)
  :hook (prog-mode . outline-minor-mode))

(use-package csv-mode
  :mode (("\\.csv\\'" . csv-mode)))

(provide 'langs-human)
;;; langs-human.el ends here
