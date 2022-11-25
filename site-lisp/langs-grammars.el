;;; langs-grammars.el --- Language grammars. -*- lexical-binding: t; -*-

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

;; Add support of language grammars for GNU Emacs.

;;; Code:

(use-package bnf-mode
  :defer t
  :mode "\\.bnf\\'")

(use-package lemon-mode
  :defer t
  :mode "\\.lemon\\'")

(use-package bison-mode
  :defer t
  :mode (("\\.lex\\'" . bison-mode)
         ("\\.yy?\\'" . bison-mode)
         ("\\.ll?\\'" . bison-mode)))

(provide 'langs-grammars)
;;; langs-grammars.el ends here
