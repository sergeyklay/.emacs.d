;;; langs-human.el --- Settings for Human Languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Serghei Iakovlev <egrep@protonmail.ch>

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
