;; lc.el --- Check the total line count of config files. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023 Serghei Iakovlev <egrep@protonmail.ch>

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

;; Check the total line count of early-init.el and init.el
;; and fails if the total line count is more than 512.

;;; Code:

(defun count-lines-in-file (file)
  "Return the total number of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (count-lines (point-min) (point-max))))

(let* ((config-directory (expand-file-name "~/.emacs.d/"))
       (early-init-file (concat config-directory "early-init.el"))
       (init-file (concat config-directory "init.el"))
       (total-lines (+ (count-lines-in-file early-init-file)
                       (count-lines-in-file init-file))))
  (if (> total-lines 512)
      (error "Total line count exceeded 512 lines: %s" total-lines)
    (message "Total line count is within limit: %s" total-lines)))

(provide 'lc)

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; eval: (display-fill-column-indicator-mode)
;; coding: utf-8-unix
;; End:

;;; lc.el ends here
