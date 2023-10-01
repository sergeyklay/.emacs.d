;; bc.el --- Byte compile and syntax check the code.

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

;; Byte compile and syntax check the code.

;;; Code:

(let ((files (directory-files-recursively
              "~/.emacs.d"
              "^[^#].*el$"
              nil
              (lambda (path)
                (let ((dir (file-name-base path)))
                  (and
                   (not (string-prefix-p ".git" dir))
                   (not (string-prefix-p "anaconda-mode" dir))
                   (not (string-prefix-p "auto-save-list" dir))
                   (not (string-prefix-p "backup" dir))
                   (not (string-prefix-p "elpa" dir))
                   (not (string-prefix-p "eshell" dir))
                   (not (string-prefix-p "transient" dir)))))))
      (byte-compile-error-on-warn t))
  (dolist (file files)
    (let ((basename (file-name-nondirectory file)))
      (when (not (member basename '(".dir-locals.el"
                                    "custom.el"
                                    "early-init.el")))
        (byte-compile-file file)))))

(provide 'bc)

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; eval: (display-fill-column-indicator-mode)
;; coding: utf-8-unix
;; End:

;;; bc.el ends here
