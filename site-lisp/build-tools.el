;;; build-tools.el --- Build tools support. -*- lexical-binding: t; -*-

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

;; Support of various build tools.

;;; Code:

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'"
  :hook
  ((cmake-mode . company-mode))
  :config
  (setq cmake-tab-width tab-width))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package company-cmake
  :ensure nil                 ; Included in company
  :after (company cmake-mode)
  :config
  (push 'company-cmake company-backends))

(use-package make-mode
  :ensure nil
  :defer t
  :after (company)
  :hook (makefile-mode . company-mode))

(provide 'build-tools)
;;; build-tools.el ends here
