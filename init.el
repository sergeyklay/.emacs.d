;;; init.el --- Initialization file. -*- lexical-binding: t; -*-

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

;;   This file bootstraps the configuration, which is divided into a number of
;; other files.
;;
;; I started this project on 4 March 2019 from this commit:
;; eb11ce25b0866508e023db4b8be6cca536cd3044

;;; Code:

;; Begin initialization
(require 'prelude (concat user-emacs-directory "site-lisp/prelude"))

(require 'advices)      ; Advices for Elisp functions
(require 'packaging)    ; Package management stuff and various related settings
(require 'appearance)   ; Set up appearance as soon as we can
(require 'windows)      ; Windows management features
(require 'modeline)     ; Modeline related configuration
(require 'defaults)     ; Sane defaults + settings for which there is no group
(require 'esession)     ; Setting for bookmarks, recentf, etc
(require 'buffer)       ; Buffer related configuration and utils
(require 'docs)         ; Setting up documentation features
(require 'completion)   ; Setting up completion system
(require 'shells)       ; Shells configuration
(require 'security)     ; GPG and security related features

;;;; Project management

(require 'projects)

;;;; IDE Features

(require 'devtools)
(require 'build-tools)
(require 'vcs)
(require 'editor)
(require 'tags)
(require 'complit)
(require 'expansion)
(require 'syntax-check)

;;;; Language support

(require 'langs-cc)
(require 'langs-python)
(require 'langs-human)
(require 'langs-conf)
(require 'langs-lisp)
(require 'langs-lua)
(require 'langs-markup)
(require 'langs-grammars)

;;;; Misc

(require 'im)           ; Key maps, IMs and relevant configurations
(require 'spelling)     ; Spell configuration
(require 'chats)        ; Chats support

;; Load settings specific for the current site
(when (file-exists-p user-host-dir)
  (mapc 'load (directory-files user-host-dir nil "^[^#].*el$")))

;;; init.el ends here
