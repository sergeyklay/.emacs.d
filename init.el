;;; init.el --- Initialization file. -*- lexical-binding: t; -*-

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

;;   This file is used to initialize GNU Emacs for my daily needs.
;; I started this project on 4 March 2019 from this commit:
;; eb11ce25b0866508e023db4b8be6cca536cd3044

;;; Code:

;;;; Core

;; Begin initialization
(require 'prelude (concat user-emacs-directory "site-lisp/prelude"))

(require 'packaging)     ; Package management stuff and various related settings
(require 'appearance)    ; Set up appearance as soon as we can
(require 'windows)       ; Windows management features
(require 'im)            ; Key maps, IMs and relevant configurations
(require 'devtools)      ; Various devtools
(require 'modeline)      ; Modeline related configuration
(require 'defaults)      ; Sane defaults + settings for which there is no group
(require 'setup-session) ; Setting for bookmarks, recentf, etc
(require 'setup-buffer)  ; Buffer related configuration and utils
(require 'docs)          ; Setting up documentation features
(require 'vcs)           ; VCS (mostly git) related features
(require 'search-tools)  ; Setting up search tools
(require 'completion)    ; Setting up completion system

;;;; Project management

(require 'projects)

;;;; IDE Features

(require 'setup-tags)

;;;; Configuration of progmodes

(require 'setup-cc)
(require 'setup-shells)
(require 'setup-python)

;;;; Minor Languages Support

(require 'setup-md)

(require 'security)     ; GPG and security related features
(require 'expansion)    ; Configuration of expansions
(require 'editor)       ; Features related to editor behavior
(require 'syntax-check) ; Syntax checkers
(require 'spelling)     ; Spell configuration
(require 'build-tools)  ; Add build tools support
(require 'langs-human)  ; Settings for human languages
(require 'langs-conf)   ; Add support for the configuration like languages
(require 'langs-lisp)   ; Configure the Lisp-family of languages
(require 'langs-lua)    ; Lua support
(require 'langs-org)    ; Org related configuration

(require 'grammars)     ; Various language grammars
(require 'chats)        ; Chats support

;; Load settings specific for the current site
(when (file-exists-p user-host-dir)
  (mapc 'load (directory-files user-host-dir nil "^[^#].*el$")))

;;; init.el ends here
