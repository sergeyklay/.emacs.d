;;; sh-autoconf.el --- autoconf flavour for sh-mode

;; Copyright 2002, 2007, 2009, 2010, 2011, 2012, 2015, 2016, 2017 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 9
;; Keywords: languages, shell
;; URL: http://user42.tuxfamily.org/sh-autoconf/index.html
;; EmacsWiki: ShMode

;; sh-autoconf.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; sh-autoconf.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This code adds a shell type "autoconf" to sh-mode, for use on autoconf
;; configure.in, acinclude.m4, etc files.  This is good if you've got a lot
;; of shell in your configury and like sh-mode for shell editing.

;;; Emacsen:

;; Designed for Emacs 21 and up.  Works in XEmacs 21.
;; Doesn't work in Emacs 20 due to "append" parameter to `add-to-list'.

;;; Install:

;; Put sh-autoconf.el in one of your `load-path' directories, and in your
;; .emacs add
;;
;;     (eval-after-load "sh-script" '(require 'sh-autoconf))
;;
;; If you want to use sh-mode instead of autoconf-mode by default on
;; autoconf files then
;;
;;     (add-to-list 'auto-mode-alist
;;                  '("/configure\\.\\(ac\\|in\\)\\'" . sh-mode))
;;     (add-to-list 'auto-mode-alist
;;                  '("/ac\\(include\\|local\\)\\.m4\\'" . sh-mode))
;;
;; You can add similar patterns if you keep macros in other kinds of
;; filenames, like "aclocal/foo.m4" or "macros/foo.m4" etc.

;;; History:

;; Version 1 - first standalone version.
;; Version 2 - any .m4 is autoconf flavour, info-look addition.
;; Version 3 - new home page
;; Version 4 - undo defadvice on unload-feature
;; Version 5 - express dependency on 'advice
;; Version 6 - allow for sh-mode in a non-file buffer
;; Version 7 - emacs24 no `sh-require-final-newline'
;; Version 8 - new email
;; Version 9 - no info-look.el sh-mode in xemacs, skip that setup

;;; Code:

;;;###autoload (eval-after-load "sh-script" '(require 'sh-autoconf))

(require 'sh-script)

;; explicit dependency on advice.el since `sh-autoconf-unload-function'
;; needs `ad-find-advice' macro when running not byte compiled and that
;; macro isn't autoloaded
(require 'advice)

;; autoconf.el not supplied with xemacs, but might have it separately
(unless (fboundp 'autoconf-current-defun-function)
  (if (locate-library "autoconf")
      (autoload 'autoconf-current-defun-function "autoconf")))


;; barest shell assumptions are recommended for autoconfery, so derive from
;; plain `sh'
(add-to-list 'sh-ancestor-alist '(autoconf . sh))

;; "AC_DEFUN([MY_FOO_BAR]" is an autoconf definition; the current
;; recommendation is to quote the name with "[ ]"
;;
;; "define(MY_OTHER_SOMETHING," is a raw m4 definition, the name might or
;; might not be quoted (usually it needn't be)
;;
;; A name is only letters, digits and underscore, as per the "Names" node in
;; the m4 manual.  In principle shell style "function foo()" is available
;; too, but normally you don't use those in autoconf due to assuming only
;; the bare minimum shell features, thus no inheritance from "sh" flavour.
;;
(add-to-list
 'sh-imenu-generic-expression
 '(autoconf
   . ((nil "^\\s-*\\(AC_DEFUN\\|define\\)(\\s-*\\[?\\([A-Za-z0-9_]+\\)" 2))))

;; probably best to have a newline if running on dodgy shells
(if (boundp 'sh-require-final-newline) ;; emacs23 and earlier
    (add-to-list 'sh-require-final-newline '(autoconf . t)))

;; `sh-assignment-regexp'
;;     for autoconf could match "AC_SUBST(FOO,bar)", but it's not clear how
;;     to do that and "FOO=bar" simultaneously with a single \( \) group
;;     match for the name

;; "test" always best since "[ ]" are the quote chars
(add-to-list 'sh-test '(autoconf "test " . 6))

(let ((sh-append
       ;; hack for "eval sh-append" (emacs 21) vs "sh-append" (emacs 22)
       ;; style of sh-feature
       (if (eq 1 (car (let ((sh-shell 'bash))
                        (sh-feature '((sh eval sh-append foo 1)
                                      (foo 2))))))
           '(eval sh-append)
         '(sh-append))))

  ;; The docstring for sh-builtins says this is used for completing read,
  ;; but is that true?  Could put a long list (a very long list) of all
  ;; autoconf and automake builtin macros if that was so, instead of just
  ;; patterns.
  (add-to-list 'sh-builtins
               `(autoconf ,@sh-append sh
                          ;; autoconf AC, AH, AS, AU, plus automake AM
                          "A[CHSUM]_[A-Za-z0-9_]+"
                          ;; m4 builtin, sometimes used in extremis
                          "changequote"
                          ;; m4 builtins renamed by m4sugar
                          "m4_[A-Za-z0-9_]+")))

;; This is the commonly used pre-defined variables from autoconf, plus the
;; most commonly used ones from standard macros like AC_PROG_CC.
;;
;; Perhaps some of the "dir" variables are excessive; they normally ought to
;; be used in the makefile, not in configure itself.
;;
(add-to-list 'sh-variables
             '(autoconf
               ;; don't inherit from "sh" or "shell" flavour, almost all
               ;; of those variables have no place in a configure script,
               ;; the following ones arise though
               "IFS" "PATH"

               "abs_builddir"     "abs_srcdir"
               "abs_top_builddir" "abs_top_srcdir"   "AWK"
               "bindir" "build" "build_alias" "build_cpu"
               "build_os" "build_vendor" "builddir"
               "CC" "CFLAGS" "CPP" "CPPFLAGS"
               "cross_compiling"
               "CXX" "CXXCPP" "CXXFLAGS"
               "datadir" "datarootdir" "docdir" "dvidir"
               "ECHO_C" "ECHO_N" "ECHO_T" "EGREP"
               "ERL" "ERLC" "ERLCFLAGS"
               "exec_prefix" "EXEEXT"
               "F77" "FC" "FCFLAGS" "FCLIBS" "FFLAGS"
               "FGREP" "FLIBS"
               "GREP"
               "host" "host_alias" "host_cpu" "host_os" "host_vendor"
               "htmldir" "includedir" "infodir"
               "LDFLAGS" "LEX" "LEXLIB"
               "libdir" "libexecdir"
               "LIBOBJDIR" "LIBOBJS" "LIBS"
               "LN_S" "localedir" "localstatedir" "mandir"
               "MKDIR_P"
               "OBJC" "OBJCFLAGS" "OBJCPP" "OBJEXT"
               "PACKAGE_BUGREPORT" "PACKAGE_NAME" "PACKAGE_STRING"
               "PACKAGE_TARNAME" "PACKAGE_VERSION"
               "pdfdir" "prefix" "psdir"
               "RANLIB" "sbindir"
               "SED" "sharedstatedir"
               "srcdir" "sysconfdir"
               "target" "target_alias" "target_cpu" "target_os"
               "target_vendor"
               "top_builddir" "top_srcdir"
               "X_CFLAGS" "X_EXTRA_LIBS" "X_LIBS" "X_PRE_LIBS"
               "YACC"))

;; "dnl" gets comment starter syntax, and newline is its comment end (the
;; same as for "#" comments)
;;
;; Could think about string syntax on the argument to AC_MSG macros like
;; AC_MSG_ERROR([some text]), to distinguish them from other macros which
;; take code arguments.  But probably that won't work with multi-line
;; messages or with nested "[ ]" delimiters.
;;
(when (eval-when-compile (boundp 'sh-font-lock-syntactic-keywords))
  (defun sh-autoconf-font-lock-dnl (limit)
    "In shell type autoconf, search for an m4 `dnl' comment starter.
This function is for use in `font-lock-syntactic-keywords'.

LIMIT is a buffer position to stop looking.  The return is
non-nil if a match is made (the entire match is the dnl).  If no
match or if not in autoconf flavour shell, then return nil."
    (and (eq sh-shell 'autoconf)
         (re-search-forward "\\<dnl\\>" limit t)))

  (add-to-list 'sh-font-lock-syntactic-keywords
               '(sh-autoconf-font-lock-dnl 0 "<")))

;; In `info-lookup-alist', setup sh-mode to have autoconf-mode as an
;; "other-mode", so lookups in sh-mode also consult autoconf-mode manuals.
;; This is rather autoconf specific, but as long as it's last in the search
;; then it won't interfere with normal shell stuff.
;;
;; XEmacs 21.4 info-look.el doesn't have any sh-mode, so skip for it.
;;
(eval-after-load "info-look"
  '(progn
     (setq info-lookup-alist ;; non-destructive
           (copy-tree info-lookup-alist))
     (let* ((alist       (cdr (assq 'symbol info-lookup-alist)))
            (entry       (assq 'sh-mode alist))
            (cell        (nthcdr 5 entry)) ;; cons cell of other-modes
            (other-modes (car cell)))
       (when cell   ;; if sh-mode entry exists
         (unless (memq 'autoconf-mode other-modes)
           (setcar cell (append other-modes '(autoconf-mode)))
           ;; not sure if a reset is necessary when adding to other-modes
           (info-lookup-reset))))))

(defadvice sh-mode (after sh-autoconf activate)
  "Use flavour `autoconf' on autoconf configure.ac etc."

  ;; this ought to be in the `interpreter' selection part of the sh-mode
  ;; code, before running sh-mode-hook, but after is near enough usually
  ;;
  (when (and (buffer-file-name)
             (let (case-fold-search)
               (string-match "\\(\\.m4\\|/configure\\.\\(in\\|ac\\)\\)\\'"
                             (buffer-file-name))))

    ;; similar to comment-start-skip in autoconf-mode
    ;; might prefer a sh-feature lookup to set this, eventually
    (set (make-local-variable 'comment-start-skip)
         (if (string-match "\\`\\_<\\'" "_<")
             "\\(\\<dnl\\>\\|#+\\)[ \t]*"    ;; emacs 21, only \<
           "\\(\\_<dnl\\_>\\|#+\\)[ \t]*"))  ;; emacs 22, have \_<

    ;; it's a bit nasty to force this, but 2 is conventional in autoconfery
    ;; `sh-indentation' is an obsolete variable (as of 26.1)
    ;; (set (make-local-variable 'sh-indentation)  2)
    (set (make-local-variable 'sh-basic-offset) 2)

    ;; same as autoconf-mode
    (if (fboundp 'autoconf-current-defun-function) ;; when available
        (set (make-local-variable 'add-log-current-defun-function)
             'autoconf-current-defun-function))

    ;; this is last so the variable setups are done before sh-set-shell-hook
    ;; runs
    (sh-set-shell "autoconf" nil nil)))

(defun sh-autoconf-unload-function ()
  "Remove defadvice from `sh-mode'.
This is called by `unload-feature'."

  ;; Could undo some of the various setups applied above, but that'd be a
  ;; bit of a chore and `unload-feature' is very rarely used.  Undoing the
  ;; sh-mode mangling at least puts that back to plain operation.

  (when (ad-find-advice 'sh-mode 'after 'sh-autoconf)
    (ad-remove-advice   'sh-mode 'after 'sh-autoconf)
    (ad-activate        'sh-mode))
  nil) ;; and do normal unload-feature actions too

;; LocalWords: autoconf acinclude configury dnl ac

(provide 'sh-autoconf)

;;; sh-autoconf.el ends here
