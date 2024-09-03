# Copyright (C) 2019-2024 Serghei Iakovlev <egrep@protonmail.ch>
#
# This file is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <https://www.gnu.org/licenses/>.

include default.mk

%.elc: %.el
	@printf "Compiling $<\n"
	@$(RUNEMACS) --eval '(setq byte-compile-error-on-warn nil)' \
		-f batch-byte-compile $<

.PHONY: clean
clean:
	$(RM) *.elc
	$(RM) test/*.elc

.PHONY: distclean
distclean: clean
	$(RM) GPATH GRTAGS GTAGS
	$(RM) package-quickstart.el
	$(RM) -r elpa

.PHONY: install
install: init.el
	$(RUNEMACS) --load $(TOP)/$<

.PHONY: checkdoc
checkdoc:
	@for f in $(SRCS) ; do                                  \
		echo "Checking documentation for$$f ...";       \
		$(EMACSBATCH) --eval "(checkdoc-file \"$$f\")"; \
	done && echo "Done."
	@echo ""

.PHONY: checkstyle
checkstyle: test/passive-voice-check.el
	$(RUNEMACS) --load $(TOP)/$<

.PHONY: checkstartup
checkstartup: $(SRCS)
	$(RUNEMACS) --eval '(progn (defvar url-show-status) (let ((debug-on-error t) (url-show-status nil) (user-emacs-directory default-directory) (user-init-file (expand-file-name "init.el"))(load-path (delq default-directory load-path))) (setq package-check-signature nil) (load-file user-init-file) (run-hooks (quote after-init-hook))))'
	@echo "Startup successful"
	@echo ""

.PHONY: test
test: checkstartup checkdoc checkstyle

.PHONY: help
help:
	@echo ''
	@echo 'Run "make install" first to install dependencies.'
	@echo ''
	@echo 'Available targets:'
	@echo '  help:          Show this help and exit'
	@echo '  install:       Install dependencies'
	@echo '  checkdoc:      Check doc for errors'
	@echo '  checkstyle:    Check for passive voice in documentation'
	@echo '  checkstartup:  Run Emacs startup to validate configuration and hooks'
	@echo '  test:          Run checks for startup, documentation, and style compliance'
	@echo '  clean:         Remove byte compiled files and artifacts'
	@echo ''

# Local Variables:
# fill-column: 80
# mode: makefile-gmake
# coding: utf-8-unix
# End:
