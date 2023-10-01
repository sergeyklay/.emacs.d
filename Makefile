# Copyright (C) 2019-2023 Serghei Iakovlev <egrep@protonmail.ch>
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

.PHONY: build
build: init.el test/bc.el
	$(RUNEMACS) --eval $(INIT_CODE) $(patsubst %,--load $(TOP)/%, $^)

.PHONY: checkdoc
checkdoc: test/checkdoc.el
	$(RUNEMACS) --load $(TOP)/$<

.PHONY: test
test: checkdoc build

.PHONY: help
help:
	@echo ''
	@echo 'Run "make install" first to install dependencies.'
	@echo ''
	@echo 'Available targets:'
	@echo '  help:       Show this help and exit'
	@echo '  install:    Install dependencies'
	@echo '  checkdoc:   Check doc for errors'
	@echo '  build:      Byte compile configuration files'
	@echo '  test:       Run the non-interactive test suite'
	@echo '  clean:      Remove byte compiled files and artifacts'
	@echo ''

# Local Variables:
# fill-column: 80
# mode: makefile-gmake
# coding: utf-8-unix
# End:
