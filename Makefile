# Copyright (C) 2019, 2020, 2021, 2022 Serghei Iakovlev <egrep@protonmail.ch>
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
	$(RM) site-lisp/*.elc
	$(RM) test/*.elc

.PHONY: distclean
distclean: clean
	$(RM) GPATH GRTAGS GTAGS
	$(RM) -r elpa

.PHONY: install
install: init.el
	$(RUNEMACS) --load $(TOP)/$<
	$(info All GNU Emacs packages have been installed.)

.PHONY: build
build: init.el test/bc.el
	DEBUG=1 $(RUNEMACS) --eval $(INIT_CODE) $(patsubst %,--load $(TOP)/%, $^)
	$(info All Elisp files have been compiled.)

.PHONY: checkdoc
checkdoc: test/checkdoc.el
	$(RUNEMACS) --load $(TOP)/$<
	$(info All Elisp files have been checked for style errors.)

.PHONY: test
test: checkdoc build
