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
	$(RM) GPATH GRTAGS GTAGS
	$(RM) -r elpa

 .PHONY: install
install: init.el
	$(RUNEMACS) --load init.el

.PHONY: build
build: init.el test/bc.el
	$(RUNEMACS) --eval '(let ((debug-on-error t))(run-hooks (quote after-init-hook)))' --load init.el --load ~/.emacs.d/test/bc.el
