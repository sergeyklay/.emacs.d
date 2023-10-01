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

TOP := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

# Run “make build” by default
.DEFAULT_GOAL = build

EMACS      ?= emacs

INIT_CODE   = '(let ((debug-on-error t)(user-emacs-directory default-directory))(run-hooks (quote after-init-hook)))'
EMACSFLAGS ?=
EMACSBATCH  = $(EMACS) -Q --batch $(EMACSFLAGS)
RUNEMACS    =

# Program availability
HAVE_CASK := $(shell sh -c "command -v $(CASK)")
ifndef HAVE_CASK
RUNEMACS = $(EMACSBATCH)
else
RUNEMACS = $(CASK) exec $(EMACSBATCH)
endif

# Local Variables:
# fill-column: 80
# mode: makefile-gmake
# coding: utf-8-unix
# End:
