#
# Orxite - A static site generator for org format source
# Copyright (C) 2023  M E Leypold
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

.PHONY: example check

all: example
.ONESHELL:


example:
	set -eux
	cd example
	make
check:
	set -eux
	cd example
	make cleanest
	make
	make
	make cleaner
	make
