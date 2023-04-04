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

import os, sys

program_name   = "Orxite"
package_root   = os.path.dirname(__file__)

def _read_version_():
    with open(os.path.join(package_root,"VERSION")) as f:
        return f.readline().strip(" \n")

version        = _read_version_()
bin_name       = os.path.basename(sys.argv[0])
version_string = f"{program_name} {version}"

lisp_dir       = os.path.dirname(os.path.dirname(__file__))

