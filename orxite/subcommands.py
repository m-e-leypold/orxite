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


# TODO: @subcommand should wrap every command procedure into a callable Subcommand object
# TODO: Default tagline should come from docstring

# *  Subcommand Registry ------------------------------------------------------|

# TODO:Docstring goes here

# TODO: Do commandline-parsing the subcommand decorator, postprocessing the
# args (--short => short, <key> => key) if not suppressed, then pass with **!

# ** Environment --------------------------------------------------------------|

from docopt   import docopt
from .version import version_string

# ** Registry -----------------------------------------------------------------|

subcommands        = {}
subcommand_aliases = {}
subcommand_docs    = []

def subcommand(f = None, *, doc = None, aliases = []):
    def register(name, f , doc, aliases =[] ):
        name = name.replace("_", "-")
        global subcommand_docs
        global subcommands
        subcommands[name] = f
        subcommand_docs.append((name, doc))
        for alias in aliases:
            subcommand_aliases[alias] = name
            
    if f is None:
        def __decorate__(g):
            register(g.__name__, g, doc, aliases)
            return g
        return __decorate__
    if type(f) == str:
        def __decorate__(g):
            register(g.__name__, g, f, aliases)
            return g
        return __decorate__
    register(f.__name__, f, doc, aliases)

    # TODO: Wrap sub-command arg parsing around the sub-command
    
    return f

def get_subcommand(name):
    try:
        return subcommands[name]
    except KeyError:
        try:
            return subcommands[subcommand_aliases[name]]
        except KeyError:
            panic(F"unknown sub-command '{name}'")
        
def parse_subcommand_args(f, args):
    cmdline = [f.__name__.replace("_","-"), *args ]
    args = docopt(
        f.__doc__,
        version=F"{version_string}",
        argv = cmdline
    )
    return args


