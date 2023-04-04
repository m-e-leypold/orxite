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

from   .subcommands \
    import subcommand, parse_subcommand_args, subcommands, subcommand_docs # TODO: not use subcommands directly

import textwrap

# * Importing setup from __main__ 

from __main__ import help_pages

# * Help Page Registry ------------------------------------------------------ #

def help_page(f = None, *, doc = None):
    def register(name, f , doc):
        name = name.replace("_", "-")        
        global help_pages
        help_pages[name] = (f, doc)
    if f is None:
        def __decorate__(g):
            register(g.__name__, g, doc)
            return g
        return __decorate__
    if type(f) == str:
        def __decorate__(g):
            register(g.__name__, g, f)
            return g
        return __decorate__
    register(f.__name__, f, doc)
    return f


# * Generated Help Pages ---------------------------------------------------- #

@help_page( doc = "A list of all available help non-sub-command page topics" )
def pages():
    lines  = []
    width1 = max((len(k) for k in help_pages))
    for name in sorted(help_pages):
        spec = help_pages[name]
        if type(spec) == str:
            start = spec.index("\n")+1
            description = textwrap.dedent(spec[start:spec.index("\n", start)])
        else:
            description = spec[1] + "."
        lines.append(F"{name:{width1}}  {description}")
        
    return "\n".join(lines)

@help_page( doc = "A list of all available commands" )
def commands():
    lines  = []
    width1 = max((len(k) for k in subcommands))

    for command, doc in subcommand_docs:
        lines.append(F"{command:{width1}}  {doc}.")

    return "\n".join(lines)

# ** Processing docstrings ----------------------------------------------------|

# proper docstrings should already be delivered by subcommands
def strip_docstring(s):
    return textwrap.dedent(s)

# ** Help ---------------------------------------------------------------------|

def get_help_page(name):
    page = help_pages[name]
    if type(page) == str:
        return textwrap.dedent(page)
    gen, doc = page
    if callable(gen):
        return doc + ".\n\n" + gen()

@subcommand( doc = "Show help pages" )
def help(args):
    """greentape help -- show sub-command help or help pages

    Usage:
      greentape help <page>
      greentape help --help

    The name of the help pages pertaining to sub-commands is the the
    name of the respective sub-command. Use 'greentape help pages' to see
    all available pages not related to sub-commands and 'greentape help
    commands' to see all available sub-commands.
    
    """

    args = parse_subcommand_args(help, args)
    page = args['<page>']
    
    if page in help_pages:
        print("\n",get_help_page(page),"\n", sep="")
    elif page in subcommands:
        print("")
        print(strip_docstring(subcommands[page].__doc__))
    else:
        panic(F"help page '{page}' unknown to '{program_name}'")

