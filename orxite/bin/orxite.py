# * Orxite, (C) 2023 M E Leypold                                              #
#
#   Orxite -- A Org-Mode and Emacs bases static site generator.
#   Copyright (C) 2023  M E Leypold
#   
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#   
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#   
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
#   For altermative licensing options, see README.md
#

# * Usage & Help pages                                                 ------ #

""" orxite - Maintain a subsystem of packages built from source.

Usage:
  orxite <command> [<args> ...]
  orxite --help
  orxite --version
  orxite help <command-or-page>

Help system:
  - orxite help commands  -- List all commands with a short description.
  - orxite help <command> -- Show help page for <command>.
  - orxite help pages     -- List all available non-command help pages.
  - orxite help <page>    -- Show help page <page>.

Common commands:
  - (More TBD)
"""

help_pages = {
    'test-page' : """ asdas asdasd Pellentesque dapibus suscipit.

    ligula.  Donec posuere augue in quam.  Etiam vel tortor sodales
    tellus ultricies commodo.  Suspendisse potenti.  Aenean in sem ac
    leo mollis blandit.  Donec neque quam, dignissim in, mollis nec,
    sagittis eu, wisi.  Phasellus lacus.  Etiam laoreet quam sed arcu.
    Phasellus at dui in ligula mollis ultricies.  Integer placerat
    tristique nisl.  Praesent augue.  Fusce commodo.  Vestibulum
    convallis, lorem a tempus semper, dui dui euismod elit, vitae
    placerat urna tortor vitae lacus.  Nullam libero mauris, consequat
    quis, varius et, dictum id, arcu.  Mauris mollis tincidunt felis.
    Aliquam feugiat tellus ut neque.  Nulla facilisis, risus a rhoncus
    fermentum, tellus tellus lacinia purus, et dictum nunc justo sit
    amet elit.""",

    
    'credstore' : """ Configuring the credentials store.

    Aliquam erat volutpat.  Nunc eleifend leo vitae magna.  In id erat
    non orci commodo lobortis.  Proin neque massa, cursus ut, gravida
    ut, lobortis eget, lacus.  Sed diam.  Praesent fermentum tempor
    tellus.  Nullam tempus.  Mauris ac felis vel velit tristique
    imperdiet.  Donec at pede.  Etiam vel neque nec dui dignissim
    bibendum.  Vivamus id enim.  Phasellus neque orci, porta a, aliquet
    quis, semper a, massa.  Phasellus purus.  Pellentesque tristique
    imperdiet tortor.  Nam euismod tellus id erat."""
}

# * Environment  ------------------------------------------------------------ #

import sys
import os

sys.modules['__main__'] = sys.modules[__name__]

from   orxite.runtime import *
from   ..subcommands import subcommand, parse_subcommand_args
import subprocess

# * Cli App  ---------------------------------------------------------------- #

from   ..cliapp import main

# * Emacs ------------------------------------------------------------------- #


class Symbol:
    def __init__(self, name):
        self.name = name

class OrxiteEmacs:

    def __init__(self):
        self.arguments = ['emacs', '--batch',
                          '-l', os.path.join(version.lisp_dir,"orxite.el") ]
    @classmethod
    def to_tokens(cls, tokens, sexp):
        if isinstance(sexp, Symbol):
            tokens.append(sexp.name)
            return
        if  isinstance(sexp, list) or isinstance(sexp, tuple):
            tokens.append("(")
            for s in sexp:
                cls.to_tokens(tokens,s)
            tokens.append(")")
            return
        if  isinstance(sexp, str):
            tokens.append(f'"{sexp}"')  # can be done better! => needs escapes
            return
        if  sexp is None:
            tokens.append('nil')
            return
        if  sexp == True:
            tokens.append('t')
            return
        if  sexp == False:
            tokens.append('nil')
            return
        assert False

    @classmethod
    def stringify(cls, sexp):
        tokens = []
        cls.to_tokens(tokens, sexp)
        return " ".join(tokens)
    
    def eval(self, sexp):
        self.arguments.append("--eval=" + self.stringify(sexp))
        return self

    def call(self, *args):
        self.eval(args)
        return self
    
    def run(self):
        print("Running: ", self.arguments)
        subprocess.run(self.arguments, check=True)

# * Defaults ---------------------------------------------------------------- #

build_config = ".build/build.config"

# * Subcommmands  ----------------------------------------------------------- #
# ** Hello ------------------------------------------------------------------ #

@subcommand( doc = "Say hello")
def hello(args):
    """orxite hello -- Say hello.

    Usage:
      orxite hello <name>
    """
    
    args      = parse_subcommand_args(hello, args)
    name      = args["<name>"]

# ** to-html ---------------------------------------------------------------- #

@subcommand( doc = "Convert org file to html")
def org_to_html(args):
    """orxite org-to-html -- Convert org source to html.

    Usage:
      orxite org-to-html [--build-config=<config> | -b <config>] [--page-path=<path> | -p <path>] [-m <db> | --metadata=<db>] <source> [<target>]   
    """

    args   = parse_subcommand_args(org_to_html, args)
    config = args["<config>"]

    print(args)
    
    # (defun orxite-export-to-html
    #       (source-file &optional target-file build-setup)

    if config is None:
        if os.path.exists(build_config):
            config = build_config

    emacs = (OrxiteEmacs()
             .call(Symbol('orxite-export-to-html'),
                   args["<source>"],
                   args["<target>"],
                   config,
                   args["<path>"],
                   args["<db>"]
                   )
             .run())

    # Consider implementing 2 steps in one:
    #   - Preprocessing partially and exporting metadata
    #   - Using all the metadata to convert to HTML
    #     (one step: preprocess, process to html, insert into template)


# ** preprocess ------------------------------------------------------------- #

@subcommand( doc = "Convert org file to html")
def preprocess(args):
    """orxite preprocess -- Preprocess and preparse org files

    Usage:
      orxite preprocess [--build-config=<config> | -b <config>] [--page-path=<path> | -p <path>] <source> <destination>
    """

    args   = parse_subcommand_args(preprocess, args)
    config = args["<config>"]

    print(args)
    
    if config is None:
        if os.path.exists(build_config):
            config = build_config

    emacs = (OrxiteEmacs()
             .call(Symbol('orxite-preprocess'),
                   args["<source>"],
                   args["<destination>"],
                   config,
                   args["<path>"]
                   )
             .run())
    
# * Run main if invoked directly  ------------------------------------------- #
    
if __name__ == '__main__':
    main()
