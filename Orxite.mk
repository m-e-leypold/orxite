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

# * Default targets ------------------------------------------------------------

@all:: @site
@site::
clean::
cleaner:: clean
	rm -rf $(BUILD/BACKEND)
cleanest:: cleaner
	rm -rf $(BUILD)


.PHONY: @site all clean cleaner

# * Paths and configuration ----------------------------------------------------
# ** Defaults ------------------------------------------------------------------
#
#    These are parameters that can be overwritten or added to in Config.mk

SITE-STATIC-FILES-PATTERNS ?= style/%
SITE-STATIC-DIRS-PATTERNS  ?= style/%

# ** General  ------------------------------------------------------------------

SCRIPTS      := $(shell realpath "$(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))")
export PATH  := $(SCRIPTS):$(PATH)

ifneq ($(strip $(filter clean%,$(MAKECMDGOALS))),)
  CLEAN-MODE = yes
else
  override undefine CLEAN-MODE
endif

ASSETS   ?= $(BLOG)/.assets
-include $(ASSETS)/Config.mk
-include Config.mk
BLOG        ?= .
BACKEND     ?= orxite
SITE-CONFIG ?= Site.config

SITE-CONFIG := $(patsubst ./%,%,$(BLOG:%=%/$(SITE-CONFIG)))

BUILD         = .build
BUILD/BACKEND = $(BUILD)/$(BACKEND)

SITE-VERSION := $(shell cd $(BLOG) && $(SCRIPTS)/site-version)
export SITE_VERSION := $(SITE-VERSION)

ifneq ($(filter %-DIRTY,$(SITE-VERSION)),)
  DIRTY-VERSION = yes
else
  override undefine DIRTY-VERSION
endif


ifdef MAKE_RESTARTS
$(info )
endif

$(info -- $(lastword $(MAKEFILE_LIST)) --)

ifndef CLEAN-MODE
$(info MAKE_RESTARTS = $(MAKE_RESTARTS))
$(info )
endif

$(info SCRIPTS      =  $(SCRIPTS))
$(info PATH         =  $(PATH))
$(info ASSETS       =  $(ASSETS))
$(info BLOG         = $(BLOG))
$(info SITE-VERSION = $(SITE-VERSION))
$(info )

$(info BUILD         =  $(BUILD))
$(info BACKEND       =  $(BACKEND))
$(info BUILD/BACKEND =  $(BUILD/BACKEND))
$(info )


# ** Exporting the configuration -----------------------------------------------

BUILD.CONFIG    = $(BUILD)/build.config

BUILD-CONFIG-DATA = \
        :site-config "$(SITE-CONFIG)" \
	:page-db "$(PAGE-DB)" \
	:source-root "$(BLOG)" \
	:scripts "$(SCRIPTS)"  \

$(info BUILD.CONFIG =  $(BUILD.CONFIG))

$(BUILD.CONFIG): #  $(MAKEFILE_LIST) 
	echo '( $(BUILD-CONFIG-DATA))' >$@

# ** Python bootstrap ----------------------------------------------------------

PYENV ?= $(BUILD)/pyenv

$(PYENV)/READY:
	mkdir -p $(@D)
	python3 -mvenv $(PYENV)
	{ set -eu; \
          . $(PYENV)/bin/activate; \
	  pip install -U -r $(SCRIPTS)/requirements.txt; \
	  pip install --upgrade pip; \
          cd $(SCRIPTS); \
          pip install -e .; \
        }
	touch "$@"

ifndef CLEAN-MODE
  include $(PYENV)/READY
endif

export PATH:=$(shell if test -f $(PYENV)/bin/activate; then . $(PYENV)/bin/activate; fi; echo $$PATH)
$(info PATH    =  $(PATH))
$(info python  =  $(shell which python))

# * Orxite static assets ------------------------------------------------------

ORXITE-STATIC-FILES = $(shell cd $(SCRIPTS) && find style -maxdepth 1  -mindepth 1 -type f)
ORXITE-STATIC-DIRS  = $(shell cd $(SCRIPTS) && find style -maxdepth 1  -mindepth 1 -type d)

$(info ORXITE-STATIC-FILES   = $(ORXITE-STATIC-FILES))
$(info ORXITE-STATIC-DIRS    = $(ORXITE-STATIC-DIRS))


# * Reading files and directories  ---------------------------------------------

IGNORE-DIRS   = ".[a-z]*" ltximg test-out
IGNORE-FILES  = '*~' '.*' '\#*' Makefile 

FIND := cd "$(BLOG)" \
        && find . \( -type d  \( -false $(IGNORE-DIRS:%= -o -name %) \) -prune \) -o

--IGNORE-HTML-SITE-STATIC-DIRS := \( -type d  \( -false $(SITE-STATIC-DIRS-PATTERNS:%%= -o -path "./%*") \) -prune \) -o

|NORMALIZE-NAMES = | sed 's|^[.]//*||;s|//*|/|g'

FIND-DIRS  := $(FIND)  $(--IGNORE-HTML-SITE-STATIC-DIRS) \( -type d -print \) $(|NORMALIZE-NAMES)
FIND-FILES := $(FIND)  $(--IGNORE-HTML-SITE-STATIC-DIRS) \( -type f  \
                          \( -not \( -false $(IGNORE-FILES:%= -o -name %) \) \) -print \) \
              $(|NORMALIZE-NAMES)

LIST-TO-MK := DO(){ echo "$$1 = \\"; sed <"$$2" 's|^|  |;s|$$| \\|'; echo; }; DO

ifndef CLEAN-MODE
  include $(BUILD)/DIRS.mk
  include $(BUILD)/FILES.mk
endif

$(BUILD)/DIRS: $(DIRS)
	mkdir -p $(@D)
	$(FIND-DIRS) >$@

$(BUILD)/FILES: $(DIRS)
	mkdir -p $(@D)
	$(FIND-FILES) >$@

$(BUILD)/%.mk: $(BUILD)/%
	$(LIST-TO-MK) "$(notdir $*)" "$<" >$@

# * Stage, then publish rule (except when overriden) ---------------------------

$(BUILD/BACKEND)/public/%: $(BUILD/BACKEND)/stage/%
	mkdir -p $(@D)
	cp $< $@

# * Static files ---------------------------------------------------------------


# ** Site static files ---------------------------------------------------------

$(info SITE-STATIC-FILES-PATTERNS = $(SITE-STATIC-FILES-PATTERNS))
$(info SITE-STATIC-DIRS-PATTERNS  = $(SITE-STATIC-DIRS-PATTERNS))

SITE-STATIC-DIRS  := $(filter $(shell ls -1d $(SITE-STATIC-DIRS-PATTERNS:%/%=%/*)),$(DIRS))
FILES             := $(filter-out $(SITE-STATIC-DIRS) $(SITE-STATIC-DIRS:%=%/%), $(FILES))
DIRS              := $(filter-out $(SITE-STATIC-DIRS) $(SITE-STATIC-DIRS:%=%/%), $(DIRS))
SITE-STATIC-FILES := $(filter $(SITE-STATIC-FILES-PATTERNS),$(FILES))
$(info SITE-STATIC-FILES = $(SITE-STATIC-FILES))
$(info SITE-STATIC-DIRS  = $(SITE-STATIC-DIRS))

STATIC-FILES := $(sort $(ORXITE-STATIC-FILES) $(SITE-STATIC-FILES))
STATIC-DIRS  := $(sort $(ORXITE-STATIC-DIRS) $(SITE-STATIC-DIRS))

STATIC-FILES.from-orxite = $(filter-out $(SITE-STATIC-FILES), $(STATIC-FILES))
STATIC-FILES.from-site   = $(SITE-STATIC-FILES)

STATIC-DIRS.from-orxite = $(filter-out $(SITE-STATIC-DIRS), $(STATIC-DIRS))
STATIC-DIRS.from-site   = $(SITE-STATIC-DIRS)

$(STATIC-FILES.from-site:%=$(BUILD/BACKEND)/stage/%): $(BUILD/BACKEND)/stage/%: %
	mkdir -p $(@D)
	cp $< $@

$(STATIC-FILES.from-orxite:%=$(BUILD/BACKEND)/stage/%): $(BUILD/BACKEND)/stage/%: $(SCRIPTS)/%
	mkdir -p $(@D)
	cp $< $@


$(STATIC-DIRS.from-site:%=$(BUILD/BACKEND)/stage/%): $(BUILD/BACKEND)/stage/%: %
	mkdir -p $(@D)
	rm -rf "$@"
	cp -r $< $@

$(STATIC-DIRS.from-orxite:%=$(BUILD/BACKEND)/stage/%): $(BUILD/BACKEND)/stage/%: $(SCRIPTS)/%
	mkdir -p $(@D)
	rm -rf "$@"
	cp -r $< $@


$(STATIC-DIRS.from-site:%=$(BUILD/BACKEND)/public/%): $(BUILD/BACKEND)/public/%: %
	mkdir -p $(@D)
	rm -rf "$@"
	cp -r $< $@

$(STATIC-DIRS.from-orxite:%=$(BUILD/BACKEND)/public/%): $(BUILD/BACKEND)/public/%: $(SCRIPTS)/%
	mkdir -p $(@D)
	rm -rf "$@"
	cp -r $< $@

publish-static-files: \
	$(STATIC-FILES.from-site:%=$(BUILD/BACKEND)/public/%)   \
	$(STATIC-FILES.from-orxite:%=$(BUILD/BACKEND)/public/%) \
	$(STATIC-DIRS.from-site:%=$(BUILD/BACKEND)/public/%)    \
	$(STATIC-DIRS.from-orxite:%=$(BUILD/BACKEND)/public/%)

stage-static-files: \
	$(STATIC-FILES.from-site:%=$(BUILD/BACKEND)/stage/%)   \
	$(STATIC-FILES.from-orxite:%=$(BUILD/BACKEND)/stage/%) \
	$(STATIC-DIRS.from-site:%=$(BUILD/BACKEND)/stage/%)    \
	$(STATIC-DIRS.from-orxite:%=$(BUILD/BACKEND)/stage/%)

@site:: publish-static-files

# * ---------------------------------------------------------------------------

ifndef CLEAN-MODE
  $(info DIRS    =  $(DIRS))
  $(info FILES   =  $(FILES))
  $(info )
endif

# * Org rendering pipeline ----------------------------------------------------

ORG-FILES := $(filter %.org, $(FILES))
$(info ORG-FILES      = $(ORG-FILES))

# ** Pre-processing and metadata extraction -----------------------------------

ORG-FILES.preprocessed := $(ORG-FILES:%=$(BUILD/BACKEND)/stage/%)
PAGE-DB     := $(BUILD/BACKEND)/page-db.meta
PAGE-DB.REC := $(BUILD/BACKEND)/page-db.rec

$(BUILD/BACKEND)/stage/%.parsed: %.org $(BUILD.CONFIG)
	mkdir -p $(@D)
	orxite preprocess -b $(BUILD.CONFIG) -p "$*" "$<" "$(@:%.parsed=%.org)" || { rm -f "$@"; false; }

$(BUILD/BACKEND)/stage/%.rec: $(BUILD/BACKEND)/stage/%.parsed
	test -f "$@" && touch "$@"

$(BUILD/BACKEND)/stage/%.meta: $(BUILD/BACKEND)/stage/%.parsed
	test -f "$@" && touch "$@"

$(BUILD/BACKEND)/stage/%.org: $(BUILD/BACKEND)/stage/%.parsed
	test -f "$@" && touch "$@"

$(PAGE-DB.REC): $(ORG-FILES.preprocessed:%.org=%.rec)
	mkdir -p $(@D)
	for REC in $^; do { cat $$REC; echo; }; done > $@

$(PAGE-DB): $(ORG-FILES.preprocessed:%.org=%.meta)
	{ echo "("; for REC in $^; do { cat $$REC; echo; }; done; echo ")"; } > $@

preprocess: $(ORG-FILES.preprocessed) $(PAGE-DB) $(PAGE-DB.REC)

# ** Exporting to HTML ---------------------------------------------------------

ORG-FILES.html          := $(ORG-FILES:%.org=$(BUILD/BACKEND)/stage/%.html)
ORG-FILES.published     := $(ORG-FILES:%.org=$(BUILD/BACKEND)/public/%.html)

$(info ORG-FILES.html = $(ORG-FILES.html))

# orxite org-to-html -b .build/build.config -p sample/test2   .build/html-site/org-preprocessed/sample/test2.org

$(BUILD/BACKEND)/stage/%.html: $(BUILD/BACKEND)/stage/%.org $(BUILD/BACKEND)/stage/%.meta $(BUILD.CONFIG)
	orxite org-to-html -b $(BUILD.CONFIG) -p "$*" $(patsubst %,-m "%",$(filter-out $(PAGE-DB),$(lastword $(filter %.meta,$^)))) "$<"

ifndef CLEAN-MODE
  include $(BUILD/BACKEND)/requiring-page-db.mk
endif

$(BUILD/BACKEND)/requiring-page-db.mk: $(PAGE-DB.REC)
	recsel -e 'requires_page_db == "t"' -R page_path <$< | awk '{printf "$$(BUILD/BACKEND)/stage/%s.html: $$(PAGE-DB)\n", $$0}' >$@.tmp
	mv $@.tmp $@


export-html:  $(ORG-FILES.html)
publish-html: $(ORG-FILES.published)

@site:: publish-html

# * Permalinks -----------------------------------------------------------------

ifndef CLEAN-MODE
  -include $(BUILD/BACKEND)/PERMALINKS.mk
endif

$(BUILD/BACKEND)/PERMALINKS: $(PAGE-DB.REC)
	mkdir -p $(@D)
	recsel < $<  -e 'id != "nil"' -R id | awk '/^$$/{next}{print}' > $@

$(BUILD/BACKEND)/%.mk: $(BUILD/BACKEND)/%
	$(LIST-TO-MK) "$(notdir $*)" "$<" >$@

PERMALINK-FILES           := $(PERMALINKS:%=$(BUILD/BACKEND)/stage/perma/%.html)
PERMALINK-FILES.published := $(PERMALINKS:%=$(BUILD/BACKEND)/public/perma/%.html)

$(info PERMALINKS = $(PERMALINKS))

html-permalinks: $(PERMALINK-FILES)

$(PERMALINK-FILES): $(BUILD/BACKEND)/stage/perma/%.html: $(PAGE-DB.REC) $(SCRIPTS)/redirect-template.html
	mkdir -p $(@D)
	recsel -e 'id = "$*"' <$< | recfmt -f $(SCRIPTS)/redirect-template.html >$@

permalinks: $(PERMALINK-FILES)
publish-permalinks: $(PERMALINK-FILES.published)

@site:: $(PERMALINK-FILES.published)

# * Publication ---------------------------------------------------------------

@site:: Public

Public:
	rm -f "$@"
	ln -s $(BUILD/BACKEND)/public "$@"

preview: site
	rsync -r --delete $(BUILD/BACKEND)/public/  $(DEPLOY-TO/TESTING:%/=%)

publish: site archive
	case "$(SITE-VERSION)" in *-DIRTY) false;; esac
	test "main" = $$(git branch  | grep '^*' | awk '{print $$2}')
	rsync -r --delete $(BUILD/BACKEND)/public/  $(DEPLOY-TO/PRODUCTION:%/=%)

# TODO: .archive should not be hard coded

archive:
	case "$(SITE-VERSION)" in *-DIRTY) false;; esac
	test "main" = $$(git branch  | grep '^*' | awk '{print $$2}')
	mkdir -p .archive
	rsync -r --delete --exclude ".git" $(BUILD/BACKEND)/public/ .archive
	echo '$(SITE-VERSION)' >.archive/VERSION 
	cd .archive && git add .
	cd .archive && { \
           if test $$(git status -s | wc -l) -ne 0; then  \
              git commit -m 'Archiving version $(SITE-VERSION) at '"$$(date -Is)"; \
           else true; fi; \
        }


