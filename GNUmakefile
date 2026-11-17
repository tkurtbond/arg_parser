# Builds and installs the library, without Alire.  gprinstall copies
# whatever lib/ holds without building it, so install builds first: a
# checkout pulled but not rebuilt would otherwise install new sources
# beside .ali files compiled from the old ones, and a program built
# against them could fail to bind ("... must be recompiled").
#
#   make                                           # build
#   make install GPRINSTALL="gprinstall --prefix=DIR"
#   make install BUILD=release                     # the release profile

GPRBUILD   ?= gprbuild
GPRINSTALL ?= gprinstall
BUILD      ?= development

.PHONY: build install uninstall

build:
	$(GPRBUILD) -p -P arg_parser.gpr -XBUILD=$(BUILD)

install: build
	$(GPRINSTALL) -f -p -P arg_parser.gpr -XBUILD=$(BUILD)

uninstall:
	$(GPRINSTALL) --uninstall -P arg_parser.gpr
