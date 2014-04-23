SOURCEDIR=../source

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

VERSION=$(shell gcc -dumpversion)

BUILDDIR=$(TARGET).build

ifeq ($(TARGET),$(HOST))
GNATMAKE=gnatmake
GCC=$(dir $(shell which $(GNATMAKE)))gcc
BINLN=bin
LFLAGS=$(shell pkg-config --libs bdw-gc)
HFLAGS=$(shell pkg-config --cflags-only-I bdw-gc)
else
GNATMAKE=$(TARGET)-gnatmake
GCC=$(TARGET)-gcc
BINLN=
ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS=-lgc -lpthread
endif
endif

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR=
MFLAGS=--RTS=$(abspath $(DRAKE_RTSDIR))
else
IMPORTDIR=$(BUILDDIR)/import
MFLAGS=-I$(abspath $(IMPORTDIR))
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

.PHONY: all clean $(EXAMPLES)

all: $(BUILDDIR)/test_controlled $(BUILDDIR)/test_gc $(BUILDDIR)/version $(BINLN)

$(BUILDDIR)/%: %.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) $(CFLAGS) ../$< -largs $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(EXAMPLES): %: $(BUILDDIR)/%

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada -gcc=$(GCC) $(HFLAGS) -p -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf *.build bin
