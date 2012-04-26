SOURCEDIR=../source

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
GNATMAKE=gnatmake
GCC=$(dir $(shell which $(GNATMAKE)))gcc
BUILDDIR=build
IMPORTDIR:=import
LFLAGS=$(shell pkg-config --libs bdw-gc)
HFLAGS=$(shell pkg-config --cflags-only-I bdw-gc)
else
GNATMAKE=$(TARGET)-gnatmake
GCC=$(TARGET)-gcc
BUILDDIR=build-$(TARGET)
IMPORTDIR:=import-$(TARGET)
ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS=-lgc -lpthread
endif
endif

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR:=
MFLAGS=--RTS=$(abspath $(DRAKE_RTSDIR))
endif

CFLAGS=-gnata -gnatwa -gnatyy-3chbs

.PHONY: all clean

all: $(BUILDDIR)/test_controlled $(BUILDDIR)/test_gc $(BUILDDIR)/version

$(BUILDDIR)/%: %.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< -largs $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada -gcc=$(GCC) $(HFLAGS) -p -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf build* import*
