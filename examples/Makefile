SOURCEDIR=../source

HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

BUILDDIR=$(TARGET).noindex

ifneq ($(TARGET),$(HOST))
 GCCPREFIX=$(TARGET)-
 BINLN=
else
 GCCPREFIX=
 BINLN=bin
endif
GCCSUFFIX=
GCC=$(GCCPREFIX)gcc$(GCCSUFFIX)

GFLAGS=
CFLAGS=-ggdb -pipe -gnatef -gnata -gnatwa -gnatyy-3cbhms \
	$(addprefix -gnatec=,$(abspath $(wildcard *.adc)))
MFLAGS=-D $(BUILDDIR) -gnatA
BFLAGS=-E
LFLAGS=-ggdb
HFLAGS=

ifneq ($(TARGET),$(HOST))
 ifneq ($(findstring freebsd,$(TARGET)),)
  LFLAGS+=-lgc -lpthread
 endif
else
 WITH_GC?=
 ifneq ($(WITH_GC),)
  GC_LFLAGS=-L$(WITH_GC)/lib
  GC_HFLAGS=-I$(WITH_GC)/include
 else
  GC_LFLAGS:=$(shell pkg-config --libs-only-L bdw-gc)
  GC_HFLAGS:=$(shell pkg-config --cflags-only-I bdw-gc)
 endif
 LFLAGS+=$(GC_LFLAGS)
 HFLAGS+=$(GC_HFLAGS)
endif

ifneq ($(DRAKE_RTSROOT),)
 VERSION:=$(shell $(GCC) -dumpversion)
 DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
 IMPORTDIR=
 GFLAGS+=--RTS=$(abspath $(DRAKE_RTSDIR))
else
 ifneq ($(wildcard import/$(TARGET)),)
  IMPORTDIR=import/$(TARGET)
 else
  IMPORTDIR=$(BUILDDIR)/import
 endif
 MFLAGS+=-I$(IMPORTDIR)
endif

ifneq ($(GCC),gcc)
 GFLAGS+=$(filter -m32 -m64,$(GCC))
 MFLAGS+=--GCC="$(GCC)"
 LFLAGS+=--GCC="$(GCC)"
 HFLAGS+=-gcc="$(GCC)"
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

.PHONY: all clean $(EXAMPLES)

all: $(EXAMPLES)

$(BUILDDIR)/%: %.adb $(wildcard $(SOURCEDIR)/*) | $(BUILDDIR) $(IMPORTDIR)
	$(GCCPREFIX)gnatmake$(GCCSUFFIX) -c -I$(SOURCEDIR) $< $(GFLAGS) $(MFLAGS) -cargs $(CFLAGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatbind$(GCCSUFFIX) $(basename $(notdir $<)).ali $(GFLAGS) $(BFLAGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatlink$(GCCSUFFIX) -o ../$@ $(basename $(notdir $<)).ali $(GFLAGS) $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): | $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(EXAMPLES): %: $(BUILDDIR)/% $(BINLN)

ifeq ($(IMPORTDIR),$(BUILDDIR)/import)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada $(HFLAGS) -p -D $@ $+
	touch $@
endif

clean:
	-rm -r *.noindex bin
