#GHC     = /opt/ghc-6.4/bin/powerpc-unknown-linux/ghc
#HSC2HS  = /opt/ghc-6.4/bin/powerpc-unknown-linux/hsc2hs
GHC = ghc
HSC2HS = hsc2hs
LIBRARY_DOC_DIR = /opt/ghc-6.4/share/html/libraries

HFLAGS = -fffi -fglasgow-exts -package unix -threaded -debug -ddump-minimal-imports
LDFLAGS = -lfuse

all: HelloFS BindFS

BindFS: BindFS.hs hfuse.conf
	$(GHC) $(HFLAGS) -package-conf hfuse.conf -o $@ \
		-package hfuse --make BindFS.hs $(LDFLAGS)

HelloFS: HelloFS.hs hfuse.conf
	$(GHC) $(HFLAGS) -package-conf hfuse.conf -o $@ \
		 -package hfuse --make HelloFS.hs $(LDFLAGS)

HFuse.o: HFuse.hs
	$(GHC) $(HFLAGS) -package-name hfuse -c HFuse.hs

HFuse.hs: HFuse.hsc
	$(HSC2HS) HFuse.hsc

libHSfuse.a: HFuse.o
	ar cqs libHSfuse.a HFuse.o

hfuse.conf: hfuse.conf.in libHSfuse.a
	echo "[]" > hfuse.conf
	PREFIX=`pwd` ghc-pkg -f hfuse.conf -u < hfuse.conf.in

doc: HFuse.hs
	mkdir -p doc
	haddock --html --odir=doc \
		--read-interface=$(LIBRARY_DOC_DIR)/base,$(LIBRARY_DOC_DIR)/base/base.haddock \
		--read-interface=$(LIBRARY_DOC_DIR)/unix,$(LIBRARY_DOC_DIR)/unix/unix.haddock \
		HFuse.hs

clean:
	rm -f *~ \#*
	rm -f *.o *.hi
	rm -f HFuse.hs HFuse.hsc.ctypes HFuse_stub.*
	rm -f *.imports

distclean: clean
	rm -f libHSfuse.a
	rm -f HelloFS BindFS
	rm -f hfuse.conf
	rm -rf doc

.PHONY: doc clean distclean
