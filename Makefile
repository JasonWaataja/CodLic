INSTALL_PREFIX ?= /usr/local
INSTALL_BIN = ${INSTALL_PREFIX}/bin

# Binary name
TARGET = codlic

# Quicklisp system to load
QL_SYSTEM = codlic

#StartCut
# Flags for manifest build
MANIFEST_FLAGS =  --no-sysinit
MANIFEST_FLAGS += --no-userinit
MANIFEST_FLAGS += --load $(SRCDIR)/prep-quicklisp.lisp
MANIFEST_FLAGS += --eval '(ql:quickload :qlot)'
MANIFEST_FLAGS += --eval '(qlot:install :$(QL_SYSTEM))'
MANIFEST_FLAGS += --eval '(qlot:quickload :$(QL_SYSTEM))'
MANIFEST_FLAGS += --eval '(qlot:with-local-quicklisp (:$(QL_SYSTEM)) (ql:write-asdf-manifest-file \#P"$(MANIFEST)" :if-exists :supersede :exclude-local-projects nil))'
MANIFEST_FLAGS += --eval '(uiop:quit)'
#EndCut

# Buildapp settings
B_FLAGS =  --output $(OUTDIR)/$(TARGET)
B_FLAGS += --manifest-file $(MANIFEST)
B_FLAGS += --asdf-path $(CURDIR)/
B_FLAGS += --load-system $(QL_SYSTEM)
B_FLAGS += --entry $(QL_SYSTEM):main

# Location: Source files
SRCDIR = $(CURDIR)/src

# Location: Build output
OUTDIR = $(CURDIR)/builds

# Location: Manifest
MANIFEST = quicklisp-manifest.txt

all: build_manifest build_app

build_manifest:
	-mkdir $(OUTDIR)
	$(LISP) $(MANIFEST_FLAGS)

build_app:
	$(BUILDAPP) $(B_FLAGS)

.PHONY: clean
clean:
	-rm $(OUTDIR)/*
	-rm $(MANIFEST) $(BUILDLOG)

# Applications
SHELL = /bin/sh
BUILDAPP = buildapp
LISP = sbcl

install:
	mkdir -p ${INSTALL_BIN}
	mkdir -p ${INSTALL_PREFIX}/share/codlic/licenses
	install -Dm755 ${OUTDIR}/${TARGET} ${INSTALL_BIN}/${TARGET}
	install -Dm755 licenses/gplv3 ${INSTALL_PREFIX}/share/codlic/licenses/gplv3
	install -Dm755 licenses/mit ${INSTALL_PREFIX}/share/codlic/licenses/mit
	install -Dm644 man/codlic.1 ${INSTALL_PREFIX}/share/man/man1
