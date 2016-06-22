PKGNAME = gh
VERSION = 0.10.0
SNAPDIR = $(PKGNAME)-$(VERSION)

PKGDEF    = $(PKGNAME)-pkg.el
AUTODEF   = $(PKGNAME)-auto.el
SPECIAL   = $(PKGDEF) $(AUTODEF)
ALLSOURCE = $(wildcard *.el)

SOURCE  = $(filter-out $(SPECIAL), $(ALLSOURCE))
TARGET  = $(patsubst %.el,%.elc, $(SOURCE))
README  = README.md

EMACS    = emacs
SITEFLAG = --no-site-file
EFLAGS   =
BATCH    = $(EMACS) $(EFLAGS) $(SITEFLAG) -batch -q -L .

PREFIX   = /usr/local
ELISPDIR = $(PREFIX)/share/emacs/site-lisp/$(PKGNAME)

TEXI2HTML = makeinfo --html --number-sections

# Location of Emacs Lisp Package Archive entries
ELPA=../../elpa

all: lisp docs

lisp: $(TARGET)

autoloads: $(AUTODEF)

$(AUTODEF): $(PKGNAME)-auto.in $(SOURCE)
	cp $(PKGNAME)-auto.in $(AUTODEF)
	rm -f $(AUTODEF)c
	@$(BATCH) -l $(PKGNAME)-auto \
		-f gh-generate-autoloads \
		$(shell pwd | sed -e 's|^/cygdrive/\([a-z]\)|\1:|')/$(AUTODEF) .

%.elc: %.el
	@$(BATCH) -f batch-byte-compile $<

clean:
	rm -f *~ $(TARGET) $(PKGNAME).info $(PKGNAME).html

realclean: clean docsclean
	rm -f $(AUTODEF)

install-bin: lisp
	install -d $(ELISPDIR)
	install -m 0644 $(ALLSOURCE) $(TARGET) $(ELISPDIR)

install: install-bin

distclean: clean
	rm -Rf ../$(SNAPDIR)

release: autoloads distclean
	mkdir ../$(SNAPDIR) && chmod 0755 ../$(SNAPDIR)
	cp $(SPECIAL) $(SOURCE) ../$(SNAPDIR)
	(cd .. && tar cjf $(PKGNAME)-$(VERSION).tar.bz2 $(SNAPDIR)/*)

elpa: info
	rm -fR $(ELPA)/$(SNAPDIR)
	rm -f $(ELPA)/$(PKGNAME)-$(VERSION).tar
	mkdir -p $(ELPA)/$(SNAPDIR) && chmod 0755 $(ELPA)/$(SNAPDIR)
	cp $(SOURCE) $(ELPA)/$(SNAPDIR)
	pandoc -f markdown_github -t plain $(README) | tail -n +3 > $(ELPA)/$(SNAPDIR)/README
	cp docs/build/texinfo/$(PKGNAME).info $(ELPA)/$(SNAPDIR)
	sed -e "s/%VERSION%/$(VERSION)/g" < $(PKGDEF) \
		> $(ELPA)/$(SNAPDIR)/$(PKGDEF)
	(cd $(ELPA) && tar cf $(PKGNAME)-$(VERSION).tar $(SNAPDIR))

info:
	$(MAKE) -C docs info

html:
	$(MAKE) -C docs html

docs: info html

docsclean:
	rm -f doc/$(PKGNAME).info doc/$(PKGNAME).html

test: lisp
	@$(BATCH) -l tests/gh-tests.el -l tests/gh-gist-tests.el  \
		-l tests/gh-issues-tests.el -l tests/gh-orgs-tests.el \
		-f ert-run-tests-batch-and-exit
