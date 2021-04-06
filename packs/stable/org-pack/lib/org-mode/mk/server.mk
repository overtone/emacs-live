#----------------------------------------------------------------------
# This file is used for maintenance of org on the server.
#----------------------------------------------------------------------
.PHONY:	helpserver \
	release rel-dirty rel-up cleanrel \
	elpa elpa-dirty elpa-up \
	doc-up \
	upload-release upload-elpa upload-doc upload \
	tagwarn version

help helpall helpserver::
	$(info )
	$(info Maintenance)
	$(info ===========)
	$(info release             - clean up, create the distribution archives)
	$(info elpa                - clean up, create the org-*.tar ELPA archive)
	$(info elpaplus            - clean up, create the org-plus-contrib-*.tar ELPA archive)
	$(info upload-release      - clean up, populate the server with arhives)
	$(info upload-elpa         - clean up, populate the server with org-*.tar)
	$(info upload-elpaplus     - clean up, populate the server with org-plus-contrib-*.tar)
	$(info upload-doc          - clean up, populate the server with docs)
	$(info upload              - clean up, populate the server with everything)

helpserver::
	@echo ""

#----------------------------------------------------------------------

SERVROOT ?= /var/www/orgmode.org
SERVERMK ?= true # or just any value at all, really

#----------------------------------------------------------------------

ORGFULL   = README COPYING lisp/ \
		Makefile request-assign-future.txt \
		mk/default.mk mk/targets.mk mk/version.mk \
		mk/org-fixup.el \
		etc/ contrib/ doc/ testing/
ORGFULL  := $(ORGFULL:%/=%/*)
ORGELPA   = README_ELPA COPYING etc/ORG-NEWS lisp/ \
		doc/dir doc/org doc/orgguide doc/orgcard.pdf \
		etc/styles/ org-pkg.el
ORGELPA  := $(ORGELPA:%/=%/*)
ORGELPAPLUS := $(ORGELPA:org-pkg%=org-plus-contrib-pkg%)

release:	cleanall info pdf card rel-dirty tagwarn
rel-dirty rel-up:	ORGDIR=org-$(GITVERSION:release_%=%)
rel-dirty:
	@$(MAKE) GITVERSION=$(GITVERSION:release_%=%)-dist version autoloads
	-@$(RM) $(ORGDIR) $(ORGTAR) $(ORGRZIP)
	ln -s . $(ORGDIR)
	tar -zcf $(ORGDIR).tar.gz $(foreach dist, $(ORGFULL), $(ORGDIR)/$(dist))
	zip -r9  $(ORGDIR).zip    $(foreach dist, $(ORGFULL), $(ORGDIR)/$(dist))
	-@$(RM) $(ORGDIR)
rel-up:	info pdf card rel-dirty
	$(CP) $(ORGDIR).tar.gz $(ORGDIR).zip $(SERVROOT)/

PKG_TAG = $(shell date +%Y%m%d)
PKG_DOC = "Outline-based notes management and organizer"
PKG_REQ = "" # marmalade chokes on explicit "nil"

elpa:		cleanall info card elpa-dirty
elpa-dirty elpa-up:	ORGDIR=org-$(PKG_TAG)
elpa-dirty:
	@$(MAKE) GITVERSION=$(GITVERSION:release_%=%)-elpa version autoloads
	-@$(RM) $(ORGDIR) $(ORGTAR) $(ORGZIP)
	ln -s . $(ORGDIR)
	echo "(define-package \"org\""                        > org-pkg.el
	echo "  \"$(PKG_TAG)\" \"$(PKG_DOC)\" ($(PKG_REQ)))" >> org-pkg.el
	echo ";; Local Variables:"                           >> org-pkg.el
	echo ";; no-byte-compile: t"                         >> org-pkg.el
	echo ";; End:"                                       >> org-pkg.el
	tar --exclude=Makefile \
	  --transform='s:\(lisp\|doc\)/::' -cf $(ORGDIR).tar \
	  $(foreach dist, $(ORGELPA), $(ORGDIR)/$(dist))
	-@$(RM) $(ORGDIR) org-pkg.el
elpa-up:	info card elpa-dirty archive-contents
	$(CP) archive-contents $(ORGDIR).tar $(SERVROOT)/elpa/

archive-contents:
	echo "(1 (org              . [($(PKG_TAG)) ($(PKG_REQ)) \"$(PKG_DOC)\" tar])"   > $@
	echo "   (org-plus-contrib . [($(PKG_TAG)) ($(PKG_REQ)) \"$(PKG_DOC)\" tar]))" >> $@

elpaplus:		cleanall info card elpaplus-dirty
elpaplus-dirty elpaplus-up:	ORG_ADD_CONTRIB=org*.el ob-*.el ox-*.el ol-*.el
elpaplus-dirty elpaplus-up:	ORGDIR=org-plus-contrib-$(PKG_TAG)
elpaplus-dirty:
	@$(MAKE) GITVERSION=$(GITVERSION:release_%=%)-elpaplus version autoloads
	-@$(RM) $(ORGDIR) $(ORGTAR) $(ORGZIP)
	ln -s . $(ORGDIR)
	echo "(define-package \"org-plus-contrib\""           > org-plus-contrib-pkg.el
	echo "  \"$(PKG_TAG)\" \"$(PKG_DOC)\" ($(PKG_REQ)))" >> org-plus-contrib-pkg.el
	echo ";; Local Variables:"                           >> org-plus-contrib-pkg.el
	echo ";; no-byte-compile: t"                         >> org-plus-contrib-pkg.el
	echo ";; End:"                                       >> org-plus-contrib-pkg.el
	tar --exclude=Makefile \
	  --transform='s:\(lisp\|doc\)/::' -cf $(ORGDIR).tar \
	  $(foreach dist, $(ORGELPAPLUS), $(ORGDIR)/$(dist))
	-@$(RM) $(ORGDIR) org-plus-contrib-pkg.el
	@$(MAKE) cleanlisp
elpaplus-up:	info card elpaplus-dirty archive-contents
	$(CP) archive-contents $(ORGDIR).tar $(SERVROOT)/elpa/

tagwarn:
	$(if $(filter-out $(ORGVERSION), $(GITVERSION)), \
	  $(info  ======================================================) \
	  $(info  =                                                    =) \
	  $(info  = A release should only be made from a revision that =) \
	  $(info  = has an annotated tag!                              =) \
	  $(info  =                                                    =) \
	  $(info  ======================================================))

version:
	@echo ORGVERSION=$(ORGVERSION) GITVERSION=$(GITVERSION)$(ORGDIST)
	@echo "ORGVERSION	?= $(ORGVERSION)"  > mk/version.mk
	@echo "GITVERSION	?= $(GITVERSION)" >> mk/version.mk

cleanall clean:	cleanrel
cleanrel:
	-$(RM) archive-contents org-$(PKG_TAG)* org-$(DISTVERSION)* org-*.zip org-*.tar* mk/version.mk

doc-up:	info pdf card html
	$(MAKE) -C doc manual guide
	$(CP) doc/org.html     $(SERVROOT)
	$(CP) doc/org.pdf      $(SERVROOT)
	$(CP) doc/orgguide.pdf $(SERVROOT)
	$(CP) doc/manual/*     $(SERVROOT)/manual
	$(CP) doc/guide/*      $(SERVROOT)/guide

upload:			cleanall rel-up doc-up elpa-up elpaplus-up
upload-elpa:		cleanall elpa-up
upload-elpaplus:	cleanall elpaplus-up
upload-release:		cleanall rel-up
upload-doc:		cleanall doc-up
