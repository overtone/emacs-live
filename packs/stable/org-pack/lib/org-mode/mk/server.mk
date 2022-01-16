#----------------------------------------------------------------
# This file is used to upload the Org documentation to the server
#----------------------------------------------------------------
.PHONY:	helpserver \
	doc-up \
	upload \
	tagwarn version

help helpserver::
	$(info )
	$(info Maintenance)
	$(info ===========)
	$(info upload              - clean up, populate the server with documentation)

helpserver::
	@echo ""

#----------------------------------------------------------------------

SERVROOT ?= upload
SERVERMK ?= true # or just any value at all, really

#----------------------------------------------------------------------

release:	cleanall info pdf card tagwarn

PKG_TAG = $(shell date +%Y%m%d)
PKG_DOC = "Outline-based notes management and organizer"
PKG_REQ = "" # marmalade chokes on explicit "nil"

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

doc-up:	info pdf card html
	$(MAKE) -C doc manual guide
	$(CP) doc/org.html      $(SERVROOT)
	$(CP) doc/org.pdf       $(SERVROOT)
	$(CP) doc/orgguide.html $(SERVROOT)
	$(CP) doc/orgguide.pdf  $(SERVROOT)
	$(CP) doc/manual/*      $(SERVROOT)/manual
	$(CP) doc/guide/*       $(SERVROOT)/guide

upload:			cleanall doc-up
