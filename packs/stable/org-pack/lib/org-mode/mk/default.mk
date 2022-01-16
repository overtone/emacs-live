##----------------------------------------------------------------------
##  NEVER EDIT THIS FILE, PUT ANY ADAPTATIONS INTO local.mk
##-8<-------------------------------------------------------------------
##  CHECK AND ADAPT THE FOLLOWING DEFINITIONS
##----------------------------------------------------------------------

# Name of your emacs binary
EMACS	= emacs

# Where local software is found
prefix	= /usr/share

# Where local lisp files go.
lispdir= $(prefix)/emacs/site-lisp/org

# Where local data files go.
datadir = $(prefix)/emacs/etc/org

# Where info files go.
infodir = $(prefix)/info

# Define if you only need info documentation, the default includes html and pdf
#ORG_MAKE_DOC = info # html pdf

# Define which git branch to switch to during update.  Does not switch
# the branch when undefined.
GIT_BRANCH =

# Where to create temporary files for the testsuite
# respect TMPDIR if it is already defined in the environment
TMPDIR ?= /tmp
testdir = $(TMPDIR)/tmp-orgtest

# Configuration for testing
# add options before standard load-path
BTEST_PRE   =
# add options after standard load path
BTEST_POST  =
              # -L <path-to>/ert      # needed for Emacs23, Emacs24 has ert built in
              # -L <path-to>/ess      # needed for running R tests
              # -L <path-to>/htmlize  # need at least version 1.34 for source code formatting
BTEST_OB_LANGUAGES = awk C fortran maxima lilypond octave perl python
              # R                     # requires ESS to be installed and configured
              # ruby                  # requires inf-ruby to be installed and configured
# extra packages to require for testing
BTEST_EXTRA =
              # ess-site  # load ESS for R tests
##->8-------------------------------------------------------------------
## YOU MAY NEED TO ADAPT THESE DEFINITIONS
##----------------------------------------------------------------------

# How to run tests
req-ob-lang = --eval '(require '"'"'ob-$(ob-lang))'
lst-ob-lang = ($(ob-lang) . t)
req-extra   = --eval '(require '"'"'$(req))'
BTEST_RE   ?= \\(org\\|ob\\)
BTEST_LOAD  = \
	--eval '(add-to-list '"'"'load-path (concat default-directory "lisp"))' \
	--eval '(add-to-list '"'"'load-path (concat default-directory "testing"))'
BTEST_INIT  = $(BTEST_PRE) $(BTEST_LOAD) $(BTEST_POST)

BTEST = $(BATCH) $(BTEST_INIT) \
	  -l org-batch-test-init \
	  --eval '(setq \
		org-batch-test t \
		org-babel-load-languages \
		  (quote ($(foreach ob-lang,\
				$(BTEST_OB_LANGUAGES) emacs-lisp shell org,\
				$(lst-ob-lang)))) \
		org-test-select-re "$(BTEST_RE)" \
		)' \
	  -l org-loaddefs.el \
	  -l cl -l testing/org-test.el \
	  -l ert -l org -l ox -l ol \
	  $(foreach req,$(BTEST_EXTRA),$(req-extra)) \
	  --eval '(org-test-run-batch-tests org-test-select-re)'

# Running a plain emacs with no config and this Org mode loaded.  This
# should be useful for manual testing and verification of problems.
NOBATCH = $(EMACSQ) $(BTEST_INIT) -l org -f org-version

# start Emacs with no user and site configuration
# EMACSQ = -vanilla # XEmacs
EMACSQ  = $(EMACS)  -Q

# Using emacs in batch mode.
BATCH	= $(EMACSQ) -batch \
	  --eval '(setq vc-handled-backends nil org-startup-folded nil org-element-cache-persistent nil)'

# Emacs must be started in toplevel directory
BATCHO	= $(BATCH) \
	  --eval '(add-to-list '"'"'load-path "./lisp")'

# How to generate local.mk
MAKE_LOCAL_MK = $(BATCHO) \
	  --eval '(load "org-compat.el")' \
	  --eval '(load "../mk/org-fixup.el")' \
	  --eval '(org-make-local-mk)'

# Emacs must be started in lisp directory
BATCHL	= $(BATCH) \
	  --eval '(add-to-list '"'"'load-path ".")'

# How to generate org-loaddefs.el
MAKE_ORG_INSTALL = $(BATCHL) \
	  --eval '(load "org-compat.el")' \
	  --eval '(load "../mk/org-fixup.el")' \
	  --eval '(org-make-org-loaddefs)'

# How to generate org-version.el
MAKE_ORG_VERSION = $(BATCHL) \
	  --eval '(load "org-compat.el")' \
	  --eval '(load "../mk/org-fixup.el")' \
	  --eval '(org-make-org-version "$(ORGVERSION)" "$(GITVERSION)")'

# How to byte-compile the whole source directory
ELCDIR	= $(BATCHL) \
	  --eval '(batch-byte-recompile-directory 0)'

# How to byte-compile a single file
ELC	= $(BATCHL) \
	  --eval '(batch-byte-compile)'

# How to make a pdf file from a texinfo file
TEXI2PDF = texi2pdf --batch --clean --expand

# How to make a pdf file from a tex file
PDFTEX = pdftex

# How to create directories with leading path components
# MKDIR	= mkdir -m 755 -p # try this if you have no install
MKDIR	= install -m 755 -d

# How to create the info files from the texinfo file
MAKEINFO = makeinfo

# How to create the HTML file
TEXI2HTML = makeinfo --html --number-sections --css-ref "https://www.gnu.org/software/emacs/manual.css"

# How to find files
FIND	= find

# How to remove files
RM	= rm -f

# How to remove files recursively
RMR	= rm -fr

# How to change file permissions
# currently only needed for git-annex due to its "lockdown" feature
CHMOD   = chmod

# How to copy the lisp files and elc files to their destination.
# CP	= cp -p	# try this if you have no install
CP	= install -m 644 -p

# How to obtain administrative privileges
# leave blank if you don't need this
# SUDO	=
SUDO	= sudo

# Name of the program to install info files
# INSTALL_INFO = ginstall-info # Debian: avoid harmless warning message
INSTALL_INFO = install-info

# target method for 'compile'
ORGCM	= dirall
# ORGCM	= dirall #   1x slowdown compared to default compilation method
# ORGCM	= single #   4x one Emacs process per compilation
# ORGCM	= source #   5x ditto, but remove compiled file immediately
# ORGCM	= slint1 #   3x possibly elicit more warnings
# ORGCM	= slint2 #   7x possibly elicit even more warnings
