EMACS  ?= emacs
EFLAGS ?=
BATCH   = $(EMACS) $(EFLAGS) -batch -Q
BATCHE  = $(BATCH) -eval
BATCHC  = $(BATCH) -L . $(LOADPATH) -f batch-byte-compile

ELS  = auto-compile.el
ELCS = $(ELS:.el=.elc)

LOADPATH ?= -L ../packed

lisp: $(ELCS)
%.elc: %.el
	@$(BATCHC) $<

.PHONY: clean
clean:
	@echo "Cleaning..."
	@rm -f $(ELCS)

.PHONY: README.md
README.md:
	@echo "Generating README.md..."
	@$(BATCHE) "\
(let (start end commentary)\
  (with-temp-buffer\
    (insert-file \"auto-compile.el\")\
    (re-search-forward \"^;;; Commentary:\n\n\")\
    (setq start (point))\
    (re-search-forward \"^;;; Code:\")\
    (forward-line -1)\
    (setq end (point-marker))\
    (replace-regexp \"^;; ?\" \"\"  nil start end)\
    (replace-regexp \"^- \" \"* \"  nil start end)\
    (replace-regexp \"\\\\(\`[^']+\\\\)'\" \"\\\\1\`\" nil start end)\
    (setq commentary (buffer-substring start end)))\
  (with-current-buffer (find-file-noselect \"README.md\")\
    (erase-buffer)\
    (insert \"Automatically compile Emacs Lisp libraries\n\")\
    (insert \"------------------------------------------\n\n\")\
    (insert commentary ?\n)\
    (save-buffer)))"
