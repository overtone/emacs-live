If you have `linkd.el' turn on `linkd-mode'
and (setq linkd-use-icons t ) more easily navigation.
You can get `linkd.el' here:
 http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el
 http://www.emacswiki.org/emacs/linkd.tar.gz -- with cool icon


(@* "Index" )

(@> "What's this")        I am ...
(@> "Setup")              Basic setup
(@> "Screencast")         Screencast
(@> "Mode map")           Key binding

(@> "Note")               Performance note

(@> "Custom variable")    Customizable varible
(@> "Face")               Face used in auto-highlight-symbol-mode
(@> "Highlight Rules")    Whether to highlight the symbol.

(@> "Internal variable")  Internal variables
(@> "Logging")            Log data
(@> "Range plugin")       Range plugin functions
(@> "Built-in plugin")    Built-in plugin section
(@> "Timer")              Timer functions
(@> "Idle")               Idle functions
(@> "Highlight")          Highlight functions
(@> "Edit mode")          Edit mode functions
(@> "Select")             Selective functions
(@> "Misc")               Miscellaneous
(@> "Interactive")        Interactive functions
(@> "Define mode")        Mode definition
(@> "Revert")             Protect from revert-buffer


(@* "What's this" )

 A minor mode for emacs.

  * automatic highlighting current symbol like eclipse IDE.
  * cycle through highlighted locations.
  * can specify the range to highlight.
  * can edit the highlighted symbols at a time.

 Tested on GNU Emacs 22.3/23.2/24.0.50/24.2


(@* "Setup" )

Basic steps to setup:
  1. Place `auto-highlight-symbol.el' in your `load-path'.

  2. In your `.emacs.el' file
     (require 'auto-highlight-symbol)
     (global-auto-highlight-symbol-mode t)


(@* "Screencast" )

 Screencast on YouTube and ScreenToaster
   YouTube -- http://www.youtube.com/watch?v=xzJ2r4-s7fo
   ScreenToaster -- http://www.screentoaster.com/watch/stUE9VQ0dMRFtXRlVeU19cX1Bd/auto_highlight_symbol_mode_screencast


More Information (currently underconstruction)
  See also http://github.com/mitsuo-saito/auto-highlight-symbol-mode/wiki/
