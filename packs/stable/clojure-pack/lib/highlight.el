;;; highlight.el --- Highlighting commands.
;;
;; Filename: highlight.el
;; Description: Highlighting commands.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1995-2010, Drew Adams, all rights reserved.
;; Created: Wed Oct 11 15:07:46 1995
;; Version: 21.0
;; Last-Updated: Fri Jan 15 13:18:50 2010 (-0800)
;;           By: dradams
;;     Update #: 2540
;; URL: http://www.emacswiki.org/cgi-bin/wiki/highlight.el
;; Keywords: faces, help, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `faces', `faces+', `fit-frame',
;;   `frame-fns', `help+20', `info', `info+', `menu-bar',
;;   `menu-bar+', `misc-cmds', `misc-fns', `second-sel', `strings',
;;   `thingatpt', `thingatpt+', `unaccent', `w32browser-dlgopen',
;;   `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Highlighting commands.
;;
;;    More description below.

;;(@> "Index")
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Things Defined Here")
;;  (@> "Documentation")
;;    (@* "Library `facemenu+.el' Puts Highlight on the Menu")
;;    (@* "User Option `hlt-use-overlays-flag'")
;;    (@* "Commands")
;;    (@* "User Option `hlt-act-on-any-face-flag'")
;;    (@* "Hiding and Showing Text")
;;    (@* "What Gets Highlighted: Region, Buffer, New Text You Type")
;;    (@* "Interference by Font Lock")
;;    (@* "Suggested Bindings")
;;    (@* "Relation to Hi-Lock Mode")
;;    (@* "Commands That Won't Work in Emacs 20")
;;    (@* "To Do")
;;  (@> "Change log")
;;  (@> "Menu-Bar Region Menu")
;;  (@> "Variables and Faces")
;;  (@> "Misc Functions - Emacs 20+")
;;  (@> "Misc Functions - Emacs 21+")
;;  (@> "Functions for Use with Icicles - Emacs 21+")
;;  (@> "Functions for Highlighting Propertized Text - Emacs 21+")
;;  (@> "General functions")

;;(@* "Things Defined Here")
;;
;;  Things Defined Here
;;  -------------------
;;
;;  Commands defined here:
;;
;;    `hlt-choose-default-face', `hlt-choose-faces',
;;    `hlt-choose-invisible-faces', `hlt-choose-visible-faces',
;;    `hlt-eraser', `hlt-eraser-mouse', `hlt-hide',
;;    `hlt-hide-default-face', `hlt-hide-only', `hlt-highlight',
;;    `hlt-highlight-all-prop', `hlt-highlighter',
;;    `hlt-highlighter-mouse', `hlt-highlight-property-with-value',
;;    `hlt-highlight-regexp-region', `hlt-highlight-regexp-to-end',
;;    `hlt-highlight-region', `hlt-highlight-single-quotations',
;;    `hlt-mouse-face-each-line', `hlt-next-highlight',
;;    `hlt-previous-highlight', `hlt-replace-highlight-face',
;;    `hlt-show', `hlt-show-default-face', `hlt-show-only',
;;    `hlt-toggle-act-on-any-face-flag',
;;    `hlt-toggle-link-highlighting',
;;    `hlt-toggle-property-highlighting',
;;    `hlt-toggle-use-overlays-flag', `hlt-unhighlight-all-prop',
;;    `hlt-unhighlight-region', `hlt-unhighlight-region-for-face'.
;;
;;  User options (variables) defined here:
;;
;;    `hlt-act-on-any-face-flag', `hlt-max-region-no-warning',
;;    `hlt-use-overlays-flag'.
;;
;;  Faces defined here:
;;
;;    `hlt-property-highlight', `minibuffer-prompt' (for Emacs 20).
;;
;;  Non-interactive functions defined here:
;;
;;    `hlt-add-listifying', `hlt-add-to-invisibility-spec',
;;    `hlt-delete-highlight-overlay', `hlt-highlight-faces-in-buffer',
;;    `hlt-flat-list', `hlt-highlight-faces-in-buffer',
;;    `hlt-listify-invisibility-spec',
;;    `hlt-mouse-toggle-link-highlighting',
;;    `hlt-mouse-toggle-property-highlighting',
;;    `hlt-region-or-buffer-limits', `hlt-set-intersection',
;;    `hlt-set-union'.
;;
;;  Internal variables defined here:
;;
;;    `hlt-last-face', `hlt-last-regexp',
;;    `hlt-previous-use-overlays-flag-value',
;;    `hlt-prop-highlighting-state'.

;;(@* "Documentation")
;;
;;  Documentation
;;  -------------
;;
;;(@* "Library `facemenu+.el' Puts Highlight on the Menu")
;;  ** Library `facemenu+.el'  Puts Highlight on the Menu **
;;
;;  If you load library `facemenu+.el' after you load library
;;  `highlight.el', then the commands defined here will also be
;;  available on a Highlight submenu in the Text Properties menus.
;;
;;(@* "User Option `hlt-use-overlays-flag'")
;;  ** User Option `hlt-use-overlays-flag'
;;
;;  You can highlight text in two ways using this library, depending
;;  on the value of user option `hlt-use-overlays-flag':
;;
;;   - non-nil means to highlight using overlays
;;   - nil means to highlight using text properties
;;
;;  Overlays are independent from the text itself.  They are not
;;  picked up when you copy and paste text.  By default, highlighting
;;  uses overlays.
;;
;;  Although highlighting recognizes only nil and non-nil values for
;;  `hlt-use-overlays-flag', other actions can have different
;;  behavior, depending on the non-nil value.  If it is `only' (the
;;  default value), then only overlay highlighting is affected.  If it
;;  is any other non-nil value, then both overlay highlighting and
;;  text-property highlighting are effected.  This is the case, for
;;  instance, for unhighlighting and for navigating among highlights.
;;
;;  For example, for unhighlighting, if `hlt-use-overlays-flag' is
;;  non-nil, then overlay highlighting is removed.  If it is not
;;  `only', then text-property highlighting is removed.  A value of
;;  nil thus removes both overlays and text properties.
;;
;;  Keep this sensitivity to the value of `hlt-use-overlays-flag' in
;;  mind.  For example, if you change the value after adding some
;;  highlighting, then that highlighting might not be removed by
;;  unhighlighting, unless you change the value back again.
;;
;;  You can toggle the value of `hlt-use-overlays-flag' at any time
;;  between nil and its previous non-nil value, using command
;;  `hlt-toggle-use-overlays-flag'.
;;
;;(@* "Commands")
;;  ** Commands **
;;
;;  You can use any face to highlight, and you can apply a mouse face
;;  instead of a face, if you like.  A mouse face shows up only when
;;  the mouse pointer is over it.
;;
;;  The commands you will use the most often are probably
;;  `hlt-highlight', `hlt-highlighter', `hlt-next-highlight', and
;;  `hlt-previous-highlight'.  You might also often use the various
;;  commands to hide and show highlighted text.
;;
;;  You can use command `hlt-highlight' to highlight the region,
;;  highlight a regexp throughout the region, or unhighlight the
;;  region, depending on the prefix argument.  It combines most of the
;;  behavior of commands `hlt-highlight-regexp-region',
;;  `hlt-highlight-region', and `hlt-unhighlight-region'.  Command
;;  `hlt-highlight-regexp-to-end' highlights a regexp from the text
;;  cursor position to the end of the buffer.
;;
;;  Command `hlt-highlighter' lets you highlight text by simply
;;  dragging the mouse, just as you would use a highlighter (marker).
;;  You can thus highlight text the same way that you drag the mouse
;;  to define the region.
;;
;;  If you use Emacs 21 or later, you can use various commands that
;;  highlight and unhighlight text that has certain text properties
;;  with given values.  You can use them to highlight all text in the
;;  region or buffer that has a given property value.  An example is
;;  highlighting all links (text with property `mouse-face').  These
;;  commands are:
;;
;;  `hlt-highlight-all-prop' - Highlight text that has a given
;;                             property with any (non-nil) value.
;;
;;  `hlt-highlight-property-with-value' - Highlight text that has a
;;                             given property with certain values.
;;
;;  `hlt-unhighlight-all-prop' - Unhighlight highlighted propertized
;;                             text.
;;
;;  `hlt-mouse-toggle-link-highlighting' - Alternately highlight and
;;                             unhighlight links on a mouse click.
;;
;;  `hlt-toggle-link-highlighting' - Alternately highlight and
;;                             unhighlight links.
;;
;;  `hlt-mouse-toggle-property-highlighting' - Alternately highlight
;;                             and unhighlight propertized text on a
;;                             mouse click.
;;
;;  `hlt-toggle-property-highlighting' - Alternately highlight and
;;                             unhighlight propertized text.
;;
;;  As always for library `highlight.el', this "highlighting" can use
;;  property `mouse-face' instead of `face'.  You could, for example,
;;  highlight, using `mouse-face', all text that has property `foo' -
;;  or that has property `face', for that matter.
;;
;;  If you use Emacs 21 or later, you can use commands
;;  `hlt-next-highlight' and `hlt-previous-highlight' to navigate
;;  among highlights of a given face.
;;
;;  You can unhighlight the region using command
;;  `hlt-unhighlight-region' (or using `C--' with `hlt-highlight').
;;  If you use overlay highlighting, then you can use command
;;  `hlt-unhighlight-region-for-face' to unhighlight the region for an
;;  individual highlighting face - other highlighting faces remain.
;;
;;  You can replace a highlighting face in the region by another,
;;  using command `hlt-replace-highlight-face'.  With a prefix
;;  argument, property `mouse-face' is used, not property `face'.
;;
;;  Command `hlt-eraser' lets you delete highlighting by dragging the
;;  mouse.  However, its behavior is quite different for overlays and
;;  text properties, and it is perhaps different from you expect - see
;;  the `hlt-eraser' doc string.
;;
;;(@* "User Option `hlt-act-on-any-face-flag'")
;;  ** User Option `hlt-act-on-any-face-flag' **
;;
;;  Library `highlight' generally acts only on faces that it controls,
;;  that is, faces that you have explicitly asked it to use for
;;  highlighting.  It sets the text property or overlay property
;;  `hlt-highlight' on such highlighted text, so that it can recognize
;;  which faces it has responsibility for.
;;
;;  Sometimes, you might want to hide and show text other than that
;;  controlled by library `highlight'.  Similarly, you might sometimes
;;  want to navigate among faces other than those used for
;;  highlighting.  You can control this using option
;;  `hlt-act-on-any-face-flag', which you can toggle at any time using
;;  command `hlt-toggle-act-on-any-face-flag'.
;;
;;(@* "Hiding and Showing Text")
;;  ** Hiding and Showing Text **
;;
;;  You can hide and show text that you have highlighted.  You will
;;  want to read the Emacs-Lisp manual (Elisp), section Invisible
;;  Text, to understand better what this entails.  In particular, you
;;  should understand that for library `highlight.el', hiding text
;;  means adding the symbol naming the face to be hidden to both:
;;
;;  1. a text or overlay `invisible' property, making the text or
;;     overlay susceptible to being hidden by buffer-local variable
;;     `buffer-invisibility-spec', and
;;
;;  2. the buffer's `buffer-invisibility-spec', so that it in fact
;;     becomes hidden.
;;
;;  After text has been hidden this way, and unless the highlighting
;;  as been removed completely by unhighlighting the text, the
;;  `invisible' property of that text keeps the names of the faces
;;  that have been applied to that text and hidden previously, even
;;  after you show that text again.  Showing a hidden face simply
;;  removes it from the `buffer-invisibility-spec'; it does not change
;;  any `invisible' properties.
;;
;;  For example, if you hide face `foo' at some buffer position:
;;
;;  1. The `invisible' property of the text or overlay at that
;;     position is updated to include `foo'.  If there are no other
;;     faces that have been applied to this text and then hidden, the
;;     `invisible' property is just (`foo').
;;
;;  2. `buffer-invisibility-spec' is also updated to include `foo'.
;;     This hides all text properties and overlay properties with
;;     `invisible' property `foo', throughout the buffer.  If there
;;     are no other invisible faces in the buffer, then
;;     `buffer-invisibility-spec' has value (foo).
;;
;;  If you then show face `foo' at that same buffer position, there is
;;  no change to the `invisible' property.  `buffer-invisibility-spec'
;;  is updated, by removing `foo': if it was (foo), it becomes ().
;;
;;  There are several commands for hiding and showing highlighted
;;  text.  The basic commands for hiding and showing are
;;  `hlt-hide-default-face' and `hlt-show-default-face', which you can
;;  use to hide and show the face last used for highlighting.  With a
;;  prefix argument, you are prompted for a different face to hide; it
;;  then becomes the default face for highlighting.  You can also
;;  change the default highlighting face at any time using command
;;  `hlt-choose-default-face'.
;;
;;  The other hide and show commands depend on your also using
;;  Icicles, which is a set of libraries that offer enhanced
;;  completion.  The Icicles-dependent commands are the following:
;;
;;  `hlt-choose-faces', `hlt-choose-invisible-faces',
;;  `hlt-choose-visible-faces', `hlt-hide', `hlt-hide-only',
;;  `hlt-show', `hlt-show-only'.
;;
;;  These are all multi-commands, which means that they each let you
;;  choose multiple completion candidates or all candidates that match
;;  your current input (a regexp).  You can use command `hlt-hide' to
;;  hide any number of visible faces.  Any text is hidden that has
;;  that face as a text property or an overlay property, depending on
;;  the value of `hlt-use-overlays-flag'.
;;
;;  Command `hlt-show' is the opposite of `hlt-hide': it shows
;;  invisible text that has the faces you choose.  Neither `hlt-hide'
;;  nor `hlt-show' has any effect on other faces, besides those you
;;  choose to hide or show, respectively; they each do only one thing,
;;  hide or show.
;;
;;  Command `hlt-hide-only' hides the faces you choose, and shows all
;;  other faces, and command `hlt-show-only' does the opposite.  You
;;  can thus use these commands to specify exactly what faces should
;;  be invisible and visible.  Empty input means none: If you choose
;;  no faces to hide (that is, hit `RET' with an empty minibuffer),
;;  then all faces will be made visible; if you choose no faces to
;;  show, then all will be hidden.
;;
;;  Currently, face attributes for highlighting are combined when
;;  overlays overlap, but the same is not true for text properties.
;;  For example, if you highlight a word with face `foo', and then you
;;  highlight it with face `bar', only `bar' remains as the face for
;;  that word.  With overlays, the attributes of the two faces are
;;  composed.  When you hide or show faces, this behavior difference
;;  has an effect.
;;
;;  You can hide text using the commands in this library for any of
;;  the purposes that you might use invisible text in Emacs.  This
;;  gives you an easy, interactive way to control which sections of
;;  text are seen by search and other Emacs tools.  Use the regexp
;;  highlighting commands, for instance, to highlight text
;;  syntactically, and then hide that highlighted text.  Or use
;;  `hlt-highlighter' to sweep over text that you want to hide with
;;  the mouse.
;;
;;  Hiding and showing faces also provides a "conditional text"
;;  feature similar to that available in desktop publishing
;;  applications such as Adobe's Framemaker.  Publishers often use
;;  such a feature to produce different output documents from the same
;;  source document ("single sourcing").  You can use this feature
;;  similarly, if you have an application (printing is one example)
;;  that is sensitive to whether text is visible or invisible.  One
;;  caveat: Emacs faces are not saved when you save your file.
;;
;;(@* "What Gets Highlighted: Region, Buffer, New Text You Type")
;;  ** What Gets Highlighted: Region, Buffer, New Text You Type **
;;
;;  All mention of the "region" in this commentary should really say
;;  "region or buffer".  If the region is active and non-empty, then
;;  only the text in the region is targeted by the commands in this
;;  library.  This lets you easily control the scope of operations.
;;
;;  If the region is not active or it is empty, then:
;;
;;  - If `hlt-use-overlays-flag' is nil and there is no prefix arg,
;;    then the face is applied to the next characters that you type.
;;
;;  - Otherwise, the face is applied to the entire buffer (or the
;;    current restriction, if the buffer is narrowed).
;;
;;(@* "Interference by Font Lock")
;;  ** Interference by Font Lock **
;;
;;  If you use Emacs 22 or later, then you can use this library in
;;  conjunction with library `font-lock+.el'.  That will prevent
;;  font-locking from removing any highlighting face properties that
;;  you apply using the commands defined here.
;;
;;  Otherwise, when `hlt-use-overlays-flag' is nil, font-lock
;;  highlighting will interfere with the highlighting of this library.
;;  In most cases, you will be able to highlight text, but sooner or
;;  later font-lock will erase that highlighting when it refontifies
;;  the buffer.  If `hlt-use-overlays-flag' is non-nil, there is no
;;  such problem : font-lock has no effect on overlays.
;;
;;(@* "Suggested Bindings")
;;  ** Suggested Bindings **
;;
;;  This library adds menu items to the Region submenu of the Edit
;;  menu-bar menu, if you have a Region submenu.  To obtain this menu,
;;  load library `menu-bar+.el'.
;;
;;  Otherwise, library `highlight.el' makes no key bindings.  Here are
;;  some suggested bindings (`C-x y', `C-x mouse-2', `C-x S-mouse-2',
;;  `S-C-p', and `S-C-n'):
;;
;;   (define-key ctl-x-map [(control ?y)] 'hlt-highlight)
;;   (define-key ctl-x-map [(down-mouse-2)] 'hlt-highlighter)
;;   (define-key ctl-x-map [(S-down-mouse-2)] 'hlt-eraser)
;;   (global-set-key [(shift control ?p)]  ; Emacs 21 or later
;;                   'hlt-previous-highlight)
;;   (global-set-key [(shift control ?n)]  ; Emacs 21 or later
;;                   'hlt-next-highlight)
;;
;;  You might also want to bind `hlt-choose-default-face', which you
;;  can use to change the current default highlighting face.
;;
;;(@* "Relation to Hi-Lock Mode")
;;  ** Relation to Hi-Lock Mode **
;;
;;  The features of this library are complementary to those of the
;;  standard Emacs library `hi-lock.el', so you can use the two
;;  libraries together.
;;
;;(@* "Commands That Won't Work in Emacs 20")
;;  ** Commands That Won't Work in Emacs 20 **
;;
;;  The following commands and options work only for Emacs versions
;;  more recent than Emacs 20:
;;
;;  `hlt-act-on-any-face-flag', `hlt-choose-faces',
;;  `hlt-choose-invisible-faces', `hlt-choose-visible-faces',
;;  `hlt-hide', `hlt-hide-default-face', `hlt-hide-only',
;;  `hlt-highlight-property-with-value', `hlt-next-highlight',
;;  `hlt-previous-highlight', `hlt-show', `hlt-show-default-face',
;;  `hlt-show-only', `hlt-toggle-act-on-any-face-flag'.
;;
;;(@* "To Do")
;;  ** To Do **
;;
;;  1. Add commands to show and hide boolean combinations of faces.
;;
;;  2. Faces are not accumulated as text properties.
;;     Highlighting with one face completely replaces the previous
;;     highlight.  Overlays don't have this limitation.  Text
;;     properties need not have it either, but they do, for now.
;;
;;(@* "Acknowledgement")
;;  **  Acknowledgement **
;;
;;  Parts of this library are based on a library of the same name
;;  written and copyrighted by Dave Brennan, brennan@hal.com, in 1992.
;;  I haven't been able to locate that file, so my change log is the
;;  only record I have of what our relative contributions are.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;(@* "Change log")
;;
;; 2009/09/24 dadams
;;     Removed hlt-no-op - use function ignore instead.
;; 2009/08/02 dadams
;;     Added: hlt(-mouse)-toggle-(link|property)-highlighting, hlt-(un)highlight-all-prop,
;;            hlt-property-highlight, hlt-prop-highlighting-state.
;; 2009/07/31 dadams
;;     Added: hlt-highlight-property-with-value, hlt-flat-list, hlt-set-intersection.
;; 2009/04/26 dadams
;;     hlt-mouse-face-each-line: Bind inhibit-field-text-motion to  t, for real eol.
;; 2008/01/17 dadams
;;     Removed soft require of icicles.el.
;; 2007/11/27 dadams
;;     hlt-highlight-regexp-region: If available, use icicle-read-string-completing.
;; 2007/08/12 dadams
;;     Moved here from menu-bar+.el: Add to Edit>Region menu.  Soft require menu-bar.el.
;; 2007/06/07 dadams
;;     Use face-name-history or icicle-face-name-history, if defined, else face-name-history.
;; 2007/06/05 dadams
;;     Added: hlt-(highlighter|eraser)-mouse.
;; 2007/06/04 dadams
;;     Added: hlt-previous-use-overlays-flag-value.
;;     hlt-use-overlays-flag: 3 values now; default is only.
;;     hlt-eraser, hlt-unhighlight-region, hlt-hide-default-face, hlt-next-highlight:
;;       Treat non-only, non-nil hlt-use-overlays-flag.
;;     hlt-toggle-use-overlays-flag: Use hlt-previous-use-overlays-flag-value.
;;     Updated doc.
;; 2007/06/03 dadams
;;     Added: hlt-toggle-use-overlays-flag.
;;     Don't even define hlt-act-on-any-face-flag for Emacs 20.
;;     Renamed no-op to hlt-no-op. Removed soft require of misc-cmds.el.
;; 2007/06/02 dadams
;;     Added: hlt-act-on-any-face-flag, hlt-add-listifying, hlt-add-to-invisibility-spec,
;;            hlt-choose(-(in)visible)-faces, hlt-(hide|show)(-default-face|-only),
;;            hlt-highlight-faces-in-buffer, hlt-set-union, hlt-toggle-act-on-any-face-flag.
;;     Renamed: highlight-use-overlays-flag to hlt-use-overlays-flag,
;;              highlight-max-region-no-warning to hlt-max-region-no-warning,
;;              highlight-last-regexp to hlt-last-regexp, highlight-last-face to hlt-last-face,
;;              highlight-face to hlt-choose-default-face,
;;              highlight-highlighter to hlt-highlighter, highlight-eraser to hlt-eraser,
;;              mouse-face-each-line to hlt-mouse-face-each-line,
;;              unhighlight-region(-for-face) to hlt-unhighlight-region(-for-face).
;;     hlt-highlighter, hlt-highlight-region, hlt-mouse-face-each-line:
;;       Also put face as hlt-highlight property.
;;     hlt-eraser: Also remove hlt-highlight property.
;;     hlt-highlight-region, hlt-unhighlight-region, hlt-replace-highlight-face,
;;       hlt-next-highlight, hlt-mouse-face-each-line, hlt-highlight-regexp-region:
;;         Made start, end, and regexp args optional too.  Default for non-interactive too.
;;     hlt-unhighlight-region-for-face: Made all args optional.  Default them.
;;     hlt-unhighlight-region: Only remove highlighting for FACE, not all faces.
;;     hlt-highlight-single-quotations: Update hlt-last-face.
;;     hlt-next-highlight:
;;       Respect hlt-act-on-any-face-flag.  Return a cons of the limits.  Added no-error-p arg.
;;     hlt-previous-highlight: Added no-error-p arg.
;;     Added soft require of Icicles.
;; 2007/04/02 dadams
;;     Renamed highlight-region-beg-end to highlight-region-or-buffer-limits.
;; 2007/03/25 dadams
;;     highlight-highlighter, highlight-eraser, highlight-region, unhighlight-region:
;;       Use font-lock-ignore property.
;;     highlight-regexp-*: Use hi-lock-regexp-history or regexp-history.
;; 2007/03/23 dadams
;;     highlight-region:
;;       If no region and no overlay, apply face to next char typed & add to facemenu menu.
;;     highlight-highlighter: Don't create overlay unless highlight-use-overlays-flag.
;;     highlight-highlighter, highlight-region, highlight-eraser:
;;       Don't bother to call font-lock-after-fontify-buffer.
;;     highlight-highlighter, highlight-region: Prepare for possible font-lock-ignore prop.
;;     highlight: Removed message.
;; 2007/03/20 dadams
;;     highlight-face: Add face as arg.  Added final message.
;; 2007/03/17 dadams
;;     Added: highlight-(next|previous)-highlight, highlight-region-beg-end,
;;            highlight-eraser.
;;     highlight-region, highlight-regexp-to-end, highlight-regexp-region:
;;       Interactively, use highlight-last-face as the face.
;;     highlight-single-quotations: Added prefix arg, meaning prompt for face.
;;     highlight-region, highlight-regexp-region, unhighlight-region(-for-face),
;;     *-replace-face, *-single-quotations: If no region, then use whole buffer.
;;     highlight-single-quotations:
;;       Use highlight-regexp-region, not highlight-regexp-to-end.  Msg if interactive.
;;     highlight-regexp-region: Ensure move past match in loop.  Face is optional now.
;;     mouse-face-each-line: Added args start, end, face, msg-p. Restrict to region.
;;     Removed: mouse-face-following-lines.
;;     highlight-region: Added msg-p arg and progress message.
;;     unhighlight-region, highlight-replace-face: Simple message, no where part.
;;     unhighlight-region: Changed order of optional args, for consistency.
;;     highlight-highlighter:
;;       Make overlay once, and move it.  Initialize end-point to start-point.
;; 2007/03/16 dadams
;;     Renamed highlight-regexp to highlight-regexp-to-end, because Emacs now uses that name.
;;     Renamed max-highlight-w-o-warning to highlight-max-region-no-warning.
;;     Added: highlight-use-overlays-flag, highlight-last-face, highlight-face,
;;            highlight-highlighter, unhighlight-region-for-face,
;;            highlight-replace-face, highlight-delete-overlay.
;;     highlight-single-quotations: Read the face name.
;;     highlight-single-quotations, highlight-region, highlight-regexp-to-end,
;;     highlight-regexp-region: Set highlight-last-face.
;;     unhighlight-region, highlight-region, mouse-face-following-lines,
;;     mouse-face-each-line: Respect highlight-use-overlays-flag.
;;     unhighlight-region, mouse-face-*: Added optional face arg.
;;     highlight-max-region-no-warning: defvar -> defcustom.
;;     highlight-regexp-region: Use mouse-p when call highlight-region.
;; 2006/03/31 dadams
;;     No longer use display-in-minibuffer.
;; 2005/12/18 dadams
;;     Use minibuffer-prompt face.  Removed require of def-face-const.el.
;;     highlight-single-quotations: defsubst -> defun.
;; 2004/10/13 dadams
;;     Updated for Emacs 21: highlight-region: Bind
;;       inhibit-modification-hooks to non-nil to prevent Emacs 21
;;       font-lock from refontifying (removing highlighting)
;; 2004/10/12 dadams
;;     highlight-region: Use font-lock-after-fontify-buffer instead of
;;       lazy-lock-after-fontify-buffer.
;; 2004/03/16 dadams
;;     highlight-region: Prevent put-text-property from removing highlighting
;; 1996/04/26  dadams
;;     Put escaped newlines on long-line strings.
;; 1996/04/25  dadams
;;     1. Added highlight-single-quotations.
;;     2. highlight-regexp, highlight-regexp-region: Added new optional arg NTH.
;; 1996/04/25  dadams
;;     Added mouse-face-following-lines.
;; 1996/04/04  dadams
;;     1. highlight: Removed RAW-PREFIX, DISPLAY-MSGS args.  Made PREFIX optional.
;;        Set current-prefix-arg to nil so called fns don't use it as mouse-p.
;;     2. highlight-regexp, highlight-regexp-region: Added MOUSE-P arg.
;; 1996/02/27  dadams
;;     Added mouse-face-each-line.
;; 1996/02/26  dadams
;;     unhighlight-region: Added new arg MOUSE-P.
;; 1996/02/12  dadams
;;     highlight-region: Added optional arg MOUSE-P.
;; 1996/02/06  dadams
;;     Put variable-interactive property on appropriate user option vars.
;; 1996/02/01  dadams
;;     highlight: Just call subfunctions interactively.
;;     highlight-region, highlight-regexp, highlight-regexp-region: Use
;;       read-face-name
;; 1996/01/08  dadams
;;     highlight-regexp, highlight-regexp-region: message -> display-in-minibuffer.
;; 1995/11/09  dadams
;;     highlight-region: FACE arg is optional.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(and (< emacs-major-version 20) (eval-when-compile (require 'cl))) ;; when, unless

(require 'frame-fns nil t) ;; (no error if not found): flash-ding
(when (< emacs-major-version 21) (require 'faces+ nil t)) ;; (no error if not found):
                                                          ;; read-face-name
(require 'menu-bar+ nil t) ;; (no error if not found): menu-bar-edit-region-menu
;; (require 'icicles nil t)   ;; (no error if not found): icicle-define-command,
                              ;; icicle-delete-if, icicle-delete-if-not,
                              ;; icicle-make-face-candidate, icicle-read-string-completing.

;; Quiet the byte-compiler for Emacs 20
(defvar hi-lock-mode)
(defvar hlt-act-on-any-face-flag)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(@* "Menu-Bar Region Menu")

;;; Menu-Bar Region Menu --------------------------------

(when (boundp 'menu-bar-edit-region-menu) ; Defined in `menu-bar+.el'.
  (define-key menu-bar-edit-region-menu [separator-highlight] '("--"))
  (define-key menu-bar-edit-region-menu [hlt-unhighlight-region]
    '(menu-item "Unhighlight" hlt-unhighlight-region
      :help "Remove highlighting (faces) in region"
      :enable (and mark-active (not buffer-read-only))))
  (define-key menu-bar-edit-region-menu [hlt-highlight-regexp-region]
    '(menu-item "Highlight Regexp..." hlt-highlight-regexp-region
      :help "Highlight parts of selection that match a regular expression"
      :enable (and mark-active (not buffer-read-only))))
  (define-key menu-bar-edit-region-menu [hlt-highlight-region]
    '(menu-item "Highlight..." hlt-highlight-region :help "Highlight all text in the selection"
      :enable (and mark-active (not buffer-read-only)))))

;;(@* "Variables and Faces")

;;; Variables and Faces --------------------------------

;; This is defined in `faces.el', Emacs 22.  This definition is adapted to Emacs 20.
;;;###autoload
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "Face for minibuffer prompts."
    :group 'basic-faces))

;;;###autoload
(when (fboundp 'next-single-char-property-change) ; Don't bother, for Emacs 20.
  (defface hlt-property-highlight '((((background dark)) (:background "Navy"))
                                    (t (:background "Wheat")))
    "*Face used to highlight all links."
    :group 'faces)
  (defcustom hlt-act-on-any-face-flag nil
    "*Non-nil means highlight actions apply to all text with a face.
nil means that they apply only to text that has been highlighted.
Consult the doc for particular actions, to see if they are affected by
this option."
    :type 'boolean :group 'editing :group 'convenience :group 'wp :group 'faces)

  (defvar hlt-prop-highlighting-state '(nil . nil)
    "Cons indicating state of property highlighting.
The car indicates whether property highlighting is on.
The cdr is the position of the last mouse click that changed state, as
a marker."))

;;;###autoload
(defcustom hlt-max-region-no-warning 100000
  "*Max size (chars) of region to highlight without confirmation.
This is used only for highlighting of a regexp, which can be slow."
  :type 'integer :group 'editing :group 'convenience :group 'wp)

;;;###autoload
(defcustom hlt-use-overlays-flag 'only
  "*Non-nil means use overlays to highlight; nil means use text properties.
This value also affects some actions, such as unhighlighting, for text
that is highlighted.  If the value is `only' (the default value), then
those actions only affect overlay highlighting.  Otherwise, they
affect both kinds of highlighting."
  :type '(choice
          (const :tag "Highlight using text properties, not overlays" nil)
          (const :tag "Highlight using overlays, not text properties" only)
          (sexp  :tag
           "Highlight using overlays, but act also on highlight text properties" t))
  :group 'editing :group 'convenience :group 'wp :group 'faces)

(defvar hlt-last-regexp nil "The last regexp highlighted.")
(defvar hlt-last-face 'highlight "The last face used by highlight commands.")
(defvar hlt-previous-use-overlays-flag-value nil "Previous value of `hlt-use-overlays-flag'.")

;;(@* "Misc Functions - Emacs 20+")

;;; Misc Functions - Emacs 20+ ---------------------------------------

;;;###autoload
(defun hlt-choose-default-face (face)
  "Choose a face for highlighting."
  (interactive (list (read-face-name "Use highlighting face: ")))
  (setq hlt-last-face  face)
  (when (interactive-p) (message "Highlighting will now use face `%s'" face)))

;; Bind this to, for instance, `C-x mouse-2'.
;;;###autoload
(defun hlt-highlighter (start-event)
  "Highlight the text you drag the mouse over.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose a different face."
  (interactive "e")
  (save-excursion
    (run-hooks 'mouse-leave-buffer-hook) ; Let temporary modes like isearch turn off.
    (let* ((original-window  (selected-window))
           (echo-keystrokes  0)
           (start-posn       (event-start start-event))
           (start-point      (posn-point start-posn))
           (end-point        start-point)
           (start-window     (posn-window start-posn)))
      (let ((read-only                          buffer-read-only)
            (modified-p                         (buffer-modified-p))
            (inhibit-modification-hooks         t)
            (overlay                            (and hlt-use-overlays-flag
                                                     (make-overlay start-point start-point)))
            ;; Otherwise, `put-text-property' calls this, which removes highlight.
            (font-lock-fontify-region-function  'ignore)
            event)
        (setq buffer-read-only  nil)
        (track-mouse
          (while (progn (setq event  (read-event))
                        (or (mouse-movement-p event)
                            (memq (car-safe event) '(switch-frame select-window))))
            (unless (memq (car-safe event) '(switch-frame select-window))
              (setq end-point  (posn-point (event-end event))))
            (cond (hlt-use-overlays-flag
                   (setq overlay  (move-overlay overlay start-point end-point))
                   (overlay-put overlay 'face          hlt-last-face)
                   (overlay-put overlay 'hlt-highlight hlt-last-face))
                  (t
                   (put-text-property start-point end-point 'face             hlt-last-face)
                   (put-text-property start-point end-point 'hlt-highlight    hlt-last-face)
                   (put-text-property start-point end-point 'font-lock-ignore t)
                   ))))
        (setq buffer-read-only  read-only)
        (set-buffer-modified-p modified-p)))))

;; Bind this to, for instance, `C-x S-mouse-2'.
;;;###autoload
(defun hlt-eraser (start-event)
  "Erase highlights that you click or drag the mouse over.
If `hlt-use-overlays-flag' is non-nil, then overlay highlighting is
removed for the last face that was used for highlighting.  (You can
use command `hlt-choose-default-face' first to choose a different
face.)  If `hlt-use-overlays-flag' is not `only', then text-property
highlighting is removed for *ALL* faces (not just highlighting faces).
This means, in particular, that a value of nil erases both overlays
for the last face and text properties for all faces.

Note: When text properties are affected, this is like using an eraser:
only characters you drag over lose their faces.  But when overlays are
affected, an overlay is erased as soon as any part of it is touched.
You need not drag over the entire overlay to delete it, and there is
no way to erase only part of it."
  (interactive "e")
  (save-excursion
    (run-hooks 'mouse-leave-buffer-hook) ; Let temporary modes like isearch turn off.
    (let* ((original-window  (selected-window))
           (echo-keystrokes  0)
           (start-posn       (event-start start-event))
           (start-point      (posn-point start-posn))
           (end-point        start-point)
           (start-window     (posn-window start-posn)))
      (let ((read-only                          buffer-read-only)
            (modified-p                         (buffer-modified-p))
            (inhibit-modification-hooks         t)
            ;; Otherwise, `put-text-property' calls this, which removes highlight.
            (font-lock-fontify-region-function  'ignore)
            event)
        (setq buffer-read-only  nil)
        (track-mouse
          (while (progn (setq event  (read-event))
                        (or (mouse-movement-p event)
                            (memq (car-safe event) '(switch-frame select-window))))
            (unless (memq (car-safe event) '(switch-frame select-window))
              (setq end-point  (posn-point (event-end event))))
            (when hlt-use-overlays-flag
              (mapcar (lambda (o) (hlt-delete-highlight-overlay o hlt-last-face))
                      (overlays-in start-point end-point)))
            (unless (eq 'only hlt-use-overlays-flag)
              (remove-text-properties start-point end-point
                                      '(face nil hlt-highlight nil font-lock-ignore nil)))))
        (setq buffer-read-only  read-only)
        (set-buffer-modified-p modified-p)))))

(defun hlt-highlighter-mouse ()
  "Same as `hlt-highlighter', but for binding to a menu item."
  (interactive)
  (message "Drag mouse to highlight text") (sleep-for 1)
  (hlt-highlighter (read-event)))

(defun hlt-eraser-mouse ()
  "Same as `hlt-eraser', but for binding to a menu item."
  (interactive)
  (message "Drag mouse over to erase highlighting") (sleep-for 1)
  (hlt-eraser (read-event)))

;; Bind this to, for instance, `C-x C-y'.
;;;###autoload
(defun hlt-highlight (&optional prefix)
  "Highlight region, regexp (PREFIX +), or unhighlight region (PREFIX -).
PREFIX arg non-negative means `hlt-highlight-regexp-region'
PREFIX arg negative means `hlt-unhighlight-region'
PREFIX arg nil means `hlt-highlight-region'.
If the region is not active or it is empty, then use the whole buffer.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose a different face."
  (interactive "P")
  (setq current-prefix-arg  nil)         ; No mouse-p.
  (if prefix
      (if (natnump (prefix-numeric-value prefix))
          (call-interactively 'hlt-highlight-regexp-region)
        (save-excursion (call-interactively 'hlt-unhighlight-region)))
    (call-interactively 'hlt-highlight-region)))

;;;###autoload
(defun hlt-highlight-region (&optional start end face msg-p mouse-p)
  "Highlight the region or new input.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 3rd arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional 4th arg MSG-P non-nil means to display a progress message.
Optional 5th arg MOUSE-P non-nil means use the `mouse-face' property,
 not the `face' property.
Interactively, MOUSE-P is provided by the prefix arg.

If the region is not active or it is empty, then:
 - If `hlt-use-overlays-flag' is non-nil, apply FACE to the
   entire buffer.  If MOUSE-P is non-nil, use the `mouse-face'
   property; otherwise, use the `face' property.
 - Else, if MOUSE-P is non-nil, then apply FACE as the `mouse-face'
   property to the whole buffer.
 - Else, if interactive, apply FACE to the next character you type,
   and add FACE to the facemenu menu.
 - Else, apply FACE as the `face' property to the whole buffer."
  (interactive `(,@(hlt-region-or-buffer-limits) nil t ,current-prefix-arg))
  (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                            (setq start  (car start-end)
                                  end    (cadr start-end))))
  (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
  (when (and msg-p (or mark-active mouse-p)) (message "Highlighting..."))
  (let ((read-only                           buffer-read-only)
        (modified-p                          (buffer-modified-p))
        (inhibit-modification-hooks          t)
        ;; Otherwise, `put-text-property' calls this, which removes highlight.
        (font-lock-fontify-region-function  'ignore)
        overlay)
    (setq buffer-read-only  nil)
    (cond (hlt-use-overlays-flag
           (setq overlay  (make-overlay start end))
           (overlay-put overlay (if mouse-p 'mouse-face 'face) face)
           (overlay-put overlay 'hlt-highlight                 face))
          (mouse-p (put-text-property start end 'mouse-face face))
          ((interactive-p)
           (message "Text you type now will have face `%s'." face)
           (facemenu-add-new-face face)
           ;; It is `facemenu-add-face' that either uses region or next insert.
           (facemenu-add-face face (and mark-active start) (and mark-active end))
           (when (and mark-active start end (/= start end))
             (put-text-property start end 'hlt-highlight    face)
             (put-text-property start end 'font-lock-ignore t)))
          (t (put-text-property start end 'face             face)
             (put-text-property start end 'hlt-highlight    face)
             (put-text-property start end 'font-lock-ignore t)))
    (setq buffer-read-only  read-only)
    (set-buffer-modified-p modified-p))
  (let ((remove-msg  (substitute-command-keys
                      "`\\[negative-argument] \\[hlt-highlight]' to remove highlighting.")))
    (when (and msg-p (or mark-active mouse-p))
      (message "Highlighting... done. %s" remove-msg))))

;;;###autoload
(defun hlt-highlight-regexp-region (&optional start end regexp face msg-p mouse-p nth)
  "Highlight regular expression REGEXP in region.
If the region is not active or it is empty, then use the whole buffer.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 4th arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional 5th arg MSG-P:
  t means to treat this as an interactive call when deciding to
    display all messages.
  non-nil & non-t means to display only error and warning messages.
Optional 6th arg MOUSE-P non-nil means to use `mouse-face' property,
  not `face'.  Interactively, this is provided by the prefix arg.
Optional 7th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)"
  (interactive
   `(,@(hlt-region-or-buffer-limits)
     ,(if (fboundp 'icicle-read-string-completing)
          (icicle-read-string-completing "Regexp to highlight: "
                                         hlt-last-regexp
                                         (lambda (c) (string-match "regexp" (symbol-name c)))
                                         (if (and (boundp 'hi-lock-mode) hi-lock-mode)
                                             'hi-lock-regexp-history
                                           'regexp-history))
            (read-string "Regexp to highlight: "
                         nil (if (and (boundp 'hi-lock-mode) hi-lock-mode)
                                 'hi-lock-regexp-history
                               'regexp-history)
                         hlt-last-regexp))
       nil t ,current-prefix-arg)) ; interactive-p means to display all msgs.
  (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                            (setq start  (car start-end)
                                  end    (cadr start-end))))
  (unless regexp (setq regexp  hlt-last-regexp))
  (unless (stringp regexp)      ; Else re-search-forward gets an error
    (error "HIGHLIGHT-REGEXP-REGION: REGEXP arg is not a string: `%S'" regexp))
  (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
  (let ((reg-size  (abs (- end start))))
    (when (and msg-p
               (> reg-size hlt-max-region-no-warning)
               (not (progn
                      (and (fboundp 'flash-ding) ; In `frame-fns.el'
                           (flash-ding 'no-terminate-macros (selected-frame)))
                      (y-or-n-p (substitute-command-keys
                                 (format "Lots of highlighting slows \
things down.  Do you really want to highlight up to %d chars?  "
                                         reg-size))))))
      (error "OK, highlighting was cancelled")))
  (when (eq t msg-p) (message (concat "Highlighting occurrences of `" regexp "'...")))
  (save-excursion
    (goto-char start)
    (while (and (< start end) (not (eobp)) (re-search-forward regexp end t))
      (condition-case nil
          (progn (forward-char 1) (setq start  (1+ (point))))
        (end-of-buffer (setq start  end)))
      (hlt-highlight-region (match-beginning (or nth 0))
                            (match-end (or nth 0)) face nil mouse-p)))
  (when (eq t msg-p)
    (message "Highlighting occurrences of `%s' done.  %s" regexp
             (substitute-command-keys
              "`\\[negative-argument] \\[highlight]' to remove highlighting.")))
  (setq hlt-last-regexp  regexp))

;;;###autoload
(defun hlt-highlight-regexp-to-end (regexp &optional face msg-p mouse-p nth)
  "Highlight text after cursor that matches REGEXP.
Optional 2nd arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional 3rd arg MSG-P non-nil means display a progress message.
Optional 4th arg MOUSE-P non-nil means to use `mouse-face' property,
  not `face'.  Interactively, this is provided by the prefix arg.
Optional 5th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)"
  (interactive
   (list (read-string "Regexp to highlight after cursor: " nil
                      (if (and (boundp 'hi-lock-mode) hi-lock-mode)
                          'hi-lock-regexp-history
                        'regexp-history)
                      hlt-last-regexp)
         nil 'msg-p current-prefix-arg))
  (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
  (let ((remove-msg
         (and msg-p
              (substitute-command-keys
               "`\\[negative-argument] \\[highlight]' to remove highlighting."))))
    (when msg-p
      (message "Highlighting occurrences of `%s' after cursor..." regexp))
    (hlt-highlight-regexp-region (point) (point-max) regexp face
                                 (and msg-p 'error-msgs-only) mouse-p nth)
    (when msg-p
      (message "Highlighting occurrences of `%s' done.  %s" regexp remove-msg)))
  (setq hlt-last-regexp  regexp))

;;;###autoload
(defun hlt-unhighlight-region (&optional start end face msg-p mouse-p)
  "Remove all highlighting in region.
If the region is not active or it is empty, then use the whole buffer.
If `hlt-use-overlays-flag' is non-nil, then overlay highlighting is
removed.  If `hlt-use-overlays-flag' is not `only', then text-property
highlighting is removed.  This means, in particular, that a value of
nil removes both overlays and text properties.

Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 3rd arg FACE non-nil means delete only highlighting that uses
  FACE.  Nil means delete all highlighting.
Optional 4th argument MSG-P non-nil means display a progress message.
Optional 5th arg MOUSE-P non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSE-P is provided by the prefix arg."
  (interactive `(,@(hlt-region-or-buffer-limits) nil t ,current-prefix-arg))
  (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                            (setq start  (car start-end)
                                  end    (cadr start-end))))
  (when msg-p (message "Removing highlighting..."))
  (let ((read-only-p  buffer-read-only)
        (modified-p   (buffer-modified-p)))
    (setq buffer-read-only  nil)
    (when hlt-use-overlays-flag
      (mapcar (lambda (o) (hlt-delete-highlight-overlay o face)) (overlays-in start end)))
    (unless (eq 'only hlt-use-overlays-flag)
      (let ((beg  start)
            hi-face)
        (while (< beg end)
          (when (setq hi-face  (get-text-property beg 'hlt-highlight))
            (when (or (null face) (eq hi-face face))
              ;; $$$ Really, we should remove only the part of the `face'
              ;;     property that belongs to Highlight, and set the value to be
              ;;     the same as it is, but without hlt-last-face.
              (remove-text-properties
               beg (1+ beg) (if mouse-p
                                '(mouse-face nil hlt-highlight nil font-lock-ignore nil)
                              '(face nil hlt-highlight nil font-lock-ignore nil)))))
          (setq beg  (1+ beg)))))
    (setq buffer-read-only  read-only-p)
    (set-buffer-modified-p modified-p))
  (when msg-p (message "Removing highlighting... done.")))

;;;###autoload
(defun hlt-unhighlight-region-for-face (&optional face start end mouse-p)
  "Remove highlighting that uses FACE in region.
Same as `hlt-unhighlight-region', but removes only highlighting
that uses FACE.  Interactively, you are prompted for the face.

This works only for overlay highlighting, not text-property
highlighting.

Optional arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional arg MOUSE-P non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSE-P is provided by the prefix arg."
  (interactive `(,(read-face-name "Remove highlight overlays that use face: ")
                  ,@(hlt-region-or-buffer-limits) ,current-prefix-arg))
  (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
  (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                            (setq start  (car start-end)
                                  end    (cadr start-end))))
  (hlt-unhighlight-region start end face (interactive-p) mouse-p))

(defun hlt-delete-highlight-overlay (overlay &optional face)
  "Delete OVERLAY if it was created by highlighting (library `highlight').
Optional arg FACE is a face symbol.  If non-nil, then delete only
overlays with that FACE."
  (let ((highlight-face  (overlay-get overlay 'hlt-highlight)))
    (when (and highlight-face (or (not face) (eq face highlight-face)))
      (delete-overlay overlay))))

;;;###autoload
(defun hlt-replace-highlight-face (old-face new-face &optional start end msg-p mouse-p)
  "Replace OLD-FACE by NEW-FACE in all highlights in the region.
If the region is not active or it is empty, then use the whole buffer.
With a prefix argument, replace OLD-FACE as the `mouse-face' property,
 not the `face' property.
Other arguments:
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 5th argument MSG-P non-nil means display a progress message.
Optional 6th arg MOUSE-P non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSE-P is provided by the prefix arg.

This works only for overlay highlighting, not text-property
highlighting."
  (interactive `(,(read-face-name "Replace face in region highlights. Old face: ")
                  ,(read-face-name "New face: ")
                  ,@(hlt-region-or-buffer-limits) t ,current-prefix-arg))
  (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                            (setq start  (car start-end)
                                  end    (cadr start-end))))
  (when msg-p (message "Replacing highlighting face `%s'..." old-face))
  (let ((read-only-p  buffer-read-only)
        (modified-p   (buffer-modified-p)))
    (setq buffer-read-only  nil)
    (mapcar (lambda (o)
              (when (eq old-face (overlay-get o (if mouse-p 'mouse-face 'face)))
                (overlay-put o (if mouse-p 'mouse-face 'face) new-face)
                (overlay-put o 'hlt-highlight                 new-face)))
            (overlays-in start end))
    (setq buffer-read-only  read-only-p)
    (set-buffer-modified-p modified-p))
  (setq hlt-last-face  new-face)
  (when msg-p (message "Replacing highlighting face `%s'... done." old-face)))

;;;###autoload
(defun hlt-highlight-single-quotations (&optional face)
  "Highlight single-quoted text in the region.
This means, for example, commands and keys between `'s: `foobar'.
If the region is not active or it is empty, then use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face."
  (interactive "P")
  (if face
      (setq face  (read-face-name "Use highlighting face: ") hlt-last-face face)
    (setq face  hlt-last-face))
  (apply #'hlt-highlight-regexp-region
         (append (hlt-region-or-buffer-limits)
                 (list "`\\([^']+\\)'" face (and (interactive-p) t) nil 1))))

;;;###autoload
(defun hlt-mouse-face-each-line (&optional start end face msg-p)
  "Put `mouse-face' on each line of buffer in region.
If the region is active and not empty, then limit mouse-face
highlighting to the region.  Otherwise, use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional arg MSG-P non-nil means display a progress message."
  (interactive `(,@(hlt-region-or-buffer-limits) ,current-prefix-arg t))
  (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                            (setq start  (car start-end)
                                  end    (cadr start-end))))
  (if face
      (setq face  (read-face-name "Use highlighting face: ") hlt-last-face face)
    (setq face  hlt-last-face))
  (when msg-p (message "Putting mouse face `%s' on each line..." face))
  (let ((buffer-read-only           nil)
        (inhibit-field-text-motion  t)  ; Just to be sure, for `end-of-line'.
        overlay)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (not (eobp))
          (cond (hlt-use-overlays-flag
                 (setq overlay
                       (make-overlay (point) (setq start  (progn (end-of-line) (point)))))
                 (overlay-put overlay 'mouse-face    face)
                 (overlay-put overlay 'hlt-highlight face))
                (t
                 (put-text-property (point) (progn (end-of-line) (point)) 'mouse-face face)
                 (put-text-property start end 'hlt-highlight face)))
          (forward-line 1)))))
  (when msg-p (message "Putting mouse face `%s' on each line... done." face)))

(defun hlt-toggle-use-overlays-flag ()
  "Toggle `hlt-use-overlays-flag'.
If the current value is non-nil, it is set to nil.
If the current value is nil, it is set to the last non-nil value."
  (interactive)
  (let ((before-toggle  hlt-use-overlays-flag))
    (if hlt-use-overlays-flag
        (setq hlt-use-overlays-flag  nil)
      (setq hlt-use-overlays-flag  hlt-previous-use-overlays-flag-value))
    (setq hlt-previous-use-overlays-flag-value  before-toggle))
  (message
   (cond ((eq hlt-use-overlays-flag 'only)
          "Highlight actions now use only overlay properties, not text properties")
         (hlt-use-overlays-flag
          "Highlighting with overlays now, but actions affect also text properties")
         (t "Highlight actions now use only text properties, not overlay properties"))))

;;(@* "Misc Functions - Emacs 21+")

;;; Misc Functions - Emacs 21+ ---------------------------------------

;;;###autoload
(when (fboundp 'next-single-char-property-change) ; Don't bother, for Emacs 20.
  (defun hlt-show-default-face (face)
    "Show FACE, by default, the default highlighting face.
With a prefix argument, prompt for the highlighting face to show.
Otherwise, show the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face."
    (interactive (list (if current-prefix-arg
                           (read-face-name "Show highlighting face: ")
                         hlt-last-face)))
    (hlt-listify-invisibility-spec)
    (remove-from-invisibility-spec face))

  (defun hlt-listify-invisibility-spec ()
    "Convert `buffer-invisibility-spec' to list form.
If it is already a list, do nothing.
If it is t, set it to a list of all `invisible' spec values in the buffer.
That is, for each character in the buffer that has property `invisible',
the invisibility criteria specified by that value are accumulated."
    (unless (listp buffer-invisibility-spec)
      (setq buffer-invisibility-spec  nil)
      (let ((start  (point-min))
            (end    (point-max))
            spec)
        (dolist (ov (overlays-in start end))
          (when (setq spec  (overlay-get ov 'invisible))
            (unless (listp spec) (setq spec  (list spec)))
            (setq buffer-invisibility-spec
                  (hlt-set-union spec buffer-invisibility-spec))))
        (while (< start end)
          (when (setq spec  (get-text-property start 'invisible))
            (unless (listp spec) (setq spec  (list spec)))
            (setq buffer-invisibility-spec
                  (hlt-set-union spec buffer-invisibility-spec)))
          (setq start  (1+ start)))))
    buffer-invisibility-spec)

  ;; From `cl-seq.el', function `union', without keyword treatment.
  ;; Same as `simple-set-union' in `misc-fns.el'.
  (defun hlt-set-union (list1 list2)
    "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or
LIST2.  This is a non-destructive function; it copies the data if
necessary."
    (cond ((null list1) list2)
          ((null list2) list1)
          ((equal list1 list2) list1)
          (t
           (or (>= (length list1) (length list2))
               (setq list1  (prog1 list2 (setq list2  list1)))) ; Swap them.
           (while list2
             (unless (member (car list2) list1)
               (setq list1  (cons (car list2) list1)))
             (setq list2  (cdr list2)))
           list1)))

  ;; From `cl-seq.el', function `intersection', without keyword treatment.
  ;; Same as `simple-set-intersection' in `misc-fns.el'.
  (defun hlt-set-intersection (list1 list2)
    "Set intersection of lists LIST1 and LIST2.
This is a non-destructive operation: it copies the data if necessary."
    (and list1 list2
         (if (equal list1 list2)
             list1
           (let ((result  ()))
             (unless (>= (length list1) (length list2))
               (setq list1  (prog1 list2 (setq list2  list1)))) ; Swap them.
             (while list2
               (when (member (car list2) list1)
                 (setq result  (cons (car list2) result)))
               (setq list2  (cdr list2)))
             result))))

  (defun hlt-hide-default-face (&optional start end face)
    "Hide the last face used for highlighting.
With a prefix argument, prompt for the highlighting face to hide,
 instead.  You can also use command `hlt-choose-default-face' to
 choose a different face.

If `hlt-act-on-any-face-flag' is non-nil, then the face to be hidden
can be any face you choose.  Otherwise, it must be a face that has
been used for highlighting.

Hiding a face at some location means two things:
1) setting its `invisible' property there, making it susceptible to
   being hidden by `buffer-invisibility-spec', and
2) adding it to `buffer-invisibility-spec', so that it is hidden.

This command hides all text with the specified face that has the
`invisible' property, throughout the entire buffer.  However, it only
adds the `invisible' property to text with an overlay or text
property, depending on `hlt-use-overlays-flag', and it only does so
within the region, if the region is active.

Non-interactively:
FACE is the face to hide. It defaults to the last highlighting face.
START and END are the limits of the area to act on. They default to
  the region limits."
    (interactive `(,@(hlt-region-or-buffer-limits)
                   ,(if current-prefix-arg
                        (read-face-name "Hide highlighting face: ")
                        hlt-last-face)))
    (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                              (setq start  (car start-end)
                                    end    (cadr start-end))))
    (hlt-listify-invisibility-spec)
    ;; Add FACE to `invisible' property throughout START...END,
    ;; whenever it is used as a highlighting face.
    (save-excursion
      (save-window-excursion
        (goto-char start)
        (let ((zone-beg  start)
              zone-end zone)
          (while (and zone-beg (< zone-beg end))
            (setq zone      (hlt-next-highlight zone-beg end face nil nil 'no-error-msg)
                  zone-beg  (car zone)
                  zone-end  (cdr zone))
            ;; Add FACE to `invisible' property from `zone-beg' to `zone-end'.
            (when hlt-use-overlays-flag
              (let ((overlays  (overlays-at zone-beg)))
                (while overlays
                  (when (and (or hlt-act-on-any-face-flag
                                 (eq face (overlay-get (car overlays) 'hlt-highlight)))
                             (eq face (overlay-get (car overlays) 'face)))
                    (overlay-put (car overlays) 'invisible
                                 (hlt-add-listifying
                                  (overlay-get (car overlays) 'invisible)
                                  face)))
                  (when overlays (setq overlays  (cdr overlays))))))
            (when (and (not (eq hlt-use-overlays-flag 'only))
                       (or hlt-act-on-any-face-flag
                           (eq face (get-text-property (point) 'hlt-highlight)))
                       (eq face (get-text-property (point) 'face)))
              (put-text-property zone-beg zone-end 'invisible
                                 (hlt-add-listifying
                                  (get-text-property zone-beg 'invisible)
                                  face)))
            (hlt-add-to-invisibility-spec face))))))

  ;; Same as `add-to-invisibility-spec', except it doesn't allow duplicates.
  (defun hlt-add-to-invisibility-spec (element)
    "Add ELEMENT to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
    (when (eq buffer-invisibility-spec t) (setq buffer-invisibility-spec  (list t)))
    (add-to-list 'buffer-invisibility-spec element))

  (defun hlt-add-listifying (orig-val val-to-add)
    "Add VAL-TO-ADD to list ORIG-VAL, listifying ORIG-VAL first if needed."
    (unless (listp orig-val) (setq orig-val  (list orig-val)))
    (add-to-list 'orig-val val-to-add)
    orig-val)

  ;; Bind these to, for instance, `S-C-n' and `S-C-p'.
  (defun hlt-next-highlight (&optional start end face mouse-p backward-p no-error-p)
    "Go to the next highlight in FACE.
Interactively, FACE is the last face used for highlighting, but
you can use command `hlt-choose-default-face' to choose a different face.

If `hlt-act-on-any-face-flag' is non-nil, then the target face can be
any face you choose.  Otherwise, it must be a face that has been used
for highlighting.

If `hlt-use-overlays-flag' is non-nil, then overlay highlighting is
targeted.  If `hlt-use-overlays-flag' is not `only', then
text-property highlighting is targeted.  This means, in particular,
that a value of nil targets both overlays and text properties.

If the region is active and not empty, then limit movement to the
region.  Otherwise, use the whole buffer.
When called non-interactively:

 - non-nil argument NO-ERROR-P means do not raise an error if no
   highlight with FACE is found, and leave point at END.

 - Return a cons of the limits of the text starting at point that has
   property `hlt-highlight' of value FACE: (BEGIN-FACE . END-FACE), where
   BEGIN-FACE is point and END-FACE is the first position just after
   value FACE ends."
    (interactive `(,@(hlt-region-or-buffer-limits) nil ,current-prefix-arg))
    (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                              (setq start  (car start-end)
                                    end    (cadr start-end))))
    (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
    (when backward-p (setq end  (prog1 start (setq start  end))))
    (let ((face-found  nil)
          (orig-point  (point))
          (beg         start))
      (while (and (not (if backward-p (bobp) (eobp)))
                  (not (eq face face-found))
                  (not (= beg end)))
        (save-restriction
          (narrow-to-region beg end)
          (setq beg  (if backward-p
                         (goto-char (previous-single-char-property-change
                                     (point) (if mouse-p 'mouse-face 'face)
                                     nil (point-min)))
                       (goto-char (next-single-char-property-change
                                   (point) (if mouse-p 'mouse-face 'face)
                                   nil (point-max))))))
        (when hlt-use-overlays-flag
          (let ((overlays  (overlays-at (point))))
            (while overlays
              (when (and (or hlt-act-on-any-face-flag
                             (eq face (overlay-get (car overlays) 'hlt-highlight)))
                         (eq face (overlay-get (car overlays) 'face)))
                (setq face-found  face
                      overlays    ()))
              (when overlays (setq overlays  (cdr overlays))))))
        (when (and (not face-found)
                   (not (eq hlt-use-overlays-flag 'only))
                   (or hlt-act-on-any-face-flag
                       (eq face (get-text-property (point) 'hlt-highlight)))
                   (eq face (get-text-property (point) 'face)))
          (setq face-found  face)))
      (unless (or (and (eq face face-found) (not (eq (point) orig-point))) no-error-p)
        (goto-char orig-point)
        (error "No %s highlight with face `%s'" (if backward-p "previous" "next") face)))
    (unless (interactive-p)
      (cons (point)
            (next-single-char-property-change (point) (if mouse-p 'mouse-face 'face)
                                              nil (if backward-p start end)))))

  (defun hlt-previous-highlight (&optional start end face mouse-p no-error-p)
    "Go to the previous highlight in the last face used for highlighting.
This is the same as `hlt-previous-highlight', except movement is backward."
    (interactive `(,@(hlt-region-or-buffer-limits) nil ,current-prefix-arg))
    (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                              (setq start  (car start-end)
                                    end    (cadr start-end))))
    (hlt-next-highlight start end face mouse-p t no-error-p))

  (defun hlt-highlight-faces-in-buffer (start end)
    "List of highlighting faces in current buffer between START and END.
This includes faces used in overlays and as text properties.
Only highlighting faces are included, that is, faces associated with a
`hlt-highlight' property."
    (save-excursion
      (save-window-excursion
        (let ((faces  ())
              (beg  start)
              face)
          (setq end  (min end (point-max)))
          (goto-char beg)
          (while (< beg end)
            (save-restriction
              (narrow-to-region beg end)
              (setq beg  (goto-char (next-single-char-property-change (point) 'face
                                                                      nil (point-max)))))
            (when (setq face  (get-text-property (point) 'hlt-highlight))
              (add-to-list 'faces face))
            (let ((overlays  (overlays-at (point))))
              (while overlays
                (when (and (overlay-get (car overlays) 'hlt-highlight)
                           (setq face  (overlay-get (car overlays) 'face)))
                  (add-to-list 'faces face)
                  (setq overlays  ()))
                (when overlays (setq overlays  (cdr overlays))))))
          faces))))

  (defun hlt-toggle-act-on-any-face-flag ()
    "Toggle `hlt-act-on-any-face-flag'."
    (interactive)
    (setq hlt-act-on-any-face-flag  (not hlt-act-on-any-face-flag))
    (message (if hlt-act-on-any-face-flag
                 "Highlight actions now apply to any face, not just a highlighting face"
               "Highlight actions now apply only to a highlighting face")))
  )

;;(@* "Functions for Use with Icicles - Emacs 21+")

;;; Functions for Use with Icicles - Emacs 21+ -----------------------

;;;###autoload
(when (and (featurep 'icicles)          ; These are Icicles multi-commands.
           (fboundp 'next-single-char-property-change)) ; Don't bother, for Emacs 20.

  (icicle-define-command hlt-choose-faces
    "Choose a list of face names.
Option `hlt-act-on-any-face-flag' determines whether only highlighting
faces in the buffer are candidates.  The list of names (strings) is
returned."
    (lambda (name) (push name face-names)) ; Action function
    "Choose face (`RET' when done): "   ; `completing-read' args
    (mapcar #'icicle-make-face-candidate
            (if hlt-act-on-any-face-flag
                (face-list)
              (hlt-highlight-faces-in-buffer (point-min) (point-max))))
    nil t nil (cond ((boundp 'face-name-history) 'face-name-history)
                    ((boundp 'icicle-face-name-history) 'icicle-face-name-history)
                    (t 'face-name-history))
    nil nil
    ((face-names nil))                  ; Additional bindings
    nil nil                             ; First sexp and undo sexp
    (prog1 (setq face-names  (delete "" face-names)) ; Return the list of faces.
      (when (interactive-p) (message "Faces: %S" face-names))))

  (icicle-define-command hlt-choose-invisible-faces
    "Choose a list of face names from those currently invisible.
Option `hlt-act-on-any-face-flag' determines whether only highlighting
faces in the buffer are candidates.  The list of names (strings) is
returned."
    (lambda (name) (push name face-names)) ; Action function
    "Choose face (`RET' when done): "   ; `completing-read' args
    (mapcar #'icicle-make-face-candidate
            (icicle-delete-if-not (lambda (x) (memq x buffer-invisibility-spec))
                                  (if hlt-act-on-any-face-flag
                                      (face-list)
                                    (hlt-highlight-faces-in-buffer (point-min) (point-max)))))
    nil t nil (cond ((boundp 'face-name-history) 'face-name-history)
                    ((boundp 'icicle-face-name-history) 'icicle-face-name-history)
                    (t 'face-name-history))
    nil nil
    ((face-names nil))                  ; Additional bindings
    nil nil                             ; First sexp and undo sexp
    (prog1 (setq face-names  (delete "" face-names)) ; Return the list of faces.
      (when (interactive-p) (message "Faces: %S" face-names))))

  (icicle-define-command hlt-choose-visible-faces
    "Choose a list of face names from those currently visible.
Option `hlt-act-on-any-face-flag' determines whether only highlighting
faces in the buffer are candidates.  The list of names (strings) is
returned."
    (lambda (name) (push name face-names)) ; Action function
    "Choose face (`RET' when done): "   ; `completing-read' args
    (mapcar #'icicle-make-face-candidate
            (icicle-delete-if (lambda (x) (memq x buffer-invisibility-spec))
                              (if hlt-act-on-any-face-flag
                                  (face-list)
                                (hlt-highlight-faces-in-buffer (point-min) (point-max)))))
    nil t nil (cond ((boundp 'face-name-history) 'face-name-history)
                    ((boundp 'icicle-face-name-history) 'icicle-face-name-history)
                    (t 'face-name-history))
    nil nil
    ((face-names nil))                  ; Additional bindings
    nil nil                             ; First sexp and undo sexp
    (prog1 (setq face-names  (delete "" face-names)) ; Return the list of faces.
      (when (interactive-p) (message "Faces: %S" face-names))))

  (defun hlt-show-only (&optional start end faces)
    "Show only the faces you choose, hiding all others.
Non-nil `hlt-act-on-any-face-flag' means choose from among all
faces.  Nil means choose only from among faces used to highlight.

When choosing faces, completion and cycling are available. During
cycling, these keys with prefix `C-' act on the current face name:

`C-mouse-2', `C-RET' - Choose current face candidate only
`C-down'  - Choose, then move to next prefix-completion candidate
`C-up'    - Choose, then move to previous prefix-completion candidate
`C-next'  - Choose, then move to next apropos-completion candidate
`C-prior' - Choose, then move to previous apropos-completion candidate
`C-!'     - Choose *all* matching face names"
    (interactive `(,@(hlt-region-or-buffer-limits)
                   ,(mapcar #'intern (hlt-choose-faces))))
    (dolist (face (if hlt-act-on-any-face-flag
                      (face-list)
                    (hlt-highlight-faces-in-buffer start end)))
      (if (memq face faces)
          (hlt-show-default-face face)
        (hlt-hide-default-face start end face))))

  (defun hlt-hide-only (&optional start end faces)
    "hide only the faces you choose, showing all others.
Non-nil `hlt-act-on-any-face-flag' means choose from among all
faces.  Nil means choose only from among faces used to highlight.

When choosing faces, completion and cycling are available. During
cycling, these keys with prefix `C-' act on the current face name:

`C-mouse-2', `C-RET' - Choose current face candidate only
`C-down'  - Choose, then move to next prefix-completion candidate
`C-up'    - Choose, then move to previous prefix-completion candidate
`C-next'  - Choose, then move to next apropos-completion candidate
`C-prior' - Choose, then move to previous apropos-completion candidate
`C-!'     - Choose *all* matching face names"
    (interactive `(,@(hlt-region-or-buffer-limits)
                   ,(mapcar #'intern (hlt-choose-faces))))
    (dolist (face (if hlt-act-on-any-face-flag
                      (face-list)
                    (hlt-highlight-faces-in-buffer start end)))
      (if (memq face faces)
          (hlt-hide-default-face start end face)
        (hlt-show-default-face face))))

  (defun hlt-show (faces)
    "Show invisible faces that you choose.  Do nothing to other faces.
Non-nil `hlt-act-on-any-face-flag' means choose from among all
invisible faces.  Nil means choose only from among invisible faces
used to highlight.

When choosing faces, completion and cycling are available. During
cycling, these keys with prefix `C-' act on the current face name:

`C-mouse-2', `C-RET' - Choose current face candidate only
`C-down'  - Choose, then move to next prefix-completion candidate
`C-up'    - Choose, then move to previous prefix-completion candidate
`C-next'  - Choose, then move to next apropos-completion candidate
`C-prior' - Choose, then move to previous apropos-completion candidate
`C-!'     - Choose *all* matching face names"
    (interactive
     (list (let ((fs  (icicle-delete-if-not
                       (lambda (x) (memq x buffer-invisibility-spec))
                       (if hlt-act-on-any-face-flag
                           (face-list)
                         (hlt-highlight-faces-in-buffer (point-min) (point-max))))))
             (if fs
                 (mapcar #'intern (hlt-choose-invisible-faces))
               (error "No%s faces are invisible"
                      (if hlt-act-on-any-face-flag "" " highlight"))))))
    (dolist (face faces) (hlt-show-default-face face)))

  (defun hlt-hide (&optional start end faces)
    "Hide visible faces that you choose.  Do nothing to other faces.
Non-nil `hlt-act-on-any-face-flag' means choose from among all
visible faces.  Nil means choose only from among visible faces used to
highlight.

When choosing faces, completion and cycling are available. During
cycling, these keys with prefix `C-' act on the current face name:

`C-mouse-2', `C-RET' - Choose current face candidate only
`C-down'  - Choose, then move to next prefix-completion candidate
`C-up'    - Choose, then move to previous prefix-completion candidate
`C-next'  - Choose, then move to next apropos-completion candidate
`C-prior' - Choose, then move to previous apropos-completion candidate
`C-!'     - Choose *all* matching face names"
    (interactive `(,@(hlt-region-or-buffer-limits)
                   ,(mapcar #'intern (hlt-choose-faces))))
    (dolist (face faces) (hlt-hide-default-face start end face)))
  )


;;(@* "Functions for Highlighting Propertized Text - Emacs 21+")

;;; Functions for Highlighting Propertized Text - Emacs 21+ ----------

;;;###autoload
(when (fboundp 'next-single-char-property-change) ; Don't bother, for Emacs 20.
  (defun hlt-highlight-property-with-value (prop &optional values start end face
                                            type msg-p mouse-p)
    "Highlight text in region with property PROP of a value in VALUES.
Non-nil VALUES means do this only where PROP has a value in VALUES.
Interactively, you are prompted for PROP and a single value in VALUES.
  Using `RET' with no input means highlight for any non-nil value.

Optional args START and END are the limits of the area to act on.
  They default to the region limits (buffer, if no active region).
Optional 5th arg FACE is the face to use for highlighting.
  Interactively, this is the last face that was used for highlighting.
  (Use command `hlt-choose-default-face' to choose a different face.)
Optional 6th arg TYPE is `overlay', `text', or nil, and specifies the
  type of character property - nil means to look for both overlay and
  text properties.  Interactively, TYPE is derived from
  `hlt-use-overlays-flag'.
Optional 7th arg MSG-P non-nil means to display a progress message.
Optional 8th arg MOUSE-P non-nil means use the `mouse-face' property,
  not the `face' property, for highlighting.  Interactively, MOUSE-P
  is provided by the prefix arg."
    (interactive
     `(,(intern (read-string "Property to highlight: " nil 'highlight-property-history))
       ,(let* ((strg  (read-string "Property value: " nil))
               (vals  (if (string= "" strg)
                          ()
                        (list
                         (car (read-from-string (read-string "Property value: " nil))))))))
       ,@(hlt-region-or-buffer-limits)
       nil
       ,(if hlt-use-overlays-flag
            (if (eq hlt-use-overlays-flag 'only) 'overlay nil)
            'text)
       t
       ,current-prefix-arg))
    (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                              (setq start  (car start-end)
                                    end    (cadr start-end))))
    (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
    (when (and msg-p (or mark-active mouse-p)) (message "Highlighting..."))
    (let ((zone-end  nil))
      (unless (and start end)
        (setq start  (point-min)
              end    (point-max)))
      (condition-case highlight-property-with-value
          (save-excursion
            (while (and (< start end)
                        (let* ((charval  (and (or (not type) (eq type 'overlay))
                                              (get-char-property start prop)))
                               (textval  (and (or (not type) (eq type 'text))
                                              (get-text-property start prop)))
                               (currval  (hlt-flat-list charval textval)))
                          (if values
                              (not (hlt-set-intersection values currval))
                            (not currval))))
              (setq start  (next-single-char-property-change start prop nil end)))
            (while (and start (< start end))
              (setq zone-end  (or (next-single-char-property-change start prop nil end) end))
              (hlt-highlight-region start zone-end face nil mouse-p)
              (setq start  zone-end)
              (while (and (< start end)
                          (let* ((charval  (and (or (not type) (eq type 'overlay))
                                                (get-char-property start prop)))
                                 (textval  (and (or (not type) (eq type 'text))
                                                (get-text-property start prop)))
                                 (currval  (hlt-flat-list charval textval)))
                            (if values
                                (not (hlt-set-intersection values currval))
                              (not currval))))
                (setq start  (next-single-char-property-change start prop nil end)))))
        (quit (hlt-unhighlight-region start end face))
        (error (hlt-unhighlight-region start end face)
               (error (error-message-string highlight-property-with-value)))))
    (let ((remove-msg  (substitute-command-keys
                        "`\\[negative-argument] \\[hlt-highlight]' to remove highlighting.")))
      (when (and msg-p (or mark-active mouse-p))
        (message "Highlighting... done. %s" remove-msg))))

  (defun hlt-flat-list (val1 val2)
    "Return a flat list with all values in VAL1 and VAL2."
    (let ((result  ()))
      (unless (listp val1) (setq val1  (list val1)))
      (unless (listp val2) (setq val2  (list val2)))
      (while val1 (add-to-list 'result (pop val1)))
      (while val2 (add-to-list 'result (pop val2)))
      result))

  (defun hlt-mouse-toggle-link-highlighting ()
    "Alternately highlight and unhighlight links on a mouse click.
Do nothing if the click is at a different location from the last one.
This calls `hlt-toggle-link-highlighting' to do the toggling.
Links in the entire buffer are affected, even if the region is active.
This is intended to be used on `post-command-hook'."
    (when (and (string-match "mouse" (format "%S" (event-basic-type last-command-event)))
               (memq 'click (event-modifiers last-command-event)))
      (let* ((estart  (event-start last-command-event))
             (pos     (copy-marker (posn-point estart))))
        (when (integer-or-marker-p pos)
          (save-excursion
            (with-current-buffer (window-buffer (posn-window estart))
              (when (condition-case nil
                        (get-char-property (min pos (point-max)) 'mouse-face)
                      (error nil))
                (hlt-toggle-link-highlighting nil nil pos))))))))

  ;; Use it like this:
  ;; (add-hook 'post-command-hook 'hlt-mouse-toggle-link-highlighting)

  (defun hlt-toggle-link-highlighting (&optional start end pos)
    "Alternately highlight and unhighlight links.
A link is considered to be any text with property `mouse-face'.
Calls `hlt-toggle-property-highlighting', passing the args."
    (interactive `(,@(hlt-region-or-buffer-limits)))
    (hlt-toggle-property-highlighting 'mouse-face start end 'hlt-property-highlight
                                      (interactive-p) nil pos))

  (defun hlt-mouse-toggle-property-highlighting (prop &optional face msg-p mouse-p)
    "Alternately highlight and unhighlight text on a mouse click.
Do nothing if the click is at a different location from the last one.
Call `hlt-toggle-link-highlighting', passing the args.
Propertized text in the entire buffer is (un)highlighted, even if the
region is active.
This is intended to be used on `post-command-hook'."
    (when (and (string-match "mouse" (format "%S" (event-basic-type last-command-event)))
               (memq 'click (event-modifiers last-command-event)))
      (let* ((estart  (event-start last-command-event))
             (pos     (copy-marker (posn-point estart))))
        (when (integer-or-marker-p pos)
          (save-excursion
            (with-current-buffer (window-buffer (posn-window estart))
              (when (condition-case nil
                        (get-char-property (min pos (point-max)) prop)
                      (error nil))
                (hlt-toggle-property-highlighting prop nil nil
                                                  face (interactive-p) mouse-p pos))))))))

  ;; Use it like this:
  ;; (add-hook 'post-command-hook
  ;;           (lambda () (hlt-mouse-toggle-property-highlighting myprop myface)

  (defun hlt-toggle-property-highlighting (prop &optional start end face
                                           msg-p mouse-p pos)
    "Alternately highlight and unhighlight all text with property PROP.
Highlighting is done using overlays.
Optional arg POS is a buffer position.  If it is the same as the
  position recorded in `hlt-prop-highlighting-state', then do not
  toggle.  In any case, update `hlt-prop-highlighting-state' with POS.
Other args are the same as for `hlt-highlight-property-with-value'."
    (interactive
     `(,(intern (read-string "Property to highlight: " nil 'highlight-property-history))
       ,@(hlt-region-or-buffer-limits)
       nil  t  ,current-prefix-arg))
    (when (or (not pos) (equal pos (cdr hlt-prop-highlighting-state)))
      (cond ((car hlt-prop-highlighting-state)
             (hlt-unhighlight-all-prop prop start end face (interactive-p) mouse-p)
             (setcar hlt-prop-highlighting-state  nil))
            (t
             (hlt-highlight-all-prop prop start end face (interactive-p) mouse-p)
             (setcar hlt-prop-highlighting-state t))))
    (when pos (setcdr hlt-prop-highlighting-state  pos)))

  (defun hlt-highlight-all-prop (prop &optional start end face msg-p mouse-p)
    "Highlight all text that has a non-nil property PROP using FACE.
Highlight using overlays.
Args are the same as for `hlt-highlight-property-with-value'."
    (interactive `(,@(hlt-region-or-buffer-limits)))
    (hlt-highlight-property-with-value
     prop () start end face 'overlay (interactive-p) mouse-p))

  (defun hlt-unhighlight-all-prop (prop &optional start end face msg-p mouse-p)
    "Unhighlight all text highlighted with face `hlt-property-highlight'.
Args are the same as for `hlt-highlight-property-with-value'."
    (interactive `(,@(hlt-region-or-buffer-limits)))
    (let ((hlt-use-overlays-flag  'only))
      (hlt-unhighlight-region-for-face face start end mouse-p)))

  )

;;(@* "General functions")

;;; General functions

;; This is the same as `region-or-buffer-limits' in `misc-fns.el'.
(defun hlt-region-or-buffer-limits ()
  "Return the start and end of the region as a list, smallest first.
If the region is empty or not active, then bob and eob are used."
  (if (or (not mark-active) (null (mark)) (= (point) (mark)))
      (list (point-min) (point-max))
    (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; highlight.el ends here
