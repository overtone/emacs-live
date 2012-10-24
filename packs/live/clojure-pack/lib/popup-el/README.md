popup.el
========

[![Build Status](https://secure.travis-ci.org/auto-complete/popup-el.png)](http://travis-ci.org/auto-complete/popup-el)

Overview
--------

popup.el is a visual popup user interface library for Emacs. This
provides a basic API and common UI widgets such as popup tooltips and
popup menus.

Screenshots
-----------

**Tooltip**

![](http://cx4a.org/software/popup/popup1.png)

**Popup Menu**

![](http://cx4a.org/software/popup/popup2.png)

**Popup Cascade Menu**

![](http://cx4a.org/software/popup/popup3.png)

Installation
------------

Install `popup.el` into your `load-path` directory. If you have
`install-elisp` or `auto-install`, you also be able to install
`popup.el` like:

    ;; install-elisp
    (install-elisp "https://github.com/m2ym/popup-el/raw/master/popup.el")
    ;; auto-install
    (auto-install-from-url "https://github.com/m2ym/popup-el/raw/master/popup.el")

popwin is tested under GNU Emacs 22 or later.

Popup Items
-----------

Elements of `popup-list` have to be popup items. A popup item is
substantially a string but it may involve some text-properties. There
are two ways to make popup items. One is just using strings. Another
is to use the `popup-make-item` function, which just returns the string
after adding text-properties of its keywords. Effective text-properties
are:

* `value` -- This represents the **real** value of the item. This will
  be used when returning the value but not the item (or string) from
  some synchronous functions such as `popup-menu*`.
* `face` -- The background face of the item. The value of `popup-face`
  will be overridden.
* `selection-face` -- The selection face of the item. The value of
  `popup-selection-face` will be overridden.
* `document` -- The documentation string or function of the item.
* `summary` -- The summary string of the item. This will be shown
  inline with the item.
* `symbol` -- The symbol character of the item.
* `sublist` -- The sublist of the item. This is effective only with
  `popup-cascade-menu`.

All of properties can be accessed by `popup-item-<property>` utility function.

### Function: `popup-item-propertize`

    popup-item-propertize item &rest properties => item

Same as `propertize` except that this avoids overriding existed value
with `nil` property.

### Function: `popup-make-item`

    popup-make-item name &key value popup-face selection-face sublist
    document symbol summary => item

The utility function of `popup-item-propertize`.

Popups
------

This section describes the basic data structures and operations of
popups.

### Struct: `popup`

Any instance of `popup` structure has the following fields (some
unimportant fields are not listed):

* `point`
* `row` -- The line number.
* `column`
* `width` -- Max width of `popup` instance.
* `height` -- Max height of `popup` instance.
* `min-height`
* `current-height`
* `direction` -- Positive number means forward, negative number means
  backward.
* `parent` -- The parent of `popup` instance.
* `face` -- The background face.
* `selection-face`
* `margin-left`
* `margin-right`
* `scroll-bar` -- Non-nil means `popup` instance has a scroll bar.
* `symbol` -- Non-nil means `popup` instance has a space for
  displaying symbols of item.
* `cursor` -- The current position of `list`.
* `scroll-top` -- The offset of scrolling.
* `list` -- The contents of `popup` instance in a list of items
  (strings).
* `original-list` -- Same as `list` except that this is not filtered.

All of these fields can be accessed by `popup-<field>` function.

### Function: `popup-create`

    popup-create point width height &key min-height around face
    selection-face scroll-bar margin-left margin-right symbol parent
    parent-offset => popup

Create a popup instance at `POINT` with `WIDTH` and `HEIGHT`.

`MIN-HEIGHT` is the minimal height of the popup. The default value is 0.

If `AROUND` is non-nil, the popup will be displayed around the point
but not at the point.

`FACE` is the background face of the popup. The default value is
`popup-face`.

`SELECTION-FACE` is the foreground (selection) face of the popup The
default value is `popup-face`.

If `SCROLL-BAR` is non-nil, the popup will have a scroll bar at the
right.

If `MARGIN-LEFT` is non-nil, the popup will have a margin at the left.

If `MARGIN-RIGHT` is non-nil, the popup will have a margin at the
right.

`SYMBOL` is a single character which indicates the kind of the item.

`PARENT` is the parent popup instance. If `PARENT` is omitted, the popup
will be a root instance.

`PARENT-OFFSET` is a row offset from the parent popup.

Here is an example:

    (setq popup (popup-create (point) 10 10))
    (popup-set-list popup '("Foo" "Bar" "Baz"))
    (popup-draw popup)
    ;; do something here
    (popup-delete popup)

### Function: `popup-delete`

    popup-delete popup

Delete the `POPUP`.

### Function: `popup-live-p`

    popup-live-p popup => boolean

### Function: `popup-set-list`

    popup-set-list popup list

Set the contents of the `POPUP`. `LIST` has to be popup items.

### Function: `popup-draw`

    popup-draw popup

Draw the contents of the `POPUP`.

### Function: `popup-hide`

    popup-hide popup

Hide the `POPUP`. To show again, call `popup-draw`.

### Function: `popup-hidden-p`

    popup-hidden-p popup

Return non-nil if the `POPUP` is hidden.

### Function: `popup-select`

    popup-select popup index

Select the item of `INDEX` of the `POPUP`.

### Function: `popup-selected-item`

    popup-selected-item popup => item

Return the selected item of the `POPUP`.

Return non-nil if the `POPUP` is still alive.

### Function: `popup-next`

    popup-next popup

Select the next item of the `POPUP`.

### Function: `popup-previous`

    popup-previous popup

Select the next item of the `POPUP`.

### Function: `popup-scroll-down`

    popup-scroll-down popup n

Scroll down `N` items of the `POPUP`. This won't wrap.

### Function: `popup-scroll-up`

    popup-scroll-up popup n

Scroll up `N` items of the `POPUP`. This won't wrap.

### Function: `popup-isearch`

    popup-isearch popup &key cursor-color keymap callback help-delay
    => boolean

Enter incremental search event loop of `POPUP`.

Tooltips
--------

A tooltip is an useful visual UI widget for displaying information
something about what cursor points to.

### Function: `popup-tip`

    popup-tip string &key point around width height min-height
    truncate margin margin-left margin-right scroll-bar parent
    parent-offset nowait prompt

Show a tooltip with message `STRING` at `POINT`. This function is
synchronized unless `NOWAIT` specified. Almost all arguments are same as
`popup-create` except for `TRUNCATE`, `NOWAIT`, and `PROMPT`.

If `TRUNCATE` is non-nil, the tooltip can be truncated.

If `NOWAIT` is non-nil, this function immediately returns the tooltip
instance without entering event loop.

`PROMPT` is a prompt string used when reading events during the event
loop.

Here is an example:

    (popup-tip "Hello, World!")
    ;; reach here after the tooltip disappeared

Popup Menus
-----------

Popup menu is an useful visual UI widget for prompting users to
select an item of a list.

### Function: `popup-menu*`

    popup-menu* list &key point around width height margin margin-left
    margin-right scroll-bar symbol parent parent-offset keymap
    fallback help-delay nowait prompt isearch isearch-cursor-color
    isearch-keymap isearch-callback => selected-value

Show a popup menu of `LIST` at `POINT`. This function returns the value
of the selected item. Almost all arguments are same as `popup-create`
except for `KEYMAP`, `FALLBACK`, `HELP-DELAY`, `PROMPT`, `ISEARCH`,
`ISEARCH-CURSOR-COLOR`, `ISEARCH-KEYMAP`, and `ISEARCH-CALLBACK`.

If `KEYMAP` is provided, it is a keymap which is used when processing
events during event loop.

If `FALLBACK` is provided, it is a function taking two arguments; a key
and a command. `FALLBACK` is called when no special operation is found
on the key. The default value is `popup-menu-fallback`, which does
nothing.

`HELP-DELAY` is a delay of displaying helps.

If `NOWAIT` is non-nil, this function immediately returns the menu
instance without entering event loop.

`PROMPT` is a prompt string when reading events during event loop.

If `ISEARCH` is non-nil, do isearch as soon as displaying the popup
menu.

`ISEARCH-CURSOR-COLOR` is a cursor color during isearch. The default
value is `popup-isearch-cursor-color'.

`ISEARCH-KEYMAP` is a keymap which is used when processing events
during event loop. The default value is `popup-isearch-keymap`.

`ISEARCH-CALLBACK` is a function taking one argument.  `popup-menu`
calls `ISEARCH-CALLBACK`, if specified, after isearch finished or
isearch canceled. The arguments is whole filtered list of items.

Here is an example:

    (popup-menu* '("Foo" "Bar" "Baz"))
    ;; => "Baz" if you select Baz
    (popup-menu* (list (popup-make-item "Yes" :value t)
                       (popup-make-item "No" :value nil)))
    ;; => t if you select Yes

### Function: `popup-cascade-menu`

Same as `popup-menu` except that an element of `LIST` can be also a
sub-menu if the element is a cons cell formed `(ITEM . SUBLIST)` where
`ITEM` is an usual item and `SUBLIST` is a list of the sub menu.

Here is an example:

    (popup-cascade-menu '(("Top1" "Sub1" "Sub2") "Top2"))

----

Copyright (C) 2011  Tomohiro Matsuyama <<tomo@cx4a.org>>
