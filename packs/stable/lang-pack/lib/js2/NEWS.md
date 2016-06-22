# History of user-visible changes

## Next

* Object properties are highlighted using a different face:
  `js2-object-property`, which has no color by default.
* `js2-getter-setter-node` is renamed to `js2-method-node`, together with
  its related functions.  It already handles generator methods, and we
  added support for async methods (see below), so the old name would get
  more confusing.
* Support for default parameters in destructuring.  It should work for both
  objects and arrays, in both literals and function arguments.
* New mode: `js2-jsx-mode`, deriving from `js2-mode`.  Supports indentation of
  JSXElement expressions wrapped within parentheses or as function arguments.
  Indentation is customizable via `sgml-attribute-offset`.
* Experimental support for ES7 stage 3 async/await.

## 20150909

* `js2-mode` now derives from `js-mode`. That means the former
  function will run `js-mode-hook`, as well as `js2-mode-hook`. The
  key bindings will default to `js-mode-map` where they're not set in
  `js2-mode-map`. And in Emacs 25 or later (including the snapshot
  builds), `js2-mode` uses the indentation code from `js-mode`.  Where
  feasible, the user options (and functions) now have aliases, but if
  you're using Emacs 25 and you see an indentation-related setting
  that stopped working, try looking for a corresponding one in the
  `js` group: `M-x customize-group RET js RET`.

* New command: `js2-jump-to-definition`. It's bound to `M-.` by
  default, via remapping `js-find-symbol`. To get back to the default
  `M-.` binding (e.g. `find-tag`), put this in your init file:

      (eval-after-load 'js (define-key js-mode-map (kbd "M-.") nil))

## 20150713

* More comprehensive strict mode warnings and syntax errors.
* New minor mode: `js2-highlight-unused-variables-mode`.
* `js2-pretty-multiline-declarations` can take the value `dynamic` now.

## 20150202

Support for:

* [ES6 modules](http://www.2ality.com/2014/09/es6-modules-final.html).
* [Short-hand object literals](http://ariya.ofilabs.com/2013/02/es6-and-object-literal-property-value-shorthand.html).
* [Method definitions](http://ariya.ofilabs.com/2013/03/es6-and-method-definitions.html).
* ['u' and 'y' RegExp flags](https://mathiasbynens.be/notes/es6-unicode-regex).
* [Computed property names](http://people.mozilla.org/~jorendorff/es6-draft.html#sec-object-initializer).
* [Class statements and expressions](https://github.com/lukehoban/es6features#classes).
* [Template strings](http://tc39wiki.calculist.org/es6/template-strings/), including tagged ones.

The variable `js2-allow-keywords-as-property-names` has been
removed. Instead we check if `js2-language-version` is 180 or highter.

## 20141115

Support for:

* Unicode characters in identifiers (improved).
* [Delegating yield](http://wiki.ecmascript.org/doku.php?id=harmony:generators#delegating_yield).
* [ES6 numeric literals](https://people.mozilla.org/~jorendorff/es6-draft.html#sec-literals-numeric-literals) (octal, binary).
* Harmony [array and generator comprehensions](http://wingolog.org/archives/2014/03/07/es6-generator-and-array-comprehensions-in-spidermonkey).

## 20131106

Support for:

* [Arrow functions](http://wiki.ecmascript.org/doku.php?id=harmony:arrow_function_syntax)
* [Generators](http://wiki.ecmascript.org/doku.php?id=harmony:generators)
* [Spread operator](http://wiki.ecmascript.org/doku.php?id=harmony:spread)

## 20130510

### Support for JSLint global declaration

See the docstring for `js2-include-jslint-globals`.

## 20130216

### We don't rebind `RET` anymore

Because well-behaving major modes aren't supposed to do that.

So pressing it won't continue a block comment, or turn a string into a concatenation.
Pressing `M-j`, however, will.

The options `js2-indent-on-enter-key` and `js2-enter-indents-newline` were also removed.

To bring back the previous behavior, put this in your init file:

```js
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "RET") 'js2-line-break))
```

## 20120617

### Support for [default](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/default_parameters) and [rest](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/rest_parameters) parameters

## 20120614

### Support for [for..of loops](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for...of)

## Older changes

### Popular indentation style

```js
[foo, bar, baz].forEach(function (v) {
    if (validate(v))
        process(v);
});

[a, b, c].some(function (v) {
    return validate(v);
});
```

### Pretty multiline variable declaration

In the original mode,

```js
var foo = 10,
bar = 20,
baz = 30;
```

In this mode when the value of `js2-pretty-multiline-declarations` is non-nil,

```js
var foo = 10,
    bar = 20,
    baz = 30;
```

### Abbreviated destructuring assignments

```js
let {a, b}       = {a: 10, b: 20}; // Abbreviated   (Not supported in the original mode)
let {a: a, b: b} = {a: 10, b: 20}; // Same as above (Supported in the original mode)

(function ({responseText}) { /* */ })(xhr); // As the argument of function

for (let [k, { name, age }] in Iterator(obj)) // nested
    print(k, name, age);
```

### Expression closure in property value

```js
let worker = {
    get age() 20,
    get sex() "male",
    fire: function () _fire()
};
```

### Fix for odd indentation of "else if" with no braces

In the original mode,

```js
if (foo)
    return foo;
else if (bar)
return bar;      // here
```

In this mode,

```js
if (foo)
    return foo;
else if (bar)
    return bar;  // fixed
```

### Imenu support for function nesting

Supports function nesting and anonymous wrappers:

```js
(function() {
  var foo = function() {
    function bar() { // shown as foo.bar.<definition-1>
      function baz() {} // foo.bar.baz
      var qux = function() {}; // foo.bar.quux
    }
  };
});
```

Examples of output:

* [jQuery 1.5](https://gist.github.com/845449)
* [Underscore.js](https://gist.github.com/824262)
* [Backbone.js](https://gist.github.com/824260)

For library-specific extension methods like `$.extend` and `dojo.declare`, see [js2-imenu-extras](/mooz/js2-mode/blob/master/js2-imenu-extras.el).

### Undeclared/external variables highlighting

Original mode highlights them only on the left side of assignments:

```js
var house;
hose = new House(); // highlights "hose"
```

Here they are highlighted in all expressions:

```js
function feed(fishes, food) {
    for each (var fish in fshes) { // highlights "fshes"
        food.feed(fsh); // highlights "fsh"
    }
    hood.discard(); // highlights "hood"
}
```

Destructuring assignments and array comprehensions (JS 1.7) are supported:

```js
let three, [one, two] = [1, 2];
thee = one + two; // highlights "thee"

function revenue(goods) {
    // highlights "coast"
    return [price - coast for each ({price, cost} in goods)].reduce(add);
}
```
