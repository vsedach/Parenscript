## What is Parenscript?
Parenscript is a translator from an extended subset of Common Lisp to
JavaScript. Parenscript code can run almost identically on both the
browser (as JavaScript) and server (as Common Lisp).

Parenscript code is treated the same way as Common Lisp code, making
the full power of Lisp macros available for JavaScript. This provides
a web development environment that is unmatched in its ability to
reduce code duplication and provide advanced metaprogramming
facilities to web developers.

## Installation
```lisp
(ql:quickload :parenscript)
```
or download the [latest release](https://common-lisp.net/project/parenscript/release/parenscript-latest.tgz)

## Documentation
* [Reference Manual](https://common-lisp.net/project/parenscript/reference.html)
* [Tutorial](https://common-lisp.net/project/parenscript/tutorial.html)

## What's the difference?
Parenscript is different from almost all other
"language X" to JavaScript translators in that it imposes almost no
overhead:

##### No runtime dependencies
  Any piece of Parenscript code is runnable as-is. There are no 
  JavaScript files to include.


##### Native types
  Parenscript works entirely with native JavaScript datatypes. There
  are no new types introduced, and object prototypes are not
  touched.

##### Native calling convention
  Any JavaScript code can be called without the need for
  bindings. Likewise, Parenscript can be used to make efficient,
  self-contained JavaScript libraries.

##### Readable code
  Parenscript generates concise, formatted, idiomatic JavaScript
  code. Identifier names are preserved. This enables seamless
  debugging in tools like Firebug.

##### Efficiency
  Parenscript introduces minimal overhead for advanced Common Lisp
  features. The generated code is almost as fast as hand-written
  JavaScript.

## Links
* [Web site](http://common-lisp.net/project/parenscript/)
* [Source repository](https://github.com/vsedach/Parenscript)

## License
BSD (see COPYING file)
