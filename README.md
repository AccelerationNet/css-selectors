# css-selectors: a jquery-style dom query language

CSS-selectors is a query language for finding specific nodes in cxml
dom documents and dom sub trees.

## API

### query

```
 (query 
   (or css-selector-string match-fn)
   (or nodes *document*))
```

Similar to jQuery('css-selector', NodeList or *document*)

Returns a list of matching nodes. 

if the first argument is a css-selector string, compile it (as with compile-css-node-matcher)
into a matcher function and call query again with it

if the first argument is a function, it is assumed to take a single node
and to test whether or not that node matches the css-selector match function

If the css-selector-string is constantp, it will be compiled at compile time

The second argument is either a dom-node, a list of dom-nodes or the a dom document.  
It defaults to buildnode:*document*.

For full examples, please see the test suite, but a basic query call looks like:
```
(css-selectors:query "a" document) 
```
which will return a list of all "a" tags in the document.

### node-matches?

```
 (node-matches? node-node (or match-fn css-selector-string))
```

if the second argument is a css-selector, compile it (as with compile-css-node-matcher)
into a matcher function and call node-matches again with it

if the second argument is a function, it is assumed to take a single node
and to test whether or not that node matches the css-selector match function

If the css-selector-string is constantp, it will be compiled at compile time

### parse-results

```
 (parse-results inp)
```

Parses a css3 selector into an AST representing the selector expression.
Probably only useful for debugging or implementing a different compiler.  

Uses clppcre and *VERY* basic flex parser (defined in parse.lisp) to
create a lexer.  Combines that lexer with a parser generated by
cl-yacc and runs the input through the parse.

### compile-css-node-matcher

```
 (compile-css-node-matcher inp)
```

Turns a parse-tree or css-selector-string into a compiled lambda
matcher function this matcher function can be used with query and
node-matches?.  The matcher function takes two arguments

### Pseudo Selectors 

#### Implementing new pseudo selectors

Any function defined in the :pseudo (:css-selectors.pseudo) will be
available as a pseudo selector (see pseudo.lisp for examples).

All pseudo selectors should be of the following format:
(defun pseudo:MY-NEW-PSEUDO (node &optional sub-sel-function) ...)

If your pseudo selector has no args, then it should signal an error if
a sub-selction-function is passed to it and vice-versa


## Authors
 * [Acceleration.net](http://www.acceleration.net/) [Donate](http://www.acceleration.net/programming/donate-to-acceleration-net/)
  * [Russ Tyndall](http://russ.unwashedmeme.com/blog)
  * [Nathan Bird](http://the.unwashedmeme.com/blog)
  * [Ryan Davis](http://ryepup.unwashedmeme.com/blog)


```
;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
