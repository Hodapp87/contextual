# Contextual

This is yet another rewriting of my software for drawing algorithmic
images based on a context-free grammar, eventually a nondeterministic
one.

This was inspired by the wonderful
[Context Free](http://www.contextfreeart.org/).  Over the course of
the past few years, I rewrote bits of Context Free in other languages
as practice (at least, that's what I tell myself in hindsight):

* Scala: https://github.com/Hodapp87/scala_cf3
* Clojure: https://github.com/Hodapp87/contextual_clojure
* JavaScript, HTML5, Canvas/SVG:
  https://github.com/Hodapp87/html5-dabbling/tree/master/contextual

I'd highly recommend not looking at the code for any of those, since
most of it's an ill-maintained mess and proof-of-concept.  I started
this version in late 2013 when I first started learning Haskell, so
the code is probably also atrocious at the moment.

The name *contextual* is something of a pun on Context Free.  It might
change later when I'm feeling more imaginative.

## Wish-list for Contextual

* To actually work right.
* Support for [Cairo](https://hackage.haskell.org/package/cairo).
* Integration with [IHaskell](https://github.com/gibiansky/IHaskell)
  and [Jupyter](http://jupyter.org/).  Perhaps I can use the mechanism
  that
  [ihaskell-charts](https://hackage.haskell.org/package/ihaskell-charts)
  uses, which looks like it ties in with
  [Chart-cairo](https://hackage.haskell.org/package/Chart-cairo).
* Some magic with [ghcjs](https://github.com/ghcjs/ghcjs) to allow
  this to run in, and render in, the browser.
