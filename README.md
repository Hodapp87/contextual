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
* Support for randomness.
* Current work on free monad representation:
    * Add other primitives
    * Get simple exporter for something like SVG
* Use of Data.Reify to transform recursive structures, perhaps to
  backends that can express recursion natively.
* Support for [Cairo](https://hackage.haskell.org/package/cairo-0.12.3/docs/Graphics-Rendering-Cairo.html).
* Integration with [IHaskell](https://github.com/gibiansky/IHaskell)
  and [Jupyter](http://jupyter.org/).  Perhaps I can use the mechanism
  that
  [ihaskell-charts](https://hackage.haskell.org/package/ihaskell-charts)
  uses, which looks like it ties in with
  [Chart-cairo](https://hackage.haskell.org/package/Chart-cairo).
    * Starting points on that:
      [Gtk2hs Tutorial](http://www.muitovar.com/gtk2hs/app1.html),
      [Beautiful Code](http://www.renci.org/wp-content/pub/tutorials/BeautifulCode.pdf),
      [ihaskell-diagrams](https://github.com/gibiansky/IHaskell/blob/1b6d9081f2109fd50dcdbaebe9dbad1676a01d78/ihaskell-display/ihaskell-diagrams/IHaskell/Display/Diagrams.hs)
* Some magic with [ghcjs](https://github.com/ghcjs/ghcjs) to allow
  this to run in, and render in, the browser.
* Separate modules for backends (as many other libraries do)
* Perhaps rewrite using a simpler form of 'Free' that just uses the
parts I need.
