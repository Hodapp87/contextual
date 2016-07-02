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

## Examples

These are all generated from `Test.hs`.

![HSL spiral](testHSL.png)

![HSL spiral](testHSL2.png)

![Sierpinski (ish)](sierpinski.png)

![Not Sierpinski](notSierpinski.png)

## Wish-list for Contextual

* Travis CI build
* A way to specify canvas size.  Right now, non-square images will
throw off the aspect ratio - squares will be drawn as rectangles.
* Support for other primitives: circle, line, arc
* Some diagnostic information in `Context` such as the number of
primitives or average depth.
* Stopping rendering on grammars that don't converge (e.g. limiting
recursion depth or number of primitives)
* Support for separate stroke and fill, and perhaps thickness
* Better colorspace than "plain" RGB.
[colour](https://hackage.haskell.org/package/colour) can probably help
here.  Alongside this: A saner way of specifying colors.
* Support for exporting SVG without needing something as heavy as
Cairo (perhaps
[blaze-svg](https://hackage.haskell.org/package/blaze-svg)).
* Use of Data.Reify to transform recursive structures, perhaps to
backends that can express recursion natively or to a simplified
expression
* Integration with [IHaskell](https://github.com/gibiansky/IHaskell)
and [Jupyter](http://jupyter.org/).  Perhaps I can use the mechanism
that
[ihaskell-charts](https://hackage.haskell.org/package/ihaskell-charts)
uses, which looks like it ties in with
[Chart-cairo](https://hackage.haskell.org/package/Chart-cairo).  Some
starting points on that:
    * [Gtk2hs Tutorial](http://www.muitovar.com/gtk2hs/app1.html),
    * [Beautiful Code](http://www.renci.org/wp-content/pub/tutorials/BeautifulCode.pdf),
    * [ihaskell-diagrams](https://github.com/gibiansky/IHaskell/blob/1b6d9081f2109fd50dcdbaebe9dbad1676a01d78/ihaskell-display/ihaskell-diagrams/IHaskell/Display/Diagrams.hs)
* Some magic with [ghcjs](https://github.com/ghcjs/ghcjs) (looks like
[stack](http://docs.haskellstack.org/en/stable/ghcjs/) supports it) to
allow this to run in, and render in, the browser
([Canvas](https://github.com/ghcjs/ghcjs-base/tree/master/JavaScript/Web/Canvas)?
SVG?)
* Separate modules for separate backends (as many other libraries do)
* Perhaps rewriting using a simpler form of
[Free](https://hackage.haskell.org/package/free/docs/Control-Monad-Free.html)
that just uses the parts I need
* Support for animation?
* Some optimization for the use of Cairo, e.g. if we are rendering a
big scene to a raster image, then doing it in layers of N primitives
may make sense to avoid building up huge scene graphs.
* Perhaps factor out `Context` since much of it will be repeated in
other backends, particularly anything that does a more immediate-mode
drawing and lacks the `save`/`restore` of Cairo.
* Am I using
[Comonad](http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html)
implicitly?  Should I be using it explicitly?
