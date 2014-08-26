Backend Tests
=============

This is a simple test framework for diagrams.
To test diagrams, first configure

% cabal configure -fcairo

Supported backends are: cairo, svg, postscript.  canvas (HTML5) used
to be supported and hopefully will be again soon. You can also do

% cabal configure -fall

to make an executable called "all-test" which will test all backends
side-by-side. Then build:

% cabal build

Then run:

% ./dist/build/cairo-test/cairo-test

This generates a file called cairo-index.html (or svg-index.html, etc).
Open this in your browser.

% open cairo-index.html

That's it!

Adding tests
------------

Edit src/Diagrams/Tests.hs, and add to the examples list. Feel free to use
helper functions inside the Tests.hs file. The name of the test should be
a (UNIX) filename (no spaces, no slashes, no stars, etc)

Adding backends
---------------

To add a new backend, see the templates in *cabal,
src/Diagrams/Test/*, and tests/*hs.

Specifically:
 * Add a new -f<FOO> option and Executable <FOO>-test in diagrams-tests.cabal
 * Add a src/Diagrams/Test/<FOO>.hs
 * Add a <FOO>.hs in tests/ which just calls out to the above.
 * Add <FOO> to tests/All.hs

Now you can use

% cabal configure -f<FOO>
