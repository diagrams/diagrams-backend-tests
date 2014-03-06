cabal configure -fall \
  && cabal build \
  && ./dist/build/all-test/all-test \
  && rsync -rcz cairo postscript svg ref all-index.html byorgey@projects.haskell.org:/srv/projects/diagrams/backend-tests
