# Backend Tests

This is a simple test framework for diagrams.

To run a test do

```
cabal new-run test-rasterific
```

replacing `rasterific` with the test you want to run. You can check the
output in `rasterific.html`.

## Dependencies

To the backend's output png we use ghostscript (`gs`) to pdfs,
ImageMagick (`convert`) for postscript and librsvg (`rsvg-convert`) for
svgs.
