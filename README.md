# Backend Tests

The diagrams-backend-tests module contains a number of simple tests to
check that the backend is basically working. The results are displayed
in a html document that compares the output from the chosen backend to
the referene images. It turns out that this is not a trivial problem to
say whether a vector image is "close enough" to a reference image.
Simply comparing the individual pixels doesn't give a reliable enough
result. So for now all tests have to checked by eye.

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
