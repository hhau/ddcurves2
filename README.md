`ddcurves2`: depth density curve models in stan
================
Andrew Manderson

This is a private R package to enable the faster iteration of Stan models and outputs. Limited current functionality.

(The above comment is really outdated, I've removed most of the old models to speed up compilation time.)

Installation notes
==================

This package is an absolute pain to build, it is finicky and brittle.

Install via `devtools::install_github("hhau/ddcurves2", local=FALSE)`

If you don't install / build with `local = FALSE`, the compiler gets almightily confused upon reinstall
