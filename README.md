
<!-- README.md is generated from README.Rmd. Please edit that file -->
Rd
==

The goal of Rd is to extend the base utilities in the `tools` and `utils` package to facilitate creation, checking and manipulation of R documentation (Rd) objects in code.

Installation
------------

You can install the released version of Rd from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("Rd")
```

Examples
--------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code coming soon.
```

Details and Internals
=====================

Differences from core R
-----------------------

Rd Objects are composed of character strings and lists of character strings or nested lists ultimately ending in character strings. The primary tool for reading in Rd object is the `tools::parse_Rd()` function. This will return a list with the base object being an `Rd` object but none of the elements contained are classed. The `Rd` package extends this with providing more methods and functions for handling these. In `Rd` all objects are S3 classed, including tags, text, code, and comments.

Classes
-------

### `Rd`

This exists in the base R and is what is returned by `Rd_text('test')`. The `Rd` package adds S3 Methods `[[`, `[`, `c`, and `format`. `Rd` objects must be lists to be valid.

### `Rd_string`

The `Rd` text is a character string, a length one non-empty character vector. It contains several different categories of text which is specified in the `Rd_tag` attribute:

-   `"TEXT"` - The generic text type
-   `"RCODE"` - Tagged as R code, but note that this should be wrapped by an appropriate `Rd_tag` object described below.
-   `"VERB"` - Symbols such as function names.
-   `"COMMENT"` - Comments in Rd, each should lead with the comment character `'%'`.
-   `"UNKNOWN"` - If it cannot be categorized.

It should be noted that the different types of `Rd_string` is for symantic identification, but for printing and formatting all are returned verbatim.

### `Rd_tag`

The `Rd_tag` objects are converted to the latex like `\tag` commands when formatted for printing or writing files. The tag to be used is stored in the `Rd_tag` attribute.
