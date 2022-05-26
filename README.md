
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tldr

<!-- badges: start -->
<!-- badges: end -->

The goal of **tldr** is to provide short-form documentation in the
console, styled with
<a href = "https://github.com/r-lib/cli">**cli**</a>:

![tldr-ex-1](man/README-gifs/tldr-tldr.gif)

Included are RStudio addins which, when bound to keyboard shortcuts,
facilitate efficient access to **tldr** documentation:

![tldr-ex-2](man/README-gifs/tldrExample-divide.gif)

**tldr** is inspired by <a href="https://tldr.sh/">tldr-pages</a>, a
project which provides simpler documentation for command-line tools.

## Installation

If you are interested in installing **tldr** you can install the
development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jamesotto852/tldr")
```

## **tldr** documentation for objects in base R

We have provided documentation for for commonly used functions included
in the base distribution of R in
<a href="https://Github.com/jamesotto852/tldrDocs">**tldrDocs**</a>,
which you can install via:

``` r
remotes::install_github("jamesotto852/tldrDocs")
```

## Writing **tldr** documentation

This package includes a Roxygen2 extension which allows for the creation
of **tldr** documentation files from Roxygen skeletons. This is done via
`tldr_roclet()` and custom Roxygen tags such as `@paramtldr` and
`@exampletldr`. Once the system for documentation is more stable, we
will be writing a guide on its use for other developers. For an example
of what a package with **tldr**-style documentation looks like, there is
another package:
<a href="https://Github.com/jamesotto852/tldrExample">**tldrExample**</a>,
which exports several simple objects with Roxygen skeletons that produce
**tldr** documentation.

## How it works

`tldr_roclet()` creates .Rd files based on relevant tags in the Roxygen
skeleton (including the new `@paramtldr` and `@exampletldr`). These .Rd
files are written to the `/inst/tldr/` directory. Once the package is
installed, `tldr_help()` and `tldr_package()` are able to find the
relevant files which are turned into console output by `tldr()`.

This process is based on the `help()` function—for those who are
interested, I have a
<a href="https://jamesotto852.github.io/Understanding-base-documentation-functions">blog
post</a> in which I go through how `?` and `help()` work in detail.
