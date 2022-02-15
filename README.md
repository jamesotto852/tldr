
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tldr

<!-- badges: start -->
<!-- badges: end -->

The goal of tldr is to provide short-form documentation in the console:

![tldr-ex-1](man/README-gifs/tldr-tldr.gif)

Included are RStudio addins which, when bound to keyboard shortcuts,
facilitate efficient access to **tldr** style documentation:

![tldr-ex-2](man/README-gifs/tldrExample-divide.gif)

# Writing **tldr** documentation

This package includes a Roxygen2 extension which allows for the creation
of **tldr** documentation files from Roxygen skeletons. This is done via
`tldr_roclet()` and custom Roxygen tags `@paramtldr` and `@exampletldr`.
Once the system for documentation is more stable, we will be writing a
guide on its use for other developers. For an example of what a package
with **tldr**-style documentation looks like, there is another package,
<a href="https://Github.com/jamesotto852/tldrExample">**tldrExample**</a>,
which exports several simple functions, with Roxygen skeletons that
produce **tldr** documentation.

## Installation

**tldr** is in an experimental stage and is not ready to be used as a
tool by R users and developers. That being said, if you are interested
in installing **tldr** you can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jamesotto852/tldr")
```
