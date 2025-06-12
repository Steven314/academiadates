
<!-- README.md is generated from README.Rmd. Please edit that file -->

# academiadates <a href="https://steven314.github.io/academiadates/"><img src="man/figures/logo.png" align="right" height="120" alt="academiadates website" /></a>

<!-- badges: start -->

<!-- badges: end -->

The goal of `{academiadates}` is to extend `{tsibble}` by adding a new
class which is built to assist handling date intervals that are common
in academic institutions, that being three terms annually.

`{lubridate}` has [`quarter()` and
`semester()`](https://lubridate.tidyverse.org/reference/quarter.html)
methods to access the quarter or semester from a date. This lacks the
functionality for a variable number of periods per year which was
proposed in
[`tidyverse/lubridate#1134`](https://github.com/tidyverse/lubridate/issues/1134),
but has not received any further work.

`{tsibble}` has support for
[quarters](https://tsibble.tidyverts.org/reference/year-quarter.html) in
its ecosystem. It also provides
[scales](https://tsibble.tidyverts.org/reference/tsibble-scales.html)
which wrap [ggplot2’s date
scales](https://ggplot2.tidyverse.org/reference/scale_date.html). These
can be seen throughout [*Forecasting: Principles and
Practice*](https://otexts.com/fpp3/) by Hyndman and Athanasopoulos. The
first example of a quarter timescale is in [Figure
1.1](https://otexts.com/fpp3/data-methods.html#data-methods).

## Installation

You can install the development version of `{academiadates}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Steven314/academiadates")

# or

# install.packages("remotes")
remotes::install_github("Steven314/academiadates")
```

To get started check out `vignette("intro")`.

## Related Resources

Some adjacent GitHub issues and SO content that covers some
brainstorming and work surrounding extending `{tsibble}`:

- Stack Overflow answer: [R tsibble add support of custom
  index](https://stackoverflow.com/questions/65846176/r-tsibble-add-support-for-custom-index)
- [`tidyverse/lubridate#1134`](https://github.com/tidyverse/lubridate/issues/1134)
- [`tidyverts/tsibble#316`](https://github.com/tidyverts/tsibble/issues/316)
- [`tidyverts/tsibble#207`](https://github.com/tidyverts/tsibble/issues/207)
