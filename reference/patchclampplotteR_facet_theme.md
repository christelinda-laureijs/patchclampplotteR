# The patchclampplotteR theme for facet plots

This is a modified version of
[`patchclampplotteR_theme()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/patchclampplotteR_theme.md)
that is optimized for plots produced with
[`make_facet_plot()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_facet_plot.md).
It features large, easy-to-read axis labels and titles for individual
facets, and margins that allow for easier readability.

## Usage

``` r
patchclampplotteR_facet_theme(font_family = NULL)
```

## Arguments

- font_family:

  A character value describing the font family used for all text in the
  ggplot (axis text, axis titles, plot title, etc.). This must be a
  loaded font in R, which can be facilitated by loading packages like
  `extrafont` at the start of your document.

## Value

A ggplot theme

## See also

[`patchclampplotteR_theme()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/patchclampplotteR_theme.md)
for the original theme and
[`make_facet_plot()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_facet_plot.md).

## Examples

``` r
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(displ, cty)) +
  ggplot2::facet_grid(cols = ggplot2::vars(cyl)) +
  ggplot2::geom_point() +
  patchclampplotteR_facet_theme()

```
