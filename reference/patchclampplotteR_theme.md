# Add a customized ggplot2 theme

This is an attractive ggplot2 theme readily available to use with any
plots when `patchclampplotteR` is loaded. It is built on top of
`theme_classic()` and features bold and easy to read axis titles,
well-spaced margins, and a clean layout.

## Usage

``` r
patchclampplotteR_theme(font_family = NULL)
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

[`patchclampplotteR_facet_theme()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/patchclampplotteR_facet_theme.md)
to use with
[`make_facet_plot()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_facet_plot.md).

## Examples

``` r
ggplot2::ggplot(cars, ggplot2::aes(x = speed, y = dist)) +
  ggplot2::geom_point() +
  ggplot2::labs(x = "Speed (mph)", y = "Stopping Distance (ft)") +
  patchclampplotteR_theme()

```
