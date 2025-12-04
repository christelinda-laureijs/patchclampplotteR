# Get figure height

This function produces a dataframe with the ideal figure height for the
facet plots produced by
[`make_facet_plot()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_facet_plot.md).
These facet plots have a three-column layout with varying numbers of
rows. This function first determines the number of rows required for a
three-column layout, and then determines the required figure height.
Typically, this is four times the number of rows.

## Usage

``` r
get_fig_height(data, plot_category, plot_treatment, plot_sex)
```

## Arguments

- data:

  A dataframe containing the raw evoked current data generated from
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).
  If `pruned = "yes"` you must use the `$individual_cells` component of
  a pruned dataset.

- plot_category:

  A numeric value specifying the category, which can be used to
  differentiate different protocol types. In the sample dataset for this
  package, `plot_category == 2` represents experiments where insulin was
  applied continuously after a 5-minute baseline period.

- plot_treatment:

  A character value specifying the treatment you would like to plot
  (e.g. `"Control"`). `plot_treatment` represents antagonists that were
  present on the brain slice, or the animals were fasted, etc.

- plot_sex:

  A character value ("Male" or "Female") corresponding to the sex you
  would like to plot.

## Value

A dataframe with a numerical value indicating the ideal figure height
for each combination of category, treatment and sex.

## Details

This function is ideal if combined with the chunk options of an
RMarkdown document. Set `fig.width = 14` and
`fig.height = get_fig_height(...)`. Replace the `...` with the correct
category, treatment and sex you'd like to plot. Plot only one facet plot
per chunk.

## See also

[`make_facet_plot()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_facet_plot.md)

## Examples

``` r
get_fig_height(
  data = sample_raw_eEPSC_df,
  plot_category = 2,
  plot_treatment = "Control",
  plot_sex = "Male"
)
#> [1] 7
```
