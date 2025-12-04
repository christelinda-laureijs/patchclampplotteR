# Visually compare spontaneous current parameters

`plot_spontaneous_current_parameter_comparison()` is a useful function
to see how spontaneous current amplitude or frequency change after
adding a hormone. This function produces sina plots (with raw datapoints
on top) for the raw amplitude (or raw frequency) for two time intervals:
the baseline interval, and a user-specified interval after a hormone or
other modification has been applied.

## Usage

``` r
plot_spontaneous_current_parameter_comparison(
  data,
  plot_category = 2,
  plot_treatment = "Control",
  included_sexes = "both",
  facet_by_sex = "no",
  male_label = "Male",
  female_label = "Female",
  left_sex = "Female",
  y_variable = "raw_amplitude",
  y_variable_signif_brackets = NULL,
  hormone_added = "Insulin",
  baseline_interval = "t0to5",
  post_hormone_interval = "t20to25",
  test_type,
  map_signif_level_values = F,
  geom_signif_family = "",
  geom_signif_text_size = 5,
  large_axis_text = "no",
  treatment_colour_theme,
  mean_line_thickness = 1.2,
  mean_point_size = 2.5,
  geom_signif_size = 0.5,
  theme_options,
  plot_type = "violin",
  save_plot_png = "no",
  filename_suffix = "",
  ggplot_theme = patchclampplotteR_theme()
)
```

## Arguments

- data:

  Summary data for spontaneous currents generated using
  [`make_summary_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_summary_EPSC_data.md)
  where `current_type == "sEPSC"`.

- plot_category:

  A numeric value specifying the category, which can be used to
  differentiate different protocol types. In the sample dataset for this
  package, `plot_category == 2` represents experiments where insulin was
  applied continuously after a 5-minute baseline period.

- plot_treatment:

  A character value specifying the treatment you would like to plot
  (e.g. `"Control"`). `plot_treatment` represents antagonists that were
  present on the brain slice, or the animals were fasted, etc.

- included_sexes:

  A character value (`"both"`, `"male"` or `"female"`). Useful if you
  want to have a plot with data from one sex only. Defaults to `"both"`.
  If you choose a single sex, the resulting plot will have
  `"-males-only"` or `"-females-only"` in the file name. WARNING!! If
  you choose `"male"` or `"female"`, you MUST ensure that the
  `t_test_df` contains data that has been filtered to only include one
  sex. Otherwise, the significance stars will represent both sexes and
  it will be inaccurate.

- facet_by_sex:

  A character value (`"yes"` or `"no"`) describing if the plots should
  be faceted by sex. This is only available if `"included_sexes"` is
  `"both"`. The resulting plot will be split in two, with male data on
  the left and female data on the right.

- male_label:

  A character value used to describe how males are encoded in the `sex`
  column of the dataframe used in `data`. This MUST match the value for
  male data in the `sex` column, and it must be consistent across data
  sheets. Defaults to `"Male"`.

- female_label:

  A character value used to describe how females are encoded in the
  `sex` column of the dataframe used in `data`. This MUST match the
  value for female data in the `sex` column, and it must be consistent
  across data sheets. This must be consistent in all data sheets.
  Defaults to `"Female"`.

- left_sex:

  A character value ("Female" or "Male") describing the sex that will
  appear on the left side of a faceted plot. Only applies if
  `facet_by_sex` is `"yes"`.

- y_variable:

  A character value (`"raw_amplitude"` or `"raw_frequency"`) only.
  Normalized amplitude and frequency are not available because all
  baseline values are 100.

- y_variable_signif_brackets:

  A character value. You should only use this if your data did not pass
  assumptions and you had to transform it. `y_variable_signif_brackets`
  should be the name of the column of `data` which has the transformed
  data (e.g. log-transformed data). Raw data will be plotted, but the
  significance brackets (and t-test/wilcox test) will use the
  transformed data. If you did not transform the data, leave this
  argument blank, and the function will automatically use the correct
  column associated with `y_variable`.

- hormone_added:

  A character value that will be used as the label over the line
  annotating the period when a hormone was applied. Examples include
  `"500 nM Insulin"`, `"CCK + Leptin"`, and `"Insulin"`. If you applied
  a high-frequency stimulation (HFS) protocol instead, write "HFS", and
  an annotation arrow will be added instead.

- baseline_interval:

  A character value indicating the name of the interval used as the
  baseline. Defaults to `"t0to5"`, but can be changed. Make sure that
  this matches the baseline interval that you set in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- post_hormone_interval:

  A character value specifying the interval used for the data points
  after a hormone or protocol was applied. This must match the
  `post_hormone_interval` used in
  [`make_variance_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_variance_data.md).

- test_type:

  A character (must be `"wilcox.test"`, `"t.test"` or `"none"`)
  describing the statistical model used to create a significance bracket
  comparing the pre- and post-hormone groups.

- map_signif_level_values:

  A `TRUE/FALSE` value or a list of character values for mapping
  p-values. If `TRUE`, p-values will be mapped with asterisks (e.g. \\
  for p \< 0.05, for p \< 0.01). If `FALSE`, raw p-values will display.
  You can also insert a list of custom mappings or a function. For
  example, use
  `map_signif_level_values = function(p) if (p < 0.1) {round(p, 3)} else {"ns"}`
  to only display the p-values when they are below 0.1.

- geom_signif_family:

  A character value describing the font family used for the p-value
  annotations used by
  [`ggsignif::geom_signif()`](https://const-ae.github.io/ggsignif/reference/stat_signif.html).
  Defaults to `""` (empty value, will be replaced with default system
  font), but can be replaced with a named font. Use a package like
  `extrafont` to load system fonts into R.

- geom_signif_text_size:

  A numeric value describing the size of the text annotations
  (significance stars or p-values) on the plot. Defaults to `8`.

- large_axis_text:

  A character (`"yes"` or `"no"`). If `"yes"`, a ggplot theme layer will
  be applied which increases the axis text.

- treatment_colour_theme:

  A dataframe containing treatment names and their associated colours as
  hex values. See
  [sample_treatment_names_and_colours](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_treatment_names_and_colours.md)
  for an example of what this dataframe should look like.

- mean_line_thickness:

  A numeric value describing the thickness of the line used to indicate
  the mean for a group. Defaults to `1.2`.

- mean_point_size:

  A numeric value describing the size of the points used to indicate the
  means. Defaults to `2.5`.

- geom_signif_size:

  A numeric value describing the size of the `geom_signif` bracket size.
  Defaults to `0.5`, which is a good thickness for most applications.

- theme_options:

  A dataframe containing theme options. See
  [sample_theme_options](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_theme_options.md)
  for an example of what this dataframe should look like.

- plot_type:

  A character value (`"violin"` or `"lineplot"`) which determines how
  the data are plotted. This can be a pale gray violin plot with a sina
  plot overlay, or a lineplot where "before" and "after" values for each
  cell are connected with a line.

- save_plot_png:

  A character (`"yes"` or `"no"`). If `"yes"`, the plot will be saved as
  a .png using ggsave. The filepath depends on the current type, but
  they will all go in subfolders below `Figures/` in your project
  directory.

- filename_suffix:

  Optional character value to add a suffix to the filename of the .png
  file created with this plot. Could be useful if you want to specify
  anything about the data (for example, to distinguish between
  recordings produced in MiniAnalysis vs. Clampfit).

- ggplot_theme:

  The name of a ggplot theme or your custom theme. This will be added as
  a layer to a ggplot object. The default is
  [`patchclampplotteR_theme()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/patchclampplotteR_theme.md),
  but other valid entries include `theme_bw()`, `theme_classic()` or the
  name of a custom ggplot theme stored as an object.

## Value

A ggplot object

## Details

If you specify a `test_type`, the function will perform a paired t-test
or paired wilcox test and add brackets with significance stars through
[`ggsignif::geom_signif()`](https://const-ae.github.io/ggsignif/reference/stat_signif.html).

## Examples

``` r
plot_spontaneous_current_parameter_comparison(
  data = sample_summary_sEPSC_df$summary_data,
  plot_category = 2,
  plot_treatment = "Control",
  y_variable = "raw_amplitude",
  included_sexes = "both",
  facet_by_sex = "no",
  hormone_added = "Insulin",
  baseline_interval = "t0to5",
  post_hormone_interval = "t20to25",
  test_type = "wilcox.test",
  large_axis_text = "no",
  treatment_colour_theme = sample_treatment_names_and_colours,
  save_plot_png = "no",
  theme_options = sample_theme_options
)


# Facet by sex

# NOTE: To reduce the package file size, there is only one data point
# per sex for the sEPSC data. This is why there are error messages in the example.
# The plot will not have these errors if you have more than one data point
# per sex.

plot_spontaneous_current_parameter_comparison(
  data = sample_summary_sEPSC_df$summary_data,
  plot_category = 2,
  plot_treatment = "Control",
  y_variable = "raw_amplitude",
  included_sexes = "both",
  facet_by_sex = "yes",
  hormone_added = "Insulin",
  baseline_interval = "t0to5",
  post_hormone_interval = "t20to25",
  test_type = "wilcox.test",
  large_axis_text = "no",
  treatment_colour_theme = sample_treatment_names_and_colours,
  save_plot_png = "no",
  theme_options = sample_theme_options
)
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Computation failed in `stat_ydensity()`.
#> Caused by error in `$<-.data.frame`:
#> ! replacement has 1 row, data has 0
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Computation failed in `stat_ydensity()`.
#> Caused by error in `$<-.data.frame`:
#> ! replacement has 1 row, data has 0
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_segment()`).

```
