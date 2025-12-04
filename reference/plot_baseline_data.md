# Make baseline comparison plot

This function creates a scatterplot of `y_variables` such as raw
amplitude grouped according to treatment. The data are limited to values
from the baseline period, allowing for a quick comparison of baseline
parameters across treatments. For evoked currents, only raw amplitude is
available for comparison. For spontaneous currents, raw amplitude and
raw frequency during the baseline period can be plotted.

## Usage

``` r
plot_baseline_data(
  data,
  current_type = "eEPSC",
  plot_category,
  y_variable = "raw_amplitude",
  include_all_treatments = "yes",
  list_of_treatments = NULL,
  baseline_interval = "t0to5",
  filename_suffix = "",
  large_axis_text = "no",
  included_sexes = "both",
  male_label = "Male",
  female_label = "Female",
  plot_width = 8,
  treatment_colour_theme,
  theme_options,
  save_plot_png = "no",
  ggplot_theme = patchclampplotteR_theme()
)
```

## Arguments

- data:

  A dataframe containing the summary data generated from
  [`make_summary_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_summary_EPSC_data.md).
  If `current_type` is "eEPSC", this must be the `$summary_data` element
  of the list produced by
  [`make_summary_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_summary_EPSC_data.md).

- current_type:

  A character describing the current type. Allowed values are `"eEPSC"`
  or `"sEPSC"`.

- plot_category:

  A numeric value specifying the category, which can be used to
  differentiate different protocol types. In the sample dataset for this
  package, `plot_category == 2` represents experiments where insulin was
  applied continuously after a 5-minute baseline period.

- y_variable:

  A character describing the parameter used on the y-axis. If
  `current_type == "eEPSC"`, the allowed y_variable is
  `"raw_amplitude"`. If `current_type == "sEPSC"`, the allowed
  y_variables are `"raw_amplitude"` or `"raw_frequency"`. Note: It does
  not make sense to use normalized/baseline transformed amplitudes,
  since these will all be 100, and the graph will be a flat line.

- include_all_treatments:

  A character (`"yes"` or `"no"`) specifying if the plot will include
  data from all treatments. If `"no"`, you must specify a list of
  treatments in `list_of_treatments`.

- list_of_treatments:

  A list of character values describing the treatments that will be in
  the plot. Defaults to `NULL`, since include_all_treatments is `"yes"`
  by default.

- baseline_interval:

  A character value indicating the name of the interval used as the
  baseline. Defaults to `"t0to5"`, but can be changed. Make sure that
  this matches the baseline interval that you set in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- filename_suffix:

  Optional character value to add a suffix to the filename of the .png
  file created with this plot. Could be useful if you have specified a
  custom list of treatments.

- large_axis_text:

  A character (`"yes"` or `"no"`). If `"yes"`, a ggplot theme layer will
  be applied which increases the axis text.

- included_sexes:

  A character value (`"both"`, `"male"` or `"female"`). Useful if you
  want to have a plot with data from one sex only. Defaults to `"both"`.
  If you choose a single sex, the resulting plot will have
  `"-males-only"` or `"-females-only"` in the file name.

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

- plot_width:

  A numeric value specifying the width of the plot. Defaults to 8, but
  you will need to adjust this depending on how many treatments you
  have.

- treatment_colour_theme:

  A dataframe containing treatment names and their associated colours as
  hex values. See
  [sample_treatment_names_and_colours](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_treatment_names_and_colours.md)
  for an example of what this dataframe should look like.

- theme_options:

  A dataframe containing theme options, defaults to
  `sample_theme_options`. See
  [sample_theme_options](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_theme_options.md)
  for an example of what this dataframe should look like and how you can
  customize these values.

- save_plot_png:

  A character (`"yes"` or `"no"`). If `"yes"`, the plot will be saved as
  a .png using `ggsave()`. The filepath depends on the current type, but
  they will all go in subfolders below `Figures/` in your project
  directory.

- ggplot_theme:

  The name of a ggplot theme or your custom theme. This will be added as
  a layer to a ggplot object. The default is
  [`patchclampplotteR_theme()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/patchclampplotteR_theme.md),
  but other valid entries include `theme_bw()`, `theme_classic()` or the
  name of a custom ggplot theme stored as an object.

## Value

A ggplot object. If `save_plot_png == "yes"`, it will also generate a
.png file in `Figures/Evoked-currents/Output-summary-plots` or
`Figures/Spontaneous-currents/Output-summary-plots`. The .png filename
will contain the `y_variable`.

## Examples

``` r
plot_baseline_data(
  data = sample_summary_eEPSC_df$summary_data,
  current_type = "eEPSC",
  plot_category = 2,
  y_variable = "raw_amplitude",
  include_all_treatments = "yes",
  list_of_treatments = NULL,
  baseline_interval = "t0to5",
  included_sexes = "both",
  large_axis_text = "no",
  plot_width = 8,
  treatment_colour_theme = sample_treatment_names_and_colours,
  theme_options = sample_theme_options,
  save_plot_png = "no"
)
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_segment()`).
```
