# Plot action potential frequency curves for multiple treatments

This function allows you to generate a plot of action potential
frequency (y-axis) for each current injection (x-axis), coloured by
treatment. The linetype indicates which state the data belong to (a
recording taken during the baseline or after a hormone or treatment).

## Usage

``` r
plot_AP_frequencies_multiple_treatments(
  data,
  include_all_treatments = "yes",
  list_of_treatments = NULL,
  plot_category = 2,
  included_sexes = "both",
  male_label = "Male",
  female_label = "Female",
  treatment_colour_theme,
  filename_suffix = "",
  save_plot_png = "no",
  ggplot_theme = patchclampplotteR_theme()
)
```

## Arguments

- data:

  Action potential frequency data imported through
  [`add_new_cells()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/add_new_cells.md)
  with `data_type == "AP_count"`

- include_all_treatments:

  A character (`"yes"` or `"no"`) specifying if the plot will include
  data from all treatments. If `"no"`, you must specify a list of
  treatments in `list_of_treatments`.

- list_of_treatments:

  A list of character values describing the treatments that will be in
  the plot. Defaults to `NULL`, since include_all_treatments is `"yes"`
  by default.

- plot_category:

  A numeric value specifying the category, which can be used to
  differentiate different protocol types. In the sample dataset for this
  package, `plot_category == 2` represents experiments where insulin was
  applied continuously after a 5-minute baseline period.

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

- treatment_colour_theme:

  A dataframe containing treatment names and their associated colours as
  hex values. See
  [sample_treatment_names_and_colours](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_treatment_names_and_colours.md)
  for an example of what this dataframe should look like.

- filename_suffix:

  Optional character value to add a suffix to the filename of the .png
  file created with this plot. Could be useful if you have specified a
  custom list of treatments.

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
.png file in the folder `Figures/Action-potentials` relative to the
project directory.

## Examples

``` r
plot_AP_frequencies_multiple_treatments(
  data = sample_AP_count_data,
  include_all_treatments = "yes",
  plot_category = 2,
  included_sexes = "both",
  treatment_colour_theme = sample_treatment_names_and_colours
)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

```
