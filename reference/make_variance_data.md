# Make dataframe with variance measures

`make_variance_data` creates a dataframe containing variance measures at
two time points. They are the baseline period and a user-specified
interval after a hormone or protocol has been applied. The variance
measures are the inverse coefficient of variation squared and the
variance-to-mean ratio (VMR). A `"before vs. after"` comparison of these
two variance measures is useful to determine which mechanism is involved
in modifying synaptic plasticity. For more information, please see
[Huijstee & Kessels
(2020)](https://doi.org/10.1016/j.jneumeth.2019.108526).

## Usage

``` r
make_variance_data(
  data,
  include_all_categories = "yes",
  list_of_categories = NULL,
  include_all_treatments = "yes",
  list_of_treatments = NULL,
  baseline_interval = "t0to5",
  post_hormone_interval = "t20to25",
  treatment_colour_theme,
  save_output_as_RDS = "no"
)
```

## Arguments

- data:

  A dataframe containing the summary data generated from
  [`make_summary_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_summary_EPSC_data.md).
  If `current_type` is "eEPSC", this must be the `$summary_data` element
  of the list produced by
  [`make_summary_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_summary_EPSC_data.md).

- include_all_categories:

  A character (`"yes"` or `"no"`) specifying if the plot will include
  data from all categories. If `"no"`, you must specify a list of
  categories in `list_of_categories`.

- list_of_categories:

  A list of character values describing the categories that will be in
  the plot. Defaults to `NULL`, since `include_all_categories` is
  `"yes"` by default.

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

- post_hormone_interval:

  A character value indicating the name of the interval used as "after"
  timepoint for comparison. Defaults to `"t20to25"`, but can be changed.
  Make sure that this matches an interval present in `data`

- treatment_colour_theme:

  A dataframe containing treatment names and their associated colours as
  hex values. See
  [sample_treatment_names_and_colours](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_treatment_names_and_colours.md)
  for an example of what this dataframe should look like.

- save_output_as_RDS:

  A character (`"yes"` or `"no"`) describing if the resulting object
  should be saved as an RDS file in the folder
  `"Data/Output-Data-from-R"`. The function will automatically create
  this folder if it doesn't already exist. Note: This is not the
  interactive table, but it is the raw dataframe that is later inserted
  into
  [`reactable::reactable()`](https://glin.github.io/reactable/reference/reactable.html).
  This is useful if you want to build your own table using a different
  package, or you want to generate a customized reactable table
  yourself.

## Value

A dataframe containing all of the columns within the summary data (see
[sample_summary_eEPSC_df](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_summary_eEPSC_df.md)
for a detailed description of these columns) plus three additional
columns:

- `state` A character value describing if a data point belongs to the
  baseline interval (`"Baseline"`) or an interval after a hormone or
  protocol has been applied (`"Post-modification"`). These intervals are
  selected from `baseline_interval` and `post_hormone_interval`.

- `mean_cv_inverse_square` The mean inverse coefficient of variation
  squared within a specific state and sex.

- `mean_VMR` The mean variance-to-mean ratio within a specific state and
  sex.

## See also

[`plot_variance_comparison_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_variance_comparison_data.md)
to plot this data.

## Examples

``` r
make_variance_data(
  data = sample_summary_eEPSC_df$summary_data,
  include_all_categories = "no",
  list_of_categories = c(2),
  include_all_treatments = "yes",
  list_of_treatments = NULL,
  baseline_interval = "t0to5",
  post_hormone_interval = "t20to25",
  treatment_colour_theme = sample_treatment_names_and_colours,
  save_output_as_RDS = "no"
)
#> # A tibble: 38 × 25
#> # Groups:   category, treatment, state, sex [16]
#>    category letter sex    treatment interval mean_P1_transformed mean_P1_raw
#>    <fct>    <fct>  <fct>  <fct>     <fct>                  <dbl>       <dbl>
#>  1 2        AO     Male   Control   t0to5                 100          36.8 
#>  2 2        AO     Male   Control   t20to25                21.3         7.82
#>  3 2        AZ     Female Control   t0to5                 100          44.3 
#>  4 2        AZ     Female Control   t20to25                50.7        22.4 
#>  5 2        BN     Male   Control   t0to5                 100          72.1 
#>  6 2        BN     Male   Control   t20to25                19.1        13.8 
#>  7 2        L      Male   Control   t0to5                 100          77.0 
#>  8 2        L      Male   Control   t20to25                 5.75        4.43
#>  9 2        BO     Male   HNMPA     t0to5                 100          87.7 
#> 10 2        BO     Male   HNMPA     t20to25                24.3        21.4 
#> # ℹ 28 more rows
#> # ℹ 18 more variables: n <dbl>, sd <dbl>, cv <dbl>, se <dbl>,
#> #   cv_inverse_square <dbl>, variance <dbl>, VMR <dbl>, age <dbl>,
#> #   animal <dbl>, X <dbl>, Y <dbl>, time <dbl>, synapses <fct>,
#> #   days_alone <fct>, animal_or_slicing_problems <fct>, state <chr>,
#> #   mean_cv_inverse_square <dbl>, mean_VMR <dbl>
```
