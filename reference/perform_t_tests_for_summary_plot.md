# Perform t-tests (or Wilcoxon tests) for EPSC summary plots

This function enables you to perform a series of paired t-tests (or
Wilcoxon tests) comparing the mean current amplitude within each
interval relative to the mean current amplitude during the baseline.
This uses the `pairwise_t_test` or `pairwise_wilcox_test` functions from
`rstatix`, with `paired = TRUE` and Holm's adjustment for multiple
comparisons (`p_adjust_method = "holm"`) by default. The resulting
output table can also be used to apply significance stars to the plot in
[`plot_summary_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_summary_current_data.md).

## Usage

``` r
perform_t_tests_for_summary_plot(
  data,
  include_all_treatments = "yes",
  list_of_treatments = NULL,
  include_all_categories = "yes",
  list_of_categories = NULL,
  current_type = "eEPSC",
  parameter = "amplitude",
  baseline_interval = "t0to5",
  interval_length = 5,
  treatment_colour_theme,
  test_type = "pairwise.t.test",
  p_adjust_method = "holm",
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

- include_all_treatments:

  A character (`"yes"` or `"no"`) specifying if the plot will include
  data from all treatments. If `"no"`, you must specify a list of
  treatments in `list_of_treatments`.

- list_of_treatments:

  A list of character values describing the treatments that will be in
  the plot. Defaults to `NULL`, since include_all_treatments is `"yes"`
  by default.

- include_all_categories:

  A character (`"yes"` or `"no"`) specifying if the table will include
  data from all categories. If `"no"`, you must specify a list of
  categories in `list_of_categories`.

- list_of_categories:

  A list of character values describing the categories that will be in
  the table. Defaults to `NULL`, since `include_all_categories` is
  `"yes"` by default.

- current_type:

  A character describing the current type. Allowed values are `"eEPSC"`
  or `"sEPSC"`.

- parameter:

  A character value specifying the parameter to be plotted on the
  y-axis. For evoked currents (`current_type = "eEPSC"`), the available
  parameter is "amplitude", which contains amplitudes normalized
  relative to the baseline. For spontaneous currents
  (`current_type = "sEPSC"`), the available parameters are "amplitude"
  (normalized currents), "raw_amplitude", "frequency" (normalized
  frequency) or "raw_frequency".

- baseline_interval:

  A character value indicating the name of the interval used as the
  baseline. Defaults to `"t0to5"`, but can be changed. Make sure that
  this matches the baseline interval that you set in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- interval_length:

  Length of each interval (numeric; in minutes). Used to divide the
  dataset into broad ranges for statistical analysis. Important!
  `max_recording_length` must be evenly divisible by `interval_length`.
  Defaults to `5`.

- treatment_colour_theme:

  A dataframe containing treatment names and their associated colours as
  hex values. See
  [sample_treatment_names_and_colours](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_treatment_names_and_colours.md)
  for an example of what this dataframe should look like.

- test_type:

  A character (must be `"pairwise.wilcox.test"` or `"pairwise.t.test"`)
  describing the statistical model used in this function.

- p_adjust_method:

  This argument is directly related to `p.adjust.method` in
  [`rstatix::t_test`](https://rpkgs.datanovia.com/rstatix/reference/t_test.html).
  This is the method used to adjust the p-value in multiple pairwise
  comparisons. Allowed values include `"holm"`, `"hochberg"`,
  `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`
  (although `"none"` is NOT recommended).

- save_output_as_RDS:

  A character (`"yes"` or `"no"`) describing if the resulting object
  should be saved as an RDS file in the raw data folder.

## Value

A dataframe

## References

Nutter B (2018). *lazyWeave: LaTeX Wrappers for R \#' Users*. R package
version 3.0.2, <https://CRAN.R-project.org/package=lazyWeave>.

## See also

[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md)
for an example of how the normalized current amplitudes were created.

## Examples

``` r
perform_t_tests_for_summary_plot(
  data = sample_summary_eEPSC_df$summary_data,
  include_all_treatments = "yes",
  list_of_treatments = NULL,
  current_type = "eEPSC",
  parameter = "amplitude",
  baseline_interval = "t0to5",
  test_type = "pairwise.t.test",
  interval_length = 5,
  treatment_colour_theme = sample_treatment_names_and_colours,
  save_output_as_RDS = "no"
)
#> # A tibble: 16 × 15
#> # Groups:   category, treatment [4]
#>    group2  asterisk_time category treatment   .y.   group1    n1    n2 statistic
#>    <chr>           <dbl> <fct>    <fct>       <chr> <chr>  <int> <int>     <dbl>
#>  1 t5to10            7.5 2        Control     mean… t0to5      4     4      2.28
#>  2 t10to15          12.5 2        Control     mean… t0to5      4     4      4.89
#>  3 t15to20          17.5 2        Control     mean… t0to5      4     4      6.96
#>  4 t20to25          22.5 2        Control     mean… t0to5      4     4      8.01
#>  5 t5to10            7.5 2        HNMPA       mean… t0to5      5     5      3.1 
#>  6 t10to15          12.5 2        HNMPA       mean… t0to5      5     5      3.15
#>  7 t15to20          17.5 2        HNMPA       mean… t0to5      5     5      2.99
#>  8 t20to25          22.5 2        HNMPA       mean… t0to5      5     5      3.19
#>  9 t5to10            7.5 2        PPP         mean… t0to5      5     5      0.78
#> 10 t10to15          12.5 2        PPP         mean… t0to5      5     5      0.89
#> 11 t15to20          17.5 2        PPP         mean… t0to5      5     5      1.5 
#> 12 t20to25          22.5 2        PPP         mean… t0to5      5     5      1.96
#> 13 t5to10            7.5 2        PPP_and_HN… mean… t0to5      5     5     -0.59
#> 14 t10to15          12.5 2        PPP_and_HN… mean… t0to5      5     5      0.94
#> 15 t15to20          17.5 2        PPP_and_HN… mean… t0to5      5     5      0.84
#> 16 t20to25          22.5 2        PPP_and_HN… mean… t0to5      5     5      1.39
#> # ℹ 6 more variables: df <dbl>, p <dbl>, p.adj <dbl>, p.adj.signif <chr>,
#> #   p_string <chr>, significance_stars <chr>
```
