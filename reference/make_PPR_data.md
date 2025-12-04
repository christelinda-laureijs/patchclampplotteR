# Make paired-pulse ratio (PPR) dataframe for before vs. after comparisons

This function filters the raw current data into data that belong to one
of two time points. They are the baseline period and a user-specified
interval after a hormone or protocol has been applied. The
"before/after" comparison of the paired-pulse ratio (PPR) is useful to
determine which mechanism is involved in modifying synaptic plasticity.
For example, the PPR may be related to the probability of
neurotransmitter release [Oleskevich et al.,
2000](https://doi.org/10.1111/j.1469-7793.2000.00513.x).

## Usage

``` r
make_PPR_data(
  data,
  include_all_treatments = "yes",
  list_of_treatments = NULL,
  PPR_min = 0,
  PPR_max = 5,
  baseline_interval = "t0to5",
  post_hormone_interval = "t20to25",
  treatment_colour_theme,
  save_output_as_RDS = "no"
)
```

## Arguments

- data:

  A dataframe containing the raw evoked current data generated from
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- include_all_treatments:

  A character (`"yes"` or `"no"`) specifying if the plot will include
  data from all treatments. If `"no"`, you must specify a list of
  treatments in `list_of_treatments`.

- list_of_treatments:

  A list of character values describing the treatments that will be in
  the plot. Defaults to `NULL`, since include_all_treatments is `"yes"`
  by default.

- PPR_min:

  A numeric value representing the minimum PPR value permitted in the
  filtered dataset. Defaults to `0`.

- PPR_max:

  A numeric value representing the maximum PPR value permitted in the
  filtered dataset. Defaults to `5`.

- baseline_interval:

  A character value indicating the name of the interval used as the
  baseline. Defaults to `"t0to5"`, but can be changed. Make sure that
  this matches the baseline interval that you set in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- post_hormone_interval:

  A character value specifying the interval used for the data points
  after a hormone or protocol was applied.

- treatment_colour_theme:

  A dataframe containing treatment names and their associated colours as
  hex values. See
  [sample_treatment_names_and_colours](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_treatment_names_and_colours.md)
  for an example of what this dataframe should look like.

- save_output_as_RDS:

  A character (`"yes"` or `"no"`) describing if the resulting object
  should be saved as an RDS file in the raw data folder.

## Value

A dataframe containing all of the columns from
[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md),
but filtered to only include PPR values between `PPR_min` and `PPR_max`
within the `baseline_interval` and `post_hormone_interval`.

## See also

[`plot_PPR_data_single_treatment()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_PPR_data_single_treatment.md)
and
[`plot_PPR_data_multiple_treatments()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_PPR_data_multiple_treatments.md)

## Examples

``` r
make_PPR_data(
  data = sample_raw_eEPSC_df,
  include_all_treatments = "yes",
  list_of_treatments = NULL,
  PPR_min = 0,
  PPR_max = 5,
  baseline_interval = "t0to5",
  post_hormone_interval = "t20to25",
  treatment_colour_theme = sample_treatment_names_and_colours
)
#> # A tibble: 2,205 × 25
#> # Groups:   letter [19]
#>    letter synapses  sex   treatment  time ID          P1    P2     X     Y   age
#>    <fct>  <fct>     <fct> <fct>     <dbl> <fct>    <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 AO     Glutamate Male  Control    0    23623002  17.4  78.4    NA    NA    39
#>  2 AO     Glutamate Male  Control    0.08 23623002  24.1  58.9    NA    NA    39
#>  3 AO     Glutamate Male  Control    0.17 23623002  35.6  60.0    NA    NA    39
#>  4 AO     Glutamate Male  Control    0.25 23623002  15.8  32.3    NA    NA    39
#>  5 AO     Glutamate Male  Control    0.33 23623002  53.9  26.4    NA    NA    39
#>  6 AO     Glutamate Male  Control    0.42 23623002  45.5  74.8    NA    NA    39
#>  7 AO     Glutamate Male  Control    0.5  23623002  28.1  69.6    NA    NA    39
#>  8 AO     Glutamate Male  Control    0.58 23623002  27.8  62.0    NA    NA    39
#>  9 AO     Glutamate Male  Control    0.67 23623002  60.0  52.7    NA    NA    39
#> 10 AO     Glutamate Male  Control    0.75 23623002  46.9  60.3    NA    NA    39
#> # ℹ 2,195 more rows
#> # ℹ 14 more variables: animal <dbl>, category <fct>, cell <chr>, notes <lgl>,
#> #   days_alone <fct>, animal_or_slicing_problems <fct>, R_a <chr>, PPR <dbl>,
#> #   interval <fct>, baseline_range <lgl>, baseline_mean <dbl>,
#> #   P1_transformed <dbl>, P2_transformed <dbl>, state <chr>
```
