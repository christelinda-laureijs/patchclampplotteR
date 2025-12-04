# Summarize current data per 5-min for statistical tests

`make_summary_EPSC_data()` allows you to divide data from a long
recording (e.g. 30 minutes) into evenly-spaced intervals (e.g. 5
minutes). It will generate summary data like the mean current amplitude
for each interval. This can be useful for inserting into statistical
models to compare effect sizes across broad stretches of time. The
interval length would have been previously specified in
[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md)
using the `interval_length` argument.

## Usage

``` r
make_summary_EPSC_data(
  data = patchclampplotteR::sample_raw_eEPSC_df,
  current_type = "eEPSC",
  save_output_as_RDS = "no",
  decimal_places = 2,
  baseline_interval = "t0to5",
  ending_interval = "t20to25"
)
```

## Arguments

- data:

  A `data.frame` object. If the data are evoked currents
  (`current_type == "eEPSC"`), this should be the raw evoked current
  data generated using
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).
  If the data are spontaneous currents (`current_type == "sEPSC"`), this
  should be the pruned data `$individual_cells` dataset generated using
  [`make_pruned_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_pruned_EPSC_data.md).

- current_type:

  A character describing the current type. Allowed values are `"eEPSC"`
  or `"sEPSC"`.

- save_output_as_RDS:

  A character (`"yes"` or `"no"`) describing if the resulting object
  should be saved as an RDS file in the raw data folder.

- decimal_places:

  A numeric value indicating the number of decimal places the data
  should be rounded to. Used to reduce file size and prevent an
  incorrect representation of the number of significant digits.

- baseline_interval:

  A character value describing the baseline interval. Defaults to
  `"t0to5"`.

- ending_interval:

  A character value describing the last interval in the recording.
  Useful for future plots in which you compare the percent
  decrease/increase in current amplitude relative to the baseline.
  Examples include `"t20to25"`, `"t10to15"`, etc.

## Value

A list of three dataframes (`current_type = eEPSC`) or three dataframes
(`current_type = sEPSC`). For evoked currents (`current_type = "eEPSC"`)
the first dataframe (`$percent_change_data`) contains the mean current
amplitude for each interval, with a final column (`percent_change`)
containing the final percent change in amplitude in the last interval
relative to the mean amplitude during the baseline interval.

Most columns (`age`, `sex`, `animal`, etc.) come directly from the
information imported through
[`import_cell_characteristics_df()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_cell_characteristics_df.md).
However, there are some new columns of note.

- `t0to5` The mean evoked current amplitude (pA) for this cell during
  the period of 0 to 5 minutes.

- `t5to10` The mean evoked current amplitude (pA) for this cell during
  the period of 5 to 10 minutes.

- `t10to15`, `t15to20`, `tXtY` etc... The mean evoked current amplitude
  (pA) for this cell during the period of *X* to *Y* minutes.

- `percent_change` The percent change in evoked current amplitude in the
  interval `t20to25` as a percentage of the mean baseline amplitude
  (`t0to5`). For example, if currents began at 100 pA during the
  baseline period, but were 50 pA by `t20to25`, the value of
  `percent_change` will be `50%` or `0.50`. You can also change the
  value of the intervals used in this calculation through the
  `baseline_interval` and `ending_interval` arguments.

The second dataframe (accessed through `$summary_data`) contains summary
data such as the mean current amplitude, coefficient of variation,
standard deviation, standard error, variance, variance-to-mean ratio,
and inverse coefficient of variation squared for each interval.

New columns for evoked current data (`current_type == "eEPSC"`) include:

- `mean_P1_transformed` The amplitude of the first evoked current
  amplitude (% Baseline eEPSC amplitude) normalized to the mean baseline
  amplitude and averaged over the interval.

- `mean_P1_raw` The amplitude of the first evoked current amplitude (pA)
  averaged over the interval.

- `n` The number of datapoints used to create the averaged values.
  Corresponds to the number of sweeps per interval.

- `sd` The standard deviation of the normalized evoked current data
  (`P1_transformed`).

- `cv` The coefficient of variation of `P1_transformed`.

- `se` The standard error of `P1_transformed`.

- `cv_inverse_square` The inverse of the squared coefficient of
  variation of `P1_transformed`.

- `variance` The variance of `P1_transformed`.

- `VMR` The variance-to-mean ratio (VMR) of `P1_transformed`.

- `interval` A character value indicating the interval that the data
  point belongs to. For example, `interval` will be "t0to5" for any data
  points from 0 to 5 minutes. Example values: "t0to5", "t5to10", etc.

- `letter, synapses, sex, treatment, etc.` Unmodified columns from the
  original dataset describing the cell's properties.

The third dataframe (accessed with `$mean_SE`) contains summary
statistics that will be useful for publications. It presents mean evoked
current amplitudes (taken from raw `P1` values) grouped by category,
treatment, and sex. This will make it easy to report your findings as
*mean +/- SE or SD* with *n* in publications. For example, "eEPSC
amplitude decreased significantly in males (baseline: 24.1 +/- 0.11 pA,
n = 6, insulin: 12.4 +/- 0.23 pA, n = 7)."

- `category` The experiment category (please see
  [`import_cell_characteristics_df()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_cell_characteristics_df.md)
  for more details).

- `sex` The sex of the animal

- `treatment` The treatment applied.

- `n` The number of data points (i.e. cells)

- `mean_baseline_raw_P1` The average evoked current amplitude (taken
  from `mean_P1_raw`) during the `baseline_interval`.

- `sd_baseline_raw_P1` The standard deviation of `mean_baseline_raw_P1.`

- `se_baseline_raw_P1` The standard error of `mean_baseline_raw_P1`.
  Taken by dividing `sd_baseline_raw_P1` by the square root of `n`.

- `mean_ending_raw_P1` The average evoked current amplitude (taken from
  `mean_P1_raw`) during the `ending_interval`.

- `sd_ending_raw_P1` The standard deviation of `mean_ending_raw_P1.`

- `se_ending_raw_P1` The standard error of `mean_ending_raw_P1`. Taken
  by dividing `sd_ending_raw_P1` by the square root of `n`.

- `VMR` The variance-to-mean ratio (VMR) of `P1_transformed`.

- `interval` A character value indicating the interval that the data
  point belongs to. For example, `interval` will be "t0to5" for any data
  points from 0 to 5 minutes. Example values: "t0to5", "t5to10", etc.

- `letter, synapses, sex, treatment, etc.` Unmodified columns from the
  original dataset describing the cell's properties.

### Spontaneous Current Data

Spontaneous current data results in five dataframes. The first dataframe
(\$summary_data) contains summary statistics for each interval, as
outlined below:

- `mean_transformed_amplitude` The average normalized spontaneous
  current amplitude (% Baseline sEPSC amplitude).

- `mean_raw_amplitude` The average raw spontaneous current amplitude
  (pA).

- `n` The number of datapoints used to create the average.

- `sd_transformed_amplitude` The standard deviation of the normalized
  spontaneous current data (`mean_transformed_amplitude`).

- `se_transformed_amplitude` The standard error of
  `mean_transformed_amplitude`.

- `mean_transformed_frequency` The average normalized frequency (%
  Baseline frequency).

- `sd_transformed_frequency` The standard deviation of
  `mean_transformed_frequency`.

- `se_frequency` The standard error of `mean_transformed_frequency`.

- `mean_raw_frequency` The average raw frequency (Hz).

- `letter, synapses, sex, treatment, etc.` Unmodified columns from the
  original dataset describing the cell's properties.

The second and third dataframes contain percent change data for
spontaneous current amplitude and frequency, respectively. The columns
are the same as the ones produced for evoked currents (read the
documentation for `$percent_change_data`.

The fourth and fifth dataframes contain the mean, SE, SD, and n data for
spontaneous current amplitude and frequency, respectively. Read the
description for the `$mean_SE` to learn about these columns.

## Examples

``` r
# Evoked Currents
# Will return a list of three dataframes

make_summary_EPSC_data(
  data = sample_raw_eEPSC_df,
  current_type = "eEPSC",
  save_output_as_RDS = "no",
  decimal_places = 2
)
#> $percent_change_data
#> # A tibble: 19 × 17
#> # Groups:   category, treatment, letter [19]
#>    category letter sex    treatment   age animal     X     Y synapses days_alone
#>    <fct>    <fct>  <fct>  <fct>     <dbl>  <dbl> <dbl> <dbl> <fct>    <fct>     
#>  1 2        AO     Male   Control      39   17     NA    NA  Glutama… 1         
#>  2 2        AZ     Female Control      32   21    353.  332. Glutama… 0         
#>  3 2        BN     Male   Control      29   25    153.  337. Glutama… 0         
#>  4 2        BO     Male   HNMPA        32   27     NA    NA  Glutama… 0         
#>  5 2        BT     Male   HNMPA        37   30    387.  587. Glutama… 0         
#>  6 2        CG     Female HNMPA        36   35    164.  367. Glutama… 0         
#>  7 2        CZ     Male   HNMPA        28   41    297.  493. Glutama… 0         
#>  8 2        FT     Female HNMPA        39   72    153.  337. Glutama… 1         
#>  9 2        FX     Female PPP          28   74    235.  496. Glutama… 0         
#> 10 2        GF     Female PPP          36   77    217.  324. Glutama… 0         
#> 11 2        GI     Female PPP          28   81    236.  284. Glutama… 0         
#> 12 2        GK     Male   PPP          35   84    249.  574. Glutama… 0         
#> 13 2        GR     Male   PPP          34   90    314.  416. Glutama… 0         
#> 14 2        GX     Male   PPP_and_…    33   97     NA    NA  Glutama… 1         
#> 15 2        HB     Male   PPP_and_…    39  100    133.  590. Glutama… 2         
#> 16 2        HC     Male   PPP_and_…    39  100    173.  576. Glutama… 2         
#> 17 2        HG     Female PPP_and_…    34  103     NA    NA  Glutama… 0         
#> 18 2        HN     Male   PPP_and_…    38  109    289.  400. Glutama… 0         
#> 19 2        L      Male   Control      38    8.5   NA    NA  Glutama… 0         
#> # ℹ 7 more variables: animal_or_slicing_problems <fct>, t0to5 <dbl>,
#> #   t5to10 <dbl>, t10to15 <dbl>, t15to20 <dbl>, t20to25 <dbl>,
#> #   percent_change <dbl>
#> 
#> $summary_data
#> # A tibble: 95 × 22
#>    category letter sex    treatment interval mean_P1_transformed mean_P1_raw
#>    <fct>    <fct>  <fct>  <fct>     <fct>                  <dbl>       <dbl>
#>  1 2        AO     Male   Control   t0to5                  100         36.8 
#>  2 2        AO     Male   Control   t5to10                  34.0       12.5 
#>  3 2        AO     Male   Control   t10to15                 17.7        6.49
#>  4 2        AO     Male   Control   t15to20                 18.6        6.85
#>  5 2        AO     Male   Control   t20to25                 21.3        7.82
#>  6 2        AZ     Female Control   t0to5                  100         44.3 
#>  7 2        AZ     Female Control   t5to10                  74.1       32.8 
#>  8 2        AZ     Female Control   t10to15                 53.7       23.8 
#>  9 2        AZ     Female Control   t15to20                 56.0       24.8 
#> 10 2        AZ     Female Control   t20to25                 50.7       22.4 
#> # ℹ 85 more rows
#> # ℹ 15 more variables: n <dbl>, sd <dbl>, cv <dbl>, se <dbl>,
#> #   cv_inverse_square <dbl>, variance <dbl>, VMR <dbl>, age <dbl>,
#> #   animal <dbl>, X <dbl>, Y <dbl>, time <dbl>, synapses <fct>,
#> #   days_alone <fct>, animal_or_slicing_problems <fct>
#> 
#> $mean_SE
#> # A tibble: 8 × 10
#> # Groups:   category, treatment [4]
#>   category treatment     sex        n mean_baseline_raw_P1 sd_baseline_raw_P1
#>   <fct>    <fct>         <fct>  <int>                <dbl>              <dbl>
#> 1 2        Control       Female     1                 44.3               NA  
#> 2 2        Control       Male       3                 62.0               22.0
#> 3 2        HNMPA         Female     2                 61.1               23.6
#> 4 2        HNMPA         Male       3                 69.0               18.2
#> 5 2        PPP           Female     3                 60.5               19.1
#> 6 2        PPP           Male       2                 84.1               21.2
#> 7 2        PPP_and_HNMPA Female     1                 79.9               NA  
#> 8 2        PPP_and_HNMPA Male       4                 54.3               23.2
#> # ℹ 4 more variables: se_baseline_raw_P1 <dbl>, mean_ending_raw_P1 <dbl>,
#> #   sd_ending_raw_P1 <dbl>, se_ending_raw_P1 <dbl>
#> 

# Spontaneous Data
# Will return a list of three dataframes

make_summary_EPSC_data(
  data = sample_pruned_sEPSC_df$individual_cells,
  current_type = "sEPSC",
  save_output_as_RDS = "no",
  decimal_places = 2,
  baseline_interval = "t0to5",
  ending_interval = "t20to25"
)
#> $summary_data
#> # A tibble: 35 × 18
#> # Groups:   category, letter, sex, treatment [7]
#>    category letter sex    treatment interval mean_transformed_amplitude
#>    <fct>    <fct>  <fct>  <fct>     <fct>                         <dbl>
#>  1 2        AZ     Female Control   t0to5                         100. 
#>  2 2        AZ     Female Control   t5to10                         96.1
#>  3 2        AZ     Female Control   t10to15                        94.1
#>  4 2        AZ     Female Control   t15to20                        94.8
#>  5 2        AZ     Female Control   t20to25                        86.8
#>  6 2        BO     Male   HNMPA     t0to5                         100. 
#>  7 2        BO     Male   HNMPA     t5to10                         87.0
#>  8 2        BO     Male   HNMPA     t10to15                        81.9
#>  9 2        BO     Male   HNMPA     t15to20                        82.2
#> 10 2        BO     Male   HNMPA     t20to25                        82.0
#> # ℹ 25 more rows
#> # ℹ 12 more variables: mean_raw_amplitude <dbl>,
#> #   sd_transformed_amplitude <dbl>, n <dbl>, se_transformed_amplitude <dbl>,
#> #   mean_transformed_frequency <dbl>, sd_transformed_frequency <dbl>,
#> #   se_transformed_frequency <dbl>, mean_raw_frequency <dbl>, time <dbl>,
#> #   synapses <fct>, days_alone <dbl>, animal_or_slicing_problems <chr>
#> 
#> $percent_change_amplitude
#> # A tibble: 7 × 13
#> # Groups:   category, treatment, letter [7]
#>   category letter sex    treatment    synapses days_alone animal_or_slicing_pr…¹
#>   <fct>    <fct>  <fct>  <fct>        <fct>         <dbl> <chr>                 
#> 1 2        AZ     Female Control      Glutama…          0 no                    
#> 2 2        BO     Male   HNMPA        Glutama…          0 no                    
#> 3 2        FX     Female PPP          Glutama…          0 no                    
#> 4 2        GR     Male   PPP          Glutama…          0 no                    
#> 5 2        GX     Male   PPP_and_HNM… Glutama…          1 yes                   
#> 6 2        HC     Male   PPP_and_HNM… Glutama…          2 yes                   
#> 7 2        L      Male   Control      Glutama…          0 no                    
#> # ℹ abbreviated name: ¹​animal_or_slicing_problems
#> # ℹ 6 more variables: t0to5 <dbl>, t5to10 <dbl>, t10to15 <dbl>, t15to20 <dbl>,
#> #   t20to25 <dbl>, percent_change <dbl>
#> 
#> $percent_change_frequency
#> # A tibble: 7 × 13
#> # Groups:   category, treatment, letter [7]
#>   category letter sex    treatment    synapses days_alone animal_or_slicing_pr…¹
#>   <fct>    <fct>  <fct>  <fct>        <fct>         <dbl> <chr>                 
#> 1 2        AZ     Female Control      Glutama…          0 no                    
#> 2 2        BO     Male   HNMPA        Glutama…          0 no                    
#> 3 2        FX     Female PPP          Glutama…          0 no                    
#> 4 2        GR     Male   PPP          Glutama…          0 no                    
#> 5 2        GX     Male   PPP_and_HNM… Glutama…          1 yes                   
#> 6 2        HC     Male   PPP_and_HNM… Glutama…          2 yes                   
#> 7 2        L      Male   Control      Glutama…          0 no                    
#> # ℹ abbreviated name: ¹​animal_or_slicing_problems
#> # ℹ 6 more variables: t0to5 <dbl>, t5to10 <dbl>, t10to15 <dbl>, t15to20 <dbl>,
#> #   t20to25 <dbl>, percent_change <dbl>
#> 
#> $mean_SE_amplitude
#> # A tibble: 6 × 10
#> # Groups:   category, treatment [4]
#>   category treatment    sex       n mean_baseline_amplit…¹ sd_baseline_amplitude
#>   <fct>    <fct>        <fct> <int>                  <dbl>                 <dbl>
#> 1 2        Control      Fema…     1                   11.0                 NA   
#> 2 2        Control      Male      1                   29.1                 NA   
#> 3 2        HNMPA        Male      1                   12.7                 NA   
#> 4 2        PPP          Fema…     1                   14.5                 NA   
#> 5 2        PPP          Male      1                   14.1                 NA   
#> 6 2        PPP_and_HNM… Male      2                   15.3                  6.08
#> # ℹ abbreviated name: ¹​mean_baseline_amplitude
#> # ℹ 4 more variables: se_baseline_amplitude <dbl>, mean_ending_amplitude <dbl>,
#> #   sd_ending_amplitude <dbl>, se_ending_amplitude <dbl>
#> 
#> $mean_SE_frequency
#> # A tibble: 6 × 10
#> # Groups:   category, treatment [4]
#>   category treatment    sex       n mean_baseline_freque…¹ sd_baseline_frequency
#>   <fct>    <fct>        <fct> <int>                  <dbl>                 <dbl>
#> 1 2        Control      Fema…     1                   7.99                 NA   
#> 2 2        Control      Male      1                   2.25                 NA   
#> 3 2        HNMPA        Male      1                  19.2                  NA   
#> 4 2        PPP          Fema…     1                   6.74                 NA   
#> 5 2        PPP          Male      1                  13.2                  NA   
#> 6 2        PPP_and_HNM… Male      2                   6.09                  3.59
#> # ℹ abbreviated name: ¹​mean_baseline_frequency
#> # ℹ 4 more variables: se_baseline_frequency <dbl>, mean_ending_frequency <dbl>,
#> #   sd_ending_frequency <dbl>, se_ending_frequency <dbl>
#> 
```
