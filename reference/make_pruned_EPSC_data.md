# Prune and summarize raw current data per minute

`make_pruned_eEPSC_df()` creates a dataframe of evoked or spontaneous
current data summarized per minute (or some other user-defined
interval). Current amplitudes are collapsed down into the mean amplitude
per minute. This is equivalent to GraphPad Prism's "prune rows" function
to reduce data to summary values for every n rows.

## Usage

``` r
make_pruned_EPSC_data(
  data = patchclampplotteR::sample_raw_eEPSC_df,
  current_type = "eEPSC",
  min_time_value = 0,
  max_time_value = 25,
  baseline_length = 5,
  interval_length = 1,
  software = "Clampfit",
  decimal_places = 2,
  save_output_as_RDS = "no"
)
```

## Arguments

- data:

  A `data.frame` object generated using
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).
  It must contain the columns outlined in the Required columns section
  below, which will already be generated for you from the output of
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- current_type:

  A character describing the current type. Allowed values are `"eEPSC"`
  or `"sEPSC"`.

- min_time_value:

  Minimum time value (numeric; in minutes), which defaults to `0`.

- max_time_value:

  Maximum recording length (numeric; in minutes). All data points will
  be filtered to time values less than or equal to this value. Defaults
  to `25`.

- baseline_length:

  Length of the baseline (numeric; in minutes). Refers to data collected
  before applying a hormone, antagonist, or a protocol like high
  frequency stimulation. Defaults to `5`.

- interval_length:

  Length of each interval (in minutes). Used to divide the dataset into
  broad ranges for statistical analysis. Defaults to `1` for one summary
  point per minute.

- software:

  A character (`"Clampfit"` or `"MiniAnalysis"`) describing what
  software tool was used to analyze the data in `new_raw_data_csv`. This
  is relevant when `data_type` is `"sEPSC"` because the exported data is
  different. Defaults to `"Clampfit"`.

- decimal_places:

  A numeric value indicating the number of decimal places the data
  should be rounded to. Used to reduce file size and prevent an
  incorrect representation of the number of significant digits.

- save_output_as_RDS:

  A character (`"yes"` or `"no"`) describing if the resulting object
  should be saved as an RDS file in the raw data folder.

## Value

A list containing 3 dataframes that can be viewed and used for further
analyses in R. These dataframes are:

- `individual_cells`

- `for_table`

- `all_cells`

I highly recommend assigning the list to an object named something like
`pruned_eEPSC_df` to make it easy to reference the dataframes with
logical names (e.g. `pruned_eEPSC_df$all_cells`). The dataframes are:

- `individual_cells` A dataframe containing current data for each
  individual cell, but the data are reduced to a summary point per per
  minute (or another value if a different `interval_length` is set).
  This dataframe contains columns already in the raw data, like
  `category`, `letter` and `sex` plus new columns.

  New columns for evoked current data (`current_type == "eEPSC"`)
  include:

  - `interval_pruned` A character value describing the interval that was
    used for the pruning function. If the data are pruned per minute,
    this will be "t0to1", "t1to2", "t2to3", etc.

  - `mean_P1` The mean amplitude (in pA) of the first evoked current
    (P1) during a specific interval. This is an average of all data
    points within each interval. For example, the `mean_P1` for the
    interval "t0to1" contains the average current amplitude of all data
    points within the first minute of the recording.

  - `sd_P1` The standard deviation of P1.

  - `n` The number of data points used.

  - `se` The standard error of P1.

  - `cv` The coefficient of variation of P1.

  - `cv_inverse_square` The inverse coefficient of variation, which is
    then squared. This is to enable variance analysis, as in [Huijstee &
    Kessels (2020)](https://doi.org/10.1016/j.jneumeth.2019.108526).

  - `baseline_mean` The mean amplitude of the first evoked current
    during the baseline period.

  - `category, letter, sex, treatment, etc.` Columns which are from the
    raw data. For a definition of these columns, please see the
    documentation for
    [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

  - `time` The upper time value of the interval (e.g. 2 minutes for
    "t1to2") which is used on the x-axis of plots such as in
    [`plot_raw_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_raw_current_data.md).

  New columns for spontaneous current data (`current_type == "sEPSC"`)
  include:

  - `mean_amplitude` The mean amplitude of the normalized spontaneous
    current amplitude for a specific interval (obtained from
    [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md)).

  - `mean_raw_amplitude` The mean amplitude of the raw spontaneous
    current amplitude for a specific interval.

  - `sd_amplitude` The standard deviation of the normalized spontaneous
    current amplitudes.

  - `n` The number of currents

  - `frequency` The frequency (in Hz) of currents during the interval.

  - `se` The standard error of the normalized spontaneous current
    amplitudes.

  - `letter, category, interval, synapses` Columns inherited from the
    raw data.

  - `time` The upper time value of the interval (e.g. 2 minutes for
    "t1to2") which is used on the x-axis of plots such as in
    [`plot_summary_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_summary_current_data.md).

  - `baseline_range` A logical value required for the baseline
    transformation. It is set to TRUE when time is within the baseline
    period (e.g. Time \<= 5) and FALSE at all other times.

  - `baseline_mean_frequency` The mean spontaneous frequency during the
    baseline period.

  - `frequency_transformed` The spontaneous current frequency (in Hz),
    normalized relative to the baseline current frequency.

- `for_table` A dataframe containing two columns: letter and
  `P1_transformed` (for `eEPSC`) or `spont_amplitude_transformed` (for
  `sEPSC`). The current data is collapsed into a single row for each
  letter, with the current data for each letter stored as a list. This
  is required to create sparklines of current amplitude over time within
  the cell summary table. See make_cell_summary_df() and
  make_interactive_summary_table().

- `all_cells` A dataframe consisting of all data within a single
  treatment grouped and summarized per minute (or some other variable if
  you change `interval_length` to be something other than `1`). Columns
  like `category` and `sex` are retained from the raw data.

  New columns for evoked current data (`current_type == "eEPSC"`) are:

  - `interval_pruned` A character value describing the interval that was
    used for the pruning function. If the data are pruned per minute,
    this will be "t0to1", "t1to2", "t2to3", etc.

  - `mean_P1_all_cells` The mean amplitude (in pA) of the first evoked
    current (P1) during a specific interval across all cells.

  - `sd_P1_all_cells` The standard deviation of P1.

  - `n` The number of data points used.

  - `se_P1_all_cells` The standard error of P1.

  - `cv_P1_all_cells` The coefficient of variation of P1.

  New columns for spontaneous current data (`current_type == "sEPSC"`)
  are:

  - `interval_pruned` A character value describing the interval that was
    used for the pruning function. If the data are pruned per minute,
    this will be "t0to1", "t1to2", "t2to3", etc.

  - `mean_all_amplitude` The mean normalized amplitude (in % Baseline)
    of the spontaneous current amplitudes during a specific interval
    across all cells.

  - `mean_all_raw_amplitude` The mean raw amplitude (in pA) of the
    spontaneous current amplitudes during a specific interval across all
    cells.

  - `sd_all_amplitude` The standard deviation of the normalized
    spontaneous current amplitudes across all cells.

  - `n` The number of data points used.

  - `se_all_amplitude` The standard error of the normalized spontaneous
    currents across all cells.

  - `sd_all_raw_amplitude` The standard deviation of the raw spontaneous
    current amplitudes across all cells.

  - `se_raw_amplitude` The standard error of the raw spontaneous
    currents across all cells.

  - `mean_all_frequency` The mean normalized frequency (% Baseline
    frequency) of all spontaneous current amplitudes across all cells
    during the interval.

  - `sd_all_frequency` The standard deviation of the normalized
    frequency of all spontaneous current amplitudes across all cells
    during the interval.

  - `se_frequency` The standard error of the normalized frequency of all
    spontaneous current amplitudes across all cells during the interval.

  - `mean_all_raw_frequency` The mean raw frequency (Hz) of all
    spontaneous current amplitudes across all cells during the interval.

  - `sd_all_raw_frequency` The standard deviation of the raw frequency
    of all spontaneous current amplitudes across all cells during the
    interval.

  - `se_raw_frequency` The standard error of the raw frequency of all
    spontaneous current amplitudes across all cells during the interval.

## Required evoked currents columns

These columns will all be generated automatically in
[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md),
but for more details, you can look at
[sample_raw_eEPSC_df](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_raw_eEPSC_df.md)
to see an example of what the incoming raw dataset in the `data`
argument should look like. If the data are evoked currents
(`current_type == "eEPSC"`), the data must contain the basic columns
mentioned in **Required basic columns** plus these columns:

- `PPR` A numeric value that represents the paired pulse ratio (PPR) of
  the evoked currents, generated in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- `interval` A character value indicating the interval that the data
  belong to (e.g. "t0to5" for the first 5 minutes, "t5to10"). Generated
  automatically in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- `baseline_range` A logical value required for the baseline
  transformation. It is set to TRUE when time is within the baseline
  period (e.g. `time <= 5`) and FALSE at all other times. Generated
  automatically in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- `baseline_mean` A numeric value representing the mean evoked current
  amplitude during the baseline period. There is a different
  baseline_mean for each letter. Generated automatically in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- `P1_transformed` A numeric value representing the first evoked current
  amplitude (pA) normalized relative to the mean amplitude during the
  recording's baseline. Generated automatically in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- `P2_transformed` A numeric value representing the second evoked
  current amplitude (pA) normalized relative to the mean amplitude
  during the recording's baseline. Generated automatically in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

## Required spontaneous currents columns

If the data are spontaneous currents (current_type == "sEPSC"), the data
must contain the basic columns mentioned in **Required basic columns**
plus the columns listed below. For more detail, have a look at
[sample_raw_sEPSC_df](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_raw_sEPSC_df.md)
to see an example of what the incoming raw dataset in the `data`
argument should look like.

NOTE: If you exported your spontaneous data from the histogram tool in '
MiniAnalysis, you must set `software` to "MiniAnalysis".

- `interval` A character value indicating the interval that the data
  belong to (e.g. "t0to5" for the first 5 minutes, "t5to10"). Generated
  automatically in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- `baseline_range` A logical value required for the baseline
  transformation. It is set to `TRUE` when time is within the baseline
  period (e.g. `Time <= 5`) and `FALSE` at all other times. Generated
  automatically in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- `baseline_mean` A numeric value representing the mean evoked current
  amplitude during the baseline period. There is a different
  baseline_mean for each letter. Generated automatically in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

- `amplitude_transformed` A numeric value representing the spontaneous
  current amplitude (pA) normalized relative to the mean amplitude
  during the recording's baseline. Generated automatically in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

## Required basic columns

It doesn't matter if the data are evoked or current type - these columns
should be included in your data.

- `letter` A character value that is a unique identifier for a single
  recording. Used to link data sets for evoked or spontaneous currents
  and cell-characteristics.

- `synapses` A character value (e.g. `"Glutamate"` or `"GABA"`).

- `sex` A character value (e.g. `"Male"` or `"Female"`).

- `treatment` A character value (e.g. `"Control"`, `"HNMPA"`).

- `time` A numeric value that represents time in minutes. This column is
  autogenerated in
  [`add_new_cells()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/add_new_cells.md).

- `ID` A character value for the recording filename.

- `X` A numeric value representing the x-value of the cell's location in
  µm. Leave this blank if you don't have this data.

- `Y` A numeric value representing the y-value of the cell's location in
  µm. Leave this blank if you don't have this data.

- `age` A numeric value representing the animal's age. Can be any value
  as long as the time units are consistent throughout (e.g. don't mix up
  days and months when reporting animal ages).

- `animal` A numeric value representing the animal's ID or number.

- `category` A numeric value representing the experiment type. Used to
  assign top-level groups for further analyses, with `treatment` as
  subgroups.

- `cell` A character or numeric value representing the cell. For
  example, use `3.1.1` for animal \#3, slice \#1, cell \#1.

- `R_a` A list of values for the access resistance, which would have
  been monitored at several timepoints throughout the recording. See the
  documentation for the `R_a` column in the documentation for
  `sample_cell_characteristics` with
  [`?sample_cell_characteristics`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_cell_characteristics.md).

- `notes` An optional character column with notes about any issues,
  sweeps that were removed during Clampfit processing, etc.

- `days_alone` A numeric value describing the number of days that the
  animal was left alone in a cage. This typically ranges from 0 to 2.
  Fasted animals will have 1 day alone.

- `animal_or_slicing_problems` A character value (`"yes"` or `"no"`)
  describing if there were any issues with the animal (for example, the
  animal was unusually anxious) or slicing (there were delays during the
  process, the slices were crumpling, etc.).

**Evoked current data**:

If the data are evoked currents (`current_type == "eEPSC"`), the data
must contain the basic columns mentioned in **Required basic columns**
plus these columns:

- `P1` A numeric value representing the amplitude of the first evoked
  current in pA.

- `P2` A numeric value representing the amplitude of the second evoked
  current in pA.

**Spontaneous current data:**

If the data are spontaneous currents (`current_type == "sEPSC"`), the
data must contain the basic columns mentioned in **Required basic
columns** plus these columns:

- `recording_num` A numeric value representing the recording number.
  This was incorporated before we switched to concatenating all
  recordings into one, but it needs to remain here to prevent breaking
  previous projects. It should be set to `1`.

- `trace` A numeric value representing the trace (automatically
  generated in Clampfit) where the current occurred.

- `time_of_peak` A numeric value representing the time of the peak in
  milliseconds relative to trace number. This is automatically
  calculated in Clampfit.

- `time` A numeric value representing the absolute time when the current
  happened, relative to the start of the recording. This is
  autogenerated. See
  [`add_new_cells()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/add_new_cells.md)
  for a description of how the true time value (`time`) is calculated
  from the `recording_num` and `trace.`

- `amplitude` A numeric value representing the amplitude of the evoked
  current in pA.

## See also

[`make_variance_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_variance_data.md)
to see an example of how you can use pruned data to perform variance
analysis to determine the mechanism of changing synaptic plasticity.

[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md)
for a description of how baseline normalization works.

## Examples

``` r
# Evoked Current Data
make_pruned_EPSC_data(
  data = sample_raw_eEPSC_df,
  current_type = "eEPSC",
  min_time_value = 0,
  max_time_value = 25,
  baseline_length = 5,
  interval_length = 1,
  decimal_places = 2
)
#> $individual_cells
#> # A tibble: 473 × 16
#>    category letter sex   treatment interval_pruned mean_P1 sd_P1     n    se
#>    <fct>    <fct>  <fct> <fct>     <fct>             <dbl> <dbl> <dbl> <dbl>
#>  1 2        AO     Male  Control   t0to1             36.4  13.8     13  3.83
#>  2 2        AO     Male  Control   t1to2             38.1  21.4     12  6.18
#>  3 2        AO     Male  Control   t2to3             39.4  22.2     12  6.41
#>  4 2        AO     Male  Control   t3to4             39.2  18.5     12  5.34
#>  5 2        AO     Male  Control   t4to5             30.8  19.7     12  5.69
#>  6 2        AO     Male  Control   t5to6             21.3  15.6     12  4.52
#>  7 2        AO     Male  Control   t6to7             13.6  17.4     12  5.01
#>  8 2        AO     Male  Control   t7to8             14.4  16.5     12  4.77
#>  9 2        AO     Male  Control   t8to9              5.68  2       12  0.58
#> 10 2        AO     Male  Control   t9to10             7.45  5.84    12  1.69
#> # ℹ 463 more rows
#> # ℹ 7 more variables: cv <dbl>, cv_inverse_square <dbl>, time <dbl>,
#> #   baseline_mean <dbl>, synapses <fct>, days_alone <fct>,
#> #   animal_or_slicing_problems <fct>
#> 
#> $for_table
#> # A tibble: 19 × 2
#>    letter P1_transformed
#>    <fct>  <list>        
#>  1 AO     <dbl [25]>    
#>  2 AZ     <dbl [25]>    
#>  3 BN     <dbl [25]>    
#>  4 BO     <dbl [25]>    
#>  5 BT     <dbl [25]>    
#>  6 CG     <dbl [25]>    
#>  7 CZ     <dbl [25]>    
#>  8 FT     <dbl [25]>    
#>  9 FX     <dbl [25]>    
#> 10 GF     <dbl [25]>    
#> 11 GI     <dbl [25]>    
#> 12 GK     <dbl [25]>    
#> 13 GR     <dbl [25]>    
#> 14 GX     <dbl [25]>    
#> 15 HB     <dbl [25]>    
#> 16 HC     <dbl [25]>    
#> 17 HG     <dbl [23]>    
#> 18 HN     <dbl [25]>    
#> 19 L      <dbl [25]>    
#> 
#> $all_cells
#> # A tibble: 198 × 10
#>    category interval_pruned sex    treatment   mean_P1_all_cells sd_P1_all_cells
#>    <fct>    <fct>           <fct>  <fct>                   <dbl>           <dbl>
#>  1 2        t0to1           Female Control                 141.            NA   
#>  2 2        t0to1           Female HNMPA                   117.            11.0 
#>  3 2        t0to1           Female PPP                      97.3           10.2 
#>  4 2        t0to1           Female PPP_and_HN…              94.2           NA   
#>  5 2        t0to1           Male   Control                  95.9           11.2 
#>  6 2        t0to1           Male   HNMPA                   123.            21.5 
#>  7 2        t0to1           Male   PPP                     110.            13.6 
#>  8 2        t0to1           Male   PPP_and_HN…             116.             4.9 
#>  9 2        t1to2           Female Control                 104.            NA   
#> 10 2        t1to2           Female HNMPA                    96.0            1.74
#> # ℹ 188 more rows
#> # ℹ 4 more variables: n <dbl>, se_P1_all_cells <dbl>, cv_P1_all_cells <dbl>,
#> #   time <dbl>
#> 

# Spontaneous Current Data
make_pruned_EPSC_data(
  data = sample_raw_sEPSC_df,
  current_type = "sEPSC",
  min_time_value = 0,
  max_time_value = 25,
  baseline_length = 5,
  interval_length = 1,
  decimal_places = 2
)
#> $individual_cells
#> # A tibble: 175 × 19
#> # Groups:   letter [7]
#>    category letter sex    treatment interval_pruned mean_amplitude
#>    <fct>    <fct>  <fct>  <fct>     <fct>                    <dbl>
#>  1 2        AZ     Female Control   t0to1                    104. 
#>  2 2        AZ     Female Control   t1to2                     98.9
#>  3 2        AZ     Female Control   t2to3                     97.7
#>  4 2        AZ     Female Control   t3to4                    101. 
#>  5 2        AZ     Female Control   t4to5                     98.7
#>  6 2        AZ     Female Control   t5to6                     95.4
#>  7 2        AZ     Female Control   t6to7                     96.9
#>  8 2        AZ     Female Control   t7to8                     97.6
#>  9 2        AZ     Female Control   t8to9                     94.5
#> 10 2        AZ     Female Control   t9to10                    96.3
#> # ℹ 165 more rows
#> # ℹ 13 more variables: mean_raw_amplitude <dbl>, sd_amplitude <dbl>, n <dbl>,
#> #   frequency <dbl>, se <dbl>, interval <fct>, synapses <fct>,
#> #   days_alone <dbl>, animal_or_slicing_problems <chr>, time <dbl>,
#> #   baseline_range <lgl>, baseline_mean_frequency <dbl>,
#> #   frequency_transformed <dbl>
#> 
#> $for_table
#> # A tibble: 7 × 2
#>   letter spont_amplitude_transformed
#>   <fct>  <list>                     
#> 1 AZ     <dbl [25]>                 
#> 2 BO     <dbl [25]>                 
#> 3 FX     <dbl [25]>                 
#> 4 GR     <dbl [25]>                 
#> 5 GX     <dbl [25]>                 
#> 6 HC     <dbl [25]>                 
#> 7 L      <dbl [25]>                 
#> 
#> $all_cells
#> # A tibble: 150 × 19
#>    category interval_pruned sex    treatment     mean_all_amplitude
#>    <fct>    <fct>           <fct>  <fct>                      <dbl>
#>  1 2        t0to1           Female Control                    104. 
#>  2 2        t0to1           Female PPP                         98.5
#>  3 2        t0to1           Male   Control                     82.3
#>  4 2        t0to1           Male   HNMPA                       98.1
#>  5 2        t0to1           Male   PPP                        106. 
#>  6 2        t0to1           Male   PPP_and_HNMPA              105. 
#>  7 2        t1to2           Female Control                     98.9
#>  8 2        t1to2           Female PPP                        101. 
#>  9 2        t1to2           Male   Control                     97.6
#> 10 2        t1to2           Male   HNMPA                      101. 
#> # ℹ 140 more rows
#> # ℹ 14 more variables: mean_all_raw_amplitude <dbl>, sd_all_amplitude <dbl>,
#> #   n <dbl>, se_amplitude <dbl>, sd_all_raw_amplitude <dbl>,
#> #   se_raw_amplitude <dbl>, mean_all_frequency <dbl>, sd_all_frequency <dbl>,
#> #   se_frequency <dbl>, mean_all_raw_frequency <dbl>,
#> #   sd_all_raw_frequency <dbl>, se_raw_frequency <dbl>, time <dbl>,
#> #   interval <fct>
#> 
```
