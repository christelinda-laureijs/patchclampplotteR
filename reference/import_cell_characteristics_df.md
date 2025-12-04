# Import cell characteristics

`import_cell_characteristics_df()` is a wrapper around
[`read.csv()`](https://rdrr.io/r/utils/read.table.html) to import a .csv
file with information about a cell (animal, age, sex, synapses, X- and
Y-coordinates, and access resistance etc.). It replaces `NA` values in
the `R_a` column with `0` to remove errors caused by missing data. The
function will also generate useful columns such as
`percent_change_access` which describes the percent change in access of
the final `R_a` reading relative to the `R_a` at the start of the
recording. This can be helpful for cell exclusion based on access.

## Usage

``` r
import_cell_characteristics_df(filename)
```

## Arguments

- filename:

  A filepath to a .csv file containing information on cell
  characteristics. The function uses
  [`here::here()`](https://here.r-lib.org/reference/here.html) to locate
  the filepath. See the details below for information on required
  columns.

## Value

A dataframe

## Details

The resulting dataframe can be merged with raw data into a summary table
and used in downstream statistical analyses.

## Required columns

The columns listed below are required in the raw .csv file. If you do
not have data for any of these columns, please still include the column
as an "empty" column to prevent errors caused by missing columns.

- `letter` A character value that is a unique identifier for a single
  recording. Used to link data sets for evoked or spontaneous currents
  and cell-characteristics. Example: `"A"`

- `cell` A character or numeric value representing the cell. For
  example, use `3.1.1` for animal \#3, slice \#1, cell \#1.

- `sex` A character value such as "Male" or "Female".

- `X` A numeric value representing the x-value of the cell's location in
  µm.

- `Y` A numeric value representing the y-value of the cell's location in
  µm.

- `age` A numeric value representing the animal's age. Can be any value
  as long as the time units are consistent throughout (e.g. don't mix up
  days and months when reporting animal ages). Do not use characters
  (e.g. avoid "P31" and use 31 instead).

- `animal` A numeric value representing the animal's ID number.

- `synapses` A character value such as `"Glutamate"` or `"GABA"`.

- `treatment` A character value such as `"Control"` or `"HNMPA"`.

- `category` A numeric value representing an experiment type. For
  example, `'1'` may mean 4 seconds of high-frequency stimulation (HFS),
  `'2'` may mean an experiment where you added insulin, and `'3'` may
  mean HFS with insulin in the bath at all times. A category is the
  top-level division of your data. You can then have subgroups using the
  `treatment` variable. For example, perhaps you added insulin (Category
  = `2`) and also had the antagonist HNMPA present. This would be
  `Category = 2, Treatment = HNMPA`.

- `R_a` A list of values for the access resistance, which would have
  been monitored at several timepoints throughout the recording. See the
  section `R_a` formatting below.

- `days_alone` A numeric value representing the number of days that the
  animal was alone in a cage. This will always be 1 for some treatments,
  like fasting, but should ideally be low to reduce the effects of
  social isolation-related stress.

- `animal_or_slicing_problems` A character value (`"yes"` or `"no"`)
  indicating if there were any problems during any point of the slice
  preparation process or animal handling. For example, use `"yes"` if
  the slices were crumpling during slicing or the animal was unusually
  anxious.

- `percent_change_access` A numeric value indicating the percent change
  in access (`R_a`) over the recording. This is calculated by
  subtracting the first value of `R_a` from the last value of `R_a` and
  expressing the change in access as a percentage of the starting `R_a`.
  You could use this to assist with excluding cells based on changes in
  access.

## `R_a` formatting

`R_a` is a mandatory column with information about the cell's access
resistance. Each element of this column must be a sequence of numbers,
separated by a comma and a single space. Although this will be read in
as a character, do not add quotation marks around the values in this
column. For example, `1.5, 1.5, 1.6, 1.7, 1.7, 1.8` is an acceptable
`R_a` value for a single cell.

`import_cell_characteristics_df()` will convert the character value into
a list of numeric values (using
[`stringr::str_split()`](https://stringr.tidyverse.org/reference/str_split.html)).
It will also convert blanks and `NA` values to 0. This allows access to
be visualized as a sparkline in the `R_a` column of the interactive
summary table made with
[`make_interactive_summary_table()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_interactive_summary_table.md).

## See also

[`make_interactive_summary_table()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_interactive_summary_table.md)
to generate an interactive table with cell characteristics and raw data
as sparklines.

## Examples

``` r
import_cell_characteristics_df(import_ext_data("sample_cell_characteristics.csv"))
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `R_a = lapply(stringr::str_split(.data$R_a, pattern = ", "), FUN
#>   = as.numeric)`.
#> Caused by warning in `lapply()`:
#> ! NAs introduced by coercion
#>    letter    cell    sex         X        Y age animal  synapses     treatment
#> 1      BN  25.1.2   Male 152.92125 337.1888  29   25.0 Glutamate       Control
#> 2      AZ  21.1.1 Female 352.61800 331.7375  32   21.0 Glutamate       Control
#> 3      AO  17.1.1   Male        NA       NA  39   17.0 Glutamate       Control
#> 4      BO  27.2.1   Male        NA       NA  32   27.0 Glutamate         HNMPA
#> 5      BT  30.2.1   Male 387.19250 586.9837  37   30.0 Glutamate         HNMPA
#> 6      CG  35.1.3 Female 164.18000 366.5200  36   35.0 Glutamate         HNMPA
#> 7       L 8.5.2.1   Male        NA       NA  38    8.5 Glutamate       Control
#> 8      CZ  41.2.2   Male 296.51000 492.9000  28   41.0 Glutamate         HNMPA
#> 9      FT  72.2.1 Female 153.33000 337.1800  39   72.0 Glutamate         HNMPA
#> 10     FX  74.1.2 Female 234.69000 495.5000  28   74.0 Glutamate           PPP
#> 11     GF  77.3.1 Female 217.23000 323.5500  36   77.0 Glutamate           PPP
#> 12     GI  81.1.1 Female 235.96000 284.2300  28   81.0 Glutamate           PPP
#> 13     GK  84.1.1   Male 248.74000 574.4400  35   84.0 Glutamate           PPP
#> 14     GR  90.3.1   Male 313.50000 415.9700  34   90.0 Glutamate           PPP
#> 15     GX  97.2.3   Male        NA       NA  33   97.0 Glutamate PPP_and_HNMPA
#> 16     HB 100.1.1   Male 133.20000 590.2500  39  100.0 Glutamate PPP_and_HNMPA
#> 17     HC 100.2.2   Male 172.54000 576.2600  39  100.0 Glutamate PPP_and_HNMPA
#> 18     HG 103.2.1 Female        NA       NA  34  103.0 Glutamate PPP_and_HNMPA
#> 19     HN 109.1.1   Male 288.97000 400.1800  38  109.0 Glutamate PPP_and_HNMPA
#> 20     AV  20.3.1 Female  55.09656 248.8031  31   20.0 Glutamate       Control
#>    category                                              R_a notes days_alone
#> 1         2                     1.6, 1.7, 1.7, 1.8, 1.9, 2.0    NA          0
#> 2         2      2.5, 2.5, 2.6, 2.6, 2.9, 2.6, 2.6, 2.6, 2.7    NA          0
#> 3         2                     1.9, 1.9, 1.8, 1.9, 2.0, 3.0    NA          1
#> 4         2                     1.7, 1.7, 1.7, 1.8, 1.7, 1.8    NA          0
#> 5         2 1.4, 1.4, 1.4, 1.4, 1.4, 1.4, 1.8, 1.5, 1.5, 1.5    NA          0
#> 6         2                     2.0, 2.1, 2.1, 2.2, 2.2, 2.2    NA          0
#> 7         2                     1.4, 1.5, 1.5, 1.6, 1.6, 1.5    NA          0
#> 8         2                     2.0, 2.0, 2.1, 2.2, 2.1, 2.1    NA          0
#> 9         2                     1.5, 1.5, 1.6, 1.7, 0.0, 2.0    NA          1
#> 10        2                          1.8, 1.9, 2.1, 2.0, 2.3    NA          0
#> 11        2                     1.6, 1.6, 1.7, 1.7, 1.8, 1.7    NA          0
#> 12        2                     1.3, 1.3, 1.3, 1.3, 1.4, 1.4    NA          0
#> 13        2                          2.2, 2.2, 2.2, 2.3, 2.5    NA          0
#> 14        2                     1.3, 1.4, 1.5, 1.5, 1.6, 1.8    NA          0
#> 15        2                     1.6, 1.6, 1.7, 1.7, 1.8, 1.7    NA          1
#> 16        2                     2.2, 2.5, 2.5, 2.6, 2.6, 2.6    NA          2
#> 17        2                     1.4, 1.5, 1.6, 1.9, 1.8, 2.3    NA          2
#> 18        2                               1.7, 1.7, 1.7, 1.6    NA          0
#> 19        2                     1.7, 1.7, 1.7, 1.7, 1.7, 1.7    NA          0
#> 20        2                     1.6, 1.8, 1.8, 2.0, 2.0, 2.0    NA          0
#>    animal_or_slicing_problems percent_change_access
#> 1                          no                    25
#> 2                          no                     8
#> 3                          no                    58
#> 4                          no                     6
#> 5                          no                     7
#> 6                          no                    10
#> 7                          no                     7
#> 8                          no                     5
#> 9                          no                    33
#> 10                         no                    28
#> 11                         no                     6
#> 12                         no                     8
#> 13                        yes                    14
#> 14                         no                    38
#> 15                        yes                     6
#> 16                        yes                    18
#> 17                        yes                    64
#> 18                         no                    -6
#> 19                        yes                     0
#> 20                         no                    25
```
