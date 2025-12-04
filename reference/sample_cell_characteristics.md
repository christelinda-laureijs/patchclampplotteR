# Information about sex, age, treatment, and animal ID

This dataset provides an example of the type of cell characteristics
that you should be recording for your data. The dataset contains columns
for the animal number, age, sex, the synapses being recorded from, and
treatments applied. There is also a column for access, which is stored
as a list.

The most important column is `letter`. This is a unique identifier that
you will assign to each recording. The `letter` column will enable you
to link all information relevant to the recording (evoked current data,
spontaneous current data, animal data like age, sex, etc.) from
different files.

You should import this .csv file using the
[`import_cell_characteristics_df()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_cell_characteristics_df.md)
function, which will format it in a format that makes it easy to merge
with raw recording data in functions like
[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).
This will enable you to analyze relationships between properties like
age and current amplitude.

## Format

A dataframe with 19 rows and 14 columns

- letter:

  A character value that is a unique identifier for a single recording.
  Used to link data sets for evoked or spontaneous currents and
  cell-characteristics. Example: "A"

- cell:

  A character or numeric value representing the cell. For example, use
  `3.1.1` for animal \#3, slice \#1, cell \#1.

- sex:

  A character value such as `"Male"` or `"Female"`.

- X:

  A numeric value representing the x-value of the cell's location in µm.

- Y:

  A numeric value representing the y-value of the cell's location in µm.

- age:

  A numeric value representing the animal's age. Can be any value as
  long as the time units are consistent throughout (e.g. don't mix up
  days and months when reporting animal ages). Do not use characters
  (e.g. avoid "P31" and use 31 instead).

- animal:

  A numeric value representing the animal's ID or number.

- synapses:

  A character value such as "Glutamate" or "GABA".

- treatment:

  A character value such as "Control" or "HNMPA".

- category:

  A numeric value representing the experiment type. Used to assign
  top-level groups for further analyses, with `treatment` as subgroups.
  For example, "1" may refer to an experiment where you applied
  high-frequency stimulation (HFS) to a cell, while "2" is an experiment
  where you added a hormone like leptin. "3" may be an experiment where
  you applied HFS in the continuous presence of leptin.

- R_a:

  A list of values for the access resistance, which would have been
  monitored at several timepoints throughout the recording. See the
  section `R_a` formatting in the documentation for
  [`import_cell_characteristics_df()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_cell_characteristics_df.md).

- notes:

  An optional character column with notes about any issues, sweeps that
  were removed during Clampfit processing, etc.

- days_alone:

  A numeric value describing the number of days that the animal was left
  alone in a cage. This typically ranges from 0 to 2. Fasted animals
  will have 1 day alone.

- animal_or_slicing_problems:

  A character value ("yes" or "no") describing if there were any issues
  with the animal (for example, the animal was unusually anxious) or
  slicing (there were delays during the process, the slices were
  crumpling, etc.).

## See also

[`import_cell_characteristics_df()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_cell_characteristics_df.md)
to import data like this from a .csv file.

[`make_interactive_summary_table()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_interactive_summary_table.md)
for a function which merges cell characteristics information with pruned
evoked and spontaneous current data to create an interactive overview
table of all recordings.

## Examples

``` r
utils::read.csv(import_ext_data("sample_cell_characteristics.csv"))
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
#> 9         2                    1.5, 1.5, 1.6, 1.7, 1.9,, 2.0    NA          1
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
#>    animal_or_slicing_problems
#> 1                          no
#> 2                          no
#> 3                          no
#> 4                          no
#> 5                          no
#> 6                          no
#> 7                          no
#> 8                          no
#> 9                          no
#> 10                         no
#> 11                         no
#> 12                         no
#> 13                        yes
#> 14                         no
#> 15                        yes
#> 16                        yes
#> 17                        yes
#> 18                         no
#> 19                        yes
#> 20                         no
```
