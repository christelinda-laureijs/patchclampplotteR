# Evoked current data summarized into 5-minute intervals

This is an example of the summary eEPSC data produced using
[`make_summary_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_summary_EPSC_data.md).
It is useful for demonstrating functions that build off of this dataset,
such as
[`plot_summary_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_summary_current_data.md).
This is a list of three dataframes.

## Format

An .rda file containing a list of three dataframes. The first dataframe
(`$percent_change_data`) contains the mean current amplitude for each
interval, with a final column (`percent_change`) containing the final
percent change in amplitude in the last interval relative to the
baseline interval. The second dataframe (`$summary_data`) contains
summary statistics for each interval. The third dataframe (`$mean_SE`)
contains averaged evoked current amplitudes (with standard error) for
each time interval. This is useful for publications where you want to
report your findings as means +/- SE or SD.

See below for more details.

### `$percent_change_data`

- `category`:

  A factor representing the experiment type. Used to assign top-level
  groups for further analyses, with `treatment` as subgroups. For
  example, "1" may refer to an experiment where you applied
  high-frequency stimulation (HFS) to a cell, while "2" is an experiment
  where you added a hormone like leptin. "3" may be an experiment where
  you applied HFS in the continuous presence of leptin.

- `letter`:

  A factor that is a unique identifier for a single recording. Used to
  link data sets for evoked or spontaneous currents and
  cell-characteristics.

- `sex`:

  A factor with two levels (e.g. "Male" or "Female").

- `treatment`:

  A factor based on the treatment applied (e.g. "Control", "HNMPA").
  This represents the antagonists or agonists applied, or any protocol
  applied to the animals (e.g. "Fasting").

- `age`:

  A numeric value representing the animal's age. Can be any value as
  long as the time units are consistent throughout (e.g. don't mix up
  days and months when reporting animal ages).

- `animal`:

  A numeric value representing the animal's ID or number.

- `X`:

  A numeric value representing the x-value of the cell's location in µm.
  Leave this blank if you don't have this data.

- `Y`:

  A numeric value representing the y-value of the cell's location in µm.
  Leave this blank if you don't have this data.

- `synapses`:

  A character value (e.g. "Glutamate" or "GABA").

- `days_alone`:

  A numeric value describing the number of days that the animal was left
  alone in a cage. This typically ranges from 0 to 2. Fasted animals
  will have 1 day alone.

- `animal_or_slicing_problems`:

  A character value ("yes" or "no") describing if there were any issues
  with the animal (for example, the animal was unusually anxious) or
  slicing (there were delays during the process, the slices were
  crumpling, etc.).

- `t0to5`:

  The mean evoked current amplitude (pA) for this cell during the period
  of 0 to 5 minutes.

- `t5to10`:

  The mean evoked current amplitude (pA) for this cell during the period
  of 5 to 10 minutes.

- `t10to15`, `t15to20`, `tXtY` etc...:

  The mean evoked current amplitude (pA) for this cell during the period
  of *X* to *Y* minutes.

- `percent_change`:

  The percent change in evoked current amplitude in the interval
  `t20to25` as a percentage of the mean baseline amplitude (`t0to5`).
  For example, if currents began at 100 pA during the baseline period,
  but were 50 pA by `t20to25`, the value of `percent_change` will be
  `50%` or `0.50`.

### `$summary_data`

- `category`:

  A factor representing the experiment type. Used to assign top-level
  groups for further analyses, with `treatment` as subgroups. For
  example, "1" may refer to an experiment where you applied
  high-frequency stimulation (HFS) to a cell, while "2" is an experiment
  where you added a hormone like leptin. "3" may be an experiment where
  you applied HFS in the continuous presence of leptin.

- `letter`:

  A factor that is a unique identifier for a single recording. Used to
  link data sets for evoked or spontaneous currents and
  cell-characteristics.

- `sex`:

  A factor with two levels (e.g. "Male" or "Female").

- `treatment`:

  A factor based on the treatment applied (e.g. "Control", "HNMPA").
  This represents the antagonists or agonists applied, or any protocol
  applied to the animals (e.g. "Fasting").

- `interval`:

  A factor indicating the interval that the data point belongs to. For
  example, `interval` will be "t0to5" for any data points from 0 to 5
  minutes. Example values: "t0to5", "t5to10", etc.

- `mean_P1_transformed`:

  A numeric value representing the mean current amplitude (in pA
  normalized to the baseline amplitude) of the first evoked current. The
  values used to produce mean_P1_transformed come from all data points
  within each interval (the length of the interval was specified in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md)).

- `mean_P1_raw`:

  The same values as those in `mean_P1_transformed` except these values
  contain raw values for current amplitude (e.g. the data were not
  baseline transformed).

- `n`:

  The number of data points used to produce each value in
  `mean_P1_transformed` and `mean_P1_raw`

- `sd`:

  The standard deviation of the mean current values.

- `cv`:

  The coefficient of variation of the mean current values.

- `se`:

  The standard error of the mean current values.

- `cv_inverse_square`:

  The inverse coefficient of variation squared of the mean current
  values. Calculated by using (`1/cv^2`), where `cv` is the coefficient
  of variation. Useful for variance analysis.

- `variance`:

  The variance of the current values, calculated using
  [`stats::var()`](https://rdrr.io/r/stats/cor.html).

- `VMR`:

  The variance-to-mean ratio, calculated using
  `variance/mean_P1_transformed`. Useful for variance analysis.

- `age`:

  A numeric value representing the animal's age. Can be any value as
  long as the time units are consistent throughout (e.g. don't mix up
  days and months when reporting animal ages).

- `animal`:

  A numeric value representing the animal's ID or number.

- `X`:

  A numeric value representing the x-value of the cell's location in µm.
  Leave this blank if you don't have this data.

- `Y`:

  A numeric value representing the y-value of the cell's location in µm.
  Leave this blank if you don't have this data.

- `time`:

  A numeric value that represents time in minutes. This column is
  autogenerated in
  [`add_new_cells()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/add_new_cells.md).

- `synapses`:

  A character value (e.g. "Glutamate" or "GABA").

- `days_alone`:

  A numeric value describing the number of days that the animal was left
  alone in a cage. This typically ranges from 0 to 2. Fasted animals
  will have 1 day alone.

- `animal_or_slicing_problems`:

  A character value ("yes" or "no") describing if there were any issues
  with the animal (for example, the animal was unusually anxious) or
  slicing (there were delays during the process, the slices were
  crumpling, etc.).

### `$mean_SE`

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

## See also

[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md)
and
[`make_pruned_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_pruned_EPSC_data.md)
