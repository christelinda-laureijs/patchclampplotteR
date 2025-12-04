# Add new data

This function enables you to append new raw recording data onto an
existing datasheet. It makes it easy and convenient to merge the cell
parameters (age, sex, etc.) with new data and add it to your current raw
data. This function also formats the dataset so it is immediately ready
for use in functions like
[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).

## Usage

``` r
add_new_cells(
  new_raw_data_csv,
  cell_characteristics_csv,
  old_raw_data_csv,
  data_type,
  software = "MiniAnalysis",
  bin_width = 5,
  write_new_csv = "yes",
  new_file_name,
  decimal_places = 2,
  injection_start_time = 265.4,
  length_of_current_injection = 0.5
)
```

## Arguments

- new_raw_data_csv:

  A filepath to a csv containing the new raw data. If the data are
  evoked current data (`current_type == "eEPSC"`) then this must contain
  4 columns: `letter`, `ID`, `P1` and `P2`. If the data are spontaneous
  current data, the columns must be `letter`, `ID`, `recording_num`,
  `trace`, `amplitude` and `time_of_peak`. Please see the section below
  on required columns for more details.

- cell_characteristics_csv:

  A filepath to a csv containing information about the cells. Please see
  [`import_cell_characteristics_df()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_cell_characteristics_df.md)
  for a description of what columns should be included. Don't forget to
  update this to include the cell characteristics for the new letters in
  `new_raw_data_csv`!

- old_raw_data_csv:

  A filepath to a csv containing the current raw data. Since this
  function appends the new data to the old data, this must be of the
  same current_type as the new data (e.g. the columns must be the same).
  If this is the first time you are running this function, start with a
  blank .csv file containing just some text (e.g. `letter`) in cell
  `A1`. This is required because R cannot recognize an empty `.csv`
  sheet as a valid file.

- data_type:

  A character (`"eEPSC"`, `"sEPSC"`, `"AP_parameter"` or `"AP_count"`)
  describing the data type that is being imported.

- software:

  A character (`"Clampfit"` or `"MiniAnalysis"`) describing what
  software tool was used to analyze the data in `new_raw_data_csv`. This
  is relevant when `data_type` is `"sEPSC"` because the exported data is
  different. Defaults to `"Clampfit"`.

- bin_width:

  A numeric value (defaults to `5`) describing the time interval in
  seconds (bin width) of the histogram used in MiniAnalysis. This is
  only relevant when `data_type = "sEPSC"` and
  `software = "MiniAnalysis"`.

- write_new_csv:

  A character (`"yes"` or `"no"`) describing if the new data should be
  written to a csv file. Defaults to `"yes"`. Please specify a filename
  for the new csv file in `new_file_name`.

- new_file_name:

  A filename for the csv containing the new data appended to the old
  data. Must be a character representing a filepath to a csv file.
  Examples include `"Data/20241118-Raw-eEPSC-data.csv"`.

- decimal_places:

  A numeric value indicating the number of decimal places the data
  should be rounded to. Used to reduce file size and prevent an
  incorrect representation of the number of significant digits.

- injection_start_time:

  For action potential data only: A numeric value describing the start
  time (in ms) when current injection was applied. Used to calculate the
  latency to fire.

- length_of_current_injection:

  For action potential data only: A numeric value indicating the
  duration of the current injection (in s, default is 0.5 s).

## Value

A dataframe consisting of the old raw data with information from the new
cells appended to it. If `data_type = "AP_parameter"` two new columns
will also be added based on calculations from the existing columns.
These are `latency_to_fire` (which is `time_to_peak` -
`injection_start_time`) and `antipeak_time_relative_to_threshold` (which
is `time_of_antipeak` - `time_of_threshold`). If
`data_type = "AP_count"` one new column will be added. These is
`AP_frequency` (in Hz).

## Required Columns

If the data are evoked currents (`data_type = "eEPSC"`), the data must
contain the following four columns:

- `letter` A character value that is a unique identifier for a single
  recording. Used to link data sets for evoked or spontaneous currents
  and cell-characteristics.

- `ID` A character value for the recording filename.

- `P1` A numeric value representing the amplitude of the first evoked
  current in pA.

- `P2` A numeric value representing the amplitude of the second evoked
  current in pA.

If the data are spontaneous currents (`data_type = "sEPSC"`) and these
were exported from Clampfit (`software = "Clampfit"`), the data must
contain the following columns:

- `letter` A character value that is a unique identifier for a single
  recording. Used to link data sets for evoked or spontaneous currents
  and cell-characteristics.

- `ID` A character value for the recording filename.

- `recording_num` A numeric value representing the recording number.
  This was incorporated before we switched to concatenating all
  recordings into one, but it needs to remain here to prevent breaking
  previous projects. It should be set to 1.

- `trace` A numeric value representing the trace (automatically
  generated in Clampfit) where the current occurred.

- `amplitude` A numeric value representing the amplitude of the evoked
  current in pA (automatically generated in Clampfit).

- `time_of_peak` A numeric value representing the time of the peak in
  milliseconds relative to trace number (automatically generated in
  Clampfit).

If the data are spontaneous currents (`data_type = "sEPSC"`) and these
were exported from the histogram tool in MiniAnalysis
(`software = "MiniAnalysis"`), the data must contain the following
columns:

- `letter` A character value that is a unique identifier for a single
  recording. Used to link data sets for evoked or spontaneous currents
  and cell-characteristics.

- `ID` A character value for the recording filename.

- `amplitude` The average sEPSC amplitude, averaged over the time
  interval (bin width) that you chose for the histogram (automatically
  generated from histogram tool in MiniAnalysis).

- `SE` The standard error of `amplitude` (automatically generated from
  histogram tool in MiniAnalysis).

- `time` The time in seconds (exported automatically from the histogram,
  depending on the bin width that you set). `add_new_cells()` will
  automatically convert this to minutes.

- `num_events` The number of synaptic events (sEPSCs) during the time
  interval (bin width). This is also automatically generated in
  MiniAnalysis.

If the data are action potential parameters
(`data_type == "AP_parameter"`), the data must contain the following
columns:

- `letter` A character value that is a unique identifier for a single
  recording. Used to link data sets for evoked or spontaneous currents
  and cell-characteristics.

- `ID` A character value for the recording filename.

- `state` A character (`"Baseline"` or `"Insulin"`) representing the
  timepoint that the data point belongs to.

- `time_of_threshold` The time (in ms) when the membrane potential
  reaches the threshold value.

- `threshold` The threshold (in mV). Determined using the first
  derivative method, where the threshold is the membrane potential which
  results in a derivative of 10 mV/ms or greater (Farries et al., 2010).

- `t_11` The value of the first derivative (action potential velocity in
  mV/ms) at threshold.

- `first_sweep_with_APs` The sweep number of the first sweep (going from
  lowest to higher current injection values) that resulted in an action
  potential.

- `trace_start` An automated value in Clampfit that is not used in the
  analysis.

- `peak_amplitude` The peak amplitude (in pA) of the action potential
  relative to threshold.

- `time_to_peak` The time to peak amplitude (in ms) relative to the time
  of threshold.

- `antipeak_amplitude` The after-hyperpolarization amplitude (in pA)
  relative to threshold.

- `time_of_antipeak` The time of the after-hyperpolarization (in ms).

- `half_width` The half-width, which is the width of the action
  potential and half of the peak amplitude.

## See also

[`import_cell_characteristics_df()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_cell_characteristics_df.md)
for a list of required columns in the `cell_characteristics_csv`.

[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md)
for the next step in the analysis process.

## Examples

``` r

# NOTE: Remember, if you are running this for the first time,
# don't start with a blank csv for `old_raw_data_csv`.
# `old_raw_data_csv` must have at least some text
# in cell `A1` for R to recognize it.

# NOTE: If you are importing spontaneous current data from MiniAnalysis,
# don't forget to set `bin_width` to match what you used
# in the histogram tool in MiniAnalysis!

if (FALSE) { # \dontrun{
add_new_cells(
  new_raw_data_csv = "sample_new_eEPSC_data.csv",
  cell_characteristics_csv = "sample_cell_characteristics.csv",
  old_raw_data_csv = "sample_eEPSC_data.csv",
  data_type = "eEPSC",
  write_new_csv = "no",
  new_file_name = "Raw-eEPSC-Data.csv",
  decimal_places = 2
)
} # }
```
