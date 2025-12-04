# Evoked current data pruned to a summary point per minute

This is an example of the dataframe of excitatory current amplitudes
produced using
[`make_pruned_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_pruned_EPSC_data.md).
This dataset is a list with three dataframes containing the pruned data.
It can be used for further statistical analyses or for plotting.
[`plot_raw_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_raw_current_data.md)
and
[`plot_summary_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_summary_current_data.md)
in particular depend heavily on this type of dataset.

## Format

An list of 3 dataframes: `individual_cells`, `for_table` and
`all_cells`.

individual_cells is used to make pruned plots of individual recordings
using
[plot_raw_current_data()](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_raw_current_data.md).
`interval_pruned` A character value describing the interval that was
used for the pruning function. If the data are pruned per minute, this
will be "t0to1", "t1to2", "t2to3", etc. `mean_P1` The mean amplitude (in
pA) of the first evoked current (P1) during a specific interval. This is
an average of all data points within each interval. For example, the
`mean_P1` for the interval "t0to1" contains the average current
amplitude of all data points within the first minute of the recording.
`sd_P1` The standard deviation of P1. `n` The number of data points
used. `se` The standard error of P1. `cv` The coefficient of variation
of P1. `cv_inverse_square` The inverse coefficient of variation, which
is then squared. This is to enable variance analysis, as in [Huijstee &
Kessels (2020)](https://doi.org/10.1016/j.jneumeth.2019.108526).
`baseline_mean` The mean amplitude of the first evoked current during
the baseline period. `category, letter, sex, treatment, etc.` Columns
which are from the raw data. For a definition of these columns, please
see the *Required basic columns* section in the documentation for
[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).
`time` The upper time value of the interval (e.g. 2 minutes for "t1to2")
which is used on the x-axis of plots such as in
[`plot_raw_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_raw_current_data.md).
for_table A dataframe containing two columns: letter and P1_transformed.
The current data is collapsed into a single row for each letter, with
the current data for each letter stored as a list. This is required to
create sparklines of current amplitude over time within the cell summary
table. See
[make_interactive_summary_table()](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_interactive_summary_table.md).
all_cells A dataframe consisting of all data within a single treatment
grouped and summarized per minute (or some other variable if you change
interval_length to be something other than 1). Columns like category and
sex are retained from the raw data. New columns for evoked current data
(current_type == "eEPSC") are: `interval_pruned` A character value
describing the interval that was used for the pruning function. If the
data are pruned per minute, this will be "t0to1", "t1to2", "t2to3", etc.
`mean_P1_all_cells` The mean amplitude (in pA) of the first evoked
current (P1) during a specific interval across all cells.
`sd_P1_all_cells` The standard deviation of P1. `n` The number of data
points used. `se_P1_all_cells` The standard error of P1.
`cv_P1_all_cells` The coefficient of variation of P1.

## Details

- `individual_cells` is used to make pruned plots of individual
  recordings using
  [`plot_raw_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_raw_current_data.md).

- `for_table` is used to make the experiment overview table

- `all_cells` is used in
  [`plot_summary_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_summary_current_data.md)
