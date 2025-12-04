# Spontaneous current data pruned to a summary point per minute

This is an example of the dataframe of spontaneous currents produced
using
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
`letter, category, sex, treatment, etc.` Information about cell
characteristics, which has automatically been pulled from
cell_characteristics_df in \[add_new_cells(). For a definition of these
columns, please see the *Required basic columns* section in the
documentation for
[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).
`interval_pruned` A character value describing the interval that was
used for the pruning function. If the data are pruned per minute, this
will be "t0to1", "t1to2", "t2to3", etc. `mean_amplitude` The mean
normalized amplitude (% Baseline sEPSC amplitude) of all spontaneous
currents during a specific interval. `mean_raw_amplitude` The mean raw
amplitude (pA) of all spontaneous currents during a specific interval.
`sd_amplitude` The standard deviation of the normalized spontaneous
current amplitudes. `n` The number of data points used. `frequency` The
frequency of the spontaneous currents (Hz) within the interval. `se` The
standard error of the normalized spontaneous current amplitudes.
`baseline_range` A logical value required to normalize frequency
relative to the mean baseline frequency. It is set to TRUE when time is
within the baseline period (e.g. `time <= 5`) and FALSE at all other
times. Generated automatically in
[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).
`baseline_mean_frequency` The mean frequency of the spontaneous currents
during the baseline period. `frequency_transformed` The normalized
spontaneous current frequency (% Baseline frequency). `time` The upper
time value of the interval (e.g. 2 minutes for "t1to2") which is used on
the x-axis of plots such as in
[`plot_raw_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_raw_current_data.md).
for_table A dataframe containing two columns: letter and
spont_amplitude_transformed. The current data is collapsed into a single
row for each letter, with the current data for each letter stored as a
list. This is required to create sparklines of current amplitude over
time within the cell summary table. See
[make_interactive_summary_table()](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_interactive_summary_table.md).
all_cells A dataframe consisting of all data within a single treatment
grouped and summarized per minute (or some other variable if you change
interval_length to be something other than 1). Columns like category and
sex are retained from the raw data. New columns for spontaneous current
data (current_type == "sEPSC") are: `interval_pruned` A character value
describing the interval that was used for the pruning function. If the
data are pruned per minute, this will be "t0to1", "t1to2", "t2to3", etc.
`mean_all_amplitude` The mean normalized amplitude (% Baseline sEPSC
amplitude) of the spontaneous current amplitudes during a specific
interval across all cells. `mean_all_raw_amplitude` The mean raw
amplitude (in pA) of the spontaneous current amplitudes during a
specific interval across all cells. `sd_all_amplitude` The standard
deviation of the normalized spontaneous current amplitudes across all
cells. `n` The number of data points used. `se_all_amplitude` The
standard error of the normalized spontaneous currents across all cells.
`sd_all_raw_amplitude` The standard deviation of the raw spontaneous
current amplitudes across all cells. `se_raw_amplitude` The standard
error of the raw spontaneous currents across all cells.
`mean_all_frequency` The mean normalized frequency (% Baseline
frequency) of all spontaneous current amplitudes across all cells during
the interval. `sd_all_frequency` The standard deviation of the
normalized frequency of all spontaneous current amplitudes across all
cells during the interval. `se_frequency` The standard error of the
normalized frequency of all spontaneous current amplitudes across all
cells during the interval. `mean_all_raw_frequency` The mean raw
frequency (Hz) of all spontaneous current amplitudes across all cells
during the interval. `sd_all_raw_frequency` The standard deviation of
the raw frequency of all spontaneous current amplitudes across all cells
during the interval. `se_raw_frequency` The standard error of the raw
frequency of all spontaneous current amplitudes across all cells during
the interval.

## Details

- `individual_cells` is used to make pruned plots of individual
  recordings using
  [`plot_raw_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_raw_current_data.md).

- `for_table` is used to make the experiment overview table.

- `all_cells` is used in
  [`plot_summary_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_summary_current_data.md).
