# Example of action potential count data for two time-points: during the control period (state == "Baseline") or after insulin has been applied (state == "Insulin"). These were obtained by counting the number of action potentials present within each sweep. The data were then merged with the `sample_cell_characteristics` data through `add_new_cells()` with `data_type = "AP_count"`.

Example of action potential count data for two time-points: during the
control period (state == "Baseline") or after insulin has been applied
(state == "Insulin"). These were obtained by counting the number of
action potentials present within each sweep. The data were then merged
with the `sample_cell_characteristics` data through
[`add_new_cells()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/add_new_cells.md)
with `data_type = "AP_count"`.

## Format

An .rda file containing 340 objects of 15 variables.

- `letter, synapses, ID, sex, animal, treatment, etc.`:

  All columns that were produced in
  [`add_new_cells()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/add_new_cells.md).
  Please see
  [sample_cell_characteristics](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_cell_characteristics.md)
  for detailed column descriptions.

- `state`:

  A character ("Baseline" or "Insulin") representing the timepoint that
  the data point belongs to.

- `sweep`:

  The sweep.

- `no_of_APs`:

  The number of action potentials present in the sweep.

- `AP_frequency`:

  The frequency of the action potentials (Hz) in a sweep. This column is
  generated in
  [`add_new_cells()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/add_new_cells.md).

## See also

[`plot_AP_frequencies_multiple_treatments()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_AP_frequencies_multiple_treatments.md)
and
[`plot_AP_frequencies_single_treatment()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_AP_frequencies_single_treatment.md)
