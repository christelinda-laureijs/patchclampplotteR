# Example of a complete action potential dataset used for plotting and analyses. This data contains parameters like peak_amplitude, after-hyperpolarization amplitude, and half-width for two time-points: during the control period (state == "Baseline") or after insulin has been applied (state == "Insulin").

This is an example of what the data looks like after being imported with
[`add_new_cells()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/add_new_cells.md)
with `data_type = "AP"`. You can see that the same columns from the cell
characteristics file have been appended to new rows for peak_amplitude,
after-hyperpolarization amplitude, latency to fire, and others.

## Format

An .rda file containing 34 objects of 25 variables.

- `letter, synapses, ID, sex, animal, treatment, etc.`:

  All columns that were produced in
  [`add_new_cells()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/add_new_cells.md).
  Please see
  [sample_cell_characteristics](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_cell_characteristics.md)
  for detailed column descriptions.

- `state`:

  A character ("Baseline" or "Insulin") representing the timepoint that
  the data point belongs to.

- `time_of_threshold`:

  The time (in ms) when the membrane potential reaches the threshold
  value.

- `threshold`:

  The threshold (in mV). Determined using the first derivative method,
  where the threshold is the membrane potential which results in a
  derivative of 10 mV/ms or greater (Farries et al., 2010).

- `t_11`:

  The value of the first derivative (action potential velocity in mV/ms)
  at threshold.

- `first_sweep_with_APs`:

  The sweep number of the first sweep (going from lowest to higher
  current injection values) that resulted in an action potential.

- `trace_start`:

  An automated value in Clampfit that is not used in the analysis.

- `peak_amplitude`:

  The peak amplitude (in pA) of the action potential relative to
  threshold.

- `time_to_peak`:

  The time to peak amplitude (in ms) relative to the time of threshold.

- `antipeak_amplitude`:

  The after-hyperpolarization amplitude (in pA) relative to threshold.

- `time_of_antipeak`:

  The time of the after-hyperpolarization (in ms).

- `half_width`:

  The half-width, which is the width of the action potential and half of
  the peak amplitude.

- `latency_to_fire`:

  The latency to fire, which is calculated by subtracting the injection
  start time from the `time_to_peak.`

- `antipeak_time_relative_to_threshold`:

  The time of the afterhyperpolarization amplitude (in ms) relative to
  the time of the threshold.

## See also

[`plot_AP_comparison()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_AP_comparison.md)
and
[`plot_AP_trace()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_AP_trace.md).
