# Variance-to-mean ratio and coefficient of variation of evoked current amplitudes

This is an example of the dataframe generated from evoked current
summary data processed using the function
[`make_variance_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_variance_data.md).
It contains new columns for the mean variance-to-mean ratio (VMR) and
mean inverse coefficient of variation squared for two time intervals.
They are the baseline period ("t0to5"), and a time interval after a
hormone or protocol has been applied (in this case, "t20to25"). It can
be used for further analyses and also to make the variance comparison
plots.

## Format

An .rda file containing 38 objects of 23 variables.

- `state`:

  A character value describing if a data point belongs to the baseline
  interval ("Baseline") or an interval after a hormone or protocol has
  been applied ("Post-modification"). These intervals are selected from
  `baseline_interval` and `post_hormone_interval`.

- `mean_cv_inverse_square`:

  The mean inverse coefficient of variation squared within a specific
  state and sex.

- `mean_VMR`:

  The mean variance-to-mean ratio within a specific state and sex.

- `category, letter, sex, treatment, etc.`:

  Other columns which are imported directly from the summary data
  without modifications. For a detailed description of these columns,
  please see
  [sample_summary_eEPSC_df](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_summary_eEPSC_df.md).

## See also

[`make_variance_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_variance_data.md),
[`plot_variance_comparison_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_variance_comparison_data.md),
and
[`plot_cv_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_cv_data.md)
