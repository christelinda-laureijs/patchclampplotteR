# Paired t-test results comparing evoked current amplitudes relative to baseline in males only

This is an example of the dataframe of t-test results produced using
[`perform_t_tests_for_summary_plot()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/perform_t_tests_for_summary_plot.md).
NOTE: This data has been filtered to include only MALE data. The
dataframe can be used for further statistical analyses, and it also has
specialized columns to add significance stars to the plot produced with
[`plot_summary_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_summary_current_data.md).

## Format

An .rda file containing 16 objects of 10 variables

- `treatment`:

  A character value specifying the treatment applied (e.g. "Control",
  "HNMPA") such as the antagonists or agonists, or any protocol applied
  to the animals (e.g. "Fasting").

- `.y.`:

  A character value describing the parameter compared with the t-test.
  In this case, this uses the mean normalized amplitude of the first
  evoked current ("mean_P1_transformed") which has been generated using
  [`make_summary_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_summary_EPSC_data.md).

- `group1`:

  A character value describing the first group used in the paired
  t-test. This is the baseline interval, "t0to5".

- `group2`:

  A character value describing the second group used in the paired
  t-test. Examples include "t5to10" and "t10to15".

- `n1`:

  The number of paired values used in the comparison.

- `statistic`:

  The test statistic.

- `df`:

  The degrees of freedom.

- `p_string`:

  The p-value expressed as a character value using `pvalString` from the
  `lazyWeave` package, which rounds the values and expresses them
  according to typical published values (e.g. \> 0.99, \< 0.001 instead
  of exact values).

- `significance_stars`:

  A character value containing asterisks indicating significance.
  `p < 0.05 = *`, `p < 0.01 = **`, and `p < 0.001 = ***`.

- `asterisk_time`:

  A numeric value indicating the time value where a significance star
  will be added. This is the midpoint of each interval. For example, the
  `asterisk_time` for an interval from `5` to `10` minutes will be `7.5`
  minutes.
