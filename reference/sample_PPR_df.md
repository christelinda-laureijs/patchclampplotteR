# Evoked current data filtered to focus on paired-pulse ratio (PPR) analysis

This is an example of the raw eEPSC data produced using
[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md)
after it has been filtered using
[`make_PPR_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_PPR_data.md).
The data are not modified, but they are filtered to only include data
within two time intervals (specified in the `baseline_interval` and
`post_hormone_interval` arguments). The paired-pulse ratio (PPR) values
are also filtered to only include values that fall within `PPR_min` and
`PPR_max`, which are two arguments in the
[`make_PPR_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_PPR_data.md)
function.

## Format

An .rda file containing 2206 objects of 21 variables.

- `letter, synapses, sex, etc.`:

  All columns that were produced in
  [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md).
  Please see
  [sample_raw_eEPSC_df](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_raw_eEPSC_df.md)
  for detailed column descriptions.

- `state`:

  A character ("baseline" or "post-modification") representing the
  timepoint that the data point belongs to.

## Details

This data can be used for PPR comparison plots, and further analyses to
determine the mechanism of synaptic plasticity.

## See also

[`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md),
[`make_PPR_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_PPR_data.md),
[`plot_PPR_data_single_treatment()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_PPR_data_single_treatment.md)
and
[`plot_PPR_data_multiple_treatments()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_PPR_data_multiple_treatments.md)
