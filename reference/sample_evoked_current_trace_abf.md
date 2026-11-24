# Evoked current recording excerpt for evoked trace demonstration

This is an excerpt from a raw recording of evoked currents exported from
Clampfit as an .abf file. It was then converted into a regular dataframe
using the
[`import_ABF_file()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_ABF_file.md)
function. To reduce the package installation size, I kept only data
between 0.55 and 0.63 seconds (reducing it from 50000 rows to 801 rows).
This dataset is used to demonstrate the
[`plot_evoked_current_trace()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_evoked_current_trace.md)
function.

## Format

An Axon Binary Format (.abf) file with 801 observations of 3 variables.

- episode:

  A factor representing the sweep, such as "epi1".

- current:

  Numeric value representing current amplitude in pA.

- time_sec:

  Time in seconds.

## See also

[`plot_evoked_current_trace()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_evoked_current_trace.md)

## Examples

``` r

plot_evoked_current_trace(
  data = sample_evoked_current_trace_abf,
  include_scale_bar = "yes",
  k_value = 100,
  include_scale_bar_label = "no",
  trace_annotation = "Baseline"
)

```
