# Evoked current recording excerpt

This is an excerpt from a raw recording of evoked currents exported from
Clampfit as an .abf file. It was then converted into a regular dataframe
using the
[`import_ABF_file()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_ABF_file.md)
function. To reduce the package installation size, I kept only three
columns from the original file. This dataset is used to demonstrate the
[`plot_spontaneous_current_trace()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_spontaneous_current_trace.md)
function.

## Format

An Axon Binary Format (.abf) file with 43061 observations of 3
variables.

- episode:

  A factor representing the sweep, such as "epi1".

- current:

  Numeric value representing current amplitude in pA.

- time_sec:

  Time in seconds.

## See also

[`plot_spontaneous_current_trace()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_spontaneous_current_trace.md)

## Examples

``` r
plot_spontaneous_current_trace(
  data = sample_abf_file,
  plot_colour = "#6600cc",
  plot_category = 2,
  plot_treatment = "Control",
  state = "Baseline",
  plot_episode = "epi1",
  trace_annotation = "Baseline",
  geom_text_colour = "#6600cc"
)

```
