
<!-- README.md is generated from README.Rmd. Please edit that file -->

# patchclampplotteR <img src="man/figures/logo.png" align="right" height="138" alt="Hex Sticker for patchclampplotter package showing a neuron with electrodes inside it." />

<!-- badges: start -->
<!-- badges: end -->

<span style="color:#55b323;font-weight:bold;">patch</span><span style="color:#6600cc;font-weight:bold;">clamp</span><span style="color:
black;font-weight:bold;">plotte</span><span style="color:#0093fb;font-weight:bold;">R</span>
provides a set of user-friendly tools for electrophysiologists who want
to plot and analyze data from whole-cell patch clamp electrophysiology
recordings. Here’s some of the functions that this package can do (see
below for examples!):

<img src="man/figures/README-plot-sampler.png" alt="A banner with pictures of some of the plots that can be created with the patchclampplotteR package." width="100%" style="display: block; margin: auto;" />

- Plot raw evoked or spontaneous current amplitudes over time for all
  recordings at once.
- Normalize current amplitudes relative to mean baseline values.
- Plot normalized current amplitudes over time for all recordings.
- Plot summary data for a specific treatment, grouped by sex.
- Plot representative spontaneous current traces from an .abf file with
  a scale bar.
- Compare spontaneous current amplitude and frequency
- Compare variance parameters to help determine presynaptic mechanisms.
- And more!

## Examples

Plot raw evoked currents for a specific cell:

``` r
raw_eEPSC_control_plots <- plot_raw_current_data(
  data = sample_raw_eEPSC_df,
  plot_treatment = "Control",
  plot_category = 2,
  current_type = "eEPSC",
  y_variable = "P1",
  pruned = "no",
  hormone_added = "Insulin",
  hormone_or_HFS_start_time = 5,
  theme_options = sample_theme_options,
  treatment_colour_theme = sample_treatment_names_and_colours
)

raw_eEPSC_control_plots$AO
```

<img src="man/figures/README-example-raw-eEPSC-plot-1.png" alt="A plot of evoked current amplitude (in pA) over time in minutes showing a decrease in evoked current amplitude after adding insulin." width="100%" />

Plot evoked current amplitudes summarized by sex:

``` r
plot_summary_current_data(
  plot_category = 2,
  plot_treatment = "Control",
  data = sample_pruned_eEPSC_df$all_cells,
  current_type = "eEPSC",
  y_variable = "amplitude",
  include_representative_trace = "no",
  y_axis_limit = 175,
  signif_stars = "yes",
  t_test_df = sample_eEPSC_t_test_df,
  hormone_added = "Insulin",
  large_axis_text = "no",
  shade_intervals = "no",
  hormone_or_HFS_start_time = 5,
  treatment_colour_theme = sample_treatment_names_and_colours,
  theme_options = sample_theme_options
)
```

<img src="man/figures/README-example-summary-eEPSC-plot-1.png" alt="A scatterplot showing evoked current amplitude (% baseline) versus time in minutes, where 500 nM of insulin have been added from 5 minutes and onwards. Insulin significantly decreased current amplitude in both males and females." width="100%" />

Plot a representative recording trace showing spontaneous currents from
a raw Axon Binary File (.ABF):

``` r
plot_spontaneous_current_trace(
  file = sample_abf_file,
  plot_colour = "#6600cc",
  include_scale_bar = "yes",
  plot_episode = "epi1",
  scale_bar_x_length = 1,
  scale_bar_y_length = 10,
  plot_x_min = 1,
  plot_x_max = 3
)
```

<img src="man/figures/README-example-spontaneous-current-trace-plot-1.png" alt="A representative trace from a raw abf recording, showing a series of noisy, negative-going peaks. Each peak represents a spontaneous current" width="100%" />

## Installation

You can install the development version of patchclampplotteR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("christelinda-laureijs/patchclampplotteR")
```

And then load the package with `library()`:

``` r
library(patchclampplotteR)
```

## Analysis

To learn about how to convert raw .abf data in Clampfit to .csv files
for this package, please see the vignettes [Evoked Current
Analysis](https://christelinda-laureijs.github.io/patchclampplotteR/articles/evoked-current-analysis.html)
and [Action Potential
Analysis](https://christelinda-laureijs.github.io/patchclampplotteR/articles/action-potential-analysis.html).

## Using patchclampplotteR

Please see the [Getting
Started](https://christelinda-laureijs.github.io/patchclampplotteR/articles/patchclampplotteR.html)
page for a walk-through of the major functions of this package.
