# Package index

## Import data

Functions that import raw data (typically as a comma-separated file
(.csv) or Axon Binary Format (.abf) file).

- [`import_ABF_file()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_ABF_file.md)
  :

  Import raw `.abf` files as a dataframe

- [`import_cell_characteristics_df()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_cell_characteristics_df.md)
  : Import cell characteristics

- [`import_theme_colours()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_theme_colours.md)
  : Import colour theme

- [`import_ext_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_ext_data.md)
  : Get path to external data for package examples

- [`add_new_cells()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/add_new_cells.md)
  : Add new data

## Modify data

Functions that normalize, prune, and summarize the data.

- [`make_normalized_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_normalized_EPSC_data.md)
  : Import and normalize raw current data
- [`make_pruned_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_pruned_EPSC_data.md)
  : Prune and summarize raw current data per minute
- [`make_summary_EPSC_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_summary_EPSC_data.md)
  : Summarize current data per 5-min for statistical tests
- [`make_PPR_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_PPR_data.md)
  : Make paired-pulse ratio (PPR) dataframe for before vs. after
  comparisons
- [`make_variance_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_variance_data.md)
  : Make dataframe with variance measures
- [`make_interactive_summary_table()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_interactive_summary_table.md)
  : Make interactive overview table of all recordings
- [`perform_t_tests_for_summary_plot()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/perform_t_tests_for_summary_plot.md)
  : Perform t-tests (or Wilcoxon tests) for EPSC summary plots

## Plot data

Functions to plot the data.

- [`patchclampplotteR_theme()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/patchclampplotteR_theme.md)
  : Add a customized ggplot2 theme
- [`patchclampplotteR_facet_theme()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/patchclampplotteR_facet_theme.md)
  : The patchclampplotteR theme for facet plots
- [`get_fig_height()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/get_fig_height.md)
  : Get figure height
- [`insert_png_as_ggplot()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/insert_png_as_ggplot.md)
  : Import an image as a ggplot object
- [`plot_cell_coordinates_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_cell_coordinates_data.md)
  : Plot cell location data

### Evoked and spontaneous currents

- [`plot_raw_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_raw_current_data.md)
  : Make raw current plots
- [`plot_summary_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_summary_current_data.md)
  : Make a summary plot for a specific treatment
- [`plot_baseline_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_baseline_data.md)
  : Make baseline comparison plot
- [`plot_percent_change_comparisons()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_percent_change_comparisons.md)
  : Plot percent change comparisons
- [`make_facet_plot()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/make_facet_plot.md)
  : Make facet plot of raw data

### Spontaneous currents only

- [`plot_spontaneous_current_parameter_comparison()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_spontaneous_current_parameter_comparison.md)
  : Visually compare spontaneous current parameters
- [`plot_spontaneous_current_trace()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_spontaneous_current_trace.md)
  : Plot a representative spontaneous current trace

### Paired-pulse ratio

- [`plot_PPR_data_single_treatment()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_PPR_data_single_treatment.md)
  : Make a PPR plot for a single treatment
- [`plot_PPR_data_multiple_treatments()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_PPR_data_multiple_treatments.md)
  : Make a PPR plot for multiple treatments

### Action potentials

- [`plot_AP_trace()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_AP_trace.md)
  : Plot action potential recording
- [`plot_AP_comparison()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_AP_comparison.md)
  : Plot and compare action potential parameters before and after a
  treatment
- [`plot_AP_frequencies_single_treatment()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_AP_frequencies_single_treatment.md)
  : Plot action potential frequency curves for a single treatment
- [`plot_AP_frequencies_multiple_treatments()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_AP_frequencies_multiple_treatments.md)
  : Plot action potential frequency curves for multiple treatments

### Variance analysis

- [`plot_cv_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_cv_data.md)
  : Make a plot of coefficient of variation over time
- [`plot_variance_comparison_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_variance_comparison_data.md)
  : Plot variance comparison for a treatment

## Sample datasets

Sample datasets for vignettes and examples.

- [`sample_cell_characteristics`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_cell_characteristics.md)
  : Information about sex, age, treatment, and animal ID
- [`sample_abf_file`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_abf_file.md)
  : Evoked current recording excerpt
- [`sample_theme_options`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_theme_options.md)
  : A dataframe of theme options for things like colours and line widths
- [`sample_treatment_names_and_colours`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_treatment_names_and_colours.md)
  : A dataframe of treatments and their assigned colours for consistency
  across plots

### Evoked current (eEPSC) data

Sample datasets from recordings measuring evoked current amplitude over
time. The paired-pulse ratio (PPR) data are only available for evoked
current recordings because the PPR is determined from evoked current
amplitudes, not spontaneous currents.

- [`sample_raw_eEPSC_df`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_raw_eEPSC_df.md)
  : Representative data from a series of evoked (eEPSC) currents
- [`sample_pruned_eEPSC_df`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_pruned_eEPSC_df.md)
  : Evoked current data pruned to a summary point per minute
- [`sample_summary_eEPSC_df`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_summary_eEPSC_df.md)
  : Evoked current data summarized into 5-minute intervals
- [`sample_eEPSC_variance_df`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_eEPSC_variance_df.md)
  : Variance-to-mean ratio and coefficient of variation of evoked
  current amplitudes
- [`sample_PPR_df`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_PPR_df.md)
  : Evoked current data filtered to focus on paired-pulse ratio (PPR)
  analysis
- [`sample_eEPSC_t_test_df`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_eEPSC_t_test_df.md)
  : Paired t-test results comparing evoked current amplitudes relative
  to baseline
- [`sample_eEPSC_t_test_df_male`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_eEPSC_t_test_df_male.md)
  : Paired t-test results comparing evoked current amplitudes relative
  to baseline in males only
- [`sample_eEPSC_t_test_df_female`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_eEPSC_t_test_df_female.md)
  : Paired t-test results comparing evoked current amplitudes relative
  to baseline in females only

### Spontaneous current (sEPSC) data

Sample datasets from recordings measuring spontaneous current amplitude
over time. To reduce package size, only the raw and pruned spontaneous
current data are included.

- [`sample_raw_sEPSC_df`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_raw_sEPSC_df.md)
  : Representative data from a series of spontaneous (sEPSC) currents
- [`sample_pruned_sEPSC_df`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_pruned_sEPSC_df.md)
  : Spontaneous current data pruned to a summary point per minute
- [`sample_summary_sEPSC_df`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_summary_sEPSC_df.md)
  : Spontaneous current data summarized into 5-minute intervals
- [`sample_sEPSC_t_test_df`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_sEPSC_t_test_df.md)
  : Paired t-test results comparing spontaneous current amplitudes
  relative to baseline amplitudes

### Action Potential (AP) data

Sample datasets from recordings measuring action potentials in response
to a current injection protocol.

- [`sample_AP_data`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_ap_data.md)
  : Example of a complete action potential dataset used for plotting and
  analyses. This data contains parameters like peak_amplitude,
  after-hyperpolarization amplitude, and half-width for two time-points:
  during the control period (state == "Baseline") or after insulin has
  been applied (state == "Insulin").

- [`sample_AP_count_data`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_AP_count_data.md)
  :

  Example of action potential count data for two time-points: during the
  control period (state == "Baseline") or after insulin has been applied
  (state == "Insulin"). These were obtained by counting the number of
  action potentials present within each sweep. The data were then merged
  with the `sample_cell_characteristics` data through
  [`add_new_cells()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/add_new_cells.md)
  with `data_type = "AP_count"`.

- [`sample_ap_abf_baseline`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/sample_ap_abf_baseline.md)
  : Action potential baseline recording
