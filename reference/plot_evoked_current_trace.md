# Plot an evoked current trace

This function allows you to generate an evoked current trace (averaged)
from an `.abf` file, such as the representative traces seen above
summary plots
[`plot_summary_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_summary_current_data.md).
This function uses `geom_smooth()` with `method = "gam"` to smooth the
raw recording. Follow these steps to create a trace!

## Usage

``` r
plot_evoked_current_trace(
  data,
  plot_colour = "#000000",
  filename = "Cell-A-baseline-trace",
  include_scale_bar = "yes",
  include_scale_bar_label = "no",
  scale_bar_label_size = 4,
  trace_annotation = "",
  trace_annotation_x_position = 0.57,
  trace_annotation_y_position = 6,
  geom_text_family = "",
  geom_text_colour = "#000000",
  geom_text_size = 9,
  plot_episode = "epi1",
  geom_text_fontface = "bold",
  trace_thickness = 2,
  scale_bar_x_start = 0.6,
  scale_bar_x_length = 0.02,
  scale_bar_y_length = 50,
  scale_bar_y_start = -75,
  k_value = 100,
  scale_bar_linewidth = 0.75,
  scale_bar_label_y_nudge = 1.5,
  plot_x_min = 0.57,
  plot_x_max = 0.61,
  plot_y_min = -100,
  plot_y_max = 10,
  save_plot_png = "no",
  ggplot_theme = patchclampplotteR_theme()
)
```

## Arguments

- data:

  A dataframe containing at least these columns: `time`, `episode`,
  `current`, `voltage`, `time_sec`. An easy way to obtain this is by
  importing a raw .abf file through the
  [`import_ABF_file()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_ABF_file.md)
  function.

- plot_colour:

  A character value naming the colour of the plot.

- filename:

  A character value used for the filename when saving the ggplot object
  as a `.png`.

- include_scale_bar:

  A character value that determines if a scale bar will be added to the
  plot. Allowed values are `"yes"` and `"no"`.

- include_scale_bar_label:

  A character value (`"yes"` or `"no"`) describing whether to include
  text labels on the scale bar. If `"yes"`, the text annotation values
  will be pulled from `scale_bar_x_length` and `scale_bar_y_length`.

- scale_bar_label_size:

  A numeric value describing the size of the scale bar annotation text.
  Defaults to 4.

- trace_annotation:

  A character value (e.g. `"Baseline"`) that will be added as text on
  the plot. Useful for labelling traces for publications and
  presentations. Defaults to `""` for blank.

- trace_annotation_x_position:

  A numeric value for the location of the `trace_annotation` on the
  x-axis.

- trace_annotation_y_position:

  A numeric value for the location of the `trace_annotation` on the
  y-axis.

- geom_text_family:

  A character value describing the font family used for the scale bar
  annotations. Defaults to `""` (empty, will use default system font),
  but can be replaced with a named font. Use a package like `extrafont`
  to load system fonts into R.

- geom_text_colour:

  A character value describing the colour of the `trace_annotation`
  text. Defaults to `"#000000"`, but you can match it to `plot_colour`
  for consistency.

- geom_text_size:

  A numeric value describing the size of the `trace_annotation` text.
  Defaults to `5`.

- plot_episode:

  A character value describing the sweep (e.g. `epi1`) that will be used
  for the plot.

- geom_text_fontface:

  A character value ("plain", "bold", "italic", "bold.italic"), used to
  describe the label defined in `text_annotation`. Defaults to "bold".

- trace_thickness:

  A numeric value describing the thickness of the trace line. Defaults
  to `0.8`.

- scale_bar_x_start:

  A numeric value (in seconds) describing the x-axis position of the
  scale bar.

- scale_bar_x_length:

  A numeric value describing the horizontal span (in seconds) of the
  scale bar. This will automatically be converted and displayed in
  milliseconds (default is `0.02` = 20 ms).

- scale_bar_y_length:

  A numeric value describing the vertical span (in pA) of the scale bar
  (default is `50` pA).

- scale_bar_y_start:

  A numeric value describing the y-axis position (in pA) of the scale
  bar.

- k_value:

  Used define the `k` value used for smoothing in the GAM model. You can
  think of this as the number of nodes used in the model. Choose a
  higher value for a more detailed trace, and a lower number for a more
  smooth curve. Be careful not to distort the current trace by making
  this too smooth!

- scale_bar_linewidth:

  A numeric value describing the thickness of the scalebar line (default
  is `0.75`).

- scale_bar_label_y_nudge:

  An optional numeric value that will add additional padding between the
  scale bar x-axis label and the scale bar. Defaults to `1.5`.

- plot_x_min:

  A numeric value describing the minimum value on the x-axis (in
  seconds). Defaults to `0.57`.

- plot_x_max:

  A numeric value describing the maximum value on the x-axis (in
  seconds). Defaults to `0.61`.

- plot_y_min:

  A numeric value describing the minimum value on the y-axis (in pA).
  Defaults to `-100`. !!Warning!! Be sure to check your raw current
  amplitudes and adjust this to avoid cutting off data!!

- plot_y_max:

  A numeric value describing the maximum value on the y-axis (in pA).
  Defaults to `10`. !!Warning!! Be sure to check your raw current
  amplitudes and adjust this to avoid cutting off data!!

- save_plot_png:

  A character (`"yes"` or `"no"`). If `"yes"`, the plot will be saved as
  a .png using `ggsave()`. The filepath depends on the current type, but
  they will all go in subfolders below `Figures/` in your project
  directory.

- ggplot_theme:

  The name of a ggplot theme or your custom theme. This will be added as
  a layer to a ggplot object. The default is
  [`patchclampplotteR_theme()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/patchclampplotteR_theme.md),
  but other valid entries include `theme_bw()`, `theme_classic()` or the
  name of a custom ggplot theme stored as an object.

## Value

A ggplot object. If `save_plot_png` is defined as "yes", it will also
generate a .png file in the folder `Figures/Representative-Traces`
relative to the project directory, with the `filename` characters
included in the filename.

## Details

1.  Open a recording file in Clampfit, and select the traces you would
    like to average (e.g. 60 sweeps from the baseline).

2.  Click on `Analyze` -\> `Average Traces` and ensure that
    `Display resultant trace only` is checked. In `Select`, choose
    `All visible traces`.

3.  The `.abf` file should now have only one sweep.

4.  Surround a flat region of the trace with `cursors 1 and 2`, and the
    stimulus artifact with `cursors 3 and 4`.

5.  Flatten the stimulus artifacts by clicking on `Analyze` -\>
    `Force values`. Set the `Value to assign` as the
    `Mean between cursors 1..2, and region to force as `Cursors 3..4\`.

6.  Save this file as a new `.abf` file with a different name than the
    original file.

7.  Import the file into R using
    [`import_ABF_file()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_ABF_file.md).

8.  Insert the stored abf fiile into the `data` argument of this
    function.

9.  Double-check that the plot x- and y-limits are not cutting off the
    data.

10. Adjust arguments like the `trace_annotation` and `plot_colour`.

11. Try generating separate plots for both the baseline and
    post-hormone/treatment conditions.

12. You can later assemble them into a full figure using `p1 + p2`
    notation with the `patchwork` library.

13. If you keep the limits and scales the same for both plots, you can
    hide the scale bar for one of the traces.

This function is quite similar to
[`plot_spontaneous_current_trace()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_spontaneous_current_trace.md).
Some of the default values for plot ranges (e.g. `plot_x_min` and
`plot_x_max`) are set to automatically be within the range of the evoked
current trace. IMPORTANT! Do not forget to check that your plot limits
are not cutting off any part of your trace, and ensure that if you only
show the scale bars for one trace, that they all use the same plot
limits and scale!

## See also

[`import_ABF_file()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/import_ABF_file.md)

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
