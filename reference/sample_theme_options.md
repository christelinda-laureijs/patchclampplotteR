# A dataframe of theme options for things like colours and line widths

This is an example of the dataframe used to specify the colour themes
and plot options (like line thickness). If you want to modify this file,
see the section on **Defining your own theme options** below.

## Format

An .rda file containing 23 objects of 2 variables

- `gray_shading_colour`:

  Hex code for the colour used to fill the violin plots.

- `line_col`:

  Hex code for the colour used for the line indicating periods when a
  hormone was applied.

- `rectangle_shading_colour`:

  Hex code for the colour used to shade the rectangles.

- `both_sexes_shape`:

  Numeric value representing the point shape used for data values where
  both sexes are included. Defaults to 15, which is a square.

- `male_shape`:

  Numeric value representing the point shape used for data values where
  `Sex == Male`. Defaults to 16, which is a circle.

- `female_shape`:

  Numeric value representing the point shape used for data values where
  `Sex == Female`. Defaults to 17, which is a triangle.

- `mean_point_colour`:

  Hex code for the colour used to fill the mean data point in plots like
  the PPR comparison plot.

- `connecting_line_width`:

  The width of the connecting line in action potential comparison plots.

- `connecting_line_colour`:

  The colour of the connecting line in action potential comparison
  plots.

- `connecting_line_width_PPR`:

  The width of the connecting line in paired-pulse ratio comparison
  plots.

- `mean_point_size`:

  The size of the mean data point in plots like the PPR comparison plot.

- `baseline_group_colour`:

  Hex code for the colour used when `state == "Baseline"` in
  [`plot_AP_comparison()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_AP_comparison.md)
  and
  [`plot_AP_frequencies_single_treatment()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_AP_frequencies_single_treatment.md).

## Defining your own theme options

Step 1: Create a .csv file with two columns (`option` and `value`)
modelled after this sample dataset. **Important!**: Your .csv must have
identical columns and rows as the sample data, or else some plots won't
work!

Step 2: Read in the .csv file in using `utils::read_csv()`. This will
now be an object in your R environment.

Step 3: **Important!!** You must convert the first column (`option`)
into the `rownames`. This is a mandatory step to allow the theme_options
to be indexed by row name in plotting functions.

Step 4: Run the following code:

[`library(tibble)`](https://tibble.tidyverse.org/)
`my_sample_theme_options <- read.csv(here::here("Data/your_sample_theme_options.csv")) %>% remove_rownames %>% column_to_rownames(var="option")`

Step 5: Check the resulting object. You should now have 11 objects of 1
variable, and the row names should be `gray_shading_colour`, `line_col`,
etc.

Step 6: Go to a plotting function like
[`plot_raw_current_data()`](https://christelinda-laureijs.github.io/patchclampplotteR/reference/plot_raw_current_data.md)
and replace `sample_theme_options` with your newly created object from
Step 5.
