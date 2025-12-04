# A dataframe of treatments and their assigned colours for consistency across plots

This is an example of the dataframe used to specify a colour theme for
each treatment. It is used to create a consistent colour scheme across
all plots produced with this package.

## Format

An .rda file containing 4 objects of 5 variables

- `category`:

  A numeric value representing the experiment type. Used to assign
  top-level groups for further analyses, with `treatment` as subgroups.
  For example, "1" may refer to an experiment where you applied
  high-frequency stimulation (HFS) to a cell, while "2" is an experiment
  where you added a hormone like leptin. "3" may be an experiment where
  you applied HFS in the continuous presence of leptin.

- `treatment`:

  A character value specifying the treatment applied (e.g. "Control",
  "HNMPA") such as antagonists or agonists, or any protocol applied to
  the animals (e.g. "Fasting").

- `display_names`:

  A character value with the same treatment names as in `treatment`
  except you would replace underscores with spaces and add line breaks
  if needed. This is to create more attractive, human-readable axis
  labels and table values.

- `colours`:

  A character value with hex colour values.

- `very_pale_colours`:

  A character value with hex colour values. These are all lighter than
  the colours in `colours` and they are used to create contrast in
  summary plots with both sexes.

## Defining your own treatment names and colours

Use the data.frame() function to define an object, then refer to this
named object in all `treatment_colour_theme` arguments. E.g.

`my_theme_colours <- data.frame(` `category = c(2, 2, 2, 2),`
`treatment = c("Control", "HNMPA", "PPP", "PPP_and_HNMPA"),`
`display_names = c("Control", "HNMPA", "PPP", "PPP\n&\nHNMPA"),`
`colours = c("#f07e05", "#f50599", "#70008c", "#DCE319"),`
`very_pale_colours = c("#fabb78", "#fa98d5", "#ce90de", "yellow")` `)`
