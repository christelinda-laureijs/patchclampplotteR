# Import colour theme

Import colour theme

## Usage

``` r
import_theme_colours(filename)
```

## Arguments

- filename:

  A filepath to a .csv file containing colours and theme parameters.
  Must contain two columns:

  - `option` The name of the parameter. Required parameters include:

    - “

    `line_col`, `baseline_colour`, etc.

  - `value` The value of the parameter

## Value

A dataframe with one column and row names.

## Examples

``` r
import_theme_colours(import_ext_data("sample_theme_options.csv"))
```
