# Import raw `.abf` files as a dataframe

`import_ABF_file()` is a wrapper around
[`abftools::abf2_load()`](https://rdrr.io/pkg/abftools/man/abf2_load.html)
and
[`abftools::MeltAbf()`](https://rdrr.io/pkg/abftools/man/MeltAbf.html).
It converts the array from `abf2_load()` into a dataframe, and it also
converts time to minutes.

## Usage

``` r
import_ABF_file(file_name, recording_mode)
```

## Arguments

- file_name:

  Filepath to an `.abf` file (e.g. "Data/23711004.abf")

- recording_mode:

  The mode used for the recording. If in
  `recording_mode = "voltage_clamp"` (e.g. clamping cell at -70 mV and
  measuring current amplitude) the primary channel (`chan1`) is set to
  `"current"` and the secondary channel (`chan2`) is `"voltage"`. If
  `recording_mode = "current_clamp"`, these values are reversed, where
  the primary channel is `"voltage"` and the secondary channel is
  `"current"`.

## Value

A dataframe with 5 or 6 columns, depending on the value of
`recording_mode`.

- `time` Time value from Clampfit which is in milliseconds x 10. For
  example, 5 seconds = 50000 in this column. Only appears if
  `recording_mode = "current_clamp"`.

- `episode` Character value (e.g. "epi1", "epi2") which corresponds
  directly to "sweep" in Clampfit.

- `current` Current in pA.

- `voltage` Voltage in mV.

- `time_sec` Time in seconds. Note that this resets each sweep (for
  example, this may be `0` to `5` seconds for `"epi1"`, then `0` to `5`
  seconds for `"epi2"`). For the cumulative elapsed time, see
  `time_sec_total`.

- `time_sec_total` The total cumulative elapsed time in seconds. Useful
  for when you want to plot raw data vs. time for multiple sweeps.

## Details

The file progresses from `.abf` to an array and then to a dataframe that
can be easily manipulated in R. Be sure to assign a value to the
dataframe so you can use it in later functions.

To export a flattened `.abf` file as a `.csv` file, you can use
`fwrite()` from the `data.table` package.

## Examples

``` r
import_ABF_file(import_ext_data("sample_abf.abf"), recording_mode = "voltage_clamp")

# To export this dataset as a csv use `fwrite()`
# from the `data.table` package and `here()` from the `here` package.

if (FALSE) { # \dontrun{
recording1 <- import_ABF_file(import_ext_data("sample_abf.abf"), recording_mode = "voltage_clamp")

} # }

#
```
