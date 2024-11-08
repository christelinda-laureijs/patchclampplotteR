#' Get path to external data for package examples
#'
#' Access sample files in `inst/extdata`
#' directory.
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' import_ext_data()
#' import_ext_data("sample_cell_characteristics.csv")
import_ext_data <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "patchclampplotteR"))
  } else {
    system.file("extdata", file, package = "patchclampplotteR", mustWork = TRUE)
  }
}

#' Import cell characteristics
#'
#' `import_cell_characteristics_df()` is a wrapper around `read.csv()` to import
#' a .csv file with information about a cell (animal, age, sex, synapses, X- and
#' Y-coordinates, etc.). It replaces `NA` values in the `R_a` column with `0` to
#' remove errors caused by missing data. The resulting dataframe can be merged
#' with raw data into a summary table and used in downstream statistical
#' analyses.
#'
#' @param filename A filepath to a .csv file, written as a character. The
#'   function uses `here::here()` to locate the filepath. See the details below
#'   for information on required columns.
#'
#' @returns A dataframe
#'
#' @section Required columns:
#'
#'  These columns are required in the raw .csv file:
#' \itemize{
#'  \item `letter` A character value that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics. Example: "A"
#'  \item `cell` A character or numeric value representing the cell. For
#'  example, use `3.1.1` for animal #3, slice #1, cell #1.
#'  \item `sex` A character value such as "Male" or "Female".
#'  \item `X` A numeric value representing the x-value of the cell's location in
#'  µm.
#'  \item `Y` A numeric value representing the y-value of the cell's location in
#'  µm.
#'  \item `age` A numeric value representing the animal's age. Can be any value
#'  as long as the time units are consistent throughout (e.g. don't mix up days
#'  and months when reporting animal ages). Do not use characters (e.g. avoid
#'  "P31" and use 31 instead).
#'  \item `animal` A numeric value representing the animal's ID or number.
#'  \item `synapses` A character value such as "Glutamate" or "GABA".
#'  \item `treatment` A character value such as "Control" or "HNMPA".
#'  \item `category` A numeric value representing the experiment type. Used to
#'  assign top-level groups for further analyses, with `treatment` as subgroups.
#' \item `R_a` A list of values for the access resistance, which would have been
#' monitored at several timepoints throughout the recording. See the section
#' `R_a` formatting below. }
#' @section `R_a` formatting:
#'
#' `R_a` is a mandatory column with information about the cell's access
#' resistance. Each element of this column must be a sequence of numbers,
#' separated by a comma and a single space. Although this will be read in as a
#' character, do not add quotation marks around the values in this column. For
#' example, `1.5, 1.5, 1.6, 1.7, 1.7, 1.8` is an acceptable `R_a` value for a
#' single cell.
#'
#' `import_cell_characteristics_df()` will convert the character value into a
#' list of numeric values (using `stringr::str_split()`). It will also convert
#' blanks and `NA` values to 0. This allows access to be visualized as a
#' sparkline in the `R_a` column of the interactive summary table made with
#' [make_interactive_summary_table()].
#'
#' @export
#'
#' @examples
#' import_cell_characteristics_df(import_ext_data("sample_cell_characteristics.csv"))
#'
#' @seealso [make_interactive_summary_table()] to generate an interactive table
#'   with cell characteristics and raw data as sparklines.

import_cell_characteristics_df <- function(filename) {
  utils::read.csv(here::here(filename)) %>%
    dplyr::mutate(
      R_a = lapply(stringr::str_split(.data$R_a, pattern = ", "), FUN = as.numeric),
      R_a = lapply(.data$R_a, FUN = tidyr::replace_na, replace = 0),
      letter = factor(.data$letter)
    )
}


#' Import raw .abf files as a dataframe
#'
#' `import_ABF_file()` is a wrapper around `abftools::abf2_load()` and
#' `abftools::MeltAbf()`. It converts the array from `abf2_load()` into a
#' dataframe, and it also converts time to minutes.
#'
#' The file progresses from .abf to an array and then to a dataframe that can be
#' easily manipulated in R.
#'
#' @param file_name Filepath to an .abf file (e.g. "Data/23711004.abf")
#'
#' @returns A dataframe with 5 columns:
#'
#' \itemize{
#'  \item `time` Time value from Clampfit which is in milliseconds x 10. For example, 5 seconds = 50000 in this column.
#'  \item `episode` Character value (e.g. "epi1", "epi2") which corresponds
#'  directly to "sweep" in Clampfit.
#'  \item `current` Current in pA.
#'  \item `voltage` Voltage in mV.
#'  \item `time_sec` Time in seconds.
#' }
#'
#' @export
#'
#' @examples
#' import_ABF_file(import_ext_data("sample_abf.abf"))
import_ABF_file <-
  function(file_name) {
    abftools::abf2_load(here::here(file_name)) %>%
      abftools::MeltAbf() %>%
      dplyr::rename("current" = .data$chan1, "voltage" = .data$chan2) %>%
      dplyr::rename_with(tolower) %>%
      dplyr::mutate(time_sec = .data$time / 10000) %>%
      invisible()
  }

#' Import colour theme
#'
#' @param filename A filepath to a .csv file containing colours and theme parameters. Must contain two columns:
#'
#' \itemize{
#' \item `option` The name of the parameter. Required parameters include:
#' \itemize{
#'  \item ``
#'
#'
#' }
#'
#'
#' `line_col`, `baseline_colour`, etc.
#' \item `value` The value of the parameter
#' }
#'
#' @return
#' A dataframe with one column and row names.
#' @export
#'
#' @examples
#'
#' import_theme_colours(import_ext_data("sample_theme_options.csv"))
#'
import_theme_colours <- function(filename) {
  theme_options <- utils::read.csv(here::here(filename)) %>%
    tibble::column_to_rownames(var = "option")
}
