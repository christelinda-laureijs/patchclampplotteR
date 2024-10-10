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
#' @return A dataframe
#'
#' @section Required columns:
#'
#'  These columns are required in the raw .csv file:
#'
#'  * `letter` A character value that is a unique identifier for a single recording. Used to link data sets for evoked or spontaneous currents and cell-characteristics. Example: "A"
#'  * `cell` A character or numeric value representing the cell. For example, use `3.1.1` for animal #3, slice #1, cell #1.
#'  * `sex` A character value (e.g. "Male" or "Female").
#'  * `X` A numeric value representing the x-value of the cell's location in µm.
#'  * `Y` A numeric value representing the y-value of the cell's location in µm.
#'  * `age` A numeric value representing the animal's age. Can be any value as long as the time units are consistent throughout (e.g. don't mix up days and months when reporting animal ages). Do not use characters (e.g. avoid "P31" and use 31 instead).
#'  * `animal` A numeric value representing the animal's ID or number.
#'  * `synapses` A character value (e.g. "Glutamate" or "GABA").
#'  * `treatment` A character value (e.g. "Control", "HNMPA").
#'  * `category` A numeric value representing the experiment type. Used to assign top-level groups for further analyses, with `treatment` as subgroups.
#'  * `R_a` A list of values for the access resistance, which would have been monitored at several timepoints throughout the recording. See the section `R_a` formatting below.
#'
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
#' sparkline in the interactive summary table made with
#' [make_interactive_summary_table()].
#'
#' @export
#'
#' @examples
#' import_cell_characteristics_df(filename =
#' "Data/sample-cell-characteristics.csv")
#'
#' @seealso [make_interactive_summary_table()] to generate an interactive table
#'   with cell characteristics and raw data as sparklines.

import_cell_characteristics_df <- function(filename) {
  cell_characteristics <- read.csv(here(filename)) %>%
    mutate(
      R_a = lapply(str_split(R_a, pattern = ", "), FUN = as.numeric),
      R_a = lapply(R_a, FUN = replace_na, replace = 0),
      letter = factor(letter)
    )
  assign("cell_characteristics", cell_characteristics, envir = .GlobalEnv)
}
