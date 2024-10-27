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
#'\itemize{
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
#'\item `R_a` A list of values for the access resistance, which would have been
#'monitored at several timepoints throughout the recording. See the section
#'`R_a` formatting below. }
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
#' make_interactive_summary_table().
#'
#' @export
#'
#' @examples
#' import_cell_characteristics_df(load_ext_data("sample_cell_characteristics.csv"))
#'
#' @seealso make_interactive_summary_table() to generate an interactive table
#'   with cell characteristics and raw data as sparklines.

import_cell_characteristics_df <- function(filename) {
  utils::read.csv(here::here(filename)) %>%
    dplyr::mutate(
      R_a = lapply(stringr::str_split(.data$R_a, pattern = ", "), FUN = as.numeric),
      R_a = lapply(.data$R_a, FUN = tidyr::replace_na, replace = 0),
      letter = factor(.data$letter)
    )
}


# sample_cell_characteristics <- read.csv("C:/Users/cslau/OneDrive/Documents/sample-cell-characteristics.csv")
# write.csv(sample_cell_characteristics, "inst/extdata/sample_cell_characteristics.csv")

# sample_eEPSC_data <- read.csv("C:/Users/cslau/OneDrive/Desktop/Masters-Work/crosby-lab-code/Data/Sample-eEPSC-data.csv")
# write.csv(sample_eEPSC_data, "inst/extdata/sample_eEPSC_data.csv")


#' Normalize raw current data
#'
#' `make_normalized_EPSC_df()` creates a dataframe of evoked or spontaneous
#' current data from a raw .csv file. The function will create a new column
#' containing the evoked or spontaneous current amplitudes normalized relative
#' to the mean current amplitude during the baseline period. For evoked current
#' data, the function also adds a column for the paired pulse ratio (P2/P1,
#' where P2 is the amplitude of the second evoked current).
#'
#' @param filename A filepath to a .csv file. See add_new_cells() for the
#'   function that will merge raw data (a .csv with 4 columns: `letter`, `ID`,
#'   `P1`, and `P2`) and a `cell-characteristics.csv` file (with columns for
#'   factors like `animal`, `age`, `sex`, `synapses`). Please see the section on
#'   "Required columns" below.
#' @param current_type A character describing the current type. Allowed values
#'   are "eEPSC" or "sEPSC".
#' @param min_time_value Minimum time value (in minutes), which defaults to 0.
#' @param max_time_value Maximum recording length (in minutes). All data points
#'   will be filtered to time values less than or equal to this value. Defaults
#'   to 25.
#' @param baseline_length Length of the baseline (in minutes). Refers to data
#'   collected before applying a hormone, antagonist, or a protocol like high
#'   frequency stimulation. Defaults to 5.
#' @param interval_length Length of each interval (in minutes). Used to divide
#'   the dataset into broad ranges for statistical analysis. Important!
#'   `max_recording_length` must be evenly divisible by `interval_length`.
#'   Defaults to 5.
#' @param negative_transform_currents A character ("yes" or "no") describing if
#'   `P1` and `P2` should be negative transformed. If "yes", the values will be
#'   multiplied by (-1).
#' @param save_output_as_RDS A character ("yes" or "no") describing if the
#'   resulting object should be saved as an RDS file in the raw data folder.
#'
#'   returns A dataframe that can be viewed and used for further analyses in R.
#'   New or modified columns include:
#'\itemize{
#'  \item `P1` (for evoked currents only) May be negative-transformed if
#'  `negative_transform` == "yes"
#'  \item `P2` (for evoked currents only) May be negative-transformed if
#'  `negative_transform` == "yes"
#'  \item `PPR` (for evoked currents only) A numeric value that represents the
#'  paired pulse ratio (PPR) of the evoked currents, generated using
#'  `dplyr::mutate(PPR = P2/P1)`.
#'  \item `baseline_range` A logical value required for the baseline
#'  transformation. It is set to TRUE when time is within the baseline period
#'  (e.g. Time <= 5) and FALSE at all other times.
#'  \item `baseline_mean` A numeric value representing the mean evoked current
#'  amplitude during the baseline period. There is a different baseline_mean for
#'  each letter.
#'  \item `P1_transformed` A numeric value representing the first evoked current
#'  amplitude (pA) normalized relative to the mean amplitude during the
#'  recording's baseline.
#'  \item `P2-transformed` A numeric value representing the second evoked
#'  current amplitude (pA) normalized relative to the mean amplitude during the
#'  recording's baseline.
#'  \item `amplitude_transformed` (for spontaneous currents only) A numeric
#'  value representing the spontaneous current amplitude (pA) normalized
#'  relative to the mean amplitude during the recording's baseline.
#' }
#'
#' @section Required columns:
#' If the data are evoked currents (current_type == "eEPSC"), the .csv file must
#' contain the following columns and data types:
#'
#' \itemize{
#'  \item `letter` A character value that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics.
#'  \item `synapses` A character value (e.g. "Glutamate" or "GABA").
#'  \item `sex` A character value (e.g. "Male" or "Female").
#'  \item `treatment` A character value (e.g. "Control", "HNMPA").
#'  \item `time` A numeric value that represents time in minutes. This column is
#'  autogenerated in add_new_cells().
#'  \item `ID` A character value for the recording filename.
#'  \item `P1` A numeric value representing the amplitude of the first evoked
#'  current in pA.
#'  \item `P2` A numeric value representing the amplitude of the second evoked
#'  current in pA.
#'  \item `X` A numeric value representing the x-value of the cell's location in
#'  µm.
#'  \item `Y` A numeric value representing the y-value of the cell's location in
#'  µm.
#'  \item `age` A numeric value representing the animal's age. Can be any value
#'  as long as the time units are consistent throughout (e.g. don't mix up days
#'  and months when reporting animal ages).
#'  \item `animal` A numeric value representing the animal's ID or number.
#'  \item `category` A numeric value representing the experiment type. Used to
#'  assign top-level groups for further analyses, with `treatment` as subgroups.
#'  \item `cell` A character or numeric value representing the cell. For
#'  example, use `3.1.1` for animal #3, slice #1, cell #1.
#'}
#'
#' If the data are spontaneous currents (current_type == "sEPSC"), the .csv file
#' must contain the following columns and data types:
#'
#'\itemize{
#'  \item `letter` A character value that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics.
#'  \item `synapses` A character value (e.g. "Glutamate" or "GABA").
#'  \item `sex` A character value (e.g. "Male" or "Female").
#'  \item `treatment` A character value (e.g. "Control", "HNMPA").
#'  \item `time` A numeric value that represents time in minutes. This column is
#'  autogenerated in add_new_cells().
#'  \item `ID` A character value for the recording filename.
#'  \item `recording_num` A numeric value representing the recording number.
#'  This was incorporated before we switched to concatenating all recordings
#'  into one, but it needs to remain here to prevent breaking previous projects.
#'  It should be set to 1.
#'  \item `time_of_peak` A numeric value representing the time of the peak in
#'  milliseconds relative to trace number, which is in turn relative to the
#'  start of the recording. See add_new_cells() for a description of how the
#'  true time value (`time`) is calculated from the `recording_num` and `trace.`
#'  \item `amplitude` A numeric value representing the amplitude of the evoked
#'  current in pA.
#'  \item `X` A numeric value representing the x-value of the cell's location in
#'  µm.
#'  \item `Y` A numeric value representing the y-value of the cell's location in
#'  µm.
#'  \item `age` A numeric value representing the animal's age. Can be any value
#'  as long as the time units are consistent throughout (e.g. don't mix up days
#'  and months when reporting animal ages).
#'  \item `animal` A numeric value representing the animal's ID or number.
#'  \item `category` A numeric value representing the experiment type. Used to
#'  assign top-level groups for further analyses, with `treatment` as subgroups.
#'  \item `cell` A character or numeric value representing the cell. For
#'  example, use `3.1.1` for animal #3, slice #1, cell #1.
#' }
#'
#' @examples
#' make_normalized_EPSC_data(
#'   filename = load_ext_data("sample_eEPSC_data.csv"),
#'   current_type = "eEPSC",
#'   min_time_value = 0,
#'   max_time_value = 25,
#'   interval_length = 5,
#'   baseline_length = 5,
#'   negative_transform_currents = "yes"
#' )
#'
#' @seealso add_new_cells() to add new recording data to your existing raw
#'   csv. It will merge raw data (a .csv with 4 columns: `letter`, `ID`, `P1`,
#'   and `P2`) and a `cell-characteristics.csv` file (with columns for factors
#'   like `animal`, `age`, `sex`, `synapses`).
#'
#' @export

make_normalized_EPSC_data <- function(filename = "Data/Sample-eEPSC-data.csv",
                                      current_type = "eEPSC",
                                      min_time_value = 0,
                                      max_time_value = 25,
                                      baseline_length = 5,
                                      interval_length = 5,
                                      negative_transform_currents = "yes",
                                      save_output_as_RDS = "no") {

  list_of_argument_names <- c(filename, current_type)

  if (is.null(current_type) ||
      length(current_type) != 1L ||
      !current_type %in% c("eEPSC", "sEPSC")) {
    stop("'current_type' argument must be one of: 'eEPSC' or 'sEPSC'")
  }

  if (!save_output_as_RDS %in% c("yes", "no")) {
    stop("'save_output_as_RDS' argument must be one of: 'yes' or 'no'")
  }

  if (!negative_transform_currents %in% c("yes", "no")) {
    stop("'negative_transform_currents' argument must be one of: 'yes' or 'no'")
  }


  if (current_type == "eEPSC") {
    if (any(grepl("sEPSC", list_of_argument_names))) {
      stop(
        "current_type = \"",
        current_type,
        "\" but some arguments have the text ",
        "\"sEPSC\".",
        "\n Are you sure that you selected the correct current type?"
      )
    }
  }

  if (current_type == "sEPSC") {
    if (any(grepl("eEPSC", list_of_argument_names))) {
      stop(
        "current_type = \"",
        current_type,
        "\" but some arguments have the text ",
        "\"eEPSC\".",
        "\n Are you sure that you selected the correct current type?"
      )
    }
  }

  if (max_time_value %% baseline_length != 0) {
    stop("max_time_value is ", max_time_value, ", which is not divisible by interval_length, ", interval_length)
  }

  raw_df <- utils::read.csv(here::here(filename), header = TRUE) %>%
    dplyr::rename_with(tolower)

  if (current_type == "eEPSC") {
    raw_df <- raw_df %>%
      dplyr::rename(
        ID = .data$id,
        P1 = .data$p1,
        P2 = .data$p2,
        X = .data$x,
        Y = .data$y
      )
  }

  if (current_type == "sEPSC") {
    raw_df <- raw_df %>%
      dplyr::rename(ID = .data$id, X = .data$x, Y = .data$y)
  }

  raw_df <- raw_df %>%
    dplyr::mutate(dplyr::across(c(
      .data$ID, .data$letter, .data$category, .data$treatment, .data$sex, .data$synapses
    ), as.factor)) %>%
    dplyr::filter(.data$time <= max_time_value)


  if (current_type == "eEPSC") {
    if (negative_transform_currents == "yes") {
      raw_df <- raw_df %>%
        dplyr::mutate(P1 = .data$P1 * -1,
                      # Need positive current amplitude values to make plots more intuitive
                      P2 = .data$P2 * -1,
                      PPR = .data$P2 / .data$P1)
    } else {
      raw_df <- raw_df %>%
        dplyr::mutate(PPR = .data$P2 / .data$P1)
    }
  }

  # Divide data into intervals (e.g. 5-min intervals)


  time_sequence <- seq(from = min_time_value, to = max_time_value, by = interval_length)
  time_labels <- utils::head(paste0("t", time_sequence, "to", time_sequence + interval_length),
                      -1)

  raw_df <- raw_df %>%
    dplyr::mutate(interval = cut(
      .data$time,
      breaks = time_sequence,
      include.lowest = TRUE,
      labels = time_labels
    )) %>%
    dplyr::group_by(.data$letter)

  # Within each cell, normalize all of the eEPSC amplitudes
  # relative to the mean baseline amplitude

  if (current_type == "eEPSC") {
    raw_df <- raw_df %>%
      dplyr::mutate(
        baseline_range = (.data$time <= baseline_length),
        baseline_mean = sum(.data$P1 * .data$baseline_range) / sum(.data$baseline_range),
        P1_transformed = (.data$P1 / .data$baseline_mean) * 100,
        P2_transformed = (.data$P2 / .data$baseline_mean) * 100
      )
  }

  if (current_type == "sEPSC") {
    raw_df <- raw_df %>%
      dplyr::mutate(
        baseline_range = (.data$time <= baseline_length),
        baseline_mean = sum(.data$amplitude * .data$baseline_range) / sum(.data$baseline_range),
        amplitude_transformed = (.data$amplitude / .data$baseline_mean) * 100
      )
  }

  return(raw_df)

  if (save_output_as_RDS == "yes") {
    saveRDS(raw_df, file = here::here(
      paste0("Data/Output-Data-from-R/raw_", current_type, "_df.RDS")
    ))
  }
}
