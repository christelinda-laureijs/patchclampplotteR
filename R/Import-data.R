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
#' @param filename A filepath to a .csv file containing information on cell
#'   characteristics. The function uses `here::here()` to locate the filepath.
#'   See the details below for information on required columns.
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


#' Add new data
#'
#' This function enables you to append new raw recording data onto an existing
#' datasheet. It makes it easy and convenient to merge the cell parameters (age,
#' sex, etc.) with new data and add it to your current raw data. This function
#' also formats the dataset so it is immediately ready for use in functions like
#' [make_normalized_EPSC_data()].
#'
#' @param new_raw_data_csv A filepath to a csv containing the new raw data. If
#'   the data are evoked current data (`current_type == "eEPSC"`) then this must
#'   contain 4 columns: `letter`, `ID`, `P1` and `P2`. If the data are
#'   spontaneous current data, the columns must be `letter`, `ID`,
#'   `recording_num`, `trace`, `peak_amplitude` and `time_of_peak`. Please see
#'   the section below on required columns for more details.
#' @param cell_characteristics_csv A filepath to a csv containing information
#'   about the cells. Please see [import_cell_characteristics_df()] for a
#'   description of what columns should be included. Don't forget to update this
#'   to include the cell characteristics for the new letters in
#'   `new_raw_data_csv`!
#' @param old_raw_data_csv A filepath to a csv containing the current raw data.
#'   Since this function appends the new data to the old data, this must be of
#'   the same current_type as the new data (e.g. the columns must be the same).
#'   If this is the first time you are running this function, start with a blank
#'   .csv file containing just the column titles in the first row.
#' @param data_type A character ("eEPSC", "sEPSC" or "AP") describing the data type that is being imported.
#' @param write_new_csv A character ("yes" or "no") describing if the new data
#'   should be written to a csv file. Defaults to "yes". Please specify
#'   a filename for the new csv file in `new_file_name`.
#' @param new_file_name A filename for the csv containing the new data appended
#'   to the old data. Must be a character representing a filepath to a csv file.
#'   Examples include "Data/20241118-Raw-eEPSC-data.csv".
#' @param injection_start_time For action potential data only: A numeric value describing the start time (in ms) when current injection was applied. Used to calculate the latency to fire.
#' @param decimal_places A numeric value indicating the number of decimal places the data should be rounded to. Used to reduce file size and prevent an incorrect representation of the number of significant digits.
#'
#' @return
#'
#' A dataframe consisting of the old raw data with information from the new
#' cells appended to it. If `data_type == "AP"` two new columns will also be
#' added based on calculations from the existing columns. These are
#' `latency_to_fire` (which is `time_to_peak` - `injection_start_time`) and
#' `antipeak_time_relative_to_threshold` (which is `time_of_antipeak` -
#' `time_of_threshold`).
#'
#' @export
#'
#'
#' @seealso [import_cell_characteristics_df()] for a list of required columns in the `cell_characteristics_csv`.
#'
#' @seealso [make_normalized_EPSC_data()] for the next step in the analysis process.
#'
#' @section Required Columns:
#'
#' If the data are evoked currents (`data_type == "eEPSC"`), the data must
#' contain the following four columns:
#'
#' \itemize{
#'  \item `letter` A character value that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics.
#'  \item `ID` A character value for the recording filename.
#'  \item `P1` A numeric value representing the
#'  amplitude of the first evoked current in pA.
#'  \item `P2` A numeric value representing the
#'  amplitude of the second evoked current in pA.
#' }
#'
#' If the data are spontaneous currents (`data_type == "sEPSC"`), the data
#' must contain the following columns:
#' \itemize{
#'  \item `letter` A character value that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics.
#'  \item `ID` A character value for the recording filename.
#'  \item `recording_num` A numeric value representing the recording number.
#'  This was incorporated before we switched to concatenating all recordings
#'  into one, but it needs to remain here to prevent breaking previous projects.
#'  It should be set to 1.
#'  \item `trace` A numeric value representing the trace (automatically
#'  generated in Clampfit) where the current occurred.
#'  \item `time_of_peak` A numeric value representing the time of the peak in
#'  milliseconds relative to trace number. This is automatically calculated in
#'  Clampfit.
#'  \item `time` A numeric value representing the absolute time when the current
#'  happened, relative to the start of the recording. This is autogenerated. See
#'  [add_new_cells()] for a description of how the true time value (`time`) is
#'  calculated from the `recording_num` and `trace.`
#'  \item `amplitude` A numeric value representing the amplitude of the evoked
#'  current in pA.
#' }
#'
#'  If the data are action potential parameters (`data_type == "AP"`), the data
#'   must contain the following columns:
#' \itemize{
#'  \item `letter` A character value that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics.
#'  \item `ID` A character value for the recording filename.
#'  \item `state` A character ("Baseline" or "Insulin") representing
#'  the timepoint that the data point belongs to.
#'  \item `time_of_threshold` The time (in ms) when the membrane potential
#'  reaches the threshold value.
#'  \item `threshold` The threshold (in mV). Determined using the first
#'  derivative method, where the threshold is the membrane potential which
#'  results in a derivative of 10 mV/ms or greater (Farries et al., 2010).
#'  \item `t_11` The value of the first derivative (action potential velocity in
#'  mV/ms) at threshold.
#'  \item `first_sweep_with_APs` The sweep number of the first sweep (going from
#'  lowest to higher current injection values) that resulted in an action
#'  potential.
#'  \item `trace_start` An automated value in Clampfit that is not used in the
#'  analysis.
#'  \item `peak_amplitude` The peak amplitude (in pA) of the action
#'  potential relative to threshold.
#'  \item `time_to_peak` The time to peak
#'  amplitude (in ms) relative to the time of threshold.
#'  \item `antipeak_amplitude` The after-hyperpolarization amplitude (in pA) relative
#'  to threshold.
#'  \item `time_of_antipeak` The time of the after-hyperpolarization (in ms).
#'  \item `half_width` The half-width, which is the width of the action potential and half of the peak amplitude.
#' }
#'
#' @examples
#' \dontrun{
#' add_new_cells(
#'   new_raw_data_csv = import_ext_data("sample_new_eEPSC_data.csv"),
#'   cell_characteristics_csv = import_ext_data("sample_cell_characteristics.csv"),
#'   old_raw_data_csv = import_ext_data("sample_eEPSC_data.csv"),
#'   data_type = "eEPSC",
#'   write_new_csv = "no",
#'   new_file_name = "20241118-Raw-eEPSC-Data.csv",
#'   decimal_places = 2
#' )
#' }
#'
add_new_cells <- function(new_raw_data_csv,
                          cell_characteristics_csv,
                          old_raw_data_csv,
                          data_type,
                          write_new_csv = "yes",
                          new_file_name,
                          decimal_places = 2,
                          injection_start_time = 265.4) {
  # Obtain argument values as strings
  # Required to check if the filenames and data_type match
  # (e.g. User enters raw-sEPSC-data.csv for data_type = "sEPSC")

  new_raw_data_name <- deparse(substitute(new_raw_data_csv))
  cell_characteristics_csv_name <- deparse(substitute(cell_characteristics_csv))
  old_raw_data_csv_name <- deparse(substitute(old_raw_data_csv))
  data_type_name <- deparse(substitute(data_type))

  list_of_argument_names <- c(
    new_raw_data_name,
    cell_characteristics_csv_name,
    old_raw_data_csv_name,
    data_type_name
  )

  if (!is.character(new_file_name)) {
    stop(
      "'new_file_name' must be a character (e.g. \"Data/Raw-CSVs/eEPSC-data/20240912-raw-data.csv\")"
    )
  }

  if (is.null(cell_characteristics_csv) ||
    !is.character(cell_characteristics_csv)) {
    stop(
      "'cell_characteristics_csv' must be a character (e.g. \"Data/Plaintext-Cell-Characteristics.csv\")"
    )
  }

  if (!is.numeric(decimal_places)) {
    stop("'decimal_places' must be a numeric value")
  }

  if (!write_new_csv %in% c("yes", "no")) {
    stop("'write_new_csv' argument must be one of: 'yes' or 'no'")
  }

  if (!is.null(new_raw_data_name) &
    !is.character(new_raw_data_name)) {
    stop("'new_raw_data_name' must be a character (e.g. \"Data/Raw-eEPSC-data.csv\")")
  }

  if (is.null(data_type) ||
    length(data_type) != 1L ||
    !data_type %in% c("eEPSC", "sEPSC", "AP")) {
    stop("'data_type' argument must be one of: 'eEPSC', 'sEPSC' or 'AP'")
  }

  # Required to see if new cells have associated data for synapses, treatment, sex, age, etc.
  cell_characteristics <-
    utils::read.csv(here::here(cell_characteristics_csv)) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::rename(X = .data$x, Y = .data$y)

  new_raw_data <- utils::read.csv(here::here(new_raw_data_csv)) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(id = factor(.data$id)) %>%
    dplyr::rename(ID = .data$id)

  if (data_type == "eEPSC") {
    if (any(grepl("sEPSC", list_of_argument_names))) {
      if (utils::menu(
        c("Yes", "No"),
        title = paste0(
          "data_type = \"",
          data_type,
          "\" but some filenames contain other keywords like ",
          "\"sEPSC\".",
          "\n Are you sure that you selected the correct files corresponding to the data type you've chosen?"
        )
      ) != 1) {
        stop(
          "Please double-check that you have selected csv files belonging to the same data type."
        )
      } else {
        "Okay, moving forward"
      }
    }

    if (any(grepl("AP", list_of_argument_names))) {
      if (menu(
        c("Yes", "No"),
        title = paste0(
          "data_type = \"",
          data_type,
          "\" but some filenames contain other keywords like ",
          "\"AP\".",
          "\n Are you sure that you selected the correct files corresponding to the data type you've chosen?"
        )
      ) != 1) {
        stop(
          "Please double-check that you have selected csv files belonging to the same data type."
        )
      } else {
        "Okay, moving forward"
      }
    }

    new_raw_data <- new_raw_data %>%
      dplyr::rename(P1 = .data$p1, P2 = .data$p2) %>%
      dplyr::group_by(.data$letter) %>%
      dplyr::mutate(time = (dplyr::row_number() - 1) / 12)
  }

  if (data_type == "sEPSC") {
    if (any(grepl("eEPSC", list_of_argument_names))) {
      if (menu(
        c("Yes", "No"),
        title = paste0(
          "data_type = \"",
          data_type,
          "\" but some filenames contain other keywords like ",
          "\"eEPSC\".",
          "\n Are you sure that you selected the correct files corresponding to the data type you've chosen?"
        )
      ) != 1) {
        stop(
          "Please double-check that you have selected csv files belonging to the same data type."
        )
      } else {
        "Okay, moving forward"
      }
    }

    if (any(grepl("AP", list_of_argument_names))) {
      if (menu(
        c("Yes", "No"),
        title = paste0(
          "data_type = \"",
          data_type,
          "\" but some filenames contain other keywords like ",
          "\"AP\".",
          "\n Are you sure that you selected the correct files corresponding to the data type you've chosen?"
        )
      ) != 1) {
        stop(
          "Please double-check that you have selected csv files belonging to the same data type."
        )
      } else {
        "Okay, moving forward"
      }
    }

    new_raw_data <- new_raw_data %>%
      dplyr::group_by(.data$letter) %>%
      dplyr::mutate(
        amplitude = (-1) * .data$amplitude,
        time = ((.data$recording_num - 1) * 300 + (.data$trace - 1) * 5 + (.data$time_of_peak /
          1000)
        ) / 60
      )
  }


  if (data_type == "AP") {
    if (any(grepl("eEPSC", list_of_argument_names))) {
      if (menu(
        c("Yes", "No"),
        title = paste0(
          "data_type = \"",
          data_type,
          "\" but some filenames contain other keywords like ",
          "\"eEPSC\".",
          "\n Are you sure that you selected the correct files corresponding to the data type you've chosen?"
        )
      ) != 1) {
        stop(
          "Please double-check that you have selected csv files belonging to the same data type."
        )
      } else {
        "Okay, moving forward"
      }
    }

    if (any(grepl("sEPSC", list_of_argument_names))) {
      if (menu(
        c("Yes", "No"),
        title = paste0(
          "data_type = \"",
          data_type,
          "\" but some filenames contain other keywords like ",
          "\"sEPSC\".",
          "\n Are you sure that you selected the correct files corresponding to the data type you've chosen?"
        )
      ) != 1) {
        stop(
          "Please double-check that you have selected csv files belonging to the same data type."
        )
      } else {
        "Okay, moving forward"
      }
    }

    new_raw_data <- new_raw_data %>%
      dplyr::rename(
        first_sweep_with_APs = .data$first_sweep_with_aps,
        threshold = .data$t_x
      ) %>%
      dplyr::mutate(
        latency_to_fire = .data$time_to_peak - injection_start_time,
        antipeak_time_relative_to_threshold = .data$time_of_antipeak - .data$time_of_threshold,
        peak_amplitude = dplyr::case_when(
          is.na(.data$peak_amplitude) ~ 0,
          T ~ .data$peak_amplitude
        )
      )
  }


  warning("Renamed dataframe columns to lowercase")

  new_letters <- unique(new_raw_data$letter)
  new_letters_spaces <- paste0(new_letters, " ")

  if (all(new_letters %in% cell_characteristics$letter)) {
    message(
      "\n",
      "\n",
      "Letter check passed: All letters are present in both \"",
      cell_characteristics_csv,
      "\" \nand \"",
      new_raw_data_csv,
      "\".",
      "\n \nThe matching cells are:",
      "\n",
      new_letters_spaces
    )
  } else {
    stop(
      "\n",
      "\n",
      "Missing letters detected in \"",
      cell_characteristics_csv,
      "\". \nDid you add cell characteristics for ALL the new cells in \"",
      new_raw_data_csv,
      "\"?"
    )
  }

  new_raw_data_complete <-
    merge(new_raw_data, cell_characteristics, by = "letter")

  # Import old Raw-Data sheet
  old_raw_data <- utils::read.csv(here::here(old_raw_data_csv), header = T) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(id = factor(.data$id), letter = factor(.data$letter)) %>%
    dplyr::rename(
      ID = .data$id,
      X = .data$x,
      Y = .data$y
    )

  if (data_type == "eEPSC") {
    old_raw_data <- old_raw_data %>%
      dplyr::rename(
        P1 = .data$p1,
        P2 = .data$p2
      )
  }

  if (data_type == "AP") {
    old_raw_data <- old_raw_data %>%
      dplyr::rename(
        first_sweep_with_APs = .data$first_sweep_with_aps,
        threshold = .data$t_x
      )

    warning("Renamed column 't_x' to 'threshold'")
  }

  if (any(grepl("cells", colnames(old_raw_data)))) {
    old_raw_data <- old_raw_data %>%
      dplyr::rename(cell = .data$cells)

    warning("Renamed column 'cells' to 'cell'")
  }

  letters_in_old_raw_data <- unique(old_raw_data$letter)
  letters_in_new_raw_data <- unique(new_raw_data_complete$letter)
  letters_in_new_raw_data_spaces <- paste0(letters_in_new_raw_data, " ")

  # Check for accidental letter duplication (e.g. running same code twice)
  if (any(letters_in_new_raw_data %in% letters_in_old_raw_data)) {
    overlapping_letters <- intersect(letters_in_old_raw_data, letters_in_new_raw_data)
    overlapping_letters_spaces <- paste0(overlapping_letters, " ")

    stop(
      "\n",
      "\n!! Overlapping letter(s) detected!!    --->  ",
      overlapping_letters_spaces,
      "\nThese cells in \"",
      new_raw_data_csv,
      "\" already exist in ",
      "\"",
      old_raw_data_csv,
      "\"",
      "\n \nDuplicate letters will produce plotting errors and distorted statistics.",
      "\nDid you accidentally run this code twice?"
    )
  } else {
    message(
      "\n \nDuplication check passed: All letters in \"",
      new_raw_data_csv,
      "\" are new relative to ",
      "\"",
      old_raw_data_csv,
      "\"",
      "\n \nAdding the new following new cells:",
      "\n",
      letters_in_new_raw_data_spaces
    )

    full_raw_data <- dplyr::bind_rows(old_raw_data, new_raw_data_complete) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, decimal_places)))

    if (write_new_csv == "yes") {
      utils::write.csv(full_raw_data, here::here(new_file_name), row.names = F)
    }

    return(full_raw_data)
  }
}
