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
#' Y-coordinates, and access resistance etc.). It replaces `NA` values in the `R_a` column with `0` to
#' remove errors caused by missing data. The function will also generate useful columns such as `percent_change_access` which describes the percent change in access of the final `R_a` reading relative to the `R_a` at the start of the recording. This can be helpful for cell exclusion based on access.
#'
#' The resulting dataframe can be merged with raw data into a summary table
#' and used in downstream statistical analyses.
#'
#' @param filename A filepath to a .csv file containing information on cell
#'   characteristics. The function uses `here::here()` to locate the filepath.
#'   See the details below for information on required columns.
#'
#' @returns A dataframe
#'
#' @section Required columns:
#'
#'  The columns listed below are required in the raw .csv file. If you do not have data for any of these columns, please still include the column as an "empty" column to prevent errors caused by missing columns.
#' \itemize{
#'  \item `letter` A character value that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics. Example: `"A"`
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
#'  \item `animal` A numeric value representing the animal's ID number.
#'  \item `synapses` A character value such as `"Glutamate"` or `"GABA"`.
#'  \item `treatment` A character value such as `"Control"` or `"HNMPA"`.
#'  \item `category` A numeric value representing an experiment type. For example, `'1'` may mean 4 seconds of high-frequency stimulation (HFS), `'2'` may mean an experiment where you added insulin, and `'3'` may mean HFS with insulin in the bath at all times. A category is the top-level division of your data. You can then have subgroups using the `treatment` variable. For example, perhaps you added insulin (Category = `2`) and also had the antagonist HNMPA present. This would be `Category = 2, Treatment = HNMPA`.
#' \item `R_a` A list of values for the access resistance, which would have been
#' monitored at several timepoints throughout the recording. See the section
#' `R_a` formatting below.
#' \item `days_alone` A numeric value representing the number of days that the animal was alone in a cage. This will always be 1 for some treatments, like fasting, but should ideally be low to reduce the effects of social isolation-related stress.
#' \item `animal_or_slicing_problems` A character value (`"yes"` or `"no"`) indicating if there were any problems during any point of the slice preparation process or animal handling. For example, use `"yes"` if the slices were crumpling during slicing or the animal was unusually anxious.
#' \item `percent_change_access` A numeric value indicating the percent change in access (`R_a`) over the recording. This is calculated by subtracting the first value of `R_a` from the last value of `R_a` and expressing the change in access as a percentage of the starting `R_a`. You could use this to assist with excluding cells based on changes in access.
#'
#' }
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

  cell_characteristics <- utils::read.csv(here::here(filename)) %>%
    dplyr::mutate(
      R_a = lapply(stringr::str_split(.data$R_a, pattern = ", "), FUN = as.numeric),
      R_a = lapply(.data$R_a, FUN = tidyr::replace_na, replace = 0),
      letter = factor(.data$letter)
    ) %>%

    # Get percent change in access over recording
    dplyr::mutate(
      starting_access = purrr::map_dbl(.data$R_a, dplyr::first),
      ending_access = purrr::map_dbl(.data$R_a, dplyr::last),
      percent_change_access = round((.data$ending_access - .data$starting_access) / .data$starting_access * 100, 0)
    ) %>%
    dplyr::select(-.data$starting_access, -.data$ending_access)

  return(cell_characteristics)
}


#' Import raw `.abf` files as a dataframe
#'
#' `import_ABF_file()` is a wrapper around `abftools::abf2_load()` and
#' `abftools::MeltAbf()`. It converts the array from `abf2_load()` into a
#' dataframe, and it also converts time to minutes.
#'
#' The file progresses from `.abf` to an array and then to a dataframe that can be
#' easily manipulated in R. Be sure to assign a value to the dataframe so you can use it in later functions.
#'
#' To export a flattened `.abf` file as a `.csv` file, you can use `fwrite()` from the `data.table` package.
#'
#' @param file_name Filepath to an `.abf` file (e.g. "Data/23711004.abf")
#' @param recording_mode The mode used for the recording. If in `recording_mode = "voltage_clamp"` (e.g. clamping cell at -70 mV and measuring current amplitude) the primary channel (`chan1`) is set to `"current"` and the secondary channel (`chan2`) is `"voltage"`. If `recording_mode = "current_clamp"`, these values are reversed, where the primary channel is `"voltage"` and the secondary channel is `"current"`.
#'
#' @returns A dataframe with 5 or 6 columns, depending on the value of `recording_mode`.
#'
#' \itemize{
#'  \item `time` Time value from Clampfit which is in milliseconds x 10. For example, 5 seconds = 50000 in this column. Only appears if `recording_mode = "current_clamp"`.
#'  \item `episode` Character value (e.g. "epi1", "epi2") which corresponds
#'  directly to "sweep" in Clampfit.
#'  \item `current` Current in pA.
#'  \item `voltage` Voltage in mV.
#'  \item `time_sec` Time in seconds. Note that this resets each sweep (for example, this may be `0` to `5` seconds for `"epi1"`, then `0` to `5` seconds for `"epi2"`). For the cumulative elapsed time, see `time_sec_total`.
#'  \item `time_sec_total` The total cumulative elapsed time in seconds. Useful for when you want to plot raw data vs. time for multiple sweeps.
#' }
#'
#' @export
#'
#' @examples
#'
#' import_ABF_file(import_ext_data("sample_abf.abf"), recording_mode = "voltage_clamp")
#'
#' # To export this dataset as a csv use `fwrite()`
#' # from the `data.table` package and `here()` from the `here` package.
#'
#' \dontrun{
#' recording1 <- import_ABF_file(import_ext_data("sample_abf.abf"), recording_mode = "voltage_clamp")
#'
# data.table::fwrite(recording1, here("recording1.csv"))
#' }
#'
#' #
#'
import_ABF_file <-
  function(file_name, recording_mode) {
    if (!recording_mode %in% c("voltage_clamp", "current_clamp")) {
      cli::cli_abort(c("'recording_mode' argument must be either 'voltage_clamp' or 'current_clamp'",
        "x" = "You did not use the correct value"
      ))
    }

    data <- abftools::abf2_load(here::here(file_name)) %>%
      abftools::MeltAbf() %>%
      dplyr::rename_with(tolower)

    if (recording_mode == "voltage_clamp") {
      data <- data %>%
        dplyr::rename("current" = .data$chan1, "voltage" = .data$chan2)
    }

    if (recording_mode == "current_clamp") {
      data <- data %>%
        dplyr::rename("voltage" = .data$chan1, "current" = .data$chan2)
    }

    list_of_episodes <- data$episode

    max_episode <- max(as.numeric(stringr::str_replace_all(list_of_episodes, "epi", "")))

    data <- data %>%
      dplyr::mutate(episode = factor(.data$episode, levels = paste0("epi", seq(1, max_episode, by = 1)))) %>%
      dplyr::mutate(time_sec = .data$time / 10000) %>%
      dplyr::arrange(.data$episode, .data$time_sec) %>%
      dplyr::mutate(time_sec_total = dplyr::row_number() * 0.0001)

    if (recording_mode == "voltage_clamp") {
      data <- data %>%
        dplyr::select(-.data$time)
    }

    data %>%
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
#'   `recording_num`, `trace`, `amplitude` and `time_of_peak`. Please see
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
#'   .csv file containing just the column titles in the first row, and some text (e.g. `"text"`) below the `letter` column. This is required so that R correctly recognizes the columns.
#' @param data_type A character (`"eEPSC"`, `"sEPSC"`, `"AP_parameter"` or `"AP_count"`) describing the data type that is being imported.
#' @param write_new_csv A character (`"yes"` or `"no"`) describing if the new data
#'   should be written to a csv file. Defaults to `"yes"`. Please specify
#'   a filename for the new csv file in `new_file_name`.
#' @param new_file_name A filename for the csv containing the new data appended
#'   to the old data. Must be a character representing a filepath to a csv file.
#'   Examples include `"Data/20241118-Raw-eEPSC-data.csv"`.
#' @param injection_start_time For action potential data only: A numeric value describing the start time (in ms) when current injection was applied. Used to calculate the latency to fire.
#' @param length_of_current_injection For action potential data only: A numeric value indicating the duration of the current injection (in s, default is 0.5 s).
#' @param decimal_places A numeric value indicating the number of decimal places the data should be rounded to. Used to reduce file size and prevent an incorrect representation of the number of significant digits.
#'
#' @return
#'
#' A dataframe consisting of the old raw data with information from the new
#' cells appended to it. If `data_type == "AP_parameter"` two new columns will also be
#' added based on calculations from the existing columns. These are
#' `latency_to_fire` (which is `time_to_peak` - `injection_start_time`) and
#' `antipeak_time_relative_to_threshold` (which is `time_of_antipeak` -
#' `time_of_threshold`). If `data_type == "AP_count"` one new column will be added. These is `AP_frequency` (in Hz).
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
#'  \item `amplitude` A numeric value representing the amplitude of the evoked
#'  current in pA.
#'  \item `time_of_peak` A numeric value representing the time of the peak in
#'  milliseconds relative to trace number. This is automatically calculated in
#'  Clampfit.
#' }
#'
#'  If the data are action potential parameters (`data_type == "AP_parameter"`), the data
#'   must contain the following columns:
#' \itemize{
#'  \item `letter` A character value that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics.
#'  \item `ID` A character value for the recording filename.
#'  \item `state` A character (`"Baseline"` or `"Insulin"`) representing
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
                          injection_start_time = 265.4,
                          length_of_current_injection = 0.5) {
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
    cli::cli_abort(c(
      "x" = "'new_file_name' must be a character",
      "i" = "An example of a correct value for `new_file_name` is \"Data/Raw-CSVs/eEPSC-data/20240912-raw-data.csv\")"
    ))
  }

  if (is.null(cell_characteristics_csv) ||
    !is.character(cell_characteristics_csv)) {
    cli::cli_abort(c(
      "x" = "'cell_characteristics_csv' must be a character",
      "i" = "An example of a correct value for `cell_characteristics_csv` is \"Data/Plaintext-Cell-Characteristics.csv\""
    ))
  }

  if (!is.numeric(decimal_places)) {
    cli::cli_abort(c("x" = "'decimal_places' must be a numeric value"))
  }

  if (!write_new_csv %in% c("yes", "no")) {
    cli::cli_abort(c("x" = "'write_new_csv' argument must be either 'yes' or 'no'"))
  }

  if (!is.null(new_raw_data_name) &
    !is.character(new_raw_data_name)) {
    cli::cli_abort(c("x" = "'new_raw_data_name' must be a character (e.g. \"Data/Raw-eEPSC-data.csv\")"))
  }

  if (is.null(data_type) ||
    length(data_type) != 1L ||
    !data_type %in% c("eEPSC", "sEPSC", "AP_parameter", "AP_count")) {
    cli::cli_abort(c("x" = "'data_type' argument must be one of: 'eEPSC', 'sEPSC', 'AP_parameter' or 'AP_count'."))
  }

  # Required to see if new cells have associated data for synapses, treatment, sex, age, etc.
  cell_characteristics <-
    utils::read.csv(here::here(cell_characteristics_csv)) %>%
    dplyr::rename_with(tolower)

  if ("x" %in% colnames(cell_characteristics)) {
    cell_characteristics <- cell_characteristics %>%
      dplyr::rename(X = .data$x)
  }

  if ("r_a" %in% colnames(cell_characteristics)) {
    cell_characteristics <- cell_characteristics %>%
      dplyr::rename(R_a = .data$r_a)
  }

  if ("y" %in% colnames(cell_characteristics)) {
    cell_characteristics <- cell_characteristics %>%
      dplyr::rename(Y = .data$y)
  }

  if ("days_alone" %in% colnames(cell_characteristics)) {
    cell_characteristics <- cell_characteristics %>%
      dplyr::mutate(days_alone = factor(.data$days_alone))
  }

  if ("animal_or_slicing_problems" %in% colnames(cell_characteristics)) {
    cell_characteristics <- cell_characteristics %>%
      dplyr::mutate(animal_or_slicing_problems = factor(.data$animal_or_slicing_problems))
  }


  new_raw_data <- utils::read.csv(here::here(new_raw_data_csv)) %>%
    dplyr::rename_with(tolower)

  if (sum(stringr::str_detect(new_raw_data$letter, "^$") > 0)) {
    cli::cli_abort(c(
      "x" = "There is at least one empty cell somewhere in the letter column.",
      "i" = "Did you forget to fill in the letters for one of your recordings?"
    ))
  }

  if (data_type %in% c("eEPSC", "sEPSC", "AP_parameter")) {
    new_raw_data <- new_raw_data %>%
      dplyr::mutate(id = factor(.data$id)) %>%
      dplyr::rename(ID = .data$id)
  }

  if (data_type == "eEPSC") {
    if (any(grepl("sEPSC|AP_parameter|AP_count", list_of_argument_names))) {
      if (utils::menu(
        c("Yes", "No"),
        title = paste0(
          "data_type = \"",
          data_type,
          "\" but some filenames contain other keywords like ",
          "\"sEPSC\" or \"AP\".",
          "\n Are you sure that you selected the correct files corresponding to the data type you've chosen?"
        )
      ) != 1) {
        cli::cli_abort(c("x" = "Cannot combine files of mixed data types", "i" = "Please double-check that you have selected csv files belonging to the same data type."))
      }
    }

    new_raw_data <- new_raw_data %>%
      dplyr::rename(P1 = .data$p1, P2 = .data$p2) %>%
      dplyr::group_by(.data$letter) %>%
      dplyr::mutate(time = (dplyr::row_number() - 1) / 12)
  }

  if (data_type == "sEPSC") {
    if (any(grepl("eEPSC|AP_parameter|AP_count", list_of_argument_names))) {
      if (menu(
        c("Yes", "No"),
        title = paste0(
          "data_type = \"",
          data_type,
          "\" but some filenames contain other keywords like ",
          "\"eEPSC\" or \"AP_count\".",
          "\n Are you sure that you selected the correct files corresponding to the data type you've chosen?"
        )
      ) != 1) {
        cli::cli_abort(c("x" = "Cannot combine files of mixed data types", "i" = "Please double-check that you have selected csv files belonging to the same data type."))
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


  if (data_type == "AP_parameter") {
    if (any(grepl("eEPSC|sEPSC|AP_count", list_of_argument_names))) {
      if (menu(
        c("Yes", "No"),
        title = paste0(
          "data_type = \"",
          data_type,
          "\" but some filenames contain other keywords like ",
          "\"eEPSC\" or \"AP_count\".",
          "\n Are you sure that you selected the correct files corresponding to the data type you've chosen?"
        )
      ) != 1) {
        cli::cli_abort(c("x" = "Cannot combine files of mixed data types", "i" = "Please double-check that you have selected csv files belonging to the same data type."))
      }
    }

    new_raw_data <- new_raw_data %>%
      dplyr::rename(
        first_sweep_with_APs = .data$first_sweep_with_aps,
        threshold = .data$t_x
      ) %>%
      dplyr::mutate(
        latency_to_fire = .data$time_to_peak - injection_start_time,
        antipeak_time_relative_to_threshold = .data$time_of_antipeak - .data$time_of_threshold
        # peak_amplitude = dplyr::case_when(
        #   is.na(.data$peak_amplitude) ~ 0,
        #   T ~ .data$peak_amplitude
        # )
      )
  }

  if (data_type == "AP_count") {
    if (any(grepl("eEPSC|sEPSC|AP_parameter", list_of_argument_names))) {
      if (menu(
        c("Yes", "No"),
        title = paste0(
          "data_type = \"",
          data_type,
          "\" but some filenames contain other keywords like ",
          "\"eEPSC\" or \"AP\".",
          "\n Are you sure that you selected the correct files corresponding to the data type you've chosen?"
        )
      ) != 1) {
        cli::cli_abort(c("x" = "Cannot combine files of mixed data types", "i" = "Please double-check that you have selected csv files belonging to the same data type."))
      }
    }

    new_raw_data <- new_raw_data %>%
      dplyr::rename(no_of_APs = .data$no_of_aps) %>%
      dplyr::mutate(
        AP_frequency = .data$no_of_APs / length_of_current_injection # ,
        # current_injection = dplyr::case_when(
        #   sweep == 1 ~ -50,
        #   sweep == 2 ~ -40,
        #   sweep == 3 ~ -30,
        #   sweep == 4 ~ -20,
        #   sweep == 5 ~ -10,
        #   sweep == 6 ~ 0,
        #   sweep == 7 ~ 10,
        #   sweep == 8 ~ 20,
        #   sweep == 9 ~ 30,
        #   sweep == 10 ~ 40,
        #   T ~ 50
        # )
      )
  }


  cli::cli_alert_info(cli::col_silver("Renamed dataframe columns to lowercase"))

  new_letters <- unique(new_raw_data$letter)
  new_letters_spaces <- paste0(new_letters, " ")

  if (all(new_letters %in% cell_characteristics$letter)) {
    cli::cli_alert_success(cli::col_green("Cell info check passed"))
    cli::cli_alert_success(cli::col_silver(paste0(
      "All letters are present in both \"",
      cell_characteristics_csv,
      "\" \nand \"",
      new_raw_data_csv,
      "\"."
    )))
  } else {
    cli::cli_abort(c("x" = paste0(
      "Missing letters detected in \"",
      cell_characteristics_csv,
      "\". Did you add cell characteristics for ALL the new cells in \"",
      new_raw_data_csv,
      "\"?"
    )))
  }

  new_raw_data_complete <-
    merge(new_raw_data, cell_characteristics, by = "letter")

  # Import old Raw-Data sheet
  old_raw_data <- utils::read.csv(here::here(old_raw_data_csv), header = T) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(letter = factor(.data$letter))

  if ("days_alone" %in% colnames(old_raw_data)) {
    old_raw_data <- old_raw_data %>%
      dplyr::mutate(days_alone = factor(.data$days_alone))
  }

  if ("animal_or_slicing_problems" %in% colnames(old_raw_data)) {
    old_raw_data <- old_raw_data %>%
      dplyr::mutate(animal_or_slicing_problems = factor(.data$animal_or_slicing_problems))
  }

  if ("id" %in% colnames(old_raw_data)) {
    old_raw_data <- old_raw_data %>%
      dplyr::mutate(id = factor(.data$id)) %>%
      dplyr::rename(ID = .data$id)
  }

  if ("r_a" %in% colnames(old_raw_data)) {
    old_raw_data <- old_raw_data %>%
      dplyr::rename(R_a = .data$r_a)
  }

  if ("x" %in% colnames(old_raw_data)) {
    old_raw_data <- old_raw_data %>%
      dplyr::rename(X = .data$x)
  }

  if ("y" %in% colnames(old_raw_data)) {
    old_raw_data <- old_raw_data %>%
      dplyr::rename(Y = .data$y)
  }

  if ("p1" %in% colnames(old_raw_data)) {
    old_raw_data <- old_raw_data %>%
      dplyr::rename(P1 = .data$p1)
  }

  if ("p2" %in% colnames(old_raw_data)) {
    old_raw_data <- old_raw_data %>%
      dplyr::rename(P2 = .data$p2)
  }

  if ("first_sweep_with_aps" %in% colnames(old_raw_data)) {
    old_raw_data <- old_raw_data %>%
      dplyr::rename(first_sweep_with_APs = .data$first_sweep_with_aps)
  }

  if ("ap_frequency" %in% colnames(old_raw_data)) {
    old_raw_data <- old_raw_data %>%
      dplyr::rename(AP_frequency = .data$ap_frequency)
  }

  if ("no_of_aps" %in% colnames(old_raw_data)) {
    old_raw_data <- old_raw_data %>%
      dplyr::rename(no_of_APs = .data$no_of_aps)
  }

  if (any(grepl("cells", colnames(old_raw_data)))) {
    old_raw_data <- old_raw_data %>%
      dplyr::rename(cell = .data$cells)

    cli::cli_alert_info("Renamed column 'cells' to 'cell'")
  }

  letters_in_old_raw_data <- unique(old_raw_data$letter)
  letters_in_new_raw_data <- unique(new_raw_data_complete$letter)
  letters_in_new_raw_data_spaces <- paste0(letters_in_new_raw_data, " ")

  # Check for accidental letter duplication (e.g. running same code twice)
  if (any(letters_in_new_raw_data %in% letters_in_old_raw_data)) {
    overlapping_letters <- intersect(letters_in_old_raw_data, letters_in_new_raw_data)
    overlapping_letters_spaces <- paste0(overlapping_letters, " ")

    cli::cli_abort(c(
      "x" = "Overlapping letter(s) detected!!",
      "!" = paste0(overlapping_letters_spaces),
      "!" = paste0(
        "Some or all cells in \"",
        new_raw_data_csv,
        "\" already exist in ",
        "\"",
        old_raw_data_csv,
        "\""
      ),
      "i" = "Duplicate letters will produce plotting errors and distorted statistics. Did you accidentally run this code twice?"
    ))
  } else {
    cli::cli_alert_success(cli::col_green("Letter duplication check passed"))
    cli::cli_alert_info(cli::col_silver(paste0(
      "All letters in \"",
      new_raw_data_csv,
      "\" are new relative to ",
      "\"",
      old_raw_data_csv,
      "\""
    )))
    cli::cli_alert_info(cli::col_green("Adding the following new cells:"))
    cli::cli_alert_success(
      cli::col_silver(letters_in_new_raw_data_spaces)
    )

    full_raw_data <- dplyr::bind_rows(old_raw_data, new_raw_data_complete) %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, decimal_places)))

    if (write_new_csv == "yes") {
      if (!grepl("\\.csv", new_file_name)) {
        cli::cli_abort("Filename does not contain .csv. Did you forget to include this in `new_file_name?")
      }

      utils::write.csv(full_raw_data, here::here(new_file_name), row.names = F)

      cli::cli_alert_success(cli::col_green(paste0("\nSaved data to .csv file ", new_file_name)))
    }

    return(full_raw_data)
  }
}
