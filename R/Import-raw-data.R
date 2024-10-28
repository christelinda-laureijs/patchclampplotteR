#' Get path to external file
#'
#' Access sample files in `inst/extdata`
#' directory.
#'
#' @param file Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' load_ext_data()
#' load_ext_data("sample_cell_characteristics.csv")
load_ext_data <- function(file = NULL) {
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


sample_cell_characteristics <- read.csv("C:/Users/cslau/OneDrive/Documents/sample-cell-characteristics.csv")
write.csv(sample_cell_characteristics, "inst/extdata/sample_cell_characteristics.csv", row.names = FALSE)

sample_eEPSC_data <- read.csv("C:/Users/cslau/OneDrive/Desktop/Masters-Work/crosby-lab-code/Data/Sample-eEPSC-data.csv")
write.csv(sample_eEPSC_data, "inst/extdata/sample_eEPSC_data.csv", row.names = FALSE)


#' Normalize raw current data
#'
#' `make_normalized_EPSC_df()` creates a dataframe of evoked or spontaneous
#' current data from a raw .csv file. The function will create a new column containing the evoked or spontaneous current amplitudes normalized relative
#' to the mean current amplitude during the baseline period. For evoked current
#' data, the function also adds a column for the paired pulse ratio (`PPR = P2/P1`,
#' where `P2` is the amplitude of the second evoked current).
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
#' @returns A dataframe that can be viewed and used for further analyses in R.
#'   New or modified columns include:
#'\itemize{
#'  \item `P1` (for evoked currents only) May be negative-transformed if
#'  `negative_transform` == "yes"
#'  \item `P2` (for evoked currents only) May be negative-transformed if
#'  `negative_transform` == "yes"
#'  \item `PPR` (for evoked currents only) A numeric value that represents the
#'  paired pulse ratio (PPR) of the evoked currents, generated using
#'  `dplyr::mutate(PPR = P2/P1)`.
#'  \item `interval` A character value indicating the interval that the data
#'  point belongs to. For example, `interval` will be "t0to5" for any data
#'  points from 0 to 5 minutes. Example values: "t0to5", "t5to10", etc.
#'  \item `baseline_range` A logical value required for the baseline
#'  transformation. It is set to TRUE when time is within the baseline period
#'  (e.g. Time <= 5) and FALSE at all other times.
#'  \item `baseline_mean` A numeric value representing the mean evoked current
#'  amplitude during the baseline period. There is a different baseline_mean for
#'  each letter.
#'  \item `P1_transformed` (for evoked currents only) A numeric value representing the first evoked current
#'  amplitude (pA) normalized relative to the mean amplitude during the
#'  recording's baseline.
#'  \item `P2_transformed` (for evoked currents only) A numeric value representing the second evoked
#'  current amplitude (pA) normalized relative to the mean amplitude during the
#'  recording's baseline.
#'  \item `amplitude_transformed` (for spontaneous currents only) A numeric
#'  value representing the spontaneous current amplitude (pA) normalized
#'  relative to the mean amplitude during the recording's baseline.
#' }
#'
#' @section Required basic columns:
#' It doesn't matter if the data are evoked or current type - these columns
#' should be included in your data.
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
#'  \item `X` A numeric value representing the x-value of the cell's location in
#'  µm. Leave this blank if you don't have this data.
#'  \item `Y` A numeric value representing the y-value of the cell's location in
#'  µm. Leave this blank if you don't have this data.
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
#' **Evoked current data**:
#'
#' If the data are evoked currents (`current_type == "eEPSC"`), the data must contain the basic columns mentioned in **Required basic columns** plus these columns:
#'
#' \itemize{
#'  \item `P1` A numeric value representing the
#'  amplitude of the first evoked current in pA.
#'  \item `P2` A numeric value representing the
#'  amplitude of the second evoked current in pA.
#' }
#'
#' **Spontaneous current data:**
#'
#' If the data are spontaneous currents (`current_type == "sEPSC"`), the data must contain the basic columns mentioned in **Required basic columns** plus these columns:
#'\itemize{
#'  \item `recording_num` A numeric value representing the recording number.
#'  This was incorporated before we switched to concatenating all recordings
#'  into one, but it needs to remain here to prevent breaking previous projects.
#'  It should be set to 1.
#'  \item `trace` A numeric value representing the trace (automatically
#'  generated in Clampfit) where the current occured.
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
    stop(
      "max_time_value is ",
      max_time_value,
      ", which is not divisible by interval_length, ",
      interval_length
    )
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
      dplyr::rename(ID = .data$id,
                    X = .data$x,
                    Y = .data$y)
  }

  raw_df <- raw_df %>%
    dplyr::mutate(dplyr::across(
      c(
        .data$ID,
        .data$letter,
        .data$category,
        .data$treatment,
        .data$sex,
        .data$synapses
      ),
      as.factor
    )) %>%
    dplyr::filter(.data$time <= max_time_value)


  if (current_type == "eEPSC") {
    if (negative_transform_currents == "yes") {
      raw_df <- raw_df %>%
        dplyr::mutate(
          P1 = .data$P1 * -1,
          # Need positive current amplitude values to make plots more intuitive
          P2 = .data$P2 * -1,
          PPR = .data$P2 / .data$P1
        )
    } else {
      raw_df <- raw_df %>%
        dplyr::mutate(PPR = .data$P2 / .data$P1)
    }
  }

  # Divide data into intervals (e.g. 5-min intervals)


  time_sequence <- seq(from = min_time_value, to = max_time_value, by = interval_length)
  time_labels <- utils::head(paste0("t", time_sequence, "to", time_sequence + interval_length),-1)

  raw_df <- raw_df %>%
    dplyr::mutate(
      interval = cut(
        .data$time,
        breaks = time_sequence,
        include.lowest = TRUE,
        labels = time_labels
      )
    ) %>%
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


#' Prune and summarize current data per minute
#'
#' `make_pruned_eEPSC_df()` creates a dataframe of evoked or spontaneous current
#' data summarized per minute (or some other user-defined interval). Current
#' amplitudes are collapsed down into the mean amplitude per minute. This is
#' equivalent to GraphPad Prism's "prune rows" function to reduce data to
#' summary values for every n rows.
#'
#' @param data A `data.frame` object generated using [make_normalized_EPSC_data()].
#' It must contain the columns outlined in the Required columns section below.
#' @inheritParams make_normalized_EPSC_data
#' @param interval_length Length of each interval (in minutes). Used to divide the dataset into broad ranges for statistical analysis. Defaults to 1 for one summary point per minute.
#'
#'
#' @returns A list with 3 named elements. These elements are dataframes that can be viewed and used for further analyses in R. I highly recommend assigning the list to an object named something like `pruned_eEPSC_df` to make it easy to reference the dataframes with logical names (e.g. `pruned_eEPSC_df$all_cells`). The dataframes are:
#'\itemize{
#'  \item `individual_cells` A dataframe containing current data for each
#'  individual cell, but the data are reduced to a summary  point per per minute
#'  (or another value if a different `interval_length` is set). New columns
#'  include mean amplitude (`mean_P1` in pA), standard deviation (`sd_P1`),
#'  standard error (`se`), coefficient of variation (`cv`) and, inverse
#'  coefficient of variation squared (`cv_inverse_square`).
#'  \item `all_cells` A dataframe consisting of all data within a single
#'  treatment grouped and summarized per minute.
#'  \item `for_table` A dataframe containing two columns: letter and
#'  `P1_transformed` (for `eEPSC`) or `spont_amplitude_transformed` (for
#'  `sEPSC`). The current data (`P1_transformed``spont_amplitude_transformed`)
#'  is collapsed into a single row for each letter, with the current data for
#'  each letter stored as a list. This is required to create sparklines of
#'  current amplitude over time within the cell summary table. See
#'  [make_cell_summary_df()] and [make_interactive_summary_table()].
#'}
#'
#' @inheritSection make_normalized_EPSC_data Required basic columns
#'
#' @section Required evoked currents columns:
#' If the data are evoked currents (current_type == "eEPSC"), the data must
#' contain the basic columns mentioned in **Required basic columns** plus these
#' columns:
#'
#' \itemize{
#'  \item `PPR` A numeric value that represents the paired pulse ratio (PPR) of
#'  the evoked currents, generated in
#'  [make_normalized_EPSC_data()]
#'  \item `interval` A character value indicating the interval that the data
#'  belong to (e.g. "t0to5" for the first 5 minutes, "t5to10"). Generated
#'  automatically in [make_normalized_EPSC_data()].
#'  \item `baseline_range` A logical value required for the baseline
#'  transformation. It is set to TRUE when time is within the baseline period
#'  (e.g. Time <= 5) and FALSE at all other times.
#'  \item `baseline_mean` A numeric value representing the mean evoked current
#'  amplitude during the baseline period. There is a different baseline_mean for
#'  each letter. Generated automatically in [make_normalized_EPSC_data()].
#'  \item `P1_transformed` A numeric value representing the first evoked current
#'  amplitude (pA) normalized relative to the mean amplitude during the
#'  recording's baseline. Generated automatically in
#'  [make_normalized_EPSC_data()].
#'  \item `P2_transformed` A numeric value representing the second evoked
#'  current amplitude (pA) normalized relative to the mean amplitude during the
#'  recording's baseline. Generated automatically in
#'  [make_normalized_EPSC_data()].
#'}
#' @section Required spontaneous currents columns:
#' If the data are spontaneous currents (current_type == "sEPSC"), the data must
#' contain the basic columns mentioned in **Required basic columns** plus these
#' columns:
#'
#'\itemize{
#'  \item `interval` A character value indicating the interval that the data
#'  belong to (e.g. "t0to5" for the first 5 minutes, "t5to10"). Generated
#'  automatically in [make_normalized_EPSC_data()].
#'  \item `baseline_range` A logical value required for the baseline
#'  transformation. It is set to TRUE when time is within the baseline period
#'  (e.g. Time <= 5) and FALSE at all other times.
#'  \item `baseline_mean` A numeric value representing the mean evoked current
#'  amplitude during the baseline period. There is a different baseline_mean for
#'  each letter. Generated automatically in [make_normalized_EPSC_data()].
#'  \item `amplitude_transformed` A numeric value representing the spontaneous
#'  current amplitude (pA) normalized relative to the mean amplitude during the
#'  recording's baseline. Generated automatically in
#'  [make_normalized_EPSC_data()].
#'}
#'
#' @export
#' @examples
#' make_pruned_EPSC_data(data = sample_raw_eEPSC_df,
#'   current_type = "eEPSC",
#'   min_time_value = 0,
#'   max_time_value = 25,
#'   baseline_length = 5,
#'   interval_length = 1)

make_pruned_EPSC_data <- function(data = patchclampplotteR::sample_raw_eEPSC_df,
                                  current_type = "eEPSC",
                                  min_time_value = 0,
                                  max_time_value = 25,
                                  baseline_length = 5,
                                  interval_length = 1,
                                  save_output_as_RDS = "no") {
  time_sequence <- seq(from = min_time_value, to = max_time_value, by = interval_length)
  time_labels <- utils::head(paste0("t", time_sequence, "to", time_sequence + interval_length),
                             -1)

  if (is.null(current_type) ||
      length(current_type) != 1L ||
      !current_type %in% c("eEPSC", "sEPSC")) {
    stop("'current_type' argument must be one of: 'eEPSC' or 'sEPSC'")
  }

  if (!save_output_as_RDS %in% c("yes", "no")) {
    stop("'save_output_as_RDS' argument must be one of: 'yes' or 'no'")
  }

  # Prune within individual cells
  pruned_df_individual_cells <- data %>%
    dplyr::mutate(
      interval_pruned = cut(
        .data$time,
        breaks = time_sequence,
        include.lowest = TRUE,
        labels = time_labels
      )
    ) %>%
    dplyr::group_by(.data$category,
                    .data$letter,
                    .data$sex,
                    .data$treatment,
                    .data$interval_pruned)

  if (current_type == "eEPSC") {
    pruned_df_individual_cells <- pruned_df_individual_cells %>%
      dplyr::reframe(
        mean_P1 = mean(.data$P1, na.rm = TRUE),
        # Mean amplitude per minute across all cells
        sd_P1 = stats::sd(.data$P1, na.rm = TRUE),
        n = dplyr::n(),
        se = .data$sd_P1 / sqrt(.data$n),
        cv = .data$sd_P1 / .data$mean_P1,
        cv_inverse_square = 1 / (.data$cv ^ 2),
        letter = unique(.data$letter),
        category = unique(.data$category),
        time = dplyr::last(.data$time),
        baseline_mean = unique(.data$baseline_mean),
        synapses = unique(.data$synapses)
      )

    pruned_df_for_table <- pruned_df_individual_cells %>%
      dplyr::group_by(.data$letter) %>%
      dplyr::summarize(P1_transformed = list(.data$mean_P1))
  }

  if (current_type == "sEPSC") {
    pruned_df_individual_cells <- pruned_df_individual_cells %>%
      dplyr::reframe(
        mean_amplitude = mean(.data$amplitude_transformed, na.rm = TRUE),
        mean_raw_amplitude = mean(.data$amplitude, na.rm = TRUE),
        sd_amplitude = stats::sd(.data$amplitude_transformed, na.rm = TRUE),
        n = dplyr::n(),
        # Gets number of currents within each minute
        frequency = .data$n / 60,
        # Frequency in Hz
        se = .data$sd_amplitude / sqrt(.data$n),
        letter = unique(.data$letter),
        category = unique(.data$category),
        interval = unique(.data$interval),
        synapses = unique(.data$synapses),
        time = dplyr::last(.data$time) # Time value at the end of the interval; used for plots
      ) %>%
      dplyr::group_by(.data$letter) %>%
      # Obtain normalized frequency
      dplyr::mutate(
        baseline_range = (.data$time <= baseline_length),
        baseline_mean_frequency = sum(.data$frequency * .data$baseline_range) / sum(.data$baseline_range),
        frequency_transformed = (.data$frequency / .data$baseline_mean_frequency) * 100
      )

    pruned_df_for_table <- pruned_df_individual_cells %>%
      dplyr::group_by(.data$letter) %>%
      dplyr::summarize(spont_amplitude_transformed = list(.data$mean_amplitude))
  }

  if (save_output_as_RDS == "yes") {
    saveRDS(pruned_df_individual_cells, file = here::here(
      paste0(
        "Data/Output-Data-from-R/pruned_",
        current_type,
        "_df_individual_cells.RDS"
      )
    ))
  }

  # Prune all cells
  if (current_type == "eEPSC") {
    pruned_df_all_cells <- data %>%
      dplyr::mutate(
        interval_pruned = cut(
          .data$time,
          breaks = time_sequence,
          include.lowest = TRUE,
          labels = time_labels
        )
      ) %>%
      dplyr::group_by(
        .data$category,
        .data$letter,
        .data$sex,
        .data$treatment,
        .data$interval_pruned
      ) %>%
      dplyr::reframe(
        mean_P1 = mean(.data$P1_transformed, na.rm = TRUE),
        sd_P1 = stats::sd(.data$P1_transformed, na.rm = TRUE),
        n = dplyr::n(),
        se = .data$sd_P1 / sqrt(.data$n),
        cv = .data$sd_P1 / .data$mean_P1 * 100,
        letter = unique(.data$letter),
        category = unique(.data$category),
        time = dplyr::last(.data$time)
      ) %>%
      dplyr::group_by(.data$category,
                      .data$interval_pruned,
                      .data$sex,
                      .data$treatment) %>%
      dplyr::reframe(
        mean_P1_all_cells = mean(.data$mean_P1, na.rm = TRUE),
        sd_P1_all_cells = stats::sd(.data$mean_P1, na.rm = TRUE),
        n = dplyr::n(),
        se_P1_all_cells = .data$sd_P1_all_cells / sqrt(.data$n),
        cv_P1_all_cells = .data$sd_P1_all_cells / .data$mean_P1_all_cells * 100,
        time = dplyr::last(.data$time),
        category = unique(.data$category),

      )
  }

  if (current_type == "sEPSC") {
    pruned_df_all_cells <- pruned_df_individual_cells %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$category,
                      .data$interval_pruned,
                      .data$sex,
                      .data$treatment) %>%
      dplyr::reframe(
        mean_all_amplitude = mean(.data$mean_amplitude, na.rm = TRUE),
        mean_all_raw_amplitude = mean(.data$mean_raw_amplitude, na.rm = TRUE),
        sd_all_amplitude = stats::sd(.data$mean_amplitude, na.rm = TRUE),
        n = dplyr::n(),
        se_amplitude = .data$sd_all_amplitude / sqrt(.data$n),
        sd_all_raw_amplitude = stats::sd(.data$mean_raw_amplitude, na.rm = TRUE),
        se_raw_amplitude = .data$sd_all_raw_amplitude / sqrt(.data$n),
        mean_all_frequency = mean(.data$frequency_transformed, na.rm = TRUE),
        sd_all_frequency = stats::sd(.data$frequency_transformed, na.rm = TRUE),
        se_frequency = .data$sd_all_frequency / sqrt(.data$n),
        mean_all_raw_frequency = mean(.data$frequency, na.rm = TRUE),
        sd_all_raw_frequency = stats::sd(.data$frequency, na.rm = TRUE),
        se_raw_frequency = .data$sd_all_raw_frequency / sqrt(.data$n),
        time = dplyr::last(.data$time),
        interval = unique(.data$interval),
        category = unique(.data$category)
      )
  }

  if (save_output_as_RDS == "yes") {
    saveRDS(pruned_df_all_cells, file = here::here(
      paste0(
        "Data/Output-Data-from-R/pruned_",
        current_type,
        "_df_all_cells.RDS"
      )
    ))
  }

  return(list(individual_cells = pruned_df_individual_cells,
              for_table = pruned_df_for_table,
              all_cells = pruned_df_all_cells))
}
