# Cell characteristics -----

#' Information about sex, age, treatment, and animal ID
#'
#' @description
#' This dataset provides an example of the type of cell characteristics that you
#' should be recording for your data. The dataset contains columns for the animal
#' number, age, sex, the synapses being recorded from, and treatments applied.
#' There is also a column for access, which is stored as a list.
#'
#' The most important column is \code{letter}. This is a unique identifier that
#' you will assign to each recording. The \code{letter} column will enable you to
#' link all information relevant to the recording (evoked current data,
#' spontaneous current data, animal data like age, sex, etc.) from different
#' files.
#'
#' You should import this .csv file using the [import_cell_characteristics_df()]
#' function, which will format it in a format that makes it easy to merge with
#' raw recording data in functions like [make_normalized_EPSC_data()]. This will
#' enable you to analyze relationships between properties like age and current
#' amplitude.
#'
#' @name sample_cell_characteristics
#' @docType data
#' @format A dataframe with 19 rows and 14 columns
#' \describe{
#'  \item{letter}{A character value that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics. Example: "A"}
#'  \item{cell}{A character or numeric value representing the cell. For example,
#'  use `3.1.1` for animal #3, slice #1, cell #1.}
#'  \item{sex}{A character value such as "Male" or "Female".}
#'  \item{X}{A numeric value representing the x-value of the cell's location in
#'  µm.}
#'  \item{Y}{A numeric value representing the y-value of the cell's location in
#'  µm.}
#'  \item{age}{A numeric value representing the animal's age. Can be any value
#'  as long as the time units are consistent throughout (e.g. don't mix up days
#'  and months when reporting animal ages). Do not use characters (e.g. avoid
#'  "P31" and use 31 instead).}
#'  \item{animal}{A numeric value representing the animal's ID or number.}
#'  \item{synapses}{A character value such as "Glutamate" or "GABA".}
#'  \item{treatment}{A character value such as "Control" or "HNMPA".}
#'  \item{category}{A numeric value representing the experiment type. Used to
#'  assign top-level groups for further analyses, with `treatment` as
#'  subgroups. For example, "1" may refer to an experiment where you applied high-frequency stimulation (HFS) to a cell, while "2" is an experiment where you added a hormone like leptin. "3" may be an experiment where you applied HFS in the continuous presence of leptin.}
#'  \item{R_a}{A list of values for the access resistance, which would have been
#'  monitored at several timepoints throughout the recording. See the section
#'  `R_a` formatting in the documentation for [import_cell_characteristics_df()].}
#'  \item{notes}{An optional character column with notes about any issues, sweeps that were removed during Clampfit processing, etc.}
#'  \item{days_alone}{A numeric value describing the number of days that the animal was left alone in a cage. This typically ranges from 0 to 2. Fasted animals will have 1 day alone.}
#'  \item{animal_or_slicing_problems}{A character value ("yes" or "no") describing if there were any issues with the animal (for example, the animal was unusually anxious) or slicing (there were delays during the process, the slices were crumpling, etc.).}
#' }
#' @keywords data
#' @examples
#' utils::read.csv(import_ext_data("sample_cell_characteristics.csv"))
#'
#' @seealso [import_cell_characteristics_df()] to import data like this from a
#'   .csv file.
#' @seealso [make_interactive_summary_table()] for a function which merges cell
#'   characteristics information with pruned evoked and spontaneous current data
#'   to create an interactive overview table of all recordings.
#'
NULL

# Sample ABF -----


#' Evoked current recording excerpt
#'
#' This is an excerpt from a raw recording of evoked currents exported from
#' Clampfit as an .abf file. It can be read using the [import_ABF_file()]
#' function, which will convert it into a regular dataframe for further
#' manipulation and plotting in R. It is used to demonstrate the
#' [plot_spontaneous_current_trace()] function.
#'
#' @name sample_abf_file
#' @docType data
#' @format An Axon Binary Format (.abf) file with 10000 observations of 5
#' variables.
#' \describe{
#'  \item{episode}{A factor representing the sweep, such as "epi1".}
#'  \item{time_sec}{Time in seconds.}
#'  \item{time}{Time in ms*100.}
#'  \item{voltage}{Numeric value representing voltage in mV.}
#'  \item{current}{Numeric value representing current amplitude in pA.}
#' }
#' @keywords data
#' @examples import_ABF_file(import_ext_data("sample_abf.abf"), recording_mode = "voltage_clamp")
NULL



#' Action potential baseline recording
#'
#' This is an excerpt of a raw recording of action potentials imported as a dataframe using [import_ABF_file()]. It is used to demonstrate the
#' [plot_AP_trace()] function.
#'
#' This is a current injection protocol taken during the baseline period. See `sample_ap_abf_insulin` for data from the same protocol taken after 25 minutes of exposure to 500 nM insulin.
#'
#' @name sample_ap_abf_baseline
#' @docType data
#' @format A dataframe of 100000 objects of 5 variables.
#' \describe{
#'  \item{episode}{A factor representing the sweep, such as "epi1".}
#'  \item{time_sec}{Time in seconds.}
#'  \item{time}{Time in ms*100.}
#'  \item{voltage}{Numeric value representing voltage in mV.}
#'  \item{current}{Numeric value representing current amplitude in pA.}
#' }
#' @keywords data
#' @examples plot_AP_trace(
#'   data = sample_ap_abf_baseline,
#'   sweeps = as.character(unique(sample_ap_abf_baseline$episode)),
#'   custom_scale_colours = c(
#'     "#edd03a", "#cced34",
#'     "#a3fd3d", "#6bfe64",
#'     "#31f199", "#18dcc3",
#'     "#29bbec", "#4294ff",
#'     "#466be3", "#4040a2"
#'   ),
#'   colour_scale_option = "custom",
#'   plot_category = 2,
#'   plot_treatment = "Control"
#' )
NULL

#' Action potential recording with insulin
#'
#' This is an excerpt of a raw recording of action potentials imported as a dataframe using [import_ABF_file()]. It is used to demonstrate the
#' [plot_AP_trace()] function.
#'
#' This is a current injection protocol taken during after 25 minutes of exposure to 500 nM insulin. See `sample_ap_abf_baseline` for data from the same protocol taken during the baseline period.
#'
#' @name sample_ap_abf_insulin
#' @docType data
#' @format A dataframe of 100000 objects of 5 variables.
#' \describe{
#'  \item{episode}{A factor representing the sweep, such as "epi1".}
#'  \item{time_sec}{Time in seconds.}
#'  \item{time}{Time in ms*100.}
#'  \item{voltage}{Numeric value representing voltage in mV.}
#'  \item{current}{Numeric value representing current amplitude in pA.}
#' }
#' @keywords data
#' @examples plot_AP_trace(
#'   data = sample_ap_abf_insulin,
#'   sweeps = as.character(unique(sample_ap_abf_insulin$episode)),
#'   custom_scale_colours = c(
#'     "#edd03a", "#cced34",
#'     "#a3fd3d", "#6bfe64",
#'     "#31f199", "#18dcc3",
#'     "#29bbec", "#4294ff",
#'     "#466be3", "#4040a2"
#'   ),
#'   colour_scale_option = "custom",
#'   plot_category = 2,
#'   plot_treatment = "Control"
#' )
NULL

# Raw eEPSC -----


#' Representative data from a series of evoked (eEPSC) currents
#'
#' This is an example of the raw eEPSC data produced using
#' [make_normalized_EPSC_data()]. It is useful for demonstrating functions that
#' build off of this dataset, such as [make_pruned_EPSC_data()] and plotting
#' functions like [plot_raw_current_data()] and [plot_summary_current_data()].
#'
#' @name sample_raw_eEPSC_df
#' @docType data
#' @format An .rda file containing 5680 objects of 20 variables.
#' \describe{
#'  \item{`letter`}{A character value that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics.}
#'  \item{`synapses`}{A character value (e.g. "Glutamate" or "GABA").}
#'  \item{`sex`}{A character value (e.g. "Male" or "Female").}
#'  \item{`treatment`}{A character value (e.g. "Control", "HNMPA") representing
#'  the antagonists or agonists applied, or any protocol applied to the animals
#'  (e.g. "Fasting").}
#'  \item{`time`}{A numeric value that represents time in minutes. This column is
#'  autogenerated in [add_new_cells()].}
#'  \item{`ID`}{A character value for the recording filename.}
#'  \item{`P1`}{A numeric value representing the amplitude of the first evoked
#'  current in pA.}
#'  \item{`P2`}{A numeric value representing the amplitude of the second evoked
#'  current in pA.}
#'  \item{`X`}{A numeric value representing the x-value of the cell's location in
#'  µm. Leave this blank if you don't have this data.}
#'  \item{`Y`}{A numeric value representing the y-value of the cell's location in
#'  µm. Leave this blank if you don't have this data.}
#'  \item{`age`}{A numeric value representing the animal's age. Can be any value
#'  as long as the time units are consistent throughout (e.g. don't mix up days
#'  and months when reporting animal ages).}
#'  \item{`animal`}{A numeric value representing the animal's ID or number.}
#'  \item{`category`}{A numeric value representing the experiment type. Used to
#'  assign top-level groups for further analyses, with `treatment` as subgroups. For example, "1" may refer to an experiment where you applied high-frequency stimulation (HFS) to a cell, while "2" is an experiment where you added a hormone like leptin. "3" may be an experiment where you applied HFS in the continuous presence of leptin.}
#'  \item{`cell`}{A character or numeric value representing the cell. For
#'  example, use `3.1.1` for animal #3, slice #1, cell #1.}
#'  \item{`notes`}{An optional column for notes.}
#'  \item{`days_alone`}{A numeric value describing the number of days that the animal was left alone in a cage. This typically ranges from 0 to 2. Fasted animals will have 1 day alone.}
#'  \item{`animal_or_slicing_problems`}{A character value ("yes" or "no") describing if there were any issues with the animal (for example, the animal was unusually anxious) or slicing (there were delays during the process, the slices were crumpling, etc.).}
#'  \item{`R_a`}{A list of numeric values indicating the access resistance. Please see the documentation for the dataset `sample_cell_characteristics`.}
#'  \item{`PPR`}{(for evoked currents only) A numeric value that represents the
#'  paired pulse ratio (PPR) of the evoked currents, generated using
#'  `dplyr::mutate(PPR = P2/P1)`.}
#'  \item{`interval`}{A character value indicating the interval that the data
#'  point belongs to. For example, `interval` will be "t0to5" for any data
#'  points from 0 to 5 minutes. Example values: "t0to5", "t5to10", etc.}
#'  \item{`baseline_range`}{A logical value required for the baseline
#'  transformation. It is set to TRUE when time is within the baseline period
#'  (e.g. Time <= 5) and FALSE at all other times.}
#'  \item{`baseline_mean`}{A numeric value representing the mean evoked current
#'  amplitude during the baseline period. There is a different baseline_mean for
#'  each letter.}
#'  \item{`P1_transformed`}{A numeric value representing the first evoked current
#'  amplitude (pA) normalized relative to the mean amplitude during the
#'  recording's baseline.}
#'  \item{`P2_transformed`}{A numeric value representing the second evoked
#'  current amplitude (pA) normalized relative to the mean amplitude during the
#'  recording's baseline.}
#' }
#' @keywords data
#'
#' @seealso [make_normalized_EPSC_data()], [make_pruned_EPSC_data()], and [plot_raw_current_data()]
NULL

# Raw sEPSC -----


#' Representative data from a series of spontaneous (sEPSC) currents
#'
#' This is an example of the raw spontaneous current data produced using
#' [make_normalized_EPSC_data()]. It is useful for demonstrating functions that
#' build off of this dataset, such as [make_pruned_EPSC_data()] and plotting
#' functions like [plot_raw_current_data()] and [plot_summary_current_data()].
#'
#' @name sample_raw_sEPSC_df
#' @docType data
#' @format An .rda file containing 5680 objects of 20 variables.
#' \describe{
#'  \item{`letter`}{A character value that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics.}
#'  \item{`ID`}{A character value for the recording filename.}
#'  \item{`recording_num`}{A numeric value representing the recording number.
#'  This was incorporated before we switched to concatenating all recordings
#'  into one, but it needs to remain here to prevent breaking previous projects.
#'  It should be set to 1.}
#'  \item{`trace`}{A numeric value representing the trace (automatically
#'  generated in Clampfit) where the current occurred.}
#'  \item{`time_of_peak`}{A numeric value representing the time of the peak in
#'  milliseconds relative to trace number. This is automatically calculated in
#'  Clampfit.}
#'  \item{`amplitude`}{A numeric value representing the amplitude of the evoked
#'  current in pA.}
#'  \item{`cell`}{A character or numeric value representing the cell. For
#'  example, use `3.1.1` for animal #3, slice #1, cell #1.}
#'  \item{`sex`}{A character value (e.g. "Male" or "Female").}
#'  \item{`X`}{A numeric value representing the x-value of the cell's location in
#'  µm. Leave this blank if you don't have this data.}
#'  \item{`Y`}{A numeric value representing the y-value of the cell's location in
#'  µm. Leave this blank if you don't have this data.}
#'  \item{`age`}{A numeric value representing the animal's age. Can be any value
#'  as long as the time units are consistent throughout (e.g. don't mix up days
#'  and months when reporting animal ages).}
#'  \item{`animal`}{A numeric value representing the animal's ID or number.}
#'  \item{`synapses`}{A character value (e.g. "Glutamate" or "GABA").}
#'  \item{`notes`}{An optional column for notes.}
#'  \item{`days_alone`}{A numeric value describing the number of days that the animal was left alone in a cage. This typically ranges from 0 to 2. Fasted animals will have 1 day alone.}
#'  \item{`animal_or_slicing_problems`}{A character value ("yes" or "no") describing if there were any issues with the animal (for example, the animal was unusually anxious) or slicing (there were delays during the process, the slices were crumpling, etc.).}
#'  \item{`R_a`}{A list of numeric values indicating the access resistance. Please see the documentation for the dataset `sample_cell_characteristics`.}
#'  \item{`treatment`}{A character value (e.g. "Control", "HNMPA") representing
#'  the antagonists or agonists applied, or any protocol applied to the animals
#'  (e.g. "Fasting").}
#'  \item{`category`}{A numeric value representing the experiment type. Used to
#'  assign top-level groups for further analyses, with `treatment` as subgroups. For example, "1" may refer to an experiment where you applied high-frequency stimulation (HFS) to a cell, while "2" is an experiment where you added a hormone like leptin. "3" may be an experiment where you applied HFS in the continuous presence of leptin.}
#'  \item{`time`}{A numeric value representing the absolute time when the current
#'  happened, relative to the start of the recording. This is autogenerated. See
#'  [add_new_cells() for a description of how the true time value (`time`) is
#'  calculated from the `recording_num` and `trace.`}
#'  \item{`interval`}{A character value indicating the interval that the data
#'  point belongs to. For example, `interval` will be "t0to5" for any data
#'  points from 0 to 5 minutes. Example values: "t0to5", "t5to10", etc.}
#'  \item{`baseline_range`}{A logical value required for the baseline
#'  transformation. It is set to TRUE when time is within the baseline period
#'  (e.g. Time <= 5) and FALSE at all other times.}
#'  \item{`baseline_mean`}{A numeric value representing the mean evoked current
#'  amplitude during the baseline period. There is a different baseline_mean for
#'  each letter.}
#'  \item{`amplitude_transformed`}{A numeric value representing the spontaneous
#'  current amplitude (pA) normalized relative to the mean spontaneous current
#'  amplitude during the recording's baseline.}
#' }
#' @keywords data
#'
#' @seealso [make_normalized_EPSC_data()] and [make_pruned_EPSC_data()] for
#'   functions that use this dataset.
NULL

# Summary eEPSC -----

#' Evoked current data summarized into 5-minute intervals
#'
#' This is an example of the summary eEPSC data produced using
#' [make_summary_EPSC_data()]. It is useful for demonstrating functions that
#' build off of this dataset, such as [plot_summary_current_data()]. This is a list of two dataframes.
#'
#'
#' @name sample_summary_eEPSC_df
#' @docType data
#' @format An .rda file containing a list of two dataframes. The first dataframe (`$percent_change_data`) contains the mean current amplitude for each interval, with a final column (`percent_change`) containing the final percent change in amplitude in the last interval relative to the baseline interval. The second dataframe (`$summary_data`) contains summary statistics for each interval. See below for more details.
#'
#' ## `$percent_change_data`
#' \describe{
#'  \item{`category`}{A factor representing the experiment type. Used to
#'  assign top-level groups for further analyses, with `treatment` as subgroups. For example, "1" may refer to an experiment where you applied high-frequency stimulation (HFS) to a cell, while "2" is an experiment where you added a hormone like leptin. "3" may be an experiment where you applied HFS in the continuous presence of leptin.}
#'  \item{`letter`}{A factor that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics.}
#'  \item{`sex`}{A factor with two levels (e.g. "Male" or "Female").}
#'  \item{`treatment`}{A factor based on the treatment applied (e.g. "Control",
#'  "HNMPA"). This represents the antagonists or agonists applied, or any
#'  protocol applied to the animals (e.g. "Fasting").}
#'  \item{`age`}{A numeric value representing the animal's age. Can be any value
#'  as long as the time units are consistent throughout (e.g. don't mix up days
#'  and months when reporting animal ages).}
#'  \item{`animal`}{A numeric value representing the animal's ID or number.}
#'  \item{`X`}{A numeric value representing the x-value of the cell's location in
#'  µm. Leave this blank if you don't have this data.}
#'  \item{`Y`}{A numeric value representing the y-value of the cell's location in
#'  µm. Leave this blank if you don't have this data.}
#'  \item{`synapses`}{A character value (e.g. "Glutamate" or "GABA").}
#'  \item{`days_alone`}{A numeric value describing the number of days that the animal was left alone in a cage. This typically ranges from 0 to 2. Fasted animals will have 1 day alone.}
#'  \item{`animal_or_slicing_problems`}{A character value ("yes" or "no") describing if there were any issues with the animal (for example, the animal was unusually anxious) or slicing (there were delays during the process, the slices were crumpling, etc.).}
#'  \item{`t0to5`}{The mean evoked current amplitude (pA) for this cell during the period of 0 to 5 minutes.}
#'  \item{`t5to10`}{The mean evoked current amplitude (pA) for this cell during the period of 5 to 10 minutes.}
#'  \item{`t10to15`, `t15to20`, `tXtY` etc...}{The mean evoked current amplitude (pA) for this cell during the period of *X* to *Y* minutes.}
#'  \item{`percent_change`}{The percent change in evoked current amplitude in the interval `t20to25` as a percentage of the mean baseline amplitude (`t0to5`). For example, if currents began at 100 pA during the baseline period, but were 50 pA by `t20to25`, the value of `percent_change` will be `50%` or `0.50`.}
#' }
#'
#'
#' ## `$summary_data`
#' \describe{
#'  \item{`category`}{A factor representing the experiment type. Used to
#'  assign top-level groups for further analyses, with `treatment` as subgroups. For example, "1" may refer to an experiment where you applied high-frequency stimulation (HFS) to a cell, while "2" is an experiment where you added a hormone like leptin. "3" may be an experiment where you applied HFS in the continuous presence of leptin.}
#'  \item{`letter`}{A factor that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics.}
#'  \item{`sex`}{A factor with two levels (e.g. "Male" or "Female").}
#'  \item{`treatment`}{A factor based on the treatment applied (e.g. "Control",
#'  "HNMPA"). This represents the antagonists or agonists applied, or any
#'  protocol applied to the animals (e.g. "Fasting").}
#'  \item{`interval`}{A factor indicating the interval that the data
#'  point belongs to. For example, `interval` will be "t0to5" for any data
#'  points from 0 to 5 minutes. Example values: "t0to5", "t5to10", etc.}
#'  \item{`mean_P1_transformed`}{A numeric value representing the mean current
#'  amplitude (in pA normalized to the baseline amplitude) of the first evoked
#'  current. The values used to produce mean_P1_transformed come from all data
#'  points within each interval (the length of the interval was specified in
#'  [make_normalized_EPSC_data()]).}
#'  \item{`mean_P1_raw`}{The same values as those in `mean_P1_transformed` except
#'  these values contain raw values for current amplitude (e.g. the data were
#'  not baseline transformed).}
#'  \item{`n`}{The number of data points used to produce each value in
#'  `mean_P1_transformed` and `mean_P1_raw`}
#'  \item{`sd`}{The standard deviation of the mean current values.}
#'  \item{`cv`}{The coefficient of variation of the mean current values.}
#'  \item{`se`}{The standard error of the mean current values.}
#'  \item{`cv_inverse_square`}{The inverse coefficient of variation squared of
#'  the mean current values. Calculated by using (`1/cv^2`), where `cv` is the
#'  coefficient of variation. Useful for variance analysis.}
#'  \item{`variance`}{The variance of the current values, calculated using `stats::var()`.}
#'  \item{`VMR`}{The variance-to-mean ratio, calculated using
#'  `variance/mean_P1_transformed`. Useful for variance analysis.}
#'  \item{`age`}{A numeric value representing the animal's age. Can be any value
#'  as long as the time units are consistent throughout (e.g. don't mix up days
#'  and months when reporting animal ages).}
#'  \item{`animal`}{A numeric value representing the animal's ID or number.}
#'  \item{`X`}{A numeric value representing the x-value of the cell's location in
#'  µm. Leave this blank if you don't have this data.}
#'  \item{`Y`}{A numeric value representing the y-value of the cell's location in
#'  µm. Leave this blank if you don't have this data.}
#'  \item{`time`}{A numeric value that represents time in minutes. This column is
#'  autogenerated in [add_new_cells()].}
#'  \item{`synapses`}{A character value (e.g. "Glutamate" or "GABA").}
#'  \item{`days_alone`}{A numeric value describing the number of days that the animal was left alone in a cage. This typically ranges from 0 to 2. Fasted animals will have 1 day alone.}
#'  \item{`animal_or_slicing_problems`}{A character value ("yes" or "no") describing if there were any issues with the animal (for example, the animal was unusually anxious) or slicing (there were delays during the process, the slices were crumpling, etc.).}
#' }
#' @keywords data
#'
#' @seealso [make_normalized_EPSC_data()] and [make_pruned_EPSC_data()]
NULL


# Summary sEPSC-----

#' Spontaneous current data summarized into 5-minute intervals
#'
#' This is an example of the summary eEPSC data produced using
#' [make_summary_EPSC_data()]. It is useful for demonstrating functions that
#' build off of this dataset, such as [plot_summary_current_data()].
#'
#' @name sample_summary_sEPSC_df
#' @docType data
#' @format An .rda file containing 70 objects of 16 variables.
#' \describe{
#'  \item{`category`}{A factor representing the experiment type. Used to
#'  assign top-level groups for further analyses, with `treatment` as subgroups. For example, "1" may refer to an experiment where you applied high-frequency stimulation (HFS) to a cell, while "2" is an experiment where you added a hormone like leptin. "3" may be an experiment where you applied HFS in the continuous presence of leptin.}
#'  \item{`letter`}{A factor that is a unique identifier for a single
#'  recording. Used to link data sets for evoked or spontaneous currents and
#'  cell-characteristics.}
#'  \item{`sex`}{A factor with two levels (e.g. "Male" or "Female").}
#'  \item{`treatment`}{A factor based on the treatment applied (e.g. "Control",
#'  "HNMPA"). This represents the antagonists or agonists applied, or any
#'  protocol applied to the animals (e.g. "Fasting").}
#'  \item{`interval`}{A factor indicating the interval that the data
#'  point belongs to. For example, `interval` will be "t0to5" for any data
#'  points from 0 to 5 minutes. Example values: "t0to5", "t5to10", etc.}
#'  \item{`mean_transformed_amplitude`}{The average normalized spontaneous
#'  current amplitude (% Baseline sEPSC amplitude).}
#'  \item{`mean_raw_amplitude`}{The average raw spontaneous current amplitude
#'  (pA).}
#'  \item{`n`}{The number of datapoints used to create the average.}
#'  \item{`sd_transformed_amplitude`}{The standard deviation of the normalized
#'  spontaneous current data (`mean_transformed_amplitude`).}
#'  \item{`se_transformed_amplitude`}{The standard error of
#'  `mean_transformed_amplitude`.}
#'  \item{`mean_transformed_frequency`}{The average normalized frequency (%
#'  Baseline frequency).}
#'  \item{`sd_transformed_frequency`}{The standard deviation of `mean_transformed_frequency`.}
#'  \item{`se_frequency`}{The standard error of `mean_transformed_frequency`.}
#'  \item{`mean_raw_frequency`}{The average raw frequency (Hz).}
#'  \item{`time`}{A numeric value that represents time in minutes. This column is
#'  autogenerated in [add_new_cells()].}
#'  \item{`synapses`}{A character value (e.g. "Glutamate" or "GABA").}
#'  \item{`days_alone`}{A numeric value describing the number of days that the animal was left alone in a cage. This typically ranges from 0 to 2. Fasted animals will have 1 day alone.}
#'  \item{`animal_or_slicing_problems`}{A character value ("yes" or "no") describing if there were any issues with the animal (for example, the animal was unusually anxious) or slicing (there were delays during the process, the slices were crumpling, etc.).}
#' }
#' @keywords data
#'
#' @seealso [make_normalized_EPSC_data()] and [make_pruned_EPSC_data()]
NULL


# t-test eEPSC -----

#' Paired t-test results comparing evoked current amplitudes relative to baseline
#'
#' This is an example of the dataframe of t-test results produced using
#' [perform_t_tests_for_summary_plot()]. The dataframe can be used for further
#' statistical analyses, and it also has specialized columns to add significance
#' stars to the plot produced with [plot_summary_current_data()].
#'
#' @name sample_eEPSC_t_test_df
#' @docType data
#' @format An .rda file containing 16 objects of 10 variables
#' \describe{
#'  \item{`treatment`}{A character value specifying the treatment applied (e.g.
#'  "Control", "HNMPA") such as the antagonists or agonists, or any protocol
#'  applied to the animals (e.g. "Fasting").}
#'  \item{`.y.`}{A character value describing the parameter compared with the
#'  t-test. In this case, this uses the mean normalized amplitude of the first
#'  evoked current ("mean_P1_transformed") which has been generated using
#'  [make_summary_EPSC_data()].}
#'  \item{`group1`}{A character value describing the first group used in the
#'  paired t-test. This is the baseline interval, "t0to5".}
#'  \item{`group2`}{A character value describing the second group used in
#'  the paired t-test. Examples include "t5to10" and "t10to15".}
#'  \item{`n1`}{The number of paired values used in the comparison.}
#'  \item{`statistic`}{The test statistic.}
#'  \item{`df`}{The degrees of freedom.}
#'  \item{`p_string`}{The p-value expressed as a character value using
#'  `lazyWeave::pvalString()`, which rounds the values and expresses them
#'  according to typical published values (e.g. > 0.99, < 0.001 instead of
#'  exact values).}
#'  \item{`significance_stars`}{A character value containing asterisks indicating
#'  significance. `p < 0.05 = *`, `p < 0.01 = **`, and `p < 0.001 = ***`.}
#'  \item{`asterisk_time`}{A numeric value indicating the time value where a
#'  significance star will be added. This is the midpoint of each interval. For
#'  example, the `asterisk_time` for an interval from `5` to `10` minutes will
#'  be `7.5` minutes.}
#' }
#' @keywords data
NULL

# t-test sEPSC -----

#' Paired t-test results comparing spontaneous current amplitudes relative to
#' baseline amplitudes
#'
#' This is an example of the dataframe of t-test results produced using
#' [perform_t_tests_for_summary_plot()] where `current_type == "sEPSC"`. The
#' dataframe can be used for further statistical analyses, and it also has
#' specialized columns to add significance stars to the plot produced with
#' [plot_summary_current_data()].
#'
#' @name sample_sEPSC_t_test_df
#' @docType data
#' @format An .rda file containing 16 objects of 10 variables
#' \describe{
#'  \item{`treatment`}{A character value specifying the treatment applied (e.g.
#'  "Control", "HNMPA") such as the antagonists or agonists, or any protocol
#'  applied to the animals (e.g. "Fasting").}
#'  \item{`.y.`}{A character value describing the parameter compared with the
#'  t-test. In this case, this uses the mean normalized amplitude of spontaneous current ("mean_transformed amplitude") which has been generated using
#'  [make_summary_EPSC_data()].}
#'  \item{`group1`}{A character value describing the first group used in the
#'  paired t-test. This is the baseline interval, "t0to5".}
#'  \item{`group2`}{A character value describing the second group used in
#'  the paired t-test. Examples include "t5to10" and "t10to15".}
#'  \item{`n1`}{The number of paired values used in the comparison.}
#'  \item{`statistic`}{The test statistic.}
#'  \item{`df`}{The degrees of freedom.}
#'  \item{`p_string`}{The p-value expressed as a character value using
#'  `lazyWeave::pvalString()`, which rounds the values and expresses them
#'  according to typical published values (e.g. > 0.99, < 0.001 instead of
#'  exact values).}
#'  \item{`significance_stars`}{A character value containing asterisks indicating
#'  significance. `p < 0.05 = *`, `p < 0.01 = **`, and `p < 0.001 = ***`.}
#'  \item{`asterisk_time`}{A numeric value indicating the time value where a
#'  significance star will be added. This is the midpoint of each interval. For
#'  example, the `asterisk_time` for an interval from `5` to `10` minutes will
#'  be `7.5` minutes.}
#' }
#' @keywords data
NULL


# Pruned eEPSC -----

#' Evoked current data pruned to a summary point per minute
#'
#' This is an example of the dataframe of excitatory current amplitudes produced
#' using [make_pruned_EPSC_data()]. This dataset is a list with three dataframes
#' containing the pruned data. It can be used for further statistical analyses
#' or for plotting. [plot_raw_current_data()] and [plot_summary_current_data()]
#' in particular depend heavily on this type of dataset.
#'
#' \itemize{
#'  \item `individual_cells` is used to make pruned plots of individual
#'  recordings using [plot_raw_current_data()].
#'  \item `for_table` is used to make the experiment overview table
#'  \item `all_cells` is used in [plot_summary_current_data()]
#' }
#'
#' @name sample_pruned_eEPSC_df
#' @docType data
#' @format An list of 3 dataframes: `individual_cells`, `for_table` and
#'   `all_cells`.
#' \describe{
#'  `individual_cells` is used to make pruned plots of individual
#'  recordings using [plot_raw_current_data()].
#'  \itemize{
#'    \item `interval_pruned` A character value describing the interval that was
#'    used for the pruning function. If the data are pruned per minute, this
#'    will be "t0to1", "t1to2", "t2to3", etc.
#'    \item `mean_P1` The mean amplitude (in pA) of the first evoked current
#'    (P1) during a specific interval. This is an average of all data points
#'    within each interval. For example, the `mean_P1` for the interval "t0to1"
#'    contains the average current amplitude of all data points within the first
#'    minute of the recording.
#'    \item `sd_P1` The standard deviation of P1.
#'    \item `n` The number of data points used.
#'    \item `se` The standard error of P1.
#'    \item `cv` The coefficient of variation of P1.
#'    \item `cv_inverse_square` The inverse coefficient of variation, which is
#'    then squared. This is to enable variance analysis, as in
#'    [Huijstee & Kessels (2020)](https://doi.org/10.1016/j.jneumeth.2019.108526).
#'    \item `baseline_mean` The mean amplitude of the first evoked current
#'    during the baseline period.
#'    \item `category, letter, sex, treatment, etc.` Columns which are
#'    from the raw data. For a definition of these columns, please see the
#'    *Required basic columns* section in the documentation for
#'    [make_normalized_EPSC_data()].
#'    \item `time` The upper time value of the interval (e.g. 2 minutes for
#'    "t1to2") which is used on the x-axis of plots such as in
#'    [plot_raw_current_data()].
#'  }
#'
#'  `for_table` A dataframe containing two columns: letter and
#'  `P1_transformed`. The current data is collapsed into a single row for each
#'  letter, with the current data for each letter stored as a list. This is
#'  required to create sparklines of current amplitude over time within the cell
#'  summary table. See [make_interactive_summary_table()].
#'
#'  `all_cells` A dataframe consisting of all data within a single
#'  treatment grouped and summarized per minute (or some other variable if you
#'  change `interval_length` to be something other than `1`). Columns like
#'  `category` and `sex` are retained from the raw data. New columns for evoked
#'  current data (`current_type == "eEPSC"`) are:
#'
#'  \itemize{
#'    \item `interval_pruned` A character value describing the interval that was
#'    used for the pruning function. If the data are pruned per minute, this
#'    will be "t0to1", "t1to2", "t2to3", etc.
#'    \item `mean_P1_all_cells` The mean amplitude (in pA) of the first evoked
#'    current (P1) during a specific interval across all cells.
#'    \item `sd_P1_all_cells` The standard deviation of P1.
#'    \item `n` The number of data points used.
#'    \item `se_P1_all_cells` The standard error of P1.
#'    \item `cv_P1_all_cells` The coefficient of variation of P1.
#'  }
#' }
#' @keywords data
NULL


# Pruned sEPSC -----

#' Spontaneous current data pruned to a summary point per minute
#'
#' This is an example of the dataframe of spontaneous currents
#' produced using [make_pruned_EPSC_data()]. This dataset is a list with three
#' dataframes containing the pruned data. It can be used for further statistical
#' analyses or for plotting. [plot_raw_current_data()] and
#' [plot_summary_current_data()] in particular depend heavily on this type of
#' dataset.
#'
#' \itemize{
#'  \item `individual_cells` is used to make pruned plots of individual
#'  recordings using [plot_raw_current_data()].
#'  \item `for_table` is used to make the experiment overview table.
#'  \item `all_cells` is used in [plot_summary_current_data()].
#' }
#'
#' @name sample_pruned_sEPSC_df
#' @docType data
#' @format An list of 3 dataframes: `individual_cells`, `for_table` and
#'   `all_cells`.
#' \describe{
#'
#'  `individual_cells` is used to make pruned plots of individual
#'  recordings using [plot_raw_current_data()].
#'  \itemize{
#'    \item `letter, category, sex, treatment, etc.` Information about cell
#'    characteristics, which has automatically been pulled from
#'    cell_characteristics_df in [add_new_cells(). For a definition of these
#'    columns, please see the
#'    *Required basic columns* section in the documentation for
#'    [make_normalized_EPSC_data()].
#'    \item `interval_pruned` A character value describing the interval that was
#'    used for the pruning function. If the data are pruned per minute, this
#'    will be "t0to1", "t1to2", "t2to3", etc.
#'    \item `mean_amplitude` The mean normalized amplitude (% Baseline sEPSC
#'    amplitude) of all spontaneous currents during a specific interval.
#'    \item `mean_raw_amplitude` The mean raw amplitude (pA) of all spontaneous
#'    currents during a specific interval.
#'    \item `sd_amplitude` The standard deviation of the normalized spontaneous
#'    current amplitudes.
#'    \item `n` The number of data points used.
#'    \item `frequency` The frequency of the spontaneous currents (Hz) within
#'    the interval.
#'    \item `se` The standard error of the normalized spontaneous current
#'    amplitudes.
#'  \item `baseline_range` A logical value required to normalize frequency
#'  relative to the mean baseline frequency. It is set to TRUE when time is
#'  within the baseline period (e.g. `time <= 5`) and FALSE at all other times.
#'  Generated automatically in [make_normalized_EPSC_data()].
#'    \item `baseline_mean_frequency` The mean frequency of the spontaneous
#'    currents during the baseline period.
#'    \item `frequency_transformed` The normalized spontaneous current frequency
#'    (% Baseline frequency).
#'    \item `time` The upper time value of the interval (e.g. 2 minutes for
#'    "t1to2") which is used on the x-axis of plots such as in
#'    [plot_raw_current_data()].
#'  }
#'
#' `for_table` A dataframe containing two columns: letter and
#'  `spont_amplitude_transformed`. The current data is collapsed into a single
#'  row for each letter, with the current data for each letter stored as a list.
#'  This is required to create sparklines of current amplitude over time within
#'  the cell summary table. See [make_interactive_summary_table()].
#'
#'  `all_cells` A dataframe consisting of all data within a single
#'  treatment grouped and summarized per minute (or some other variable if you
#'  change `interval_length` to be something other than `1`). Columns like
#'  `category` and `sex` are retained from the raw data. New columns for
#'  spontaneous current data (`current_type == "sEPSC"`) are:
#'
#'  \itemize{
#'    \item `interval_pruned` A character value describing the interval that was
#'    used for the pruning function. If the data are pruned per minute, this
#'    will be "t0to1", "t1to2", "t2to3", etc.
#'    \item `mean_all_amplitude` The mean normalized amplitude (% Baseline
#'    sEPSC amplitude) of the spontaneous current amplitudes during a specific
#'    interval across all cells.
#'    \item `mean_all_raw_amplitude` The mean raw amplitude (in pA) of the
#'    spontaneous current amplitudes during a specific interval across all
#'    cells.
#'    \item `sd_all_amplitude` The standard deviation of the normalized
#'    spontaneous current amplitudes across all cells.
#'    \item `n` The number of data points used.
#'    \item `se_all_amplitude` The standard error of the normalized spontaneous
#'    currents across all cells.
#'    \item `sd_all_raw_amplitude` The standard deviation of the raw
#'    spontaneous current amplitudes across all cells.
#'    \item `se_raw_amplitude` The standard error of the raw spontaneous
#'    currents across all cells.
#'    \item `mean_all_frequency` The mean normalized frequency (% Baseline
#'    frequency) of all spontaneous current amplitudes across all cells during
#'    the interval.
#'    \item `sd_all_frequency` The standard deviation of the normalized
#'    frequency of all spontaneous current amplitudes across all cells during
#'    the interval.
#'    \item `se_frequency` The standard error of the normalized
#'    frequency of all spontaneous current amplitudes across all cells during
#'    the interval.
#'    \item `mean_all_raw_frequency` The mean raw frequency (Hz) of all
#'    spontaneous current amplitudes across all cells during the interval.
#'    \item `sd_all_raw_frequency` The standard deviation of the raw
#'    frequency of all spontaneous current amplitudes across all cells during
#'    the interval.
#'    \item `se_raw_frequency` The standard error of the raw
#'    frequency of all spontaneous current amplitudes across all cells during
#'    the interval.
#'  }
#' }
#' @keywords data
NULL

# Variance eEPSC -----


#' Variance-to-mean ratio and coefficient of variation of evoked current amplitudes
#'
#' This is an example of the dataframe generated from evoked current summary
#' data processed using the function [make_variance_data()]. It contains new
#' columns for the mean variance-to-mean ratio (VMR) and mean inverse
#' coefficient of variation squared for two time intervals. They are the
#' baseline period ("t0to5"), and a time interval after a hormone or protocol
#' has been applied (in this case, "t20to25"). It can be used for further
#' analyses and also to make the variance comparison plots.
#'
#' @name sample_eEPSC_variance_df
#' @docType data
#' @format An .rda file containing 38 objects of 23 variables.
#' \describe{
#' \item{`state`}{A character value describing if a data point belongs to the
#' baseline interval ("Baseline") or an interval after a hormone or protocol has
#' been applied ("Post-modification"). These intervals are selected from
#' `baseline_interval` and `post_hormone_interval`.}
#'  \item{`mean_cv_inverse_square`}{The mean inverse coefficient of variation
#'  squared within a specific state.}
#'  \item{`mean_VMR`}{The mean variance-to-mean ratio within a specific state.}
#'  \item{`category, letter, sex, treatment, etc.`}{Other columns which are
#'  imported directly from the summary data without modifications. For a
#'  detailed description of these columns, please see [sample_summary_eEPSC_df].}
#' }
#' @keywords data
#' @seealso [make_variance_data()], [plot_variance_comparison_data()], and [plot_cv_data()]
NULL

# PPR eEPSC ------

#' Evoked current data filtered to focus on paired-pulse ratio (PPR) analysis
#'
#' This is an example of the raw eEPSC data produced using
#' [make_normalized_EPSC_data()] after it has been filtered using
#' [make_PPR_data()]. The data are not modified, but they are filtered to only
#' include data within two time intervals (specified in the `baseline_interval`
#' and `post_hormone_interval` arguments). The paired-pulse ratio (PPR) values
#' are also filtered to only include values that fall within `PPR_min` and
#' `PPR_max`, which are two arguments in the [make_PPR_data()] function.
#'
#' This data can be used for PPR comparison plots, and further analyses to
#' determine the mechanism of synaptic plasticity.
#'
#'
#' @name sample_PPR_df
#' @docType data
#' @format An .rda file containing 2206 objects of 21 variables.
#' \describe{
#'  \item{`letter, synapses, sex, etc.`}{All columns that were produced in
#'  [make_normalized_EPSC_data()]. Please see [sample_raw_eEPSC_df] for detailed
#'  column descriptions.}
#'  \item{`state`}{A character ("baseline" or "post-modification") representing
#'  the timepoint that the data point belongs to.}
#' }
#' @keywords data
#'
#' @seealso [make_normalized_EPSC_data()], [make_PPR_data()], [plot_PPR_data_single_treatment()] and [plot_PPR_data_multiple_treatments()]
NULL


# Sample AP data ------

#' Example of a complete action potential dataset used for plotting and
#' analyses. This data contains parameters like peak_amplitude,
#' after-hyperpolarization amplitude, and half-width for two time-points: during
#' the control period (state == "Baseline") or after insulin has been applied
#' (state == "Insulin").
#'
#' This is an example of what the data looks like after being imported with `add_new_cells()` with `data_type = "AP"`. You can see that the same columns from the cell characteristics file have been appended to new rows for peak_amplitude, after-hyperpolarization amplitude, latency to fire, and others.
#'
#' @name sample_AP_data
#' @docType data
#' @format An .rda file containing 34 objects of 25 variables.
#' \describe{
#'  \item{`letter, synapses, ID, sex, animal, treatment, etc.`}{All columns that were produced in
#'  [add_new_cells()]. Please see [sample_cell_characteristics] for detailed
#'  column descriptions.}
#'  \item{`state`}{A character ("Baseline" or "Insulin") representing
#'  the timepoint that the data point belongs to.}
#'  \item{`time_of_threshold`}{The time (in ms) when the membrane potential reaches the threshold value.}
#'  \item{`threshold`}{The threshold (in mV). Determined using the first derivative method, where the threshold is the membrane potential which results in a derivative of 10 mV/ms or greater (Farries et al., 2010).}
#'  \item{`t_11`}{The value of the first derivative (action potential velocity in mV/ms) at threshold.}
#'  \item{`first_sweep_with_APs`}{The sweep number of the first sweep (going from lowest to higher current injection values) that resulted in an action potential.}
#'  \item{`trace_start`}{An automated value in Clampfit that is not used in the analysis.}
#'  \item{`peak_amplitude`}{The peak amplitude (in pA) of the action potential relative to threshold.}
#'  \item{`time_to_peak`}{The time to peak amplitude (in ms) relative to the time of threshold.}
#'  \item{`antipeak_amplitude`}{The after-hyperpolarization amplitude (in pA) relative to threshold.}
#'  \item{`time_of_antipeak`}{The time of the after-hyperpolarization (in ms).}
#'  \item{`half_width`}{The half-width, which is the width of the action potential and half of the peak amplitude.}
#'  \item{`latency_to_fire`}{The latency to fire, which is calculated by subtracting the injection start time from the `time_to_peak.`}
#'  \item{`antipeak_time_relative_to_threshold`}{The time of the afterhyperpolarization amplitude (in ms) relative to the time of the threshold.}
#' }
#' @keywords data
#'
#' @seealso [plot_AP_comparison()] and [plot_AP_trace()].
NULL

# Sample AP count data ------

#' Example of action potential count data for two time-points: during
#' the control period (state == "Baseline") or after insulin has been applied
#' (state == "Insulin"). These were obtained by counting the number of action potentials present within each sweep. The data were then merged with the `sample_cell_characteristics` data through `add_new_cells()` with `data_type = "AP_count"`.
#'
#' @name sample_AP_count_data
#' @docType data
#' @format An .rda file containing 340 objects of 16 variables.
#' \describe{
#'  \item{`letter, synapses, ID, sex, animal, treatment, etc.`}{All columns that were produced in
#'  [add_new_cells()]. Please see [sample_cell_characteristics] for detailed
#'  column descriptions.}
#'  \item{`state`}{A character ("Baseline" or "Insulin") representing
#'  the timepoint that the data point belongs to.}
#'  \item{`sweep`}{The sweep.}
#'  \item{`no_of_APs`}{The number of action potentials present in the sweep.}
#'  \item{`AP_frequency}{The frequency of the action potentials (Hz) in a sweep. This column is generated in `add_new_cells()`.}
#'  \item{`current_injection`}{The value of the current injection (in pA)  at a particular sweep.}
#' }
#' @keywords data
#'
#' @seealso [plot_AP_frequencies_multiple_treatments()] and
#' [plot_AP_frequencies_single_treatment()]
NULL

# Treatment colour theme -----

#' A dataframe of treatments and their assigned colours for consistency across plots
#'
#' This is an example of the dataframe used to specify a colour theme for each
#' treatment. It is used to create a consistent colour scheme across all plots
#' produced with this package.
#'
#' @section Defining your own treatment names and colours:
#'
#' Use the data.frame() function to define an object, then refer to this named object in all `treatment_colour_theme` arguments. E.g.
#'
#' `my_theme_colours <- data.frame(`
#' `category = c(2, 2, 2, 2),`
#' `treatment = c("Control", "HNMPA", "PPP", "PPP_and_HNMPA"),`
#' `display_names = c("Control", "HNMPA", "PPP", "PPP\n&\nHNMPA"),`
#' `colours = c("#f07e05", "#f50599", "#70008c", "#DCE319"),`
#' `very_pale_colours = c("#fabb78", "#fa98d5", "#ce90de", "yellow")`
#' `)`
#'
#' @name sample_treatment_names_and_colours
#' @docType data
#' @format An .rda file containing 4 objects of 5 variables
#' \describe{
#'  \item{`category`}{A numeric value representing the experiment type. Used to
#'  assign top-level groups for further analyses, with `treatment` as
#'  subgroups. For example, "1" may refer to an experiment where you applied high-frequency stimulation (HFS) to a cell, while "2" is an experiment where you added a hormone like leptin. "3" may be an experiment where you applied HFS in the continuous presence of leptin.}
#'  \item{`treatment`}{A character value specifying the treatment applied (e.g.
#'  "Control", "HNMPA") such as antagonists or agonists, or any protocol applied
#'  to the animals (e.g. "Fasting").}
#'  \item{`display_names`}{A character value with the same treatment names as in
#'  `treatment` except you would replace underscores with spaces and add line
#'  breaks if needed. This is to create more attractive, human-readable axis
#'  labels and table values.}
#'  \item{`colours`}{A character value with hex colour values.}
#'  \item{`very_pale_colours`}{A character value with hex colour values. These
#'  are all lighter than the colours in `colours` and they are used to create
#'  contrast in summary plots with both sexes.}
#' }
#' @keywords data
NULL


# Theme options -----

#' A dataframe of theme options for things like colours and line widths
#' @description This is an example of the dataframe used to specify the colour themes and
#' plot options (like line thickness). If you want to modify this file, see the section on **Defining your own theme options** below.
#'
#'
#' @section Defining your own theme options:
#'
#' Step 1: Create a .csv file with two columns (`option` and `value`) modelled after this sample dataset. **Important!**: Your .csv must have identical columns and rows as the sample data, or else some plots won't work!
#'
#' Step 2: Read in the .csv file in using `utils::read_csv()`. This will now be an object in your R environment.
#'
#' Step 3: **Important!!** You must convert the first column (`option`) into the `rownames`. This is a mandatory step to allow the theme_options to be indexed by row name in plotting functions.
#'
#' Step 4: Run the following code:
#'
#' `library(tibble)`
#' `my_sample_theme_options <- read.csv(here::here("Data/your_sample_theme_options.csv")) %>% remove_rownames %>% column_to_rownames(var="option")`
#'
#' Step 5: Check the resulting object. You should now have 11 objects of 1 variable, and the row names should be `gray_shading_colour`, `line_col`, etc.
#'
#' Step 6: Go to a plotting function like `plot_raw_current_data()` and replace `sample_theme_options` with your newly created object from Step 5.
#'
#' @name sample_theme_options
#' @docType data
#' @format An .rda file containing 23 objects of 2 variables
#'
#' \describe{
#'  \item{`gray_shading_colour`}{Hex code for the colour used to fill the violin
#'  plots.}
#'  \item{`line_col`}{Hex code for the colour used for the line indicating
#'  periods when a hormone was applied.}
#'  \item{`rectangle_shading_colour`}{Hex code for the colour used to shade the
#'  rectangles.}
#'  \item{`male_shape`}{Numeric value representing the point shape used for data
#'  values where `Sex == Male`.}
#'  \item{`female_shape`}{Numeric value representing the point shape used for
#'  data values where `Sex == Female`.}
#'  \item{`mean_point_colour`}{Hex code for the colour used to fill the mean data point in plots like the PPR comparison plot.}
#'  \item{`connecting_line_width`}{The width of the connecting line in action potential comparison plots.}
#'  \item{`connecting_line_colour`}{The colour of the connecting line in action potential comparison plots.}
#'  \item{`connecting_line_width_PPR`}{The width of the connecting line in paired-pulse ratio comparison plots.}
#'  \item{`mean_point_size`}{The size of the mean data point in plots like the PPR comparison plot.}
#'  \item{`geom_signif_text_size`}{The size of the asterisk text in plots like the spontaneous current comparison plots.}
#'  \item{`baseline_group_colour`}{Hex code for the colour used when `state == "Baseline"` in `plot_AP_comparison()` and `plot_AP_frequencies_single_treatment()`.}
#' }
#' @keywords data
NULL


# Important for future modifications of theme options
# If redefining this, you must convert the first column (option) into the rownames
# This is so the data can be indexed by row name in plotting functions
# library(tibble)
#
# theme_options <- read.csv(here::here("inst/extdata/sample_theme_options.csv"))
#
# sample_theme_options <- theme_options %>% remove_rownames %>% column_to_rownames(var="option")
# usethis::use_data(sample_theme_options, overwrite=T, compress="xz")
