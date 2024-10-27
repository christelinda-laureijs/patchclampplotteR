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

#' Sample_cell_characteristics
#'
#' @description
#'  This dataset provides an example of the type of cell characteristics that you
#'should be recording for your data. The dataset contains columns for the animal
#'number, age, sex, the synapses being recorded from, and treatments applied.
#'There is also a column for access, which is stored as a list.
#'
#'The most important column is \code{letter}. This is a unique identifier that
#'you will assign to each recording. The \code{letter} column will enable you to
#'link all information relevant to the recording (evoked current data,
#'spontaneous current data, animal data like age, sex, etc.) from different
#'files.
#'
#'The information in your cell_characteristics dataset will be merged with raw
#'recording data in functions like make_normalized_EPSC_data(). This will enable
#'you to analyze relationships between properties like age and current
#'amplitude.
#'
#'
#' @name sample_cell_characteristics
#' @docType data
#' @format A dataframe with 19 rows and 11 columns
#' \describe{
#'  \item{letter}{A character value that is a unique identifier for a single recording. Used to link data sets for evoked or spontaneous currents and cell-characteristics. Example: "A"}
#'  \item{cell}{A character or numeric value representing the cell. For example, use `3.1.1` for animal #3, slice #1, cell #1.}
#'  \item{sex}{A character value such as "Male" or "Female".}
#'  \item{X}{A numeric value representing the x-value of the cell's location in µm.}
#'  \item{Y}{A numeric value representing the y-value of the cell's location in µm.}
#'  \item{age}{A numeric value representing the animal's age. Can be any value as long as the time units are consistent throughout (e.g. don't mix up days and months when reporting animal ages). Do not use characters (e.g. avoid "P31" and use 31 instead).}
#'  \item{animal}{A numeric value representing the animal's ID or number.}
#'  \item{synapses}{A character value such as "Glutamate" or "GABA".}
#'  \item{treatment}{A character value such as "Control" or "HNMPA".}
#'  \item{category}{A numeric value representing the experiment type. Used to assign top-level groups for further analyses, with `treatment` as subgroups.}
#'  \item{R_a}{A list of values for the access resistance, which would have been monitored at several timepoints throughout the recording. See the section `R_a` formatting below.}
#' }
#' @keywords data
#' @examples
#' utils::read.csv(load_ext_data("sample_cell_characteristics.csv"))
#'
NULL

#' Sample ABF file
#'
#' This is an excerpt from a raw recording file exported from Clampfit as an
#' .abf file. It can be read using the import_ABF_file() function, which will
#' convert it into a regular dataframe for further manipulation and plotting in
#' R. It is used to demonstrate the make_representative_traces() function.
#'
#' @name sample_abf
#' @docType data
#' @format An Axon Binary Format (.abf) file with 10000 observations of 5
#' variables.
#' \describe{
#'  \item{episode}{A factor representing the sweep, such as "epi1".}
#'  \item{time_sec}{Time in seconds.}
#'  \item{time}{Time in ms*100.}
#'  \item{voltage}{Numeric value representing voltage in mV.}
#'  \item{current}{Numeric value representing current amplitude in pA.}
#'}
#' @keywords data
#' @examples import_ABF_file(load_ext_data("sample_abf.abf"))
NULL
