# Documentation for sample datasets

#' Cell Characteristics Data
#'
#' @format A dataframe with 19 rows and 11 columns
#'
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
#'}
#'
#'@export
"sample-cell-characteristics.csv"
