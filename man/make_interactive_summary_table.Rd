% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot-data.R
\name{make_interactive_summary_table}
\alias{make_interactive_summary_table}
\title{Make interactive overview table of all recordings}
\usage{
make_interactive_summary_table(
  cell_characteristics_dataframe,
  pruned_eEPSC_dataframe = NULL,
  pruned_sEPSC_dataframe = NULL,
  treatment_colour_theme,
  include_all_treatments = "yes",
  list_of_treatments = NULL,
  include_all_categories = "yes",
  list_of_categories = NULL,
  save_output_as_RDS = "no",
  ggplot_theme = patchclampplotteR_theme()
)
}
\arguments{
\item{cell_characteristics_dataframe}{A dataframe containing the cell
characteristics, generated from \code{\link[=import_cell_characteristics_df]{import_cell_characteristics_df()}}.}

\item{pruned_eEPSC_dataframe}{A dataframe containing pruned evoked current
data, generated from \code{\link[=make_pruned_EPSC_data]{make_pruned_EPSC_data()}}, where \code{current_type == "eEPSC"}. Defaults to NULL to enable flexibility if you only have spontaneous data.}

\item{pruned_sEPSC_dataframe}{A dataframe containing pruned spontaneous
current data, generated from \code{\link[=make_pruned_EPSC_data]{make_pruned_EPSC_data()}}, where \code{current_type == "sEPSC"}. Defaults to NULL to enable flexibility if you only have evoked current data.}

\item{treatment_colour_theme}{A dataframe containing treatment names and
their associated colours as hex values. See
\link{sample_treatment_names_and_colours} for an example of what this dataframe
should look like.}

\item{include_all_treatments}{A character (\code{"yes"} or \code{"no"}) specifying if the
plot will include data from all treatments. If \code{"no"}, you must specify a
list of treatments in \code{list_of_treatments}.}

\item{list_of_treatments}{A list of character values describing the
treatments that will be in the plot. Defaults to \code{NULL}, since
include_all_treatments is \code{"yes"} by default.}

\item{include_all_categories}{A character (\code{"yes"} or \code{"no"}) specifying if the
plot will include data from all categories. If \code{"no"}, you must specify a
list of categories in \code{list_of_categories}.}

\item{list_of_categories}{A list of character values describing the
categories that will be in the plot. Defaults to \code{NULL}, since
\code{include_all_categories} is \code{"yes"} by default.}

\item{save_output_as_RDS}{A character (\code{"yes"} or \code{"no"}) describing if the
resulting object should be saved as an RDS file in the folder \code{"Data/Output-Data-from-R"}. The function will automatically create this folder if it doesn't already exist.
Note: This is not the interactive table, but it is the raw dataframe that
is later inserted into \code{reactable::reactable()}. This is useful if you want
to build your own table using a different package, or you want to generate
a customized reactable table yourself.}

\item{ggplot_theme}{The name of a ggplot theme or your custom theme. This will be added as a layer to a ggplot object. The default is \code{patchclampplotteR_theme()}, but other valid entries include \code{theme_bw()}, \code{theme_classic()} or the name of a custom ggplot theme stored as an object.}
}
\value{
A reactable HTML widget that can be viewed in RStudio or exported in
RMarkdown HTML documents. If \code{save_output_as_RDS == "yes"}, the raw
dataframe used to create the reactable is also exported as an .rds file
into \verb{Data/Output-Data-from-R/}.
}
\description{
This function pulls information from multiple dataframes to display
everything about a cell (cell characteristics, evoked current data, and
spontaneous current data) in an interactive table. The table is made with
\code{reactable::reactable()}, so it can be filtered, sorted, and rearranged.
}
\details{
The table contains sparklines of the evoked current and spontaneous current
amplitudes over time, which allows you to visually compare the overall
response of a group of cells.

The sparklines are colour-coded by treatment, allowing you to quickly
identify trends in response to a hormone/protocol for all cells belonging to
a particular treatment.

If you only have one type of current data you can omit one dataframe. For example, if you have evoked current data but not spontaneous current data, fill in \code{pruned_eEPSC_dataframe}, and delete the argument \code{pruned_sEPSC_dataframe}, or fill it in as \code{NULL}.
}
\examples{

# Note, the number of treatments is limited to "Control" and "PPP" to reduce run-time

## Both evoked and spontaneous data

make_interactive_summary_table(
  cell_characteristics_dataframe = sample_cell_characteristics,
  pruned_eEPSC_dataframe = sample_pruned_eEPSC_df,
  pruned_sEPSC_dataframe = sample_pruned_sEPSC_df,
  treatment_colour_theme = sample_treatment_names_and_colours,
  include_all_treatments = "no",
  list_of_treatments = c("Control", "PPP"),
  include_all_categories = "yes",
  list_of_categories = NULL,
  save_output_as_RDS = "no"
)

## Evoked current data only
make_interactive_summary_table(
  cell_characteristics_dataframe = sample_cell_characteristics,
  pruned_eEPSC_dataframe = sample_pruned_eEPSC_df,
  treatment_colour_theme = sample_treatment_names_and_colours,
  include_all_treatments = "no",
  list_of_treatments = c("Control", "PPP"),
  include_all_categories = "yes",
  list_of_categories = NULL,
  save_output_as_RDS = "no"
)

## Spontaneous current data only
make_interactive_summary_table(
  cell_characteristics_dataframe = sample_cell_characteristics,
  pruned_sEPSC_dataframe = sample_pruned_sEPSC_df,
  treatment_colour_theme = sample_treatment_names_and_colours,
  include_all_treatments = "no",
  list_of_treatments = c("Control", "PPP"),
  include_all_categories = "yes",
  list_of_categories = NULL,
  save_output_as_RDS = "no"
)
}
