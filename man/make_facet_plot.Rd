% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot-data.R
\name{make_facet_plot}
\alias{make_facet_plot}
\title{Make facet plot of raw data}
\usage{
make_facet_plot(
  data,
  plot_category,
  plot_treatment,
  plot_sex,
  pruned = "no",
  current_type = "eEPSC",
  y_variable = "P1",
  x_label = "Time (min)",
  treatment_colour_theme,
  ggplot_theme = patchclampplotteR_facet_theme()
)
}
\arguments{
\item{data}{A dataframe containing the raw evoked current data generated from
\code{\link[=make_normalized_EPSC_data]{make_normalized_EPSC_data()}}. If \code{pruned = "yes"} you must use the \verb{$individual_cells} component of a pruned dataset.}

\item{plot_category}{A numeric value specifying the category, which can be
used to differentiate different protocol types. In the sample dataset for
this package, \code{plot_category == 2} represents experiments where insulin was
applied continuously after a 5-minute baseline period.}

\item{plot_treatment}{A character value specifying the treatment you would
like to plot (e.g. \code{"Control"}). \code{plot_treatment} represents antagonists that
were present on the brain slice, or the animals were fasted, etc.}

\item{plot_sex}{A character value ("Male" or "Female") corresponding to the sex you would like to plot.}

\item{pruned}{A character value (\code{"yes"} or \code{"no"}) specifying if the data are
pruned. The plot will then present the data as means with error bars. This is only relevant for y variables like \code{mean_P1} for \code{current_type = "eEPSC"}.}

\item{current_type}{A character describing the current type. Allowed values
are \code{"eEPSC"} or \code{"sEPSC"}.}

\item{y_variable}{A character value specifying the variable to be plotted on
the y-axis. For evoked currents (\code{current_type = "eEPSC"}), the available
y_variables are \code{"P1"}, \code{"P1_transformed"}, \code{"mean_P1"} and \code{"PPR"}. \emph{Note}: If you
select \code{"mean_P1"}, you must set the \code{pruned} argument to \code{"yes"}. For
spontaneous currents (\code{current_type = "sEPSC"}), the available y_variables
are \code{"amplitude"} or \code{"frequency"}. NOTE: \code{"frequency"} is only available if \code{pruned = "yes"}.}

\item{x_label}{A character value specifying the x-axis label. Defaults to "Time (min)".}

\item{treatment_colour_theme}{A dataframe containing treatment names and
their associated colours as hex values. See
\link{sample_treatment_names_and_colours} for an example of what this dataframe
should look like.}

\item{ggplot_theme}{The name of a ggplot theme or your custom theme. This will be added as a layer to a ggplot object. The default is \code{patchclampplotteR_theme()}, but other valid entries include \code{theme_bw()}, \code{theme_classic()} or the name of a custom ggplot theme stored as an object.}
}
\value{
A ggplot object, which is a facetted plot of the raw data.
}
\description{
This function provides a useful way to quickly plot all data for a specific treatment and sex in one facet plot. This can be helpful if you want to create a display of the different treatments.
}
\details{
I have found it helpful to use this with knitr's RMarkdown features to create an interactive HTML page with the raw plots from all my cells, an interactive summary table (\code{\link[=make_interactive_summary_table]{make_interactive_summary_table()}}), and summary plots (\code{\link[=plot_summary_current_data]{plot_summary_current_data()}}) for all treatments. This makes it easier to review cells while preparing presentations.

If you are using \code{\link[=make_facet_plot]{make_facet_plot()}} within an RMarkdown document, you will need to change the \code{fig.height} and \code{fig.width} options.

(Taken from the documentation for \code{\link[=get_fig_height]{get_fig_height()}}): Set \code{fig.width = 14} and \code{fig.height = get_fig_height(...)}. Replace the \code{...} with the correct category, treatment and sex you'd like to plot. Plot only one facet plot per chunk.
}
\examples{


# Raw eEPSC facet plots

make_facet_plot(sample_raw_eEPSC_df,
  plot_category = 2,
  plot_treatment = "Control",
  plot_sex = "Male",
  pruned = "no",
  current_type = "eEPSC",
  y_variable = "P1",
  treatment_colour_theme = sample_treatment_names_and_colours,
  ggplot_theme = patchclampplotteR_facet_theme()
)


# Pruned eEPSC facet plots

make_facet_plot(sample_pruned_eEPSC_df$individual_cells,
  plot_category = 2,
  plot_treatment = "Control",
  plot_sex = "Male",
  pruned = "yes",
  current_type = "eEPSC",
  y_variable = "mean_P1",
  treatment_colour_theme = sample_treatment_names_and_colours,
  ggplot_theme = patchclampplotteR_facet_theme()
)
}
