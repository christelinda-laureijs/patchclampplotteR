% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot-data.R
\name{plot_spontaneous_current_trace}
\alias{plot_spontaneous_current_trace}
\title{Plot a representative spontaneous current trace}
\usage{
plot_spontaneous_current_trace(
  file,
  plot_colour,
  include_scale_bar = "yes",
  plot_episode = "epi1",
  scale_bar_x_start = 1.25,
  scale_bar_x_length = 0.5,
  scale_bar_y_start = 15,
  scale_bar_y_length = 20,
  plot_x_min = 1,
  plot_x_max = 5,
  plot_y_min = -100,
  plot_y_max = 35,
  save_plot_pngs = "no"
)
}
\arguments{
\item{file}{A dataframe containing at least these columns: \code{time},
\code{episode}, \code{current}, \code{voltage}, \code{time_sec}. An easy way to obtain this is
by importing a raw .abf file through the \code{\link[=import_ABF_file]{import_ABF_file()}} function.}

\item{plot_colour}{A character value naming the colour of the plot.}

\item{include_scale_bar}{A character value that determines if a scale bar
will be added to the plot. Allowed values are "yes" and "no".}

\item{plot_episode}{A character value describing the sweep (e.g. \code{epi1}) that
will be used for the plot.}

\item{scale_bar_x_start}{A numeric value describing the x-axis position of
the scale bar.}

\item{scale_bar_x_length}{A numeric value describing the horizontal span (in
seconds) of the scale bar. This will automatically be converted and
displayed in milliseconds.}

\item{scale_bar_y_start}{A numeric value describing the y-axis position of
the scale bar.}

\item{scale_bar_y_length}{A numeric value describing the vertical span (in
pA) of the scale bar.}

\item{plot_x_min}{A numeric value describing the minimum value on the x-axis
(in seconds).}

\item{plot_x_max}{A numeric value describing the maximum value on the x-axis
(in seconds).}

\item{plot_y_min}{A numeric value describing the minimum value on the y-axis
(in pA).}

\item{plot_y_max}{A numeric value describing the maximum value on the y-axis
(in pA).}

\item{save_plot_pngs}{A character ("yes" or "no") defining if the plot should
be saved as a PNG through \code{ggplot::ggsave()}.}
}
\value{
A ggplot object. If save_plot_PNGs is defined as "yes", it will also
generate a .png file in the folder
\code{Figures/Spontaneous-currents/Representative-Traces} relative to the
project directory.
}
\description{
\code{plot_spontaneous_current_trace()} generates a plot of raw current amplitude
over time for a specified sweep from an ABF file. It requires a dataframe
generated from raw .abf data with \code{\link[=import_ABF_file]{import_ABF_file()}}. The function returns a
ggplot object with an optional scale bar.
}
\examples{
plot_spontaneous_current_trace(
  file = sample_abf_file,
  plot_colour = "#6600cc",
  include_scale_bar = "yes",
  plot_episode = "epi1",
  scale_bar_x_length = 1,
  scale_bar_y_length = 10,
  plot_x_min = 1,
  plot_x_max = 3
)

}
