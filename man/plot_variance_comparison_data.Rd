% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plot-data.R
\name{plot_variance_comparison_data}
\alias{plot_variance_comparison_data}
\title{Plot variance comparison for a treatment}
\usage{
plot_variance_comparison_data(
  data,
  plot_category,
  plot_treatment,
  variance_measure = "cv",
  baseline_interval = "t0to5",
  baseline_label = "Baseline",
  post_hormone_interval = "t20to25",
  post_hormone_label = "Insulin",
  test_type,
  large_axis_text = "no",
  treatment_colour_theme,
  theme_options,
  save_plot_png = "no",
  ggplot_theme = patchclampplotteR_theme()
)
}
\arguments{
\item{data}{A dataframe generated using \code{\link[=make_variance_data]{make_variance_data()}}.}

\item{plot_category}{A numeric value specifying the category, which can be
used to differentiate different protocol types. In the sample dataset for
this package, \code{plot_category == 2} represents experiments where insulin was
applied continuously after a 5-minute baseline period.}

\item{plot_treatment}{A character value specifying the treatment you would
like to plot (e.g. "Control"). \code{plot_treatment} represents antagonists that
were present on the brain slice, or the animals were fasted, etc.}

\item{variance_measure}{A character value ("cv" or "VMR"). The variance
measures can be either the inverse coefficient of variation squared
(\code{variance_measure == "cv"}) or variance-to-mean ratio (\code{variance_measure == "VMR"}).}

\item{baseline_interval}{A character value indicating the name of the
interval used as the baseline. Defaults to "t0to5", but can be changed.
Make sure that this matches the baseline interval that you set in
\code{\link[=make_normalized_EPSC_data]{make_normalized_EPSC_data()}}.}

\item{baseline_label}{A character value for the x-axis label applied to the
pre-hormone state. Defaults to "Baseline".}

\item{post_hormone_interval}{A character value specifying the interval used
for the data points after a hormone or protocol was applied. This must
match the \code{post_hormone_interval} used in \code{\link[=make_variance_data]{make_variance_data()}}.}

\item{post_hormone_label}{A character value for x-axis label applied to
the post-hormone or post-protocol state. Defaults to "Post-hormone" but you
will likely change this to the hormone or protocol name.}

\item{test_type}{A character (must be "wilcox.test", "t.test" or "none")
describing the statistical model used to create a significance bracket
comparing the pre- and post-hormone groups.}

\item{large_axis_text}{A character ("yes" or "no"). If "yes", a ggplot theme
layer will be applied which increases the axis text.}

\item{treatment_colour_theme}{A dataframe containing treatment names and
their associated colours as hex values. See
\link{sample_treatment_names_and_colours} for an example of what this dataframe
should look like.}

\item{theme_options}{A dataframe containing theme options. See
\link{sample_theme_options} for an example of what this dataframe should
look like.}

\item{save_plot_png}{A character ("yes" or "no"). If "yes", the plot will be
saved as a .png using ggsave. The filepath depends on the current type, but
they will all go in subfolders below Figures/ in your project directory.}

\item{ggplot_theme}{The name of a ggplot theme or your custom theme. This will be added as a layer to a ggplot object. The default is \code{patchclampplotteR_theme()}, but other valid entries include \code{theme_bw()}, \code{theme_classic()} or the name of a custom ggplot theme stored as an object.}
}
\value{
A ggplot object. If \code{save_plot_png == "yes"}, it will also generate
a .png file exported to \code{Figures/Evoked-currents/Variance-plots}. The plot
will be named in the form of
"Variance-comparison-category-\code{plot_category}-\code{plot_treatment}-\code{variance_measure}.png".
An example filename is "Variance-comparison-category-2-Control-cv.png".
}
\description{
\code{plot_variance_comparison_data()} creates a connected  plot with time as a
categorical variable (i.e. baseline/before and after) on the x-axis and the
variance measure on the y-axis. There are also lines
connecting the "before" data point to the "after" data point for each letter.
}
\details{
If you specify a \code{test_type}, the function will perform a paired t-test or
paired wilcox test and add brackets with significance stars through
\code{ggsignif::geom_signif()}.

This allows you to visually determine if a change in synaptic plasticity is
due to a pre- or post-synaptic mechanism. For more information, please see
\href{https://doi.org/10.1016/j.jneumeth.2019.108526}{Huijstee & Kessels (2020)}.
}
\examples{
plot_variance_comparison_data(
  data = sample_eEPSC_variance_df,
  plot_category = 2,
  plot_treatment = "Control",
  variance_measure = "cv",
  baseline_interval = "t0to5",
  post_hormone_interval = "t20to25",
  post_hormone_label = "Insulin",
  test_type = "wilcox.test",
  large_axis_text = "no",
  treatment_colour_theme = sample_treatment_names_and_colours,
  theme_options = sample_theme_options
)

}
