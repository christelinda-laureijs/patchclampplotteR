% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sample-datasets.R
\docType{data}
\name{sample_PPR_df}
\alias{sample_PPR_df}
\title{Evoked current data filtered to focus on paired-pulse ratio (PPR) analysis}
\format{
An .rda file containing 2206 objects of 21 variables.
\describe{
\item{\verb{letter, synapses, sex, etc.}}{All columns that were produced in
\code{\link[=make_normalized_EPSC_data]{make_normalized_EPSC_data()}}. Please see \link{sample_raw_eEPSC_df} for detailed
column descriptions.}
\item{\code{state}}{A character ("baseline" or "post-modification") representing
the timepoint that the data point belongs to.}
}
}
\description{
This is an example of the raw eEPSC data produced using
\code{\link[=make_normalized_EPSC_data]{make_normalized_EPSC_data()}} after it has been filtered using
\code{\link[=make_PPR_data]{make_PPR_data()}}. The data are not modified, but they are filtered to only
include data within two time intervals (specified in the \code{baseline_interval}
and \code{post_hormone_interval} arguments). The paired-pulse ratio (PPR) values
are also filtered to only include values that fall within \code{PPR_min} and
\code{PPR_max}, which are two arguments in the \code{\link[=make_PPR_data]{make_PPR_data()}} function.
}
\details{
This data can be used for PPR comparison plots, and further analyses to
determine the mechanism of synaptic plasticity.
}
\seealso{
\code{\link[=make_normalized_EPSC_data]{make_normalized_EPSC_data()}}, \code{\link[=make_PPR_data]{make_PPR_data()}}, \code{\link[=plot_PPR_data_single_treatment]{plot_PPR_data_single_treatment()}} and \code{\link[=plot_PPR_data_multiple_treatments]{plot_PPR_data_multiple_treatments()}}
}
\keyword{data}
