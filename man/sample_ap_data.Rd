% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sample-datasets.R
\docType{data}
\name{sample_AP_data}
\alias{sample_AP_data}
\title{Example of a complete action potential dataset used for plotting and
analyses. This data contains parameters like peak_amplitude,
after-hyperpolarization amplitude, and half-width for two time-points: during
the control period (state == "Baseline") or after insulin has been applied
(state == "Insulin").}
\format{
An .rda file containing 34 objects of 25 variables.
\describe{
\item{\verb{letter, synapses, ID, sex, animal, treatment, etc.}}{All columns that were produced in
\code{\link[=add_new_cells]{add_new_cells()}}. Please see \link{sample_cell_characteristics} for detailed
column descriptions.}
\item{\code{state}}{A character ("Baseline" or "Insulin") representing
the timepoint that the data point belongs to.}
\item{\code{time_of_threshold}}{The time (in ms) when the membrane potential reaches the threshold value.}
\item{\code{threshold}}{The threshold (in mV). Determined using the first derivative method, where the threshold is the membrane potential which results in a derivative of 10 mV/ms or greater (Farries et al., 2010).}
\item{\code{t_11}}{The value of the first derivative (action potential velocity in mV/ms) at threshold.}
\item{\code{first_sweep_with_APs}}{The sweep number of the first sweep (going from lowest to higher current injection values) that resulted in an action potential.}
\item{\code{trace_start}}{An automated value in Clampfit that is not used in the analysis.}
\item{\code{peak_amplitude}}{The peak amplitude (in pA) of the action potential relative to threshold.}
\item{\code{time_to_peak}}{The time to peak amplitude (in ms) relative to the time of threshold.}
\item{\code{antipeak_amplitude}}{The after-hyperpolarization amplitude (in pA) relative to threshold.}
\item{\code{time_of_antipeak}}{The time of the after-hyperpolarization (in ms).}
\item{\code{half_width}}{The half-width, which is the width of the action potential and half of the peak amplitude.}
\item{\code{latency_to_fire}}{The latency to fire, which is calculated by subtracting the injection start time from the \code{time_to_peak.}}
\item{\code{antipeak_time_relative_to_threshold}}{The time of the afterhyperpolarization amplitude (in ms) relative to the time of the threshold.}
}
}
\description{
This is an example of what the data looks like after being imported with \code{add_new_cells()} with \code{data_type = "AP"}. You can see that the same columns from the cell characteristics file have been appended to new rows for peak_amplitude, after-hyperpolarization amplitude, latency to fire, and others.
}
\seealso{
\code{\link[=plot_AP_comparison]{plot_AP_comparison()}} and \code{\link[=plot_AP_trace]{plot_AP_trace()}}.
}
\keyword{data}
