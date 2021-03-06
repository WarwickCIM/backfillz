% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_slice_histogram.R
\name{plot_slice_histogram}
\alias{plot_slice_histogram}
\title{Creates a slice histogram of MCMC chains}
\usage{
plot_slice_histogram(
  object = NULL,
  slices = NULL,
  save_plot = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{object}{Backfillz or Stanfit object.
Stanfit objects are converted to Backfillz object
using the as_backfillz function.}

\item{slices}{Dataframe giving the cut
off points for the trace histograms and parameters to show.}

\item{save_plot}{Set to TRUE to save plots
in the Backfillz object.}

\item{verbose}{Set to TRUE to see function messages}
}
\description{
Creates a slice histogram of MCMC chains
}
