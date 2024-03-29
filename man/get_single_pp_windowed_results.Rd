% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-crunching-functions-dynamic.R
\name{get_single_pp_windowed_results}
\alias{get_single_pp_windowed_results}
\title{Function calculating windowed hrvhra results for a single RR time series}
\usage{
get_single_pp_windowed_results(
  RR,
  time_functions_list = glb_time_functions,
  window_type = "time",
  time_unit = "minute",
  move_type = "jump",
  window_length = 5,
  cut_end = FALSE,
  return_all = FALSE,
  tolerance = 0.05,
  shuffle = "No",
  pnnX_th,
  pnn_perc_th
)
}
\arguments{
\item{RR}{rr object}

\item{window_type}{string, jumping or sliding}

\item{time_unit}{unit of time (minutes or seconds)}

\item{move_type}{string, time based or index based}

\item{window_length}{numeric, window length}

\item{tolerance}{what is the maximum data loss in a single window in dynamic analysis that should be tolerated}

\item{shuffle}{whether the data should be shuffled}

\item{pnnX_th}{pnnX thresholds vector}

\item{pnn_perc_th}{pnn_perc_th thresholds vector}
}
\value{
data.frame with results for windows as rows
}
\description{
Function calculating windowed hrvhra results for a single RR time series
}
