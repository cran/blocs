% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uncertainty.R
\name{new_vbsum}
\alias{new_vbsum}
\title{Constructor for vbdf summaries}
\usage{
new_vbsum(x, bloc_var, var_type, summary_type, resamples)
}
\arguments{
\item{x}{data.frame of uncertainty summary}

\item{bloc_var}{string, the name of the variable that defines the voting blocs}

\item{var_type}{string, the type of variable, discrete or continuous}

\item{summary_type}{string, the type of variable, discrete or continuous}

\item{resamples}{numeric, the number of bootstrap resamples}
}
\value{
A \code{vbsum} object
}
\description{
Constructor for vbdf summaries
}
