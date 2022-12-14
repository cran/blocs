% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discrete.R
\name{wtd_table}
\alias{wtd_table}
\title{Weighted frequency table or proportions}
\usage{
wtd_table(
  ...,
  weight = NULL,
  na.rm = FALSE,
  prop = FALSE,
  return_tibble = FALSE,
  normwt = FALSE
)
}
\arguments{
\item{...}{vectors of class factor or character, or a list/data.frame of such vectors.}

\item{weight}{optional vector of weights. The default uses uniform weights of 1.}

\item{na.rm}{logical, whether to remove NA values.}

\item{prop}{logical, whether to return proportions or counts. Default returns counts.}

\item{return_tibble}{logical, whether to return a tibble or named vector.}

\item{normwt}{logical, whether to normalize weights such that they sum to 1.}
}
\value{
a vector or tibble of counts or proportions by group
}
\description{
Weighted frequency table or proportions
}
