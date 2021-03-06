% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s216-applets.R
\name{two_mean_bootstrap_CI}
\alias{two_mean_bootstrap_CI}
\title{Function to create bootstrap confidence interval for difference in two means}
\usage{
two_mean_bootstrap_CI(
  formula,
  data,
  first_in_subtraction,
  confidence_level = 0.95,
  number_repetitions = 3
)
}
\arguments{
\item{formula}{Formula of the form response~predictor, where predictor defines two groups and response is binary or two-level categorical}

\item{data}{Dataset with columns for response and predictor variable}

\item{first_in_subtraction}{Value of predictor that should be first in order of subtraction for computing statistics}

\item{confidence_level}{confidence level to use for interval construction in decimal form.  Default is 95\%}

\item{number_repetitions}{number of draws for simulation test}
}
\value{
Produces plot of distribution of bootstrapped values, with values as or more extreme than confidence interval range highlighted and reports CI as subtitle on plot
}
\description{
Function to create bootstrap confidence interval for difference in two means
}
\examples{
data(pt)
two_mean_bootstrap_CI(responses~brand, data = pt, first_in_subtraction = "B1",
           number_repetitions = 100, confidence_level = 0.98)

}
