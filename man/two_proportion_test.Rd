% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s216-applets.R
\name{two_proportion_test}
\alias{two_proportion_test}
\title{Function to perform hypothesis test for equality of two proportions using simulation}
\usage{
two_proportion_test(
  formula,
  data,
  first_in_subtraction,
  response_value_numerator,
  number_repetitions = 1,
  as_extreme_as,
  direction = c("greater", "less", "two-sided")
)
}
\arguments{
\item{formula}{Formula of the form response~predictor, where predictor defines two groups and response is binary or two-level categorical}

\item{data}{Dataset with columns for response and predictor variable}

\item{first_in_subtraction}{Value of predictor that should be first in order of subtraction for computing statistics}

\item{response_value_numerator}{Value of response that corresponds to "success" computing proportions}

\item{number_repetitions}{number of draws for simulation test}

\item{as_extreme_as}{observed statistic}

\item{direction}{one of "greater", "less", or "two-sided" to give direction of hypothsis test}
}
\value{
Produces mosaic plot of observed proportions and plot of distribution of simulated values, with values as or more extreme than specified value highlighted and count/proportion of those values reported as subtitle on plot
}
\description{
Function to perform hypothesis test for equality of two proportions using simulation
}
\examples{
data(pt)
pt$twoSeconds <- ifelse(pt$responses >=2, "Yes", "No")
two_proportion_test(twoSeconds~brand, data = pt, first_in_subtraction = "B1",
           response_value_numerator = "Yes", number_repetitions = 100, as_extreme_as = -.4,
           direction = "two-sided")

}
