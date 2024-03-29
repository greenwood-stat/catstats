% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s216-applets.R
\name{two_proportion_test}
\alias{two_proportion_test}
\title{Simulation-based hypothesis test for a difference in proportions}
\usage{
two_proportion_test(
  formula,
  data,
  first_in_subtraction,
  response_value_numerator,
  as_extreme_as,
  direction = c("greater", "less", "two-sided"),
  number_repetitions = 1,
  add_normal = FALSE
)
}
\arguments{
\item{formula}{Formula of the form \code{response ~ predictor},
where \code{predictor} defines the two groups of the explanatory variable and
\code{response} is binary or a two-level categorical variable.}

\item{data}{Data frame with columns for response and predictor variables.}

\item{first_in_subtraction}{Value of predictor variable
that should be first in order of subtraction for computing
difference in proportions.}

\item{response_value_numerator}{Value of response that corresponds
to "success" when computing proportions.}

\item{as_extreme_as}{Value of observed difference in proportions.}

\item{direction}{Direction of alternative hypothesis.
Allowed values are \code{"greater"}, \code{"less"}, or \code{"two-sided"}.}

\item{number_repetitions}{Number of simulated samples.}

\item{add_normal}{Logical value indicating whether to superimpose a normal
curve on the histogram. Defaults to FALSE.}
}
\value{
Returns plot of distribution of simulated statistics,
with values as or more extreme than specified
value highlighted, and reports
proportion of simulations as or more extreme than specified
as subtitle on plot.
}
\description{
This function will run a simulation-based hypothesis test for a
difference in proportion of successes between two independent groups.
}
\examples{
data(pt)
pt$twoSeconds <- ifelse(pt$responses >= 2, "Yes", "No")
two_proportion_test(twoSeconds ~ brand,
  data = pt,
  first_in_subtraction = "B1",
  response_value_numerator = "Yes",
  as_extreme_as = -.4,
  direction = "two-sided",
  number_repetitions = 1000
)
}
