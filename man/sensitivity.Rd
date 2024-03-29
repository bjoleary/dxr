% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percent_agreement.R
\name{sensitivity}
\alias{sensitivity}
\title{Calculate an agreement proportion for Sensitivity}
\usage{
sensitivity(
  true_positives,
  false_negatives,
  digits = 0.1,
  interval = 0.95,
  ...
)
}
\arguments{
\item{true_positives}{The number of correct calls.}

\item{false_negatives}{The number of incorrect calls.}

\item{digits}{The number of digits to round to. Defaults to \code{0.1}.}

\item{interval}{The confidence interval to calculate. Defaults to
\code{0.95} for 95\%.}

\item{...}{Parameters passed to \code{agreement()}.}
}
\value{
A formatted agreement proportion.
}
\description{
A wrapper around \code{agreement} that is tailored to PPA or sensitivity
calculations
}
\examples{
sensitivity(100, 0)
sensitivity(100, 0, 0.01, 0.99)
}
