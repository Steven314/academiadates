% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seasonal.R
\name{seasonal_trimester}
\alias{seasonal_trimester}
\title{Convert a Seasonal Name to year-trimester}
\usage{
seasonal_trimester(x = character(), start_term = c("Summer", "Fall", "Spring"))
}
\arguments{
\item{x}{A character vector of terms.}

\item{start_term}{The name of the term which starts the academic year. Must
be \code{"Fall"}, \code{"Spring"}, or \code{"Summer"} (the default).}
}
\value{
A \code{yeartrimester} object.
}
\description{
It is common to refer to an academic term by its associated season: fall,
spring, or summer. This function is built to take out the step of converting
the term name to a date and then to a \code{yeartrimester} object.
}
\examples{
seasonal_trimester("Fall 2025")

seasonal_trimester("Fall 2025", start_term = "Fall")

}
