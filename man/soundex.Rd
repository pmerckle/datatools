% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soundex.R
\name{soundex}
\alias{soundex}
\title{Daitch-Mokotoff Soundex Algorithm}
\usage{
soundex(string, method = "Daitch-Mokotoff")
}
\arguments{
\item{string}{input character vector.}

\item{method}{input character vector.}
}
\value{
A character vector.
}
\description{
This function encodes names using a soundex algorithm.
}
\examples{
soundex("Moskovitz", method = "Daitch-Mokotoff")
soundex("Lubartow", method = "Daitch-Mokotoff")
}
