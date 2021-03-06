% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_functions.R
\name{gender}
\alias{gender}
\title{Gender classifier for danish names}
\source{
https://ast.dk/born-familie/navne/navnelister/godkendte-fornavne
}
\usage{
gender(names)
}
\arguments{
\item{names}{A vector of names - either one or several}
}
\value{
list of gender classifications
}
\description{
The function uses the official danish lists of male, female and unisex first names
to classify names as either male, female or unisex. If a name can not be determined
as either one of those three it will return as NA.
}
\details{
The functions is vectorized and can accept several names at ones. It accepts both first
names and full names. In case of full names it splits on space and choses the first name.
}
\examples{
library(namesDK)

gender("Lars Løkke Rasmussen")

gender(c("Helle Thorning Smidt", "Lars Løkke Rasmussen", "Traktor Troels"))

}

