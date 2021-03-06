% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nri_multivariate.R
\name{getNRI}
\alias{getNRI}
\alias{print.getNRI}
\title{Return nri-values}
\usage{
getNRI(nri, wavelength)
}
\arguments{
\item{nri}{Object of class 'Nri'}

\item{wavelength}{Wavelength values where nri is returned. See details
section.}
}
\description{
Return normalized ratio index values giving the wavelength
}
\details{
Wavelength can be passed in three ways. As the result of
\code{\link[=nri_best_performance]{nri_best_performance()}}, as a data frame with two columns or as a
vector of length 2. In the first two cases, the result will be a data frame
(if data frames contain more than one row) with the nri-values of each pair
of wavelengths. In the latter case it will be a vector.
}
\examples{

data(spectral_data)

## Calculate all possible combinations for WorldView-2-8
spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
                              response_function = FALSE)
nri_WV <- nri(spec_WV, recursive = TRUE)

## Build glm-models
glmnri <- glm.nri(nri_WV ~ chlorophyll, preddata = spec_WV)

## Return best 5 models
BM <- nri_best_performance(glmnri, n = 5, coefficient = "p.value")

## Get nri values for the 5 models
nri_BM <- getNRI(nri_WV, BM)


}
\seealso{
\code{\link[=nri]{nri()}}, \code{\linkS4class{Nri}}
}
\author{
Lukas Lehnert
}
\keyword{multivariate}
