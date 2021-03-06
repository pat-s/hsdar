% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nri_t_test.R
\docType{methods}
\name{t.test,Nri-method}
\alias{t.test,Nri-method}
\alias{t.test.nri}
\title{t-test for nri values}
\usage{
\S4method{t.test}{Nri}(x, ...)
}
\arguments{
\item{x}{Object of class \code{'nri'}.}

\item{...}{Arguments to be passed to \code{\link[=t.test]{t.test()}}.}
}
\value{
An object of class "data.frame"
}
\description{
Performs t-tests for nri values.
}
\examples{

\%   \dontrun{
data(spectral_data)


## Calculate nri-values for WorldView-2-8
spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
                              response_function = FALSE)
nri_WV <- nri(spec_WV, recursive = TRUE)

## Perform t.tests between nri-values of both sites 
season <- spec_WV$attributes$season
ttestres <- t.test(x = nri_WV, y = season, alternative = "two.sided")
ttestres

## Plot p.values of t.tests
plot(ttestres)
\%   }

}
\seealso{
\code{\link[=t.test]{t.test()}}, \code{\link[=cor.test,Nri-method]{cor.test,Nri-method()}}
}
\author{
Lukas Lehnert & Hanna Meyer
}
