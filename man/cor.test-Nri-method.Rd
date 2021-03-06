% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nri_cor_test.R
\docType{methods}
\name{cor.test,Nri-method}
\alias{cor.test,Nri-method}
\alias{cor.test.nri}
\title{Test for association/correlation between nri values and vector of samples}
\usage{
\S4method{cor.test}{Nri}(x, y, ...)
}
\arguments{
\item{x}{Object of class \code{Nri} or numerical vector}

\item{y}{Object of class \code{Nri} or numerical vector}

\item{...}{Further arguments passed to \code{\link[=cor.test]{cor.test()}}}
}
\value{
Object of class \code{\linkS4class{Nri}}
}
\description{
Test for association between paired samples (with one variable being
nri-values), using one of Pearson's product moment correlation coefficient,
Kendall's tau or Spearman's rho.
}
\details{
NRI-values may be used as x and/or as y variable. If x and y are NRI-values
the number of samples in both datasets must be equal. For additional
information on correlation tests see details in \code{\link[=cor.test]{cor.test()}}.
}
\examples{

data(spectral_data)

## Calculate all possible combinations for WorldView-2-8
spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
                              response_function = FALSE)
nri_WV <- nri(spec_WV, recursive = TRUE)

cortestnri <- cor.test(nri_WV, attribute(spec_WV)$chlorophyll)

cortestnri

}
\seealso{
\code{\link[=plot.Nri:plot]{=plot.Nri::plot()}}, \code{\link[=cor.test]{cor.test()}},
\code{\link[=glm.nri]{glm.nri()}}, \code{\link[=lm.nri]{lm.nri()}}, \code{\link[=getNRI]{getNRI()}}
}
\author{
Lukas Lehnert
}
