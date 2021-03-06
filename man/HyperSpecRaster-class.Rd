% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hsdar-package.R
\docType{class}
\name{HyperSpecRaster-class}
\alias{HyperSpecRaster-class}
\alias{show,HyperSpecRaster-method}
\title{HyperSpecRaster* class}
\description{
Extension of *RasterBrick-class to handle hyperspectral data
}
\details{
Extension of *RasterBrick-class with three additional slots: \describe{
\item{list("wavelength")}{ A numeric vector giving the center wavelength for
each band.}\item{:}{ A numeric vector giving the center wavelength for each
band.} \item{list("fwhm")}{ A numeric vector giving the full-width-half-max
values for each band.}\item{ (optional):}{ A numeric vector giving the
full-width-half-max values for each band.} \item{list("attributes")}{ A
\code{data.frame} containing additional information for each pixel.}\item{
(optional):}{ A \code{data.frame} containing additional information for each
pixel.} } The information in the three slots are used for the convertion to
\code{\linkS4class{Speclib}}.
}
\seealso{
\code{\link[raster:brick]{raster::brick()}}, \code{\linkS4class{Speclib}}
}
\author{
Lukas Lehnert
}
\keyword{classes}
\keyword{spatial}
