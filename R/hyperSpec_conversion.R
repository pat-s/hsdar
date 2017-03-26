#' hyperSpec
#' 
#' Conversion from `Speclib`- to `hyperSpec`-object
#' 
#' 
#' @param object Object of class `Speclib`.
#' @return Object of class `hyperSpec`.
#' @note Package hyperSpec must be installed.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}
#' @keywords utilities
#' @export as.hyperSpec
as.hyperSpec <- function(object)
{
  if (!requireNamespace("hyperSpec", quietly = TRUE))
    stop("Library 'hyperSpec' is required to convert object to hyperSpec-class")

  spc <- new("hyperSpec", spc = spectra(object), wavelength = wavelength(object), data = attribute(object))
  
  return(spc)
}
