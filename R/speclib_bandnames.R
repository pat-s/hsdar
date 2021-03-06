#' Handling names of bands
#' 
#' Returning and setting names of bands in `Speclib`
#' 
#' 
#' @aliases bandnames bandnames<-
#' @param x Object of class `Speclib`.
#' @param value Character vector of the same length as `nbands(x)`, or
#' NULL.
#' @return For `bandnames<-`, the updated object. Otherwise a vector
#' giving the name of each band in `Speclib` is returned.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}
#' @keywords utilities
#' @examples
#' 
#' data(spectral_data)
#' 
#' bandnames(spectral_data)
#' 
#' @export bandnames
bandnames <- function(x)
{
  if (is.null(attr(x, "bandnames")))
  {
    return(paste("B", wavelength(x), sep = "_"))
  } else {
    return(attr(x, "bandnames"))
  }
}


"bandnames<-" <- function(x, value)
{
  xx <- x
  attr(xx, "bandnames") <- value
  x <- xx
}
