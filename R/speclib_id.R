#' Handling IDs of spectra
#' 
#' Returning and setting ID of spectra in Speclib
#' 
#' 
#' @aliases idSpeclib idSpeclib<-
#' @param x Object of class `Speclib`.
#' @param value Character vector of the same length as `nspectra(x)`, or
#' NULL.
#' @return For `idSpeclib<-`, the updated object. Otherwise a vector
#' giving the ID of each spectrum in Speclib is returned.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}
#' @keywords utilities
#' @examples
#' 
#' data(spectral_data)
#' 
#' idSpeclib(spectral_data)
#' 
#' @export idSpeclib
idSpeclib <- function(x)
{
if (!is.speclib(x))
  stop("Class of x must be Speclib")
ids <- x@ID

return(if (any(c(length(x@ID) != nspectra(x), anyDuplicated(ids)))) c(1:nspectra(x)) else ids)
}

"idSpeclib<-" <- function(x, value)
{
  xx <- x
  xx@ID <- value
  x <- xx
}
