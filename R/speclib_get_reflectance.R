# if (!isGeneric("get_reflectance")) {
#   setGeneric("get_reflectance", function(spectra, ...)
#   standardGeneric("get_reflectance"))
# }



#' Get reflectance values
#' 
#' Returns weighted or unweighted reflectance values at wavelength position.
#' 
#' 
#' @aliases get_reflectance.speclib get_reflectance,Speclib-method
#' @param spectra Object of class \code{Speclib} or data.frame with reflectance
#' values.
#' @param wavelength Vector with wavelength values.
#' @param position Numeric value passing the position of reflectance values to
#' be returned in dimensions of the wavelength values.
#' @param weighted Logical indicating if reflectance values should be
#' interpolated to fit wavelength position. If \code{FALSE} the reflectance
#' values of nearest neighbour to passed position are returned.
#' @param ...  Arguments to be passed to specific functions. For
#' \code{get_reflectance.default} ignored.
#' @return A vector with reflectance values for each spectrum is returned. If
#' position falls outside of spectral range of input values, \code{NA} values
#' are returned.
#' @author Lukas Lehnert & Hanna Meyer
#' @seealso \code{\link[=spectra.Speclib]{spectra}}
#' @examples
#' 
#' data(spectral_data)
#' 
#' @export get_reflectance
get_reflectance <- function(spectra, wavelength, position, weighted = FALSE, ...)
{
  if (wavelength[1]<=position & wavelength[length(wavelength)]>=position)
  {
    if (weighted)
    {
      if (any(wavelength==position))
      {
        return(get_reflectance(spectra, wavelength, position, weighted = FALSE))
      } else {
        temp <- abs(wavelength-position)
        ord <- order(temp)
        return((spectra[,ord[1]]*1/temp[ord[1]]+spectra[,ord[2]]*1/temp[ord[2]])/
               (1/temp[ord[1]]+1/temp[ord[2]]))
      }
    } else {
      temp <- abs(wavelength-position)
      return(spectra[,which(temp==min(temp))])
    }
  } else {
    return(rep.int(NA,nrow(spectra)))
  }
}

setMethod("get_reflectance", signature(spectra = "Speclib"), 
          function(spectra, position, ...)
{
  wavelength <- if (is.data.frame(spectra@wavelength)) rowMeans(spectra@wavelength) else spectra@wavelength
  spectra <- spectra(spectra)
  return(get_reflectance(spectra, wavelength, position, ...))  
}
)
