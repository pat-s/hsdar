#' Derivation
#' 
#' Calculate derivations of spectra
#' 
#' Two different methods are available: \itemize{ \item Finite approximation
#' (`finApprox`):
#' \deqn{\frac{dr}{d\lambda}=\frac{r(\lambda_i)-r(\lambda_{i+1})}{\Delta\lambda},}
#' where \eqn{r_i} is the reflection in band \eqn{i} and \eqn{\Delta\lambda}
#' the spectral difference between adjacent bands.  \item Savitzky-Golay
#' derivative computation (`sgolay`)%:
#' 
#' % \deqn{\frac{dr^q}{d\lambda}=\sum_{i=-m}^{m}P_i^{(q)}r_{j+i},}
#' 
#' }
#' 
#' @param x Object of class `Speclib`.
#' @param m Return the m-th derivative of the spectra.
#' @param method Character string giving the method to be used. Valid options
#' are `"finApprox"` or `"sgolay"`.
#' @param ...  Further arguments passed to [sgolayfilt()].
#' @return Object of class \code{\linkS4class{Speclib}}.
#' @author Lukas Lehnert
#' @seealso [sgolayfilt()], [vegindex()]
#' @references Tsai, F. & Philpot, W. (1998): Derivative analysis of
#' hyperspectral data. Remote Sensing of Environment 66/1. 41-51.
#' @keywords utilities
#' @examples
#' 
#' data(spectral_data)
#' 
#' ## Calculate 1st derivation
#' d1 <- derivative.speclib(spectral_data)
#' 
#' ## Calculate 2nd derivation
#' d2 <- derivative.speclib(spectral_data, m = 2)
#' 
#' ## Calculate 3rd derivation
#' d3 <- derivative.speclib(spectral_data, m = 3)
#' 
#' par(mfrow=c(2,2))
#' plot(spectral_data)
#' plot(d1)
#' plot(d2)
#' plot(d3)
#' 
#' @export derivative.speclib
derivative.speclib <- function(
                                  x,
                                  m=1,
                                  method="sgolay",
                                  ...
                                 )
{
  if (!is.speclib(x))
    stop("x must be of class 'Speclib'")
    
  if (x@spectra@fromRaster)
    return(.blockwise(speclib_obj =  "x", pos = 1))
    
  res <- x
  mf <- FALSE  
  if (method=="finApprox")
  {
    mf <- TRUE
    spectra(res) <- t(apply(spectra(x), 1, FUN = function(single_spectrum, wl, nwl)
      {
        return(c((single_spectrum[-1] - single_spectrum[-nwl]) / (wl[-1] - wl[-nwl]), 0))
      }, wavelength(res), nbands(res)))
    
#     spectra <- as.matrix(spectra(res))
#     nwl     <- ncol(spectra)
#     n       <- nrow(spectra)
#     bandc   <- res$wavelength
#     deriv   <- spectra*0
#     
#     storage.mode(nwl)     <- "integer"
#     storage.mode(n)       <- "integer"
#     storage.mode(m)       <- "integer"
#     storage.mode(bandc)   <- "double"
#     storage.mode(deriv)   <- "double"
#     storage.mode(spectra) <- "double"
# 
#     external <- .Fortran("differenciate",
#                         nwl=nwl,
#                         n=n,
#                         m=m,
#                         y=spectra,
#                         bandcenter=bandc,
#                         derivation=deriv,
#                         PACKAGE="hsdar"
#                         )
#     external$derivation <- as.data.frame(external$derivation)
#     
#     spectra(res) <- external$derivation
    usagehistory(res) <- paste(m,". derivation using finite approximation",sep="")
  }
  if (method=="sgolay")
  {
    mf <- TRUE
    spectra(res) <- t(apply(spectra(x), 1, FUN = sgolayfilt, m = m,...))
    usagehistory(res) <- paste(m,". derivation using Savitzky-Golay filter",sep="")
  }
  if (!mf) stop("Specified method not found")
  
  
  return(res) 
}
