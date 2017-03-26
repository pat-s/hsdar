#' Smooth spectra
#' 
#' Smooth spectra using Savitzky-Golay filtering, lowess-, spline-functions or
#' mean filter.
#' 
#' This function allows filtering using four different methods: \itemize{
#' \itemSavitzky-Golay: Smoothing applying Savitzky-Golay-Filter. See
#' [sgolayfilt()] for details.  \itemLowess: Smoothing applying
#' lowess-Filter. See [lowess()] for details.  \itemSpline: Smoothing
#' applying spline-Filter. See [spline()] for details.  \itemMean:
#' Smoothing applying mean-Filter. See [meanfilter()] for details.  }
#' 
#' @param x Object of class \code{\linkS4class{Speclib}}.
#' @param method Character string giving the method to be used. Predefined
#' valid options are "sgolay", "lowess", "spline" and "mean". However, method
#' can also be the (character) name of any other filter function (see
#' examples).
#' @param ...  Further arguments passed to filter functions. See examples.
#' @return Object of class `Speclib`.
#' @author Lukas Lehnert
#' @seealso [sgolayfilt()], [lowess()],
#' [spline()], [meanfilter()]
#' @references Tsai, F. & Philpot, W. (1998): Derivative analysis of
#' hyperspectral data. Remote Sensing of Environment 66/1. 41-51.
#' @keywords smooth
#' @examples
#' 
#' data(spectral_data)
#' 
#' ## Example of predefined filter functions
#' ## Savitzky-Golay
#' sgolay <- smoothSpeclib(spectral_data, method="sgolay", n=25)
#' 
#' ## Spline
#' spline <- smoothSpeclib(spectral_data, method="spline", 
#'                          n=round(nbands(spectral_data)/10,0))
#' 
#' ## Lowess
#' lowess <- smoothSpeclib(spectral_data, method="lowess", f=.01)
#' 
#' ## Mean
#' meanflt <- smoothSpeclib(spectral_data, method="mean", p=5)
#' 
#' par(mfrow=c(2,2))
#' plot(spectral_data, FUN=1, main="Savitzky-Golay")
#' plot(sgolay, FUN=1, new=FALSE, col="red", lty="dotted")
#' plot(spectral_data, FUN=1, main="Spline")
#' plot(spline, FUN=1, new=FALSE, col="red", lty="dotted")
#' plot(spectral_data, FUN=1, main="Lowess")
#' plot(lowess, FUN=1, new=FALSE, col="red", lty="dotted")
#' plot(spectral_data, FUN=1, main="Mean")
#' plot(meanflt, FUN=1, new=FALSE, col="red", lty="dotted")
#' 
#' ## Example of a not predefined filter function (Butterworth filter)
#' bf <- butter(3, 0.1)
#' bf_spec <- smoothSpeclib(spectral_data, method="filter", filt=bf)
#' plot(spectral_data, FUN=1, main="Butterworth filter")
#' plot(bf_spec, FUN=1, new=FALSE, col="red", lty="dotted")
#' 
#' 
#' @export smoothSpeclib
smoothSpeclib <- function(
                           x,
                           method="mean",
                           ...
                          )
{
  if (x@spectra@fromRaster)
    return(.blockwise(speclib_obj =  "x", pos = 1))
  
  predefinedmethod <- FALSE
  if (!is.speclib(x))
    stop("x must be of class 'Speclib'")
    
  if (!x@continuousdata)
    stop("Smoothing is only useful for continuous spectra")
  
  setmask <- FALSE  
  
  if (!is.null(attr(x, "setmask")))
  {
    if (attr(x, "setmask"))
    {
      setmask <- TRUE
      x <- interpolate.mask(x)
    }
  }
  
  res <- x
  
  if (method=="sgolay")
  {
    spectra(res) <- t(apply(spectra(x), 1, FUN = sgolayfilt, ...))
    usagehistory(res) <- paste("Smoothed with Savitzky-Golay smoothing filter")
    predefinedmethod <- TRUE
  }
  
  if (method=="lowess")
  {
    lowessFUN <- function(y, x, ...) lowess(x=x, y=y, ...)$y
    
    wavelength(res) <- lowess(x=wavelength(x), y=spectra(x)[1,], ...)$x
    spectra(res) <- t(apply(spectra(x), 1, FUN = lowessFUN, 
                            x=wavelength(x), ...))
    usagehistory(res) <- paste("Smoothed with lowess function")
    predefinedmethod <- TRUE
  } 
  if (method=="spline")
  {
    splineFUN <- function(y, x, ...) spline(x=x, y=y, ...)$y
    
    wavelength(res) <- spline(x=wavelength(x), y=spectra(x)[1,], ...)$x
    spectra(res) <- t(apply(spectra(x), 1, FUN = splineFUN, 
                            x=wavelength(x), ...))
    usagehistory(res) <- paste("Smoothed with spline function")
    predefinedmethod <- TRUE
  } 
  if (any(method==c("mean","mean_gliding")))
  {
    spectra  <- spectra(res)
    
    spectra(res) <- meanfilter(spectra, ...)
    usagehistory(res) <- paste("Smoothed with meanfilter")
    predefinedmethod <- TRUE
  }
  
  if (!predefinedmethod)
  {
    spectra(res) <- t(apply(spectra(x), 1, FUN = method, ...))
    usagehistory(res) <- paste("Smoothed with ", method," smoothing filter")
  }
  
  if (setmask) mask(res) <- attr(res, "dropped")
  return(res)            
}



#' Apply mean filter
#' 
#' Apply mean filter to data frame with spectra as rows and bands as columns.
#' Filter size is passed as number of bands averaged at both sites of the
#' respective band value.
#' 
#' 
#' @param spectra Data frame containing spectra
#' @param p Filter size.
#' @return Filtered data frame of same dimension as input data frame
#' @author Lukas Lehnert
#' @seealso [smoothSpeclib()]
#' @keywords smooth
#' @examples
#' 
#' data(spectral_data)
#' 
#' spectra_filtered <- meanfilter(spectra(spectral_data), p = 10)
#' spectra(spectral_data) <- spectra_filtered 
#' 
#'   
#' 
#' @export meanfilter
meanfilter <- function(spectra, p=5)
{
  gliding  <- FALSE
  spectra  <- as.matrix(spectra)
  nwl      <- ncol(spectra)
  n        <- nrow(spectra)
  smoothed <- spectra*0
  
  storage.mode(nwl)      <- "integer"
  storage.mode(n)        <- "integer"
  storage.mode(p)        <- "integer"
  storage.mode(spectra)  <- "double"
  storage.mode(smoothed) <- "double"  

  if (!gliding)
  {
    external <- .Fortran("meanfilter",
                        nwl=nwl,
                        n=n,
                        p=p,
                        y=spectra,
                        smoothed=smoothed,
                        PACKAGE="hsdar"
                        )
  } else {
    external <- .Fortran("gliding_meanfilter",
                        nwl=nwl,
                        n=n,
                        p=p,
                        y=spectra,
                        smoothed=smoothed,
                        PACKAGE="hsdar"
                        )
  }
  
  external$smoothed <- as.data.frame(external$smoothed)
  
  return(external$smoothed)
}
