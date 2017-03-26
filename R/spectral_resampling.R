#' Spectral resampling
#' 
#' Resample spectra to (satellite) sensors
#' 
#' The characteristics of (satellite) sensor to integrate spectra can be chosen
#' from a list of already implemented sensors. See
#' \code{\link{get.sensor.characteristics}} for available sensors.
#' 
#' Otherwise the characteristics can be passed as a \code{data.frame} with two
#' columns: first column with lower bounds of channels and second column with
#' upper bounds. Alternatively, the \code{data.frame} may encompass band centre
#' wavelength and full-width-half-maximum values of the sensor. Function will
#' check the kind of data passed by partially matching the names of the data
#' frame: If any column is named \code{"fwhm"} or \code{"center"}, it is
#' assumed that data are band centre and full-width-half-maximum values.
#' 
#' If sensor characteristics are defined manually, a Gaussian response is
#' always assumed.
#' 
#' @aliases spectralResampling spectral.resampling
#' @param x Object of class \code{Speclib}. Data to be spectrally resampled.
#' @param sensor Character or \code{data.frame} containing definition of sensor
#' characteristics. See details section for further information.
#' @param rm.NA If \code{TRUE}, channels which are not covered by input data
#' wavelength are removed
#' @param continuousdata Definition if returned \code{\linkS4class{Speclib}} is
#' containing continuous data or not.
#' @param response_function If \code{TRUE}, the spectral response function of
#' the sensor is used for integration, if \code{FALSE} a Gaussian distribution
#' is assumed and if \code{NA} the mean value of
#' \code{spectra[min(ch):max(ch)]} is calculated.
#' @return Object of class \code{Speclib}
#' @author Lukas Lehnert
#' @seealso \code{\link{get.sensor.characteristics}},
#' \code{\link{get.gaussian.response}}%%, \code{\link{speclib}},
#' @keywords multivariate
#' @examples
#' 
#' % \dontrun{
#' ## Load example data  
#' data(spectral_data)
#' 
#' ## Resample to RapidEye
#' data_RE <- spectralResampling(spectral_data, "RapidEye", 
#'                               response_function = TRUE)
#' 
#' ## Plot resampled spectra
#' plot(data_RE)
#' 
#' ## Compare different methods of spectral resampling
#' par(mfrow=c(1,3))
#' ga <- spectralResampling(spectral_data, "RapidEye", 
#'                          response_function = FALSE)
#' plot(ga)
#' re <- spectralResampling(spectral_data, "RapidEye", 
#'                          response_function = TRUE)
#' plot(re)
#' no <- spectralResampling(spectral_data, "RapidEye", 
#'                          response_function = NA)
#' plot(no)
#' % }
#' 
#' @export spectralResampling
spectralResampling <- function (
                                x,
                                sensor,
                                rm.NA=TRUE,
                                continuousdata="auto",
                                response_function=TRUE
                               )
{
no_data <- -9999.999  
if (x@spectra@fromRaster)
  return(.blockwise(speclib_obj =  "x", pos = 1))
  
if (is.na(response_function))
{
  spectral_response_function <- FALSE
  response_function <- FALSE
} else {
  spectral_response_function <- TRUE
}
if (continuousdata!="auto")
{
  if (mode(continuousdata)!="logical")
    stop("continuousdata must be 'auto', TRUE or FALSE")
}
if (!is.speclib(x))
  stop("x must be of class 'Speclib'")

result <- x
if (!is.null(attr(result, "setmask")))
{
  attr(result, "setmask") <- FALSE
  attr(result, "dropped") <- NULL
}
wavelength <- x$wavelength
x <- spectra(x)

response <- get.response(sensor, range=wavelength, response_function=response_function, 
                         continuousdata = continuousdata)
nch <- dim(response)[1]
lb <- attr(response, "lb")
ub <- attr(response, "ub")

if (is.data.frame(sensor))
{
  if (continuousdata=="auto") 
    continuousdata <- FALSE
} else {
  if (!any(get.sensor.name(sensor)==c("Hyperion", "EnMAP")))
  {
    if (continuousdata=="auto") 
      continuousdata <- FALSE      
  } else {
    if (continuousdata=="auto") 
      continuousdata <- TRUE
  }
  if (response_function)
  {
    if (attr(result,"wlunit")!=attr(response,"wlunit"))
      stop(paste("Wavelength must be in [",attr(response,"wlunits"),"]",sep=""))
  }
}
spectra <- matrix(data=0,nrow=nrow(x),ncol=nch)
rm_vec <- vector(mode="numeric")
if (spectral_response_function)
{
  responsedim <- c(as.double(attr(response, "minwl")),
                    as.double(attr(response, "maxwl")),
                    as.double(attr(response, "stepsize")))
  cha_names <- idSpeclib(response)

  x <- as.matrix(x)
  
  x[!is.finite(x)] <- no_data

  response_transformed <- as.double(t(as.matrix(spectra(response))))

  integrated <- .Fortran("apply_response",
                          nwl=as.integer(length(wavelength)),
                          nspec=as.integer(nrow(x)),
                          nband=as.integer(nch),
#                           wl=as.double(wavelength),
                          spec=as.double(x),
#                           responsedim=responsedim,
                          response=response_transformed,
                          integrated=as.double(spectra),
                          no_data = as.double(no_data),
                          package="hsdar"
                          )
  integrated$integrated[abs(integrated$integrated - no_data) < 1.0e-6] <- NA                        
  spectra <- matrix(data=integrated$integrated,ncol=nch)
  bandnames(result) <- cha_names
} else {
  for (i in 1:nch)
  {
    tmp <- x[,wavelength>=lb[i] & wavelength <= ub[i]]
    if (ncol(tmp)>0)
    {
      spectra[,i] <- apply(tmp,1,mean)
    } else {
      if (!rm.NA)
      {
        spectra[,i] <- apply(tmp,1,mean)
      } else {
        rm_vec <- c(rm_vec,i*(-1))
      }
    }
  }
  if (rm.NA & length(rm_vec) > 0)
    spectra <- spectra[,rm_vec]
}
spectra(result) <- spectra
result@wavelength <- rowMeans(data.frame(lb=lb,ub=ub))
result@fwhm <- (result@wavelength - lb) * 2
if (rm.NA & length(rm_vec) > 0)
  result@wavelength <- result@wavelength[rm_vec]
result@wavelength.is.range <- TRUE
if (any(names(result)=="unmask")) result[names(result)=="unmask"] <- NULL
if (is.data.frame(sensor))
{
  sensor <- "user defined"
} else {
  if (is.numeric(sensor)) sensor <- get.sensor.name(sensor)
}
usagehistory(result) <- paste("Integrated spectra to",sensor,"channels")

attr(result,"continuousdata") <- continuousdata
return(result)
}

spectral.resampling <- spectralResampling
