#' Gaussian response function
#' 
#' Simulate Gaussian response function for satellite sensor
#' 
#' The characteristics of the sensor must be passed as a `data.frame` with
#' three columns: first column is used as name for bands, second with lower
#' bounds of channels and third column with upper bounds. Alternatively, the
#' `data.frame` may encompass band centre wavelength and
#' full-width-half-maximum values of the sensor. Function will check the kind
#' of data passed by partially matching the names of the data frame: If any
#' column is named `"fwhm"` or `"center"`, it is assumed that data
#' are band centre and full-width-half-maximum values.
#' 
#' @param fwhm Object of class `data.frame` with three columns. See
#' details and examples sections.
#' @return Data frame with response values for all bands covering the entire
#' spectral range of satellite sensor.
#' @author Lukas Lehnert
#' @seealso [get.sensor.characteristics()],
#' [get.gaussian.response()]
#' @keywords utilities
#' @examples
#' 
#' par(mfrow=c(1,2))
#' ## Plot response function of RapidEye
#' plot(c(0,1)~c(330,1200), type = "n", xlab = "Wavelength [nm]", 
#'      ylab = "Spectral response")
#' data_RE <- get.gaussian.response(get.sensor.characteristics("RapidEye"))
#' xwl_response <- seq.int(attr(data_RE, "minwl"),
#'                         attr(data_RE, "maxwl"),
#'                         attr(data_RE, "stepsize"))
#' for (i in 1:ncol(data_RE))
#'   lines(xwl_response, data_RE[,i], col = i)
#'   
#' ## Plot original response function
#' data_RE <- get.sensor.characteristics("RapidEye", TRUE)
#' 
#' plot(c(0,1)~c(330,1200), type = "n", xlab = "Wavelength [nm]", 
#'      ylab = "Spectral response")
#' xwl_response <- seq.int(attr(data_RE$response, "minwl"),
#'                         attr(data_RE$response, "maxwl"),
#'                         attr(data_RE$response, "stepsize"))
#' for (i in 1:nrow(data_RE$characteristics))
#'   lines(xwl_response, data_RE$response[,i], col = i)
#'   
#' ## Simulate gaussian response for arbitrary sensor with 3 bands
#' sensor <- data.frame(Name = paste("Band_", c(1:3), sep = ""),
#'                      center = c(450, 570, 680),
#'                      fwhm = c(30, 40, 30))
#' 
#' ## Plot response function
#' par(mfrow=c(1,1))
#' plot(c(0,1)~c(330,800), type = "n", xlab = "Wavelength [nm]", 
#'      ylab = "Spectral response")
#' data_as <- get.gaussian.response(sensor)
#' xwl_response <- seq.int(attr(data_as, "minwl"),
#'                         attr(data_as, "maxwl"),
#'                         attr(data_as, "stepsize"))
#' for (i in 1:3)
#'   lines(xwl_response, data_as[,i], col = i)
#' 
#' @export get.gaussian.response
get.gaussian.response <- function(fwhm)
{
  if (is.null(attr(fwhm, "fwhm")))
  {
    if (any(toupper(names(fwhm))=="FWHM"))
    {
      fwhm_vec <- fwhm[, which(toupper(names(fwhm))=="FWHM")]
    } else {
      fwhm_vec <- if (pmatch("FWHM", toupper(names(fwhm)))==0) NULL else fwhm[, pmatch("FWHM", toupper(names(fwhm)))]
    }
    if (any(toupper(names(fwhm))=="CENTER"))
    {
      centerwl <- fwhm[, which(toupper(names(fwhm))=="CENTER")]
    } else {
      centerwl <- if (pmatch("CENTER", toupper(names(fwhm)))==0) NULL else fwhm[, pmatch("CENTER", toupper(names(fwhm)))]
    }
    if (any(c(is.null(fwhm_vec), is.null(centerwl))))
    {
      lb<-fwhm[,2]
      ub<-fwhm[,3]
      centerwl <- lb + (ub - lb)/2
      fwhm_vec <- (centerwl - lb) * 2
    }
    fwhm <- data.frame(channel=c(1:length(centerwl)),center=centerwl,fwhm=fwhm_vec)
  } else {
    if (!attr(fwhm, "fwhm"))
    {
      lb <- fwhm[,attr(fwhm, "50pass")[1]]
      ub <- fwhm[,attr(fwhm, "50pass")[2]]
      centerwl <- lb + (ub - lb)/2
      fwhm_vec <- (centerwl - lb) * 2
      fwhm <- data.frame(No=c(1:length(centerwl)), center=centerwl, fwhm=fwhm_vec)
    }
  }
  
  lb <- fwhm[,2]-fwhm[,3]/2
  ub <- fwhm[,2]+fwhm[,3]/2
  nch <- nrow(fwhm)
  responsedim <- c(min(lb)-(ub[which(lb==min(lb))]-lb[which(lb==min(lb))]),
                   max(ub)+(ub[which(ub==max(ub))]-lb[which(ub==max(ub))]),
                   1)
  response <- matrix(data = 0, ncol = nch, nrow =responsedim[2]-responsedim[1]+1)
  range_wl <- seq.int(responsedim[1],responsedim[2],responsedim[3])
  for (i in 1:ncol(response))
  {
    gauss <- dnorm(range_wl, mean = mean(c(lb[i],ub[i])), sd = (ub[i]-lb[i])/2)
    gauss <- (gauss-min(gauss))/(max(gauss)-min(gauss))
    response[,i] <- gauss
  }
  response <- as.data.frame(response)
  names(response) <- paste("Band",c(1:nch),sep="_")
  attr(response,"minwl") <- responsedim[1]
  attr(response,"maxwl") <- responsedim[2]
  attr(response,"stepsize") <- responsedim[3]
  return(response)
}
