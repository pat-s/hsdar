setReplaceMethod("mask", signature(object = "Speclib", value = "numeric"), 
                 function(object, value)
{
  if (length(value)/2 != round(length(value)/2,0))
    stop("In case of mask being a vector its length must be even")
  
  lb <- value[seq(1,length(value)-1,2)]
  ub <- value[seq(2,length(value),2)]
  return(maskSpeclib(object, lb, ub))
}
)

setReplaceMethod("mask", signature(object = "Speclib", value = "list"), 
                 definition = function(object, value)
{
  if (length(value)!=2)
    stop("In case of mask being a list it must contain exactly two entries")
  lb <- as.vector(value[[1]])
  ub <- as.vector(value[[2]])
  if (length(lb)!=length(ub))
    stop("Number of lower boundaries differs from number of upper boundaries") 
  return(maskSpeclib(object, lb, ub))
}
)

setReplaceMethod("mask", signature(object = "Speclib", value = "data.frame"), 
                 definition = function(object, value)
{
  if (ncol(value)!=2)
    stop("In case of mask being a data.frame it must contain exactly two columns")
  lb <- value[,1]
  ub <- value[,2]
  return(maskSpeclib(object, lb, ub))
}
)

setReplaceMethod("mask", signature(object = "Speclib", value = "matrix"), 
                 definition = function(object, value)
{
  if (ncol(value)!=2)
    stop("In case of mask being a data.frame it must contain exactly two columns")
  lb <- value[,1]
  ub <- value[,2]
  return(maskSpeclib(object, lb, ub))
}
)

maskSpeclib <- function(object, lb, ub)
{
  if (object@spectra@fromRaster)
    return(.blockwise(speclib_obj =  "object", pos = 1))
    
  range_of_wl <- if (length(object@fwhm)==1) rep.int(object@fwhm, nbands(object)) else object@fwhm
  ## Check mask boudaries
  if (any((ub-lb)<=0))
    stop("Inconsistency found in mask boundaries")
    
  restorable <- ub*0+1
  if (any(ub < (wavelength(object)[1]-range_of_wl[1])) |
      any(lb > (wavelength(object)[length(wavelength(object))]+range_of_wl[length(wavelength(object))])))
    warning("Mask exeeds spectral range of object")
    
  try(restorable[which(lb < (wavelength(object)[1]-range_of_wl[1]))] <- 0, silent = TRUE)
  try(restorable[which(ub > (wavelength(object)[length(wavelength(object))]+range_of_wl[length(wavelength(object))]))] <- 0, silent = TRUE)

  ## Apply mask
  for (i in 1:length(lb))
  {
    rm_vector <- !(lb[i]<wavelength(object) & ub[i]>wavelength(object))
    wavelength(object) <- wavelength(object)[rm_vector]
    if (!is.null(attr(object, "bandnames")))
      bandnames(object) <- bandnames(object)[rm_vector]
    if (length(fwhm(object)) > 1)
      fwhm(object) <- fwhm(object)[rm_vector]
    spectra(object) <- if (nspectra(object) == 1) matrix(data = spectra(object)[,rm_vector], nrow = 1) else spectra(object)[,rm_vector]  
  }
  attr(object, "setmask") <- TRUE
  attr(object, "dropped") <- data.frame(lb=lb, ub=ub)
  attr(object, "restorable") <- restorable
  usagehistory(object) <- "Apply mask to spectra"
  return(object)
}



#' Mask spectra
#' 
#' Returning and setting mask of spectra in Speclib. `interpolate.mask`
#' linearly interpolates masked parts in spectra.
#' 
#' Value may be an object of class vector, data frame or list. Data frames must
#' contain 2 columns with the first column giving the lower and the second the
#' upper boundary values of the mask. List must have two items consisting of
#' vectors of length = 2. The first entry is used as lower and the second as
#' upper boundary values. Vectors must contain corresponding lower and upper
#' boundary values consecutively.
#' 
#' Interpolation of masked parts is mainly intended for internal use.
#' Interpolation is only possible if mask does not exceed spectral range of
#' Speclib.
#' 
#' @aliases mask mask<- maskSpeclib mask,Speclib-method
#' mask<-,Speclib,data.frame-method mask<-,Speclib,list-method
#' mask<-,Speclib,numeric-method mask<-,Speclib,matrix-method interpolate.mask
#' @param object Object of class `Speclib`.
#' @param value Numeric vector, data frame or list giving the mask boundaries
#' in wavelength units. See details section.
#' @return For `mask<-`, the updated object. Otherwise a data frame giving
#' the mask boundaries.
#' 
#' `interpolate.mask` returns a new object of class Speclib.
#' @author Lukas Lehnert and Hanna Meyer
#' @seealso \code{\linkS4class{Speclib}}
#' @keywords utilities
#' @examples
#' 
#' data(spectral_data)
#' 
#' mask(spectral_data) ## NULL
#' 
#' 
#' ## Mask from vector
#' spectral_data_ve <- spectral_data
#' mask(spectral_data_ve) <- c(1040,1060,1300,1450)
#' mask(spectral_data_ve)
#' 
#' 
#' ## Mask from data frame
#' spectral_data_df <- spectral_data
#' mask(spectral_data_df) <- data.frame(lb=c(1040,1300),ub=c(1060,1450))
#' mask(spectral_data_df)
#' 
#' 
#' ## Mask from list
#' spectral_data_li <- spectral_data
#' mask(spectral_data_li) <- list(lb=c(1040,1300),ub=c(1060,1450))
#' mask(spectral_data_li)
#' 
#' ## Linear interpolation
#' plot(spectral_data)
#' plot(interpolate.mask(spectral_data_li), new=FALSE)
#' 
#' @export mask
mask <- function(object)
  return(attr(object, "dropped"))
  
  
interpolate.mask <- function(object)
{
  interpolate_FUN <- function(spec, wavelength, masked)
  {
    includevec_x = NULL
    includevec_y = NULL
    xpos1 = NULL
    xpos2 = NULL
    for (i in 1:nrow(masked))
    {
      xpos1[[i]] <- which(abs(masked[i,1]-wavelength)==min(abs(masked[i,1]-wavelength)))
      xpos2[[i]] <- xpos1[[i]] + 1
      x1 <- wavelength[xpos1[[i]]]
      x2 <- wavelength[xpos2[[i]]]
      y1 <- spec[xpos1[[i]]]
      y2 <- spec[xpos2[[i]]]
      m <- (y1 - y2)/(x1 - x2)
      t <- y1 - m*x1
      
      includevec_x[[i]] <- c((x1+1):(x2-1))
#       includevec_x[[i]] <- wavelength[xpos1[[i]]:xpos2[[i]]]
      
      includevec_y[[i]] <- includevec_x[[i]]*m+t
    }
      
    i <- 1
    spec2 <- c(spec[c(1:xpos1[[i]])], includevec_y[[i]])
    if (nrow(masked) > 1)
    {
      for (i in 2:nrow(masked))
        spec2 <- c(spec2, spec[c(xpos2[[i-1]]:xpos1[[i]])], includevec_y[[i]])
    }
    if (masked[i,2]<wavelength[length(wavelength)])
      spec2 <- c(spec2, spec[c(xpos2[[i]]:length(spec))])
    return(spec2)
  }
  x <- object
  mask_frame <- attr(x, "dropped")
  if (is.null(mask_frame)) 
    return(x) 
  
  mask_frame <- mask_frame[attr(x, "restorable") == 1,]
  
  mask_frame <- as.matrix(mask_frame)
  
  if (nrow(mask_frame) == 0)
    return(x)
  
  includevec_x = NULL
  includevec_y = NULL
  xpos1 = NULL
  xpos2 = NULL
  interpolated <- vector(mode="numeric", length=0)
  for (i in 1:nrow(mask_frame))
  {
    xpos1[[i]] <- which(abs(mask_frame[i,1]-wavelength(x))==min(abs(mask_frame[i,1]-wavelength(x))))
    xpos2[[i]] <- xpos1[[i]] + 1
    x2 <- wavelength(x)[xpos2[[i]]]
    
    if (!is.na(x2))
      interpolated <- c(interpolated, i)
  }
  mask_frame <- matrix(mask_frame[interpolated,], ncol = 2)
  
  spectra(x) <- t(apply(spectra(x), 1, interpolate_FUN, 
                        wavelength=wavelength(x),
                        masked=mask_frame))
  wavelength <- c(wavelength(x)[c(1:which(abs(mask_frame[1,1]-wavelength(x))==
                                         min(abs(mask_frame[1,1]-wavelength(x)))))],
                  c((mask_frame[1,1]+1):(mask_frame[1,2]-1)))
  i <- 1
  if (nrow(mask_frame) > 1)
  {
    for (i in 2:nrow(mask_frame))
      wavelength <- c(wavelength, wavelength(x)[c(which(abs(mask_frame[i-1,2]-wavelength(x))==
                                                min(abs(mask_frame[i-1,2]-wavelength(x)))):
                                                which(abs(mask_frame[i,1]-wavelength(x))==
                                                min(abs(mask_frame[i,1]-wavelength(x)))))],
                      c((mask_frame[i,1]+1):(mask_frame[i,2]-1)))
  }
  if (wavelength[length(wavelength)]<wavelength(x)[length(wavelength(x))])
    wavelength <- c(wavelength, wavelength(x)[c(which(abs(mask_frame[i,2]-wavelength(x))==
                                               min(abs(mask_frame[i,2]-wavelength(x)))):
                                               length(wavelength(x)))])
  wavelength(x) <- wavelength
  attr(x, "setmask") <- FALSE
  attr(x, "dropped") <- mask_frame
  if (!x@wavelength.is.range)
  {
    range <- wavelength[-1] - wavelength[-1*length(wavelength)]
    range <- c(as.numeric(range),range[length(range)])
    if (sd(range)==0) 
      range <- mean(range)
    x@fwhm <- range
  }
  return(x)
}
  
.isMasked <- function(x)
{
  if (length(attr(x, "setmask")) == 0)
    return(FALSE)
  return(attr(x, "setmask"))
}
