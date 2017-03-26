#' Transform spectra
#' 
#' Transform spectra by using convex hull or segmented upper hull
#' 
#' 
#' Function performs a continuum removal transformation by firstly establishing
#' a continuum line/hull which connects the local maxima of the reflectance
#' spectrum. Two kinds of this hull are well established in scientific
#' community: the convex hull (e.g. Mutanga et al. 2004) and the segmented hull
#' (e.g. Clark et al. 1987). Both hulls are established by connecting the local
#' maxima, however, the precondition of the convex hull is that the resulting
#' continuum line must be convex whereas considering the segmented hull it
#' might be concave or convex but the algebraic sign of the slope is not
#' allowed to change from the global maximum of the spectrum downwards to the
#' sides. In contrast to a convex hull, the segmented hull is able to identify
#' small absorption features.
#' 
#' Specify `method = "ch"` for the convex hull and `method = "sh"`
#' for the segmented hull. The output might be `"raw"`, `"bd"` or
#' `"ratio"`: \itemize{ \item"raw": the continuum line is returned
#' \item"bd": the spectra are transformed to band depth by \deqn{BD_\lambda =
#' 1-\frac{R_\lambda}{CV_\lambda},} where \eqn{BD} is the band depth, \eqn{R}
#' is the reflectance and \eqn{CV} is the continuum value at the wavelength
#' \eqn{\lambda}. \item"ratio": the spectra are transformed by \deqn{BD_\lambda
#' = \frac{R_\lambda}{CV_\lambda}.}
#' 
#' } In some cases it might be useful to apply [smoothSpeclib()]
#' before the transformation if too many small local maxima are present in the
#' spectra. Anyway, a manual improvement of the continuum line is possible
#' using [addcp()] and [deletecp()].
#' 
#' @param data Speclib to be transformed
#' @param method Method to be used. See details section.
#' @param out Kind of value to be returned. See details section.
#' @param ...  Further arguments passed to generic functions. Currently
#' ignored.
#' @return If `out != "raw"` an object of class
#' \code{\linkS4class{Speclib}} containing transformed spectra is returned.
#' Otherwise the return object will be of class [Clman()].
#' @author Hanna Meyer and Lukas Lehnert
#' @seealso [Clman()], [addcp()], [deletecp()],
#' [checkhull()]
#' @references Clark, R. N., King, T. V. V. and Gorelick, N. S. (1987):
#' Automatic continuum analysis of reflectance spectra.  Proceedings of the
#' Third Airborne Imaging Spectrometer Data Analysis Workshop, 30. 138-142.
#' 
#' Mutanga, O. and Skidmore, A. K. (2004): Hyperspectral band depth analysis
#' for a better estimation of grass biomass (Cenchrus ciliaris) measured under
#' controlled laboratory conditions International Journal of applied Earth
#' Observation and Geoinformation, 5, 87-96.
#' @keywords multivariate
#' @examples
#' 
#' data(spectral_data)
#' 
#' transformed_spectra <- transformSpeclib(spectral_data)
#' 
#' par(mfrow=c(1,2))
#' plot(spectral_data)
#' plot(transformed_spectra)
#' 
#' @export transformSpeclib
transformSpeclib <- function(
                               data, ...,
                               method = "ch",
                               out = "bd"
                             )
{
if (out != "raw")
{
  if (data@spectra@fromRaster)
    return(.blockwise(speclib_obj =  "data", pos = 1))
}

x <- data
methodfound = FALSE
usespeclib  = FALSE

if (!is.speclib(x))
  stop("data must be of class 'Speclib'")

setmask <- if (is.null(attr(x, "setmask"))) FALSE else attr(x, "setmask")
if (setmask)
{
  dropped <- attr(x, "dropped")
  x <- interpolate.mask(x)
  result <- x
  for (i in 1:nrow(dropped))
    spectra(x)[,x$wavelength >= dropped[i,1] & x$wavelength <= dropped[i,2]] <- 0
} else {
  result <- x
}

y <- as.data.frame(spectra(x))
x <- x$wavelength

usagehistory(result) <- paste("Reflectance = transform (",method,"), ",out,sep="")

pp <- .process_parallel()

if (method == "ch")
{
  methodfound=TRUE
  
  if (!pp[[1]])
  {
    hull <- matrix(data=0,nrow=nrow(y),ncol=ncol(y))
    cp   <- y*0
    status <- apply(matrix(c(1:nrow(y)), ncol = 1), 1, FUN = function(i, x, y)
    {
      c.hull_da <- matrix(c(x, y[i,]), ncol = 2, byrow = FALSE)
      c.hull <- chull(c.hull_da)
      min_pt <- which.min(c.hull)
      max_pt <- which.max(c.hull)
      if (min_pt < max_pt)
      {
        c.hull <- c.hull[c(min_pt:max_pt)]
      } else {
        c.hull <- c.hull[c(min_pt:length(c.hull), max_pt)]
      }
      cp[i,c.hull]   <<- x[c.hull]
      hull[i,] <<- approx(x=x[c.hull],y=y[i,c.hull], xout = x,method = "linear", ties = "mean")$y
    }, x, y)
  } else {   
    `%op%` <- pp[[2]]
    c.hull <- foreach::foreach(i=1:nrow(y), .combine = 'rbind') %op%
    {
      c.hull_da <- matrix(c(x, y[i,]), ncol = 2, byrow = FALSE)
      c.hull <- chull(c.hull_da)
      min_pt <- which.min(c.hull)
      max_pt <- which.max(c.hull)
      if (min_pt < max_pt)
      {
        c.hull <- c.hull[c(min_pt:max_pt)]
      } else {
        c.hull <- c.hull[c(min_pt:length(c.hull), max_pt)]
      }
      cp <- c(1:ncol(y)) * 0
      cp[c.hull]   <- x[c.hull]
      hull <- approx(x=x[c.hull],y=y[i,c.hull], xout = x,method = "linear", ties = "mean")$y
      matrix(c(cp, hull), ncol = 2, byrow = FALSE)
    }
    hull <- matrix(c.hull[,2], ncol = ncol(y), byrow = TRUE)
    cp   <- matrix(c.hull[,1], ncol = ncol(y), byrow = TRUE)
    .restoreParallel()
  }
}
if (method == "sh")
{
  methodfound=TRUE
  if (!pp[[1]])
  {
    hull <- y
    cp   <- y
    status <- apply(matrix(c(1:nrow(y)), ncol = 1), 1, FUN = function(i, x, y)
    {
      y_i <- as.vector(as.matrix(y[i,]))
      external <- .Fortran("localmaxima",
                          n      = as.integer(length(y_i)),
                          y      = as.single(y_i),
                          locmax = as.integer(c(1:length(y_i))*0),
                          PACKAGE="hsdar"
                          )
      lm <- external$locmax
      lm <- lm[lm>0]
                          
      external <- .Fortran("suh",
                          nlm  = as.integer(length(lm)), 
                          n    = as.integer(length(y)),
                          LMin = as.integer(lm),
                          y    = as.single(y_i),
                          hull = as.single(c(1:length(y))*0),
                          cp   = as.integer(c(1:length(y))*0),
                          PACKAGE="hsdar"
                          )
      hull[i,] <<- external$hull
      cp[i,]   <<- external$cp
    }, x, y)
  } else {
    `%op%` <- pp[[2]]
    c.hull <- foreach::foreach(i=1:nrow(y), .combine = 'rbind') %op%
    {
      y_i <- as.vector(as.matrix(y[i,]))
      external <- .Fortran("localmaxima",
                          n      = as.integer(length(y_i)),
                          y      = as.single(y_i),
                          locmax = as.integer(c(1:length(y_i))*0),
                          PACKAGE="hsdar"
                          )
      lm <- external$locmax
      lm <- lm[lm>0]
                          
      external <- .Fortran("suh",
                          nlm  = as.integer(length(lm)), 
                          n    = as.integer(length(y)),
                          LMin = as.integer(lm),
                          y    = as.single(y_i),
                          hull = as.single(c(1:length(y))*0),
                          cp   = as.integer(c(1:length(y))*0),
                          PACKAGE="hsdar"
                          )
      matrix(c(external$cp, external$hull), ncol = 2, byrow = FALSE)
    }
    
    hull <- matrix(c.hull[,2], ncol = ncol(y), byrow = TRUE)
    cp   <- matrix(c.hull[,1], ncol = ncol(y), byrow = TRUE)
    .restoreParallel()
  }
}

if (!methodfound) stop(paste("Unknown method '",method,"'!",sep=""))

if (out=="bd") 
{
  spectra(result) <- 1 - y/hull
  result@transformation <- "bd"
  result@ylabel <- "Band depth"
  if (setmask)
    mask(result) <- dropped
  return (result)
} else {
  if (out=="difference") 
  {
    spectra(result) <- hull - y
    result@transformation <- "difference"
    result@ylabel <- "Transformed difference"
    if (setmask)
      mask(result) <- dropped
    return (result)
  } else {
    if (out=="raw") 
    {
      return(new("Clman", result, cp = cp, hull = hull))
    } else {
      spectra(result) <- y/hull
      result@transformation <- "ratio"
      result@ylabel <- "Band depth ratio"
      if (setmask)
        mask(result) <- dropped
      return (result)
    }
  }
}
}



#' Check continuum line
#' 
#' Check if continuum line is intersecting the reflectance curve.
#' 
#' 
#' @param x Object of class `clman`.
#' @param ispec ID or index of spectrum to be checked.
#' @return Object of class `list`.
#' @author Lukas Lehnert and Hanna Meyer
#' @seealso [transformSpeclib()], [addcp()],
#' [deletecp()], [makehull()], [updatecl()]
#' @keywords utilities
#' @examples
#' 
#' ## Model spectra using PROSAIL
#' parameter <- data.frame(N = rep.int(c(1, 1.5),2), LAI = c(1,1,3,3))
#' spec <- PROSAIL(parameterList=parameter)
#' 
#' ## Transform spectra
#' spec_clman <- transformSpeclib(spec, method = "sh", out = "raw")
#' 
#' ## Plot original line
#' par(mfrow = c(1,2))
#' plot(spec_clman, ispec = 1, subset = c(2480, 2500))
#' 
#' ## Add fix point at 4595 nm to continuum line of first spectrum
#' spec_clman <- addcp(spec_clman, 1, 2495)
#' 
#' ## Plot new line
#' plot(spec_clman, ispec = 1, subset = c(2480, 2500))
#' 
#' ## Check new hull
#' hull <- checkhull(spec_clman, 1)
#' hull$error
#' 
#' ## Add fix point at 4596 nm to continuum line of first spectrum
#' spec_clman <- addcp(spec_clman, 1, 2496)
#' 
#' ## Check new hull
#' hull <- checkhull(spec_clman, 1)
#' hull$error
#' 
#' @export checkhull
checkhull <- function(
                      x,
                      ispec
                     )
{
  ptscon <- getcp(x,ispec)
  
  ispec  <- ptscon$ispec
  ptscon <- ptscon$ptscon$Wavelength
  
  Reflectance  <- spectra(x)[ispec,]

  result <- c(0,0)
  hull <- Reflectance*0
  
  storage.mode(ptscon)      <- "integer"
  storage.mode(Reflectance) <- "double"
  storage.mode(result)      <- "integer"
  storage.mode(hull)        <- "double"

  external <- .Fortran("checkhull",
                       ncp     = as.integer(length(ptscon)), 
                       n       = as.integer(length(Reflectance)),
                       ptscon  = ptscon, 
                       y       = Reflectance,
                       offset  = as.integer(x$wavelength[1]-1),
                       res     = result,
                       hull    = hull,
                       PACKAGE = "hsdar"
                      )
  if (external$res[1]!=0)
    warning(paste("Mismatch of continuum line at wavelength =",external$res[1],
                  "\n  Maximum distance between continuum line & spectrum at\n",
                  " Wavelength =",external$res[2]))
  return(list(hull=external$hull,error=external$res))
}



#' Check continuum line
#' 
#' Check if continuum line is intersecting the reflectance curve.
#' 
#' 
#' @param x Object of class `Clman`.
#' @param ispec Name or index of spectrum to be checked.
#' @return Object of class `list`.
#' @author Lukas Lehnert and Hanna Meyer
#' @seealso [transformSpeclib()], [addcp()],
#' [deletecp()], [makehull()], [updatecl()]
#' 
#' \code{\linkS4class{Clman}}
#' @keywords utilities
#' @examples
#' 
#' ## Model spectra using PROSAIL
#' parameter <- data.frame(N = rep.int(c(1, 1.5),2), LAI = c(1,1,3,3))
#' spec <- PROSAIL(parameterList=parameter)
#' 
#' ## Transform spectra
#' spec_clman <- transformSpeclib(spec, method = "sh", out = "raw")
#' 
#' ## Plot original line
#' par(mfrow = c(1,2))
#' plot(spec_clman, ispec = 1, subset = c(2480, 2500))
#' 
#' ## Add fix point at 4595 nm to continuum line of first spectrum
#' spec_clman <- addcp(spec_clman, 1, 2495)
#' 
#' ## Plot new line
#' plot(spec_clman, ispec = 1, subset = c(2480, 2500))
#' 
#' ## Check new hull
#' hull <- checkhull(spec_clman, 1)
#' hull$error
#' 
#' ## Add fix point at 4596 nm to continuum line of first spectrum
#' spec_clman <- addcp(spec_clman, 1, 2496)
#' 
#' ## Check new hull
#' hull <- checkhull(spec_clman, 1)
#' hull$error
#' 
#' hull <- makehull(spec_clman, 1)
#' 
#' ## Transform spectra using band depth
#' spec_bd <- transformSpeclib(spec, method = "sh", out = "bd")
#' 
#' ## Update continuum line of first spectrum
#' spec_bd <- updatecl(spec_bd, hull)
#' 
#' ## Plot modified transformed spectrum
#' plot(spec_bd, FUN = 1)
#' 
#' @export makehull
makehull <- function(
                      x,
                      ispec
                     )
{
  ptscon <- getcp(x,ispec)
  
  ispec  <- ptscon$ispec
  ptscon <- ptscon$ptscon$Wavelength
  
  Reflectance  <- spectra(x)[ispec,]

  result <- c(0,0)
  hull <- Reflectance*0
  
  storage.mode(ptscon)      <- "integer"
  storage.mode(Reflectance) <- "double"
  storage.mode(result)      <- "integer"
  storage.mode(hull)        <- "double"

  external <- .Fortran("checkhull",
                       ncp     = as.integer(length(ptscon)), 
                       n       = as.integer(length(Reflectance)),
                       ptscon  = ptscon, 
                       y       = Reflectance,
                       offset  = as.integer(x$wavelength[1]-1),
                       res     = result,
                       hull    = hull,
                       PACKAGE = "hsdar"
                      )
  if (external$res[1]!=0)
    warning(paste("Mismatch of continuum line at wavelength =",external$res[1],
                  "\n  Maximum distance between continuum line & spectrum at\n",
                  " Wavelength =",external$res[2]))
  
  result <- external$hull
  attr(result,"ispec") <- ispec
  attr(result,"reflectance") <- spectra(x)[ispec,]
  return(result)
}



#' Check continuum line
#' 
#' Check if continuum line is intersecting the reflectance curve.
#' 
#' 
#' @param x Object of class \code{\linkS4class{Speclib}} transformed by
#' [transformSpeclib()].
#' @param hull Hull to be applied to x. Output of function
#' [makehull()].
#' @return Object of class \code{\linkS4class{Speclib}}.
#' @author Lukas Lehnert and Hanna Meyer
#' @seealso [transformSpeclib()], [makehull()],
#' \code{\linkS4class{Speclib}}
#' @keywords utilities
#' @examples
#' 
#' ## Model spectra using PROSAIL
#' parameter <- data.frame(N = rep.int(c(1, 1.5),2), LAI = c(1,1,3,3))
#' spec <- PROSAIL(parameterList=parameter)
#' 
#' ## Transform spectra
#' spec_clman <- transformSpeclib(spec, method = "sh", out = "raw")
#' 
#' ## Plot original line
#' par(mfrow = c(1,2))
#' plot(spec_clman, ispec = 1, subset = c(2480, 2500))
#' 
#' ## Add fix point at 4595 nm to continuum line of first spectrum
#' spec_clman <- addcp(spec_clman, 1, 2495)
#' 
#' ## Plot new line
#' plot(spec_clman, ispec = 1, subset = c(2480, 2500))
#' 
#' ## Check new hull
#' hull <- checkhull(spec_clman, 1)
#' hull$error
#' 
#' ## Add fix point at 4596 nm to continuum line of first spectrum
#' spec_clman <- addcp(spec_clman, 1, 2496)
#' 
#' ## Check new hull
#' hull <- checkhull(spec_clman, 1)
#' hull$error
#' 
#' hull <- makehull(spec_clman, 1)
#' 
#' ## Transform spectra using band depth
#' spec_bd <- transformSpeclib(spec, method = "sh", out = "bd")
#' 
#' ## Update continuum line of first spectrum
#' spec_bd <- updatecl(spec_bd, hull)
#' 
#' ## Plot modified transformed spectrum
#' plot(spec_bd, FUN = 1)
#' 
#' @export updatecl
updatecl <- function (
                      x,
                      hull
                     )
{
  
  if (!is.speclib(x))
    stop("x must be of class 'Speclib'")
  
  setmask <- if (is.null(attr(x, "setmask"))) FALSE else attr(x, "setmask")
  
  if (is.null(attr(hull,"ispec")))
  {
    stop("hull must be output of function 'makehull'")
  } else {
    ispec <- attr(hull,"ispec")
    reflectance <- attr(hull,"reflectance")
  }
  
  if (setmask)
  {
    dropped <- mask(x)
    x <- interpolate.mask(x)
  }
  
  if (mode(x@transformation)=="NULL")
    stop("x must be a transformed speclib")
  if (x@transformation == "difference")
    spectra(x)[ispec,] <- hull - reflectance
  if (x@transformation == "bd")
    spectra(x)[ispec,] <- 1 - reflectance/hull
  if (x@transformation == "ratio")
    spectra(x)[ispec,] <- reflectance/hull
  
  if (setmask)
    mask(x) <- dropped
    
  return(x)
}
