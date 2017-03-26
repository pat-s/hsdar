# dist.default <- dist

# dist <- function(x,...) UseMethod("dist")



#' Distance between spectra
#' 
#' Calculation of distance matrices by using one of the various distance
#' measure to compute the distances between the spectra in `Speclib`.
#' Spectral Angle Mapper (SAM) is calculated with `sam` giving reference
#' spectra or with `sam_distance` taking all combinations between spectra
#' in single Speclib into account.
#' 
#' Available distance measures are "spectral angle mapper" (`sam`) and all
#' distance measures available in [dist()]. Spectral angle mapper is
#' calculated with the following formula:
#' \deqn{sam=\cos^{-1}\left(\frac{\sum_{i=1}^{nb}{t_i
#' r_i}}{\sqrt{\sum_{i=1}^{nb}{t_i^2}}\sqrt{\sum_{i=1}^{nb}{r_i^2}}}\right)}{
#' \cos^{-1}(\sum_{i=1}^{nb}{t_i r_i} \sum_{i=1}^{nb}{t_i^2}^{-0.5}
#' \sum_{i=1}^{nb}{r_i^2}^{-0.5})} \eqn{nb} is the number of bands in Speclib.
#' \eqn{t_i} and \eqn{r_i} are the reflectances of target and reference
#' spectrum in band \eqn{i}, respectively.
#' 
#' @aliases dist.speclib sam sam_distance
#' @param x Object of class `Speclib`.
#' @param method The distance measure to be used. This must be one of "sam",
#' "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#' @param ref Object of class `Speclib` containing reference spectra.
#' @param ...  Further arguments, passed to other methods.
#' @return The `dist-method` for Speclibs returns an object of class
#' `"dist"`. See [dist()] for further information on class
#' `"dist"`. Both other functions return an object of class matrix.
#' @author Lukas Lehnert
#' @seealso [dist()], \code{\linkS4class{Speclib}}
#' @references Kruse, F. A.; Lefkoff, A. B.; Boardman, J. W.; Heidebrecht, K.
#' B.; Shapiro, A. T.; Barloon, P. J. & Goetz, A. F. H. (1993). The spectral
#' image processing system (SIPS) -- interactive visualization and analysis of
#' imaging spectrometer data. Remote Sensing of Environment, 44, 145-163.
#' @examples
#' 
#' data(spectral_data)
#' 
#' ## Mask channel crossing part (arround 1050 nm) and strong 
#' ## water absorption part (above 1350 nm)
#' mask(spectral_data) <- c(1045, 1055, 1350, 1706)
#' 
#' ## Calculate distance between all spectra from spring 
#' ## using spectral angle mapper 
#' dist.speclib(subset(spectral_data, season == "spring"))
#' 
#' 
#' ## Calculate spectral angle mapper between reference spectrum
#' ## and spectral_data
#' ## Use first spectrum from summer as reference
#' distance <- sam(subset(spectral_data, season == "spring"), 
#'                 subset(spectral_data, season == "summer")[1,])
#' 
#' 
#' @export dist.speclib
dist.speclib <- function(
                         x,
                         method="sam",
                         ...
                         )
{
  if (class(x)!="Speclib") 
    stop("x must be of class 'Speclib'")
  
  if (method=="sam")
  {
    distance <- sam_distance(x)
    distance <- as.dist(distance)
  } else {
    if (!is.null(attr(x, "setmask")))
    {
      if (attr(x, "setmask"))
        x <- interpolate.mask(x)
    }
    
    spec <- spectra(x)
    
    distance <- dist(spec, method = method, ...)
  }
  return(distance)
}


sam <- function(
                x,
                ref
               )
{
  if (x@spectra@fromRaster)
    return(.blockwise(speclib_obj =  "x", pos = 1))
  
  if (class(x)!="Speclib") 
    stop("x must be of class 'Speclib'")
  if (class(ref)!="Speclib")
    stop("ref must be of class 'Speclib'")
    
  spec <- spectra(x)
  wlx <- x@wavelength
  
  
  specref <- spectra(ref)
  wlref <- ref@wavelength

  if (length(wlref) != length(wlx))
  {
    stop("Wavelength between speclibs differ")
  }
  
  spec    <- as.matrix(spec)
  specref <- as.matrix(specref)

  if (max(spec, na.rm = TRUE)>1)
  {
    spec <- spec/100
    specref <- specref/100
  }
  
  if (max(spec, na.rm = TRUE)>1)
    stop("Spectra in x must be in range [0,1]")
  if (max(specref, na.rm = TRUE)>1)
    stop("Spectra in ref must be in range [0,1]")
    
  nspec   <- nrow(spec)
  nref    <- nrow(specref)
  nbands  <- ncol(spec)
  specang <- array(0, dim = c(nspec,nref))
  

  storage.mode(nspec)    <- "integer"
  storage.mode(nref)     <- "integer"
  storage.mode(nbands)   <- "integer"
  storage.mode(spec)     <- "double"
  storage.mode(specref)  <- "double"
  storage.mode(specang)  <- "double"
  
  distance <- .Fortran("sam",
                       nspec=nspec,
                       nref=nref,
                       nbands=nbands,
                       spec=spec,
                       specref=specref,
                       specang=specang,
                       PACKAGE="hsdar"
                       )$specang
 
  distance <- as.matrix(distance)
  colnames(distance) <- rownames(specref)
  rownames(distance) <- rownames(spec)
  return(distance)                             
}

sam_distance <- function (x)
{
  if (class(x)!="Speclib") 
    stop("x must be of class 'Speclib'")
  
    
  spec <- spectra(x)
  
  if (!is.null(attr(x, "setmask")))
  {
    if (attr(x, "setmask"))
      x <- interpolate.mask(x)
  }
  
  spec    <- as.matrix(spec)  
  nspec   <- nrow(spec)
  nbands  <- ncol(spec)
  specang <- array(0, dim = c(nspec,nspec))
  if (max(spec)>1)
    spec <- spec/100
  
  storage.mode(nspec)    <- "integer"
  storage.mode(nbands)   <- "integer"
  storage.mode(spec)     <- "double"
  storage.mode(specang)  <- "double"
    
  distance <- .Fortran("sam",
                       nspec=nspec,
                       nref=nspec,
                       nbands=nbands,
                       spec=spec,
                       specref=spec,
                       specang=specang,
                       PACKAGE="hsdar"
                       )$specang
 
  distance <- as.matrix(distance)
  colnames(distance) <- rownames(spec)
  rownames(distance) <- rownames(spec)
  diag(distance) <- 0
  return(distance)     
}
