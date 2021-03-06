#' Red edge parameter
#' 
#' Derive red edge parameters from hyperspectral data
#' 
#' Shape and location of the red edge are commonly described by four
#' parameters: \itemize{ \item\eqn{R0}: minimum reflectance in the red spectrum
#' \item\eqn{\lambda 0}{l0}: wavelength of the minimum reflectance
#' \item\eqn{\lambda p}{lp}: inflection point \item\eqn{Rs}: shoulder
#' wavelength }
#' 
#' The red edge parameters are calculated as proposed in Bach (1995) from the
#' spectral area between 550 and 900 nm. \eqn{\lambda 0}{l0} is calculated as
#' the last root before the maximum value of the 2nd derivation.  The minimum
#' reflectance is the reflectance at (\eqn{\lambda 0}{l0}).  The inflection
#' point is the root of the 2nd derivative function between the maximum value
#' and the minimum value. The shoulder wavelength is the first root beyond the
#' minimum value of the 2nd derivation.
#' 
#' Optional smoothing is performed with
#' 
#' \code{\link{smoothSpeclib}(x, method = "spline", n = round(nbands(x)/10,0))}
#' 
#' prior to all calculations. Note that reflectance values returned by the
#' `rededge`-function are original values and not the smoothed
#' reflectances. This would not be the case, if already smoothed reflectance
#' values are passed to `rededge`-function.
#' 
#' @param x List of class `Speclib`
#' @param smooth Logical indicating if spectral data should be smoothed. See
#' details section.
#' @param round Logical indicating if resulting wavelength position should be
#' rounded.
#' @param ...  Further arguments passed to [derivative.speclib()]
#' @return A data frame containing parameters for each spectrum.
#' @author Hanna Meyer
#' @seealso [vegindex()], [derivative.speclib()],
#' [smoothSpeclib()]
#' @references Bach, H. (1995): Die Bestimmung hydrologischer und
#' landwirtschaftlicher Oberflaechenparameter aus hyperspektralen
#' Fernerkundungsdaten.  Muenchner Geographische Abhandlungen Reihe B, Band
#' B21.
#' @keywords multivariate
#' @examples
#' 
#' data(spectral_data)
#' rd <- rededge(spectral_data)
#' boxplot(rd$R0 ~ attribute(spectral_data)$season, ylab = "R0")
#' 
#' @export rededge
rededge <- function(x, 
                    smooth = TRUE,
                    round = FALSE,
                    ...
                   )
{
  if (!is.speclib(x))
    stop("x is not of class 'Speclib'")
  
  if (x@spectra@fromRaster)
    return(.blockwise(speclib_obj =  "x", pos = 1))
    
  if (!x@continuousdata)
    stop("x must contain continuous spectral data")
  
  if (wavelength(x)[1] > 600 || wavelength(x)[length(wavelength(x))] < 900)
    stop("x does not contain relevant spectral range. Please ensure that x covers 600 to 900 nm") 
    
  D2 <- derivative.speclib(if (smooth) smoothSpeclib(x, method="spline", n=round(nbands(x)/10,0)) else x,
                           m = 2,
                           ...
                   )
  
  RedEdge_data <- as.data.frame(t(as.matrix(sapply(c(1:nspectra(x)), FUN = .rededge_apply, spectra(x), D2), ncol = 4)))
  row.names(RedEdge_data) <- idSpeclib(x)
  names(RedEdge_data) <- c("l0", "Rs", "lp", "R0")

  if (round)
  {
    RedEdge_data[,1] <- round(RedEdge_data[,1], 0)
    RedEdge_data[,2] <- round(RedEdge_data[,2], 0)
    RedEdge_data[,3] <- round(RedEdge_data[,3], 0)
  }
  
  return(RedEdge_data)
}

.rededge_apply <- function(i, x, D2)
  {
    i <- i[1]
    model <- splinefun(wavelength(D2), spectra(D2)[i,])
    dev2Max <- 550+which(model(550:800)==max(model(550:800)))
    dev2Min <- 550+which(model(550:800)==min(model(550:800)))
    l0 <- uniroot.all(model, c(550,dev2Max))[length(uniroot.all(model, c(550,dev2Max)))]
    Rs <- uniroot.all(model, c(dev2Min,900))[1]
    lp <- uniroot.all(model, c(dev2Max,dev2Min))[1]
    RedEdge_data <- c(if (length(l0) == 0) NA else l0,
                      if (length(Rs) == 0) NA else Rs,
                      if (length(lp) == 0) NA else lp)
    R0 <- if (is.finite(RedEdge_data[1])) get_reflectance(x, wavelength(D2), RedEdge_data[1])[i] else NA
    return(c(RedEdge_data, R0))
  }  
