#' Normalised ratio index
#' 
#' Calculate normalised ratio index for a single given band combination or for
#' all possible band combinations
#' 
#' Function performs the following calculation:
#' \deqn{nri_{B1,~B2}=\frac{R_{B1}-R_{B2}}{R_{B1}-R_{B2}};} with \eqn{R} being
#' reflectance values at wavelength \eqn{B1} and \eqn{B2}, respectively.
#' 
#' If recursive = TRUE, all possible band combinations are calculated.
#' 
#' @param x List of class `Speclib` or of class `Nri` for print and
#' as.matrix methods
#' @param b1 Band 1 given as index or wavelength
#' @param b2 Band 2 given as index or wavelength
#' @param recursive If TRUE indices for all possible band combinations are
#' calculated
#' @param bywavelength Flag to determine if b1 and b2 are indices (bywavelength
#' = FALSE) or wavelength (bywavelength = TRUE)
#' @param ...  Further arguments passed to generic functions. Currently
#' ignored.
#' @return If recursive = FALSE, a data frame with index values is returned.
#' Otherwise result is an object of class `nri`. See [glm.nri()]
#' for applying a generalised linear model to an array of normalised ratio
#' indices.
#' @author Lukas Lehnert
#' @seealso [glm.nri()], [glm()],
#' \code{\linkS4class{Speclib}}, \code{\linkS4class{Nri}}
#' @references Sims, D.A.; Gamon, J.A. (2002). Relationships between leaf
#' pigment content and spectral reflectance across a wide range of species,
#' leaf structures and developmental stages. Remote Sensing of Environment:
#' 81/2, 337 - 354.
#' 
#' Thenkabail, P.S.; Smith, R.B.; Pauw, E.D. (2000). Hyperspectral vegetation
#' indices and their relationships with agricultural crop characteristics.
#' Remote Sensing of Environment: 71/2, 158 - 182.
#' @keywords multivariate
#' @examples
#' 
#' data(spectral_data)
#' 
#' ## Calculate NDVI
#' ndvi <- nri(spectral_data, b1=800, b2=680)
#' 
#' ## Calculate all possible combinations for WorldView-2-8
#' spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
#'                               response_function = FALSE)
#' nri_WV <- nri(spec_WV, recursive = TRUE)
#' nri_WV
#' 
#' @export nri
nri <- function(
                x,
                b1,
                b2,
                recursive = FALSE,
                bywavelength = TRUE
               )
{
if (!is.speclib(x))
  stop("x must be of class 'Speclib'")

range.of.wavelength <- x$fwhm

reflectance <- spectra(x)
wavelength <- wavelength(x)


if (recursive)
{
 
  if (inherits(nrow(reflectance) * (sum(1:length(wavelength))-length(wavelength)), "error"))
  {
      stop("Number of Samples*(number of wavelengths^2) exceeds maximum
      vector size of 2^31-1")
  }
  
  nri_dat <- single(length = nrow(reflectance) * (sum(1:length(wavelength))-length(wavelength)))
  result <- .Fortran("recursive_nri",
                     nwl = as.integer(length(wavelength)),
                     nspec = as.integer(nrow(reflectance)),
                     reflectance = as.single(as.matrix(reflectance)),
                     nri = nri_dat,
                     nri_length = as.integer(nrow(reflectance) *
                                  (sum(1:length(wavelength))-length(wavelength))),
                     PACKAGE = "hsdar"
                    )
  
  result <- distMat3D(as.numeric(result$nri), length(wavelength), nrow(reflectance))
  result <- new("Nri", nri = result, fwhm = range.of.wavelength,
                wavelength = wavelength,
                dimnames = list(Band_1 = paste("B_", wavelength, sep = ""),
                                Band_2 = paste("B_", wavelength, sep = ""),
                                Sample = idSpeclib(x)),
                attributes = attribute(x)                
               )
  if (!is.null(attr(x, "caretParameters")))
    attr(result, "caretParameters") <- attr(x, "caretParameters")
  result@usagehistory <- c(x@usagehistory, "NRI values calculated") 
} else {
  b1 <- as.vector(unlist(b1))
  b2 <- as.vector(unlist(b2))
  
  stopifnot(length(b1) == length(b2))
  
  if (length(b1) > 1)
  {
    res <- apply(matrix(1:length(b1), ncol = 1), 1, 
                 FUN = function(i, x, b1, b2, bywavelength)
                 {
                   index <- nri(x, b1 = b1[i], b2 = b2[i], bywavelength = bywavelength)
                   return(index)
                 }, x, b1, b2, bywavelength)
    colnames(res) <- paste("B", b1, "B", b2, sep = "_")
    rownames(res) <- idSpeclib(x)
    return(res)
  }                 
  if (bywavelength)
  {
    posb1 <- which(wavelength==b1)
    posb2 <- which(wavelength==b2)
  } else {
    posb1 <- b1
    posb2 <- b2
  }
  result <- (reflectance[,posb1]-reflectance[,posb2])/(reflectance[,posb1]+reflectance[,posb2])
  if (class(result)=="data.frame")
    names(result)<-"NRI"
}
return(result)  
}

