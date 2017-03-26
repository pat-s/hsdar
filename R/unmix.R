#' Unmix spectra
#' 
#' Unmix spectra or spectra resampled to satellite bands using endmember
#' spectra
#' 
#' Linear spectral unmixing is a frequently used method to calculate fractions
#' of land-cover classes (endmembers) within the footprint of pixels. This
#' approach has originally been intended to be used for multispectral satellite
#' images. The basic assumption is that the signal received at the sensor
#' (\eqn{\rho_{mix}}{p_{mix}}) is a linear combination of \eqn{n} pure
#' endmember signals (\eqn{\rho_{i}}{p_i}) and their cover fractions
#' (\eqn{f_{i}}{f_i}): \deqn{ }{ p_{mix} = \sum^{n}_{i=1} p_i f_i, }\deqn{
#' \rho_{mix} = \sum^{n}_{i=1} \rho_i f_i, }{ p_{mix} = \sum^{n}_{i=1} p_i f_i,
#' } where \eqn{f_1, f_2 , ..., f_n >= 0} and \eqn{\sum^{n}_{i=1} f_i = 1} to
#' fulfill two constraints: \enumerate{ \item All fractions must be greater or
#' equal 0 \item The sum of all fractions must be 1 } Since this linear equation
#' system is usually over-determined, a least square solution is performed. The
#' error between the final approximation and the observed pixel vector is
#' returned as vector (`error`) in list (`returnSpatialGrid = FALSE`)
#' or as last band if `returnSpatialGrid = TRUE`.
#' 
#' @param spectra Input spectra of class 'speclib'
#' @param endmember Endmember spectra of class 'speclib'
#' @param returnHCR Set class of value. If TRUE, value will be of class
#' '`HyperSpecRaster`', otherwise a list is returned. If `auto`,
#' function will switch to mode depending on input data characteristics.
#' @param scale Flag to scale spectra to [0,1] if necessary.
#' @param ...  Further arguments passed to [HyperSpecRaster()]
#' (ignored if returnHCR = FALSE).
#' @return A list containing the fraction of each endmember in each spectrum
#' and an error value giving the euclidean norm of the error vector after least
#' square error minimisation.
#' @note Unmixing code is based on "i.spec.unmix" for GRASS 5 written by Markus
#' Neteler (1999).
#' @author Lukas Lehnert
#' @references Sohn, Y. S. & McCoy, R. M. (1997): Mapping desert shrub
#' rangeland using spectral unmixing and modeling spectral mixtures with TM
#' data. Photogrammetric Engineering and Remote Sensing, 63, 707-716
#' @keywords multivariate
#' @examples
#' 
#' \dontrun{
#' ## Use PROSAIL to generate some vegetation spectra with different LAI
#' parameter <- data.frame(LAI = seq(0, 1, 0.01))
#' spectral_data <- PROSAIL(parameterList = parameter)
#' 
#' ## Get endmember spectra
#' ## Retrieve all available spectra
#' avl <- USGS_get_available_files()
#' 
#' ## Download all spectra matching "grass-fescue"
#' grass_spectra <- USGS_retrieve_files(avl = avl, pattern = "grass-fescue")
#' limestone <- USGS_retrieve_files(avl = avl, pattern = "limestone")
#' 
#' ## Integrate all spectra to Quickbird
#' grass_spectra_qb <- spectralResampling(grass_spectra[1,], "Quickbird")
#' limestone_qb <- spectralResampling(limestone, "Quickbird")
#' spectral_data_qb <- spectralResampling(spectral_data, "Quickbird")
#' 
#' 
#' em <- speclib(spectra = rbind(spectra(grass_spectra_qb), 
#'                               spectra(limestone_qb))/100,
#'               wavelength = wavelength(limestone_qb))
#' 
#' ## Unmix
#' unmix_res <- unmix(spectral_data_qb, em)
#' 
#' unmix_res
#' 
#' plot(unmix_res$fractions[1,] ~ attribute(spectral_data_qb)$LAI, type = "l",
#'      xlab = "LAI", ylab = "Unmixed fraction of vegetation")
#' }
#' 
#' @export unmix
unmix <- function(spectra, endmember, returnHCR = "auto", scale = FALSE, ...)
{
  if (spectra@spectra@fromRaster)
    return(.blockwise(speclib_obj =  "spectra", pos = 1))
  
  if (!all(c(is.speclib(spectra), is.speclib(endmember))))
    stop("Spectra and endmember must be of class 'speclib'")
  if (returnHCR == "auto")
    returnHCR <- .is.rastermeta(spectra)
  if (dim(endmember)[2]!=dim(spectra)[2])
    stop("Number of bands in spectra must be equal to number of bands in endmember")
  if (dim(endmember)[1] > dim(endmember)[2])
    stop("Number of endmember exceed number of bands")
    
  
  em_matrix <- t(spectra(endmember))
  spec_matrix <- t(spectra(spectra))
  
  if (max(c(max(spec_matrix, na.rm = TRUE),max(em_matrix, na.rm = TRUE)))>1)
  {
    if (!scale)
      stop("Function needs reflectance values <= 1.0")
    if (max(spec_matrix, na.rm = TRUE)>1)
      spec_matrix <- spec_matrix / 100
    if (max(em_matrix, na.rm = TRUE)>1)
      em_matrix <- em_matrix / 100   
  }
  if (max(c(max(spec_matrix, na.rm = TRUE),max(em_matrix, na.rm = TRUE)))>1)
    stop("Function needs reflectance values <= 1.0")

  if (sum(!is.finite(em_matrix)))
    stop("Spectra in 'endmember' contain infinite values")
  
  valid_data <- apply(spec_matrix, MARGIN = 2, function(x) all(is.finite(x)))
  spec_matrix[,!valid_data] <- 0
    
  if (nrow(em_matrix)!=nrow(spec_matrix))
    stop("Number of bands in spectra is not equal to number of bands in endmember matrix")
  
  n_em      <- ncol(em_matrix)
  n_spec    <- ncol(spec_matrix)
  n_band    <- nrow(em_matrix)
  fractions <- matrix(data=0, ncol=n_spec, nrow=n_em)
  error     <- rep.int(1,n_spec)
  
  storage.mode(n_em)        <- "integer"
  storage.mode(n_band)      <- "integer"
  storage.mode(n_spec)      <- "integer"
  storage.mode(em_matrix)   <- "double"
  storage.mode(spec_matrix) <- "double"
  storage.mode(fractions)   <- "double"
  storage.mode(error)       <- "double"
  
  un_mix <- .C("unmix",
               n_em        = n_em, 
               n_band      = n_band, 
               n_spec      = n_spec,
               em_matrix   = em_matrix, 
               spec_vector = spec_matrix, 
               fractions   = fractions,
               error       = error,
               package     = "hsdar"
              )
  fractions <- matrix(un_mix$fractions, ncol = n_spec)
  error <- un_mix$error
  
  colnames(fractions) <- idSpeclib(spectra)
  row.names(fractions) <- idSpeclib(endmember)
  fractions[,!valid_data] <- NA
  error[!valid_data] <- NA
  
  if (returnHCR)
  {
    spec <- as.data.frame(t(as.matrix(rbind(fractions, error))))
    names(spec)[ncol(spec)] <- "error"
    spec <- speclib(spec, c(1:ncol(spec)),
                    rastermeta = if (.is.rastermeta(spectra)) spectra@rastermeta else list())

    spec <- HyperSpecRaster(spec, ...)
    return(spec)
  } else {
    return(list(fractions = fractions, error = error))
  }
}
