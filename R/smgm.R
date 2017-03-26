.smgm_lsa_wrapper <- function(x_f, y_f, x_p, gridsize)
{
  a    <- c(1:3)
  nx   <- length(x_f)
  nx_p <- length(x_p)
  y_p  <- x_p
  rmse <- 1
  
  storage.mode(a)        <- "double" 
  storage.mode(x_f)      <- "integer"
  storage.mode(y_f)      <- "double"
  storage.mode(nx)       <- "integer"
  storage.mode(gridsize) <- "integer"
  storage.mode(rmse)     <- "double"
  
  storage.mode(x_p)      <- "integer"
  storage.mode(y_p)      <- "double"
  storage.mode(nx_p)     <- "integer"
  
  a <- .Fortran("smgm_lsa",
                x        = x_f, 
                y        = y_f, 
                nx       = nx,
                a        = a,
                gridsize = gridsize,
                rmse     = rmse,
                PACKAGE  = "hsdar"
               )
               
  y_p <- .Fortran("inv_gauss_fit",
                  x       = x_p, 
                  nx      = nx_p, 
                  a       = a$a,         
                  y       = y_p,
                  PACKAGE = "hsdar"
                 )$y
  return(list(y = y_p, a = c(a$a, a$rmse)))
}    
    


#' SMGM
#' 
#' Calculate Gaussian model on soil spectra
#' 
#' The algorithm fits a Gaussian function to the continuum points of the
#' spectra in the spectral region between approx. 1500 to 2500 nm. The
#' continuum points are derived constructing the convex hull of the spectra
#' (see [transformSpeclib()]). The Gaussian function requires three
#' parameter: (1) the mean values which is set to the water fundamental of 2800
#' nm, (2) the absorption depth at 2800 nm, and (3) the distance to the
#' inflection point of the function. The latter two parameters are iteratively
#' chosen using a grid search. The mesh size of the grid can be adjusted with
#' the `gridsize` parameter. Note that the function requires the spectral
#' reflectance values to be in interval [0, 100].
#' 
#' @param x Object of class Speclib.
#' @param percentage Flag if spectra in x are in range [0, 100]. If FALSE, the
#' spectra are scaled to [0,100].
#' @param gridsize Size of the grid used to perform least squares
#' approximation.
#' @return Object of class `Speclib` containing the fitted Gaussian
#' spectra and the parameters derived from the Gaussian curve. The three
#' parameters (absorption depth, R0; distance to the inflection point, sigma;
#' area between the curve and 100 \% reflectance, area) are stored in the
#' attributes of the new Speclib. Additionally, the function returns the final
#' root mean square error of the Gaussian fit.
#' @note The code is based on the IDL functions written by Michael L. Whiting.
#' @author Lukas Lehnert
#' @seealso [soilindex()], \linkS4class{Speclib}
#' @references Whiting, M. L., Li, L. and Ustin, S. L. (2004): Predicting water
#' content using Gaussian model on soil spectra. Remote Sensing of Environment,
#' 89, 535-552.
#' @examples
#' 
#' ## Use PROSAIL to simulate spectra with different soil moisture content
#' Spektr.lib <- smoothSpeclib(PROSAIL(parameterList = data.frame(psoil = seq(0,1,0.1), LAI = 0)))
#' 
#' smgm_val <- smgm(Spektr.lib)
#' 
#' for (i in 1:nspectra(smgm_val))
#'   plot(smgm_val, FUN = i, new = i==1, col = i)
#' 
#' attribute(smgm_val)
#' 
#' @export smgm
smgm <- function(x, percentage = TRUE, gridsize = 50)
{
  ## Set range of spectra to [0,100]
  if (!percentage)
    spectra(x) <- spectra(x) * 100
  
  ## Calculate log of spectra
  spectra(x) <- log(spectra(x), base = exp(1))
  
  ## Derive continuum hull of spectra
  ch <- transformSpeclib(x, out = "raw")

  ## Convert hull and spectra to normalized log values
  ch_2 <- ch
  ch_2@hull <- t(apply(ch@hull, MARGIN = 1, FUN = function(hull) return(hull/max(hull))))
  spectra(ch_2) <- t(apply(spectra(ch_2), MARGIN = 1, FUN = function(hull) return(hull/max(hull))))

  ## Allocate result
  result <- data.frame(ID = idSpeclib(x), area = c(1:nspectra(x)), R0 = 0, sigma = 0, rmse = 0) 
  res.wavelength <- wavelength(x)[1]:2800
  res.spectra <- matrix(NA, ncol = length(res.wavelength), nrow = nspectra(x))
  
  ## Fit Gaussian function to spectra (Loop over all spectra)
  for (i in 1:nspectra(x))
  {
    cp_points <- ch_2@cp[i,] > 0
    xy <- matrix(c(ch_2@cp[i, cp_points], ch_2@hull[i, cp_points]), ncol = 2)
    lamda_i <- xy[,2] == 1
    xy <- as.data.frame(xy[max(c(1:length(lamda_i))[lamda_i]):nrow(xy),])

    
    xy_pred <- data.frame(V1 = c(xy[1,1]:2800),
                          V2 = NA)

    V2 <- .smgm_lsa_wrapper(xy[,1], xy[,2], xy_pred[,1], gridsize)
    
    result$area[i] <- sum(V2$y*(-1))
    result$R0[i] <- V2$a[2]*(-1)
    result$sigma[i] <- V2$a[3]
    result$rmse[i] <- V2$a[4]
    
    xy_pred$V2 <- 1+V2$y
    res.spectra[i, c(which(res.wavelength == xy_pred$V1[1]):ncol(res.spectra))] <- xy_pred$V2
  }

  ## Convert to Speclib
  valid <- colSums(is.na(res.spectra)) < nrow(res.spectra)
  spec <- speclib(res.spectra[,valid], res.wavelength[valid], usagehistory = usagehistory(x))
  attribute(spec) <- result
  usagehistory(spec) <- "Gaussian model on soil spectra (SMGM)"
  return(spec)
}
