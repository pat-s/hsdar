#' Simulate plant spectrum
#' 
#' Simulate plant spectrum using PROSPECT 5. The inversion uses the concept
#' after Feret et al. (2008).
#' 
#' This function uses the FORTRAN code of PROSPECT model (Version 5). For a
#' general introduction see following web page and the links to articles
#' provided there:
#' 
#' \url{http://teledetection.ipgp.jussieu.fr/prosail/}
#' 
#' The following table summarises the abbreviations of parameters and gives
#' their units as used in PROSPECT. Please note that default values of all
#' parameters were included with the intention to provide an easy access to the
#' model and should be used with care in any scientific approach!
#' \tabular{lll}{ \tab\tab\cr Parameter \tab Description of parameter\tab
#' Units\cr \tab\tab\cr N\tab Leaf structure parameter\tab NA \cr Cab\tab
#' Chlorophyll a+b concentration\tab \eqn{\mu}{u}g/cm\eqn{^2}{2} \cr Car\tab
#' Carotenoid concentration\tab \eqn{\mu}{u}g/cm\eqn{^2}{2} \cr Cw\tab
#' Equivalent water thickness\tab cm \cr Cbrown \tab Brown pigment \tab NA \cr
#' Cm\tab Dry matter content\tab g/cm\eqn{^2}{2}\cr }
#' 
#' The inversion uses the function \code{\link[pracma]{nelder_mead}} from the
#' \pkg{pracma}-package and implements the Matlab-Code developed by Feret et
#' al. (2008).
#' 
#' @aliases PROSPECT PROSPECTinvert
#' @param N Structure parameter
#' @param Cab Chlorophyll content% (in \%)
#' @param Car Carotenoid content %(in \%)
#' @param Cbrown Brown pigment content% (arbitrary units)
#' @param Cw Equivalent water thickness% (\eqn{\textnormal{cm}}{cm})
#' @param Cm Dry matter content%
#' (\eqn{\textnormal{g}/\textnormal{cm}^2}{g/cm^2})
#' @param transmittance Logical flag, if transmittance instead of reflectance
#' values are returned.
#' @param parameterList An optional object of class \code{'data.frame'}.
#' Function will iterate over rows of parameterList setting missing entries to
#' default values. See examples section.
#' @param x,transmittance_spectra Speclib(s) containing the
#' reflectance/transmittance values to be simulated during inversion of
#' PROSPECT.
#' @param P0 Initial set of parameters (N, Cab etc.).
#' @param sam Logical if spectral angle mapper is used as distance measurement.
#' If FALSE, the root mean square error is used. Note that this flag has only
#' an effect if no transmittance spectra are passed.
#' @param ... Parameters passed to \code{\link[pracma]{nelder_mead}} from the
#' \pkg{pracma}-package
#' @return An object of class \code{Speclib}.
#' @note The function is based on the FORTRAN version of the PROSPECT-code
#' initially developed by Jean-Baptiste FERET, Stephane JACQUEMOUD and
#' Christophe FRANCOIS.
#' @author Lukas Lehnert
#' @seealso \code{\link{PROSAIL}}, \code{\link[pracma]{nelder_mead}},
#' \code{\linkS4class{Speclib}}
#' @references Jacquemoud, S. and Baret, F. (1990). PROSPECT: A model of leaf
#' optical properties spectra, Remote Sensing of Environment 34: 75 - 91.
#' 
#' Feret J.B., Francois C., Asner G.P., Gitelson A.A., Martin R.E., Bidel
#' L.P.R., Ustin S.L., le Maire G., & Jacquemoud S. (2008), PROSPECT-4 and 5:
#' advances in the leaf optical properties model separating photosynthetic
#' pigments. Remote Sensing of Environment, 112, 3030-3043.
#' @examples
#' 
#' ## Single spectrum
#' spectrum <- PROSPECT(N = 1.3, Cab = 30, Car = 10, Cbrown = 0, 
#'                      Cw = 0.01, Cm = 0.01)
#' plot(spectrum)
#' 
#' ## Example using parameterList
#' ## Test effect of leaf structure and chlorophyll content on 
#' ## spectra
#' parameter <- data.frame(N = c(rep.int(seq(0.5, 1.5, 0.5), 2)),
#'                         Cab = c(rep.int(40, 3), rep.int(20, 3)))
#' spectra <- PROSPECT(parameterList = parameter)
#' 
#' ## Print attributes table
#' attribute(spectra)
#' 
#' ## Plot spectra for range from 400 to 800 nm
#' spectra <- spectra[,wavelength(spectra) >= 400 & 
#'                     wavelength(spectra) <= 800]
#' 
#' plot(subset(spectra, Cab == 20), col = "red", ylim = c(0, 0.5))
#' plot(subset(spectra, Cab == 40), col = "green", new = FALSE)
#' 
#' @export PROSPECT
PROSPECT <- function(
                      N=1.5,
                      Cab=40,
                      Car=8,
                      Cbrown=0.0,
                      Cw=0.01,
                      Cm=0.009,
                      transmittance = FALSE,
                      parameterList = NULL
                   )
{
  if (!is.null(parameterList))
  {
    iterate_prospect <- function(x, transmittance = FALSE)
    {
      spec <- PROSPECT(N=x[1],
                       Cab=x[2],
                       Car=x[3],
                       Cbrown=x[4],
                       Cw=x[5],
                       Cm=x[6],
                       transmittance = transmittance)
      return(unlist(spectra(spec)[1,]))
    }
    
    parameter <- c("N", "Cab", "Car", "Cbrown", "Cw", "Cm")
    parameterList <- as.data.frame(parameterList)
    nam_para <- names(parameterList)
    mat <- match(names(parameterList), parameter, nomatch=0)
    if (any(mat==0))
      stop("Check names and format of parameterList")
    mat <- match(c(1:length(parameter)), mat, nomatch=0)
    for (i in 1:length(mat))
      if (mat[i]==0)
        parameterList <- cbind(parameterList, get(eval(parameter[i])))
    names(parameterList) <- c(nam_para, parameter[mat==0])
    
    parameterList <- as.matrix(parameterList[,match(parameter, names(parameterList))])
    spec <- t(apply(parameterList, 1, FUN = iterate_prospect, 
                    transmittance = transmittance))
    return(speclib(spectra=spec, wavelength=c(1:2101)+399, 
                   attributes = as.data.frame(parameterList)))
  }

  nw=2101
  RT <- array(0, dim=c(nw,2))
  
  storage.mode(nw)     <- "integer" 
  storage.mode(N)      <- "double"
  storage.mode(Cab)    <- "double"
  storage.mode(Car)    <- "double"
  storage.mode(Cbrown) <- "double"
  storage.mode(Cw)     <- "double"
  storage.mode(Cm)     <- "double"
  storage.mode(RT)     <- "double"
  
  
  extern <- .Fortran("prospect2r",
                     N=N,
                     Cab=Cab,
                     Car=Car,
                     Cbrown=Cbrown,
                     Cw=Cw,
                     Cm=Cm,
                     RT2R=RT,
                     PACKAGE="hsdar"
             )
  
  spec <- speclib(wavelength = c(1:nw) + 399, 
                  spectra = matrix(data = extern$RT2R[, 1 + transmittance*1],
                                   nrow = 1)
                 )
  return(spec)
}
