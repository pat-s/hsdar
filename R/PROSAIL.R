#' Simulate canopy spectrum
#' 
#' Simulate a canopy spectrum using PROSAIL 5B
#' 
#' This function uses the FORTRAN code of PROSAIL model (Version 5B). For a
#' general introduction see following web page and the links to articles
#' provided there:
#' 
#' \url{http://teledetection.ipgp.jussieu.fr/prosail/}
#' 
#' The following table summarises the abbreviations of parameters and gives
#' their units as used in PROSAIL. Please note that default values of all
#' parameters were included with the intention to provide an easy access to the
#' model and should be used with care in any scientific approach!
#' \tabular{lll}{ \tab\tab\cr Parameter \tab Description of parameter\tab
#' Units\cr \tab\tab\cr N\tab Leaf structure parameter\tab NA \cr Cab\tab
#' Chlorophyll a+b concentration\tab \eqn{\mu}{u}g/cm\eqn{^2}{2} \cr Car\tab
#' Carotenoid concentration\tab \eqn{\mu}{u}g/cm\eqn{^2}{2} \cr Caw\tab
#' Equivalent water thickness\tab cm \cr Cbrown \tab Brown pigment \tab NA \cr
#' Cm\tab Dry matter content\tab g/cm\eqn{^2}{2}\cr LAI \tab Leaf Area Index
#' \tab NA\cr psoil\tab Dry/Wet soil factor\tab NA \cr hspot\tab Hotspot
#' parameter\tab NA \cr tts\tab Solar zenith angle\tab deg\cr tto\tab Observer
#' zenith angle\tab deg \cr psi\tab Relative azimuth angle\tab deg \cr }
#' 
#' Functions for distribution of leaf angles within the canopy may work in two
#' modes, which is controlled via `TypeLidf`: \enumerate{
#' \item `TypeLidf == 1` (default): `lidfa` is the average leaf slope
#' and `lidfb` describes bimodality of leaf distribution. The following
#' list gives an overview on typical settings: \tabular{lrr}{ LIDF type\tab
#' `lidfa` \tab `lidfb`\cr \tab\tab\cr Planophile \tab 1 \tab 0\cr
#' Erectophile \tab -1 \tab 0\cr Plagiophile \tab 0 \tab -1\cr Extremophile\tab
#' 0 \tab 1\cr Spherical (default) \tab -0.35 \tab -0.15\cr }
#' 
#' \item `TypeLidf != 1`: `lidfa` is the average leaf angle in degree
#' (0 = planophile / 90 = erectophile); `lidfb` is 0 }
#' 
#' @param N Structure parameter
#' @param Cab Chlorophyll content (in \eqn{\mu}{u}g/cm\eqn{^2}{2})
#' @param Car Carotenoid content (in \eqn{\mu}{u}g/cm\eqn{^2}{2})
#' @param Cbrown Brown pigment content (arbitrary units)
#' @param Cw Equivalent water thickness (\eqn{\textnormal{cm}}{cm})
#' @param Cm Dry matter content (\eqn{\textnormal{g}/\textnormal{cm}^2}{g/cm^2})
#' @param psoil Dry/Wet soil factor
#' @param LAI Leaf area index
#' @param TypeLidf Type of leaf angle distribution. See details section
#' @param lidfa Leaf angle distribution. See details section
#' @param lidfb Leaf angle distribution. See details section
#' @param hspot Hotspot parameter
#' @param tts Solar zenith angle
#' @param tto Observer zenith angle
#' @param psi Relative azimuth angle
#' @param parameterList An optional object of class `'data.frame'`.
#' Function will iterate over rows of parameterList setting missing entries to
#' default values. See examples section.
#' @param rsoil An optional object of class `'Speclib'` containing the
#' background (soil) reflectance. Note that reflectance values must be in range
#' [0...1].
#' @return An object of class `Speclib`. If parameterList is used, the
#' parameter are stored in attributes table of `Speclib`.
#' @note The function is based on the FORTRAN version of the PROSAIL-code
#' initially developed by Stephane JACQUEMOUD, Jean-Baptiste FERET, Christophe
#' FRANCOIS and Eben BROADBENT. SAIL component has been developed by Wout
#' VERHOEF.
#' @author Lukas Lehnert
#' @seealso [PROSPECT()], \code{\linkS4class{Speclib}}
#' @references Jacquemoud, S., Verhoef, W., Baret, F., Bacour, C.,
#' Zarco-Tejada, P.J., Asner, G.P., Francois, C., and Ustin, S.L. (2009):
#' PROSPECT + SAIL models: a review of use for vegetation characterization,
#' Remote Sensing of Environment, 113, S56-S66.
#' @examples
#' 
#' ## Single spectrum
#' spectrum <- PROSAIL(N = 1.3)
#' plot(spectrum)
#' 
#' ## Example using parameterList
#' ## Test effect of leaf structure and LAI on spectra
#' parameter <- data.frame(N = c(rep.int(seq(0.5, 1.5, 0.5), 2)),
#'                         LAI = c(rep.int(0.5, 3), rep.int(1, 3)))
#' spectra <- PROSAIL(parameterList = parameter)
#' 
#' ## Print attributes table
#' attribute(spectra)
#' 
#' ## Plot spectra
#' plot(subset(spectra, LAI == 0.5), col = "red", ylim = c(0, 0.3))
#' plot(subset(spectra, LAI == 1), col = "green", new = FALSE)
#' 
#' @export PROSAIL
PROSAIL <- function(
                    N=1.5,
                    Cab=40,
                    Car=8,
                    Cbrown=0.0,
                    Cw=0.01,
                    Cm=0.009,
                    psoil=0,
                    LAI=1,
                    TypeLidf= 1,
                    lidfa= -0.35,
                    lidfb= -0.15,
                    hspot=0.01,
                    tts = 30,
                    tto = 10, 
                    psi= 0,
                    parameterList = NULL,
                    rsoil = NULL
                   )
{
  if (!is.null(parameterList))
  {
    iterate_prosail <- function(x, rsoil)
    {
      spec <- PROSAIL(N        = x[1],
                      Cab      = x[2],
                      Car      = x[3],
                      Cbrown   = x[4],
                      Cw       = x[5],
                      Cm       = x[6],
                      psoil    = x[7],
                      LAI      = x[8],
                      TypeLidf = x[9],
                      lidfa    = x[10],
                      lidfb    = x[11],
                      hspot    = x[12],
                      tts      = x[13],
                      tto      = x[14], 
                      psi      = x[15],
                      rsoil    = rsoil
                     )
      return(unlist(spectra(spec)[1,]))
    }
    
    parameter <- c("N", "Cab", "Car", "Cbrown", "Cw", "Cm",
                   "psoil", "LAI", "TypeLidf", "lidfa", 
                   "lidfb", "hspot", "tts", "tto", "psi")
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
    spec <- t(apply(parameterList, 1, FUN = iterate_prosail, rsoil))
    return(speclib(spectra=spec, wavelength=c(1:2101)+399, 
                   attributes = as.data.frame(parameterList)))
  }
  nw <- 2101
  RT <- array(0, dim=c(nw,1))
  
  if (!is.null(rsoil))
  {
    if (length(wavelength(rsoil)) != nw)
      stop("Wavelength of rsoil must be 400 - 2500 nm with 1 nm resolution")
    if (!all(wavelength(rsoil) == c(400:2500)))
      stop("Wavelength of rsoil must be 400 - 2500 nm with 1 nm resolution")
    if (nspectra(rsoil) > 1)
      warning("More than one spectrum in Speclib rsoil. Only the first one will be used for simulation")
    rsoil <- spectra(rsoil)[1,]
  } else {
    rsoil <- rep.int(-9999.9, nw)
  }
  
  if (TypeLidf != 1) TypeLidf <- 2
    
  if (TypeLidf == 2) lidfb <- 0
  
  storage.mode(nw)       <- "integer" 
  storage.mode(N)        <- "double"
  storage.mode(Cab)      <- "double"
  storage.mode(Car)      <- "double"
  storage.mode(Cbrown)   <- "double"
  storage.mode(Cw)       <- "double"
  storage.mode(Cm)       <- "double"
  storage.mode(psoil)    <- "double"
  storage.mode(LAI)      <- "double"
  storage.mode(TypeLidf) <- "integer" 
  storage.mode(lidfa)    <- "double"
  storage.mode(lidfb)    <- "double"
  storage.mode(hspot)    <- "double"
  storage.mode(tts)      <- "double"
  storage.mode(tto)      <- "double"
  storage.mode(psi)      <- "double"
  storage.mode(RT)       <- "double"  
  storage.mode(rsoil)    <- "double"  
  
  extern <- .Fortran("prosail2r",
                     Cab=Cab,
                     Car=Car,
                     Cbrown=Cbrown,
                     Cw=Cw,
                     Cm=Cm,
                     N=N,
                     psoil=psoil,
                     LAI=LAI,
                     TypeLidf=TypeLidf,
                     lidfa=lidfa,
                     lidfb=lidfb,
                     hspot=hspot,
                     tts=tts,
                     tto=tto,
                     psi=psi,
                     reflectance=RT,
                     rsoil=rsoil,
                     PACKAGE="hsdar"
             )
  spec <- speclib(wavelength=c(1:nw)+399,
                  spectra=extern$reflectance
                 )
  return(spec)
}
