#' soilindex
#' 
#' Function calculates a variety of hyperspectral soil indices
#' 
#' Index must be a charater vector containing pre-defined indices (selected by
#' their name) or self defined indices or any combination of pre- and
#' self-defined indices. \subsection{Pre-defined indices The following indices
#' are available: \tabular{lll}{ \tab\tab\cr \strong{Name}\tab \strong{Formula}
#' \tab \strong{Reference*}\cr \tab\tab\cr BI_TM \tab \eqn{((TM\_1^2 + TM\_2^2+
#' TM\_3^2)/3)^{0.5}}** \tab Mathieu et al. (1998)\cr CI_TM \tab \eqn{(TM\_3 -
#' TM\_2)/ (TM\_3 + TM\_2)}** \tab Escadafal and Huete \cr \tab \tab (1991)\cr
#' HI_TM \tab \eqn{(2\cdot TM\_3 - TM\_2 - TM\_1)/ (TM\_2-TM\_1)}** \tab
#' Escadafal et al. (1994)\cr NDI \tab \eqn{(R_{840} - R_{1650})/(R_{840} +
#' R_{1650})} \tab McNairn, H. and Protz, R.\cr \tab \tab (1993)\cr NSMI \tab
#' \eqn{(R_{1800} - R_{2119})/(R_{1800} + R_{2119})} \tab Haubrock et al.
#' (2008)\cr RI \tab \eqn{R_{693}^2/(R_{447}\cdot R_{556}^3)} \tab Ben-Dor et
#' al. (2006)\cr RI_TM \tab \eqn{TM\_3^2 / (TM\_1 \cdot TM\_2^3)}** \tab
#' Madeira et al. (1997),\cr \tab \tab Mathieu et al. (1998)\cr SI_TM \tab
#' \eqn{(TM\_3 - TM\_1)/ (TM\_3 + TM\_1)}** \tab Escadafal et al. (1994) \cr
#' SWIR SI\tab \eqn{-41.59 \cdot (R_{2210} - R_{2090}) + } \tab Lobell et al.
#' (2001)\cr \tab \eqn{1.24 \cdot (R_{2280} - R_{2090}) + 0.64} \tab \cr }
#' 
#' * For references please type: \code{hsdardocs("References.pdf")}.\cr ** TM_1
#' denotes the first band of Landsat Thematic Mapper. Consequently, the
#' hyperspectral data is resmapled to Landsat TM using
#' \code{\link{spectralResampling}} prior to the calculation of the index. For
#' resampling, the spectral response function is used. }
#' \subsection{Self-defining indices Self-defined indices may be passed using
#' the following syntax: \itemize{ \itemRxxx: Reflectance at wavelength 'xxx'.
#' Note that R must be upper case.  \itemDxxx: First derivation of reflectance
#' values at wavelength 'xxx'. Note that D must be upper case. } Using this
#' syntax, complex indices can be easily defined. Note that the entire
#' definition of the index must be passed as one character string.
#' Consequently, the NSMI would be written as\cr "(R1800-R2119)/(R1800+R2119)".
#' }
#' 
#' @param x Object of class \code{Speclib}
#' @param index Character string. Name or definition of index or vector with
#' names/definitions of indices to calculate. See Details section for further
#' information.
#' @param returnHCR If TRUE, the result will be of class HyperSpecRaster,
#' otherwise it is a data frame. If "auto", the class is automatically
#' determined by passed Speclib.
#' @param weighted Logical indicating if reflectance values should be
#' interpolated to fit wavelength position. If \code{FALSE} the reflectance
#' values of nearest neighbour to passed position are returned. See
#' \code{\link[=get_reflectance.speclib]{get_reflectance}} for further
#' explanation.
#' @param ...  Further arguments passed to derivative functions. Only used for
#' indices requiring derivations.
#' @return A vector containing indices values. If index is a vector with length
#' > 1, a data frame with ncol = length(index) and nrow = number of spectra in
#' x is returned.
#' 
#' If function is called without any arguments, return value will be a vector
#' containing all available indices in alphabetical order.
#' @author Lukas Lehnert
#' @seealso \code{\link{vegindex}},
#' \code{\link[=get_reflectance.speclib]{get_reflectance}}
#' @references See \code{hsdardocs("References.pdf")}
#' @keywords multivariate
#' @examples
#' 
#' data(spectral_data)
#' ## Example calculating all available indices
#' ## Get available indices
#' 
#' avl <- soilindex()
#' vi <- soilindex(spectral_data, avl)
#' 
#' 
#' @export soilindex
soilindex <- function(
                      x,
                      index,
                      returnHCR = "auto",
                      weighted = TRUE,
                      ...
             )
{
soilindex_available <- function()
{
  av <- c("SWIR SI", "NSMI", "RI_TM", "RI",
          "NDI", "BI_TM", "SI_TM",
          "HI_TM", "CI_TM"
          )
  return(sort(av))
}  

return_index <- function(x)
{
  if (eval.parent(convertSpatialGrid))
  {
    spec <- speclib(x, 1)
    spec@rastermeta <- gridMeta
    result <- HyperSpecRaster(spec)
  }
  return (x)
}

if (length(names(match.call()))==0)
{
  return(soilindex_available())
}  

if (x@spectra@fromRaster)
  return(.blockwise(speclib_obj =  "x", pos = 1))

x_back <- x

if (!is.speclib(x))
  stop("x is not of class 'Speclib'")
if (!x@continuousdata)
  stop("x does not contain continuous spectra")
if (returnHCR == "auto")
  returnHCR <- .is.rastermeta(x)

convertSpatialGrid <- returnHCR
gridMeta <- x@rastermeta

if (returnHCR)
{
  if (!.is.rastermeta(x))
    stop("If returnHCR, x must contain meta information")
}

if (length(index)>1)
{
  result <- as.data.frame(matrix(data = NA,
                                 nrow = dim(x)[1],
                                 ncol = length(index)))
  for (i in 1:length(index))
  {
    temp <- soilindex(x, index[i], returnHCR=FALSE)
    if (!is.null(temp))
    {
      result[,i] <- temp
    }
  }
  if (nspectra(x) > 1)
  {
    names(result) <- index
    row.names(result) <- idSpeclib(x)
  }
  if (returnHCR)
  {
    spec <- speclib(result, c(1:ncol(result)))
    if (.is.rastermeta(x))
      spec@rastermeta <- x@rastermeta
    result <- HyperSpecRaster(spec)
  }
  return(result)
}

d_indexs <- c()
m <- c(rep.int(1,length(d_indexs)))

# index_current <<- index
# row_names_x <<- row.names(x$spectra)

if (any(index==d_indexs)) 
  x <- derivative.speclib(x, m=m[d_indexs==index], ...)

y <- spectra(x)
x <- wavelength(x)

## Pre-defined indices
if (index=="SWIR SI")
{
  return(return_index(-41.59* (get_reflectance(y,x,2210,weighted)-get_reflectance(y,x,2090,weighted)) +
                1.24*(get_reflectance(y,x,2280,weighted)-get_reflectance(y,x,2090,weighted)) + 0.64))
}

if (index=="RI_TM")
{
  x_TM <- try(spectra(spectralResampling(x_back, "Landsat5")), silent = TRUE)
  if (inherits(x_TM, "try-error"))
  {
    warning("Unable to resample to Landsat 5 for RI_TM calculation")
    return(NULL)
  }
  return(return_index(x_TM[,3]^2/(x_TM[,1]*x_TM[,2]^3)))
}

if (index=="BI_TM")
{
  x_TM <- try(spectra(spectralResampling(x_back, "Landsat5")), silent = TRUE)
  if (inherits(x_TM, "try-error"))
  {
    warning("Unable to resample to Landsat 5 for RI_TM calculation")
    return(NULL)
  }
  return(return_index(((x_TM[,1]^2+x_TM[,2]^2+x_TM[,3]^2)/3)^0.5))
}
if (index=="SI_TM")
{
  x_TM <- try(spectra(spectralResampling(x_back, "Landsat5")), silent = TRUE)
  if (inherits(x_TM, "try-error"))
  {
    warning("Unable to resample to Landsat 5 for RI_TM calculation")
    return(NULL)
  }
  return(return_index((x_TM[,3]-x_TM[,1])/(x_TM[,3]+x_TM[,1])))
}
if (index=="HI_TM")
{
  x_TM <- try(spectra(spectralResampling(x_back, "Landsat5")), silent = TRUE)
  if (inherits(x_TM, "try-error"))
  {
    warning("Unable to resample to Landsat 5 for RI_TM calculation")
    return(NULL)
  }
  return(return_index((2*x_TM[,3]-x_TM[,2]-x_TM[,1])/(x_TM[,2]-x_TM[,1])))
}
if (index=="CI_TM")
{
  x_TM <- try(spectra(spectralResampling(x_back, "Landsat5")), silent = TRUE)
  if (inherits(x_TM, "try-error"))
  {
    warning("Unable to resample to Landsat 5 for RI_TM calculation")
    return(NULL)
  }
  return(return_index((x_TM[,3]-x_TM[,2])/(x_TM[,3]+x_TM[,2])))
}
if (index=="NSMI")
{
  return(return_index((get_reflectance(y,x,1800,weighted) - get_reflectance(y,x,2119,weighted))/
                      (get_reflectance(y,x,1800,weighted) + get_reflectance(y,x,2119,weighted))))
}
if (index=="RI")
{
  return(return_index(get_reflectance(y,x,693,weighted)^2/
                      (get_reflectance(y,x,447,weighted) * get_reflectance(y,x,556,weighted)^3)))
}
if (index=="NDI")
{
  return(return_index((get_reflectance(y,x,840,weighted)-get_reflectance(y,x,1650,weighted))/
                      (get_reflectance(y,x,840,weighted)+get_reflectance(y,x,1650,weighted))))
}

## Self-defining indices
index <- gsub("R", "", gsub("(R[0-9]+)", "get_reflectance(y,x,\\1,weighted)", index, 
                            perl = TRUE)
              )
index <- gsub("D", "", gsub("(D[0-9]+)", "get_reflectance(spectra(derivative.speclib(x_back, m=1, ...)),x,\\1,weighted)", index, 
                            perl = TRUE)
              )
index_val <- try(return_index(eval(parse(text = index))), silent = TRUE)
if (inherits(index_val, "try-error"))
{
  cat("Error in self-defined index string or unimplemented index selected\n")
  cat("Index string evals to:\n")
  cat(paste(index, "\n"))
  return(NULL)
}  
return(index_val)
}
