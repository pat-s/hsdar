# setClass(".ValidDataFromRaster",
#          representation(
#            removedPixel = "numeric",
#            validPixel   = "logical"
#          ),
#          prototype(
#            removedPixel = 0,
#            validPixel   = TRUE
#          )
#          )

setClass(".Spectra",
         representation(
           fromRaster = "logical",
           spectra_ma = "matrix",
           spectra_ra = 'RasterBrick'#,
#            valid_spec = '.ValidDataFromRaster'
         ),
         prototype(
           fromRaster = FALSE,
           spectra_ma = matrix(),
           spectra_ra = new("RasterBrick")#,
#            valid_spec = new(".ValidDataFromRaster")
         )
)

setClass("Speclib",
         representation(


#' Handling spectra
#' 
#' Returning and setting spectra in Speclib
#' 
#' For `spectra<-`, the function does not check if dimensions of spectra
#' match dimensions of Speclib. Additionally, no conversion into `matrix`
#' is performed! If spectra are not correctly stored, errors in other functions
#' may arise. Thus check always carefully, if spectra are modified by hand.
#' 
#' @aliases spectra.Speclib spectra spectra<- spectra,Speclib-method
#' spectra<-,Speclib,data.frame-method spectra<-,Speclib,matrix-method
#' spectra<-,Speclib,numeric-method spectra<-,Speclib,RasterBrick-method
#' [,.Spectra,ANY,ANY,ANY-method show,.Spectra-method print,.Spectra-method
#' cellFromCol,Speclib-method cellFromLine,Speclib-method
#' cellFromPolygon,Speclib-method cellFromRow,Speclib-method
#' cellFromRowCol,Speclib-method cellFromRowColCombine,Speclib-method
#' cellFromXY,Speclib-method colFromX,Speclib-method
#' fourCellsFromXY,Speclib-method rowFromY,Speclib-method
#' readAll,Speclib-method
#' @param object Object of class `Speclib`.
#' @param i Index of spectra to return. If missing all spectra are returned.
#' @param j Index of bands to return. If missing all bands are returned.
#' @param ...  Passed to internal function. Currently only one parameter is
#' accepted: `return_names`: Logical indicating, if names of columns and
#' rows should be set to [bandnames()] and [idSpeclib()].
#' @param value Matrix or RasterBrick-object containing spectral values. If
#' value is a matrix, columns are band values and rows are spectra.
#' @return For `spectra<-`, the updated object. Otherwise a matrix of the
#' spectra in x is returned.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}
#' @keywords utilities
#' @examples
#' 
#' data(spectral_data)
#' 
#' ## Manual plot of the first spectrum
#' plot(wavelength(spectral_data), spectra(spectral_data)[1,], type="l")
#' 
#' @export spectra
           spectra = ".Spectra", 


#' Handling wavelength and fwhm
#' 
#' Returning and setting wavelength and full-width-half-max (fwhm) values in
#' Speclib and HyperSpecRaster
#' 
#' 
#' @aliases wavelength wavelength<- wavelength,Speclib-method
#' wavelength<-,Speclib,data.frame-method wavelength<-,Speclib,numeric-method
#' fwhm<- fwhm<-,Speclib,numeric-method fwhm fwhm,Speclib-method
#' wavelength,HyperSpecRaster-method
#' wavelength<-,HyperSpecRaster,numeric-method
#' @param object Object of class `Speclib` or `HyperSpecRaster`.
#' @param value Numeric vector or data.frame containing wavelength values.
#' @return For `wavelength<-` and `fwhm<-`, the updated object.
#' Otherwise a numeric vector of the wavelength and fwhm-values is returned.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}, \code{\linkS4class{HyperSpecRaster}}
#' @keywords utilities
#' @examples
#' 
#' data(spectral_data)
#' 
#' wavelength(spectral_data)
#' 
#' @export wavelength
           wavelength = "numeric",
           attributes = "data.frame",
           fwhm = "numeric",
           continuousdata = "logical",
           wlunit = "character",
           xlabel = "character",
           ylabel = "character",
           ID = "character",
           wavelength.is.range = "logical",
           transformation = "character",


#' History of usage
#' 
#' Handling history of usage of Speclibs
#' 
#' 
#' @aliases usagehistory usagehistory<-
#' @param x Object of class Speclib
#' @param value Character string to be added to usagehistory or NULL, if
#' usagehistory should be deleted.
#' @return For `usagehistory<-`, the updated object. Otherwise a vector
#' giving the history of usage of Speclib is returned.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}
#' @keywords utilities
#' @examples
#' 
#' data(spectral_data)
#' 
#' ## Return history of usage
#' usagehistory(spectral_data)
#' 
#' ## Deleting history of usage
#' usagehistory(spectral_data) <- character() 
#' spectral_data
#' 
#' ## Adding entries
#' usagehistory(spectral_data) <- "New entry" ## Adding new entry
#' usagehistory(spectral_data) <- "New entry 2" ## Adding second entry
#' spectral_data
#' 
#' 
#' @export usagehistory
           usagehistory = "character",
           rastermeta = "list"
         ),
         prototype(
           spectra = new(".Spectra"),                   
           wavelength = numeric(),
           attributes = data.frame(),
           fwhm = 1,
           continuousdata = TRUE,
           wlunit = "nm",
           xlabel = "Wavelength",
           ylabel = "Reflectance",
           ID = character(),
           wavelength.is.range = FALSE,
           transformation = "NONE",
           usagehistory = "",
           rastermeta = list()
         ),
         validity = function(object)
         {
           c1 <- TRUE
           if (nrow(object@attributes) > 0)
           {
             c1 <- nrow(object@attributes) == nrow(object@spectra)
             if (!c1)
             {
               stop("Invalid attribute data.frame for spectra")
             }
           }
           c2 <- ncol(object@spectra) == length(object@wavelength)
           if (!c2)
           {
             stop("Invalid wavelength vector for spectra")
           }
           return(c1 & c2)
         }
)

setClass('HyperSpecRaster',
         contains = 'RasterBrick',
         representation(
           wavelength = 'numeric',
           fwhm       = 'numeric',
           attributes = 'data.frame'
         ),
         prototype (
           wavelength = numeric(),
           fwhm       = numeric(),
           attributes = data.frame()
                   )
        )

setClass("DistMat3D",
         representation(
           values = "numeric",
           ncol = "numeric",
           nlyr = "numeric"
         ),
         prototype(
           values = numeric(),
           ncol = 0,
           nlyr = 0
         ),
         validity = function(object)
         {
           if (length(object@values) != object@nlyr * (sum(1:object@ncol)-object@ncol))
             stop("Length of values do not fit dimensions of matrix")
         }
)

setClass("Nri",
         representation(
           nri = "DistMat3D",
           fwhm = "numeric",
           wavelength = "numeric",
           dimnames = "list",
           multivariate = "list",
           attributes = "data.frame",
           usagehistory = "character"
         ),
         prototype(
           nri = new("DistMat3D", values = numeric(), nlyr = 0, ncol = 0),
           fwhm = 0,
           wavelength = 0,
           dimnames = list(),
           multivariate = list(),                   
           attributes = data.frame(),
           usagehistory = ""
         ),
         validity = function(object)
         {
           if (length(object@wavelength) != object@nri@ncol)
             stop("Length of wavelength do not fit dimensions of nri")
         }
)

setClass('Clman',
         contains = 'Speclib',
         representation(
           cp         = 'matrix',
           hull       = 'matrix'
         ),
         prototype (
           cp         = matrix(),
           hull       = matrix()
                   ),
         validity = function(object)
         {
           if (ncol(object@cp) != length(object@wavelength))
             stop("Number of bands in continuum points and length of wavelength differ")
           if (ncol(object@spectra) != length(object@wavelength))
             stop("Number of bands in spectra and length of wavelength differ")
           if (nrow(object@spectra) != nrow(object@cp))
             stop("Number of samples in spectra and continuum points differ") 
           return(TRUE)
         }
        )

setClass('Specfeat',
         contains = 'Speclib',
         representation(
           features      = 'list',
           featureLimits = 'list'
         ),
         prototype (
           features      = list(),
           featureLimits = list()
                   )#,
#          validity = function(object)
#          {
#            if (ncol(object@cp) != length(object@wavelength))
#              stop("Number of bands in continuum points and length of wavelength differ")
#            if (ncol(object@spectra) != length(object@wavelength))
#              stop("Number of bands in spectra and length of wavelength differ")
#            if (nrow(object@spectra) != nrow(object@cp))
#              stop("Number of samples in spectra and continuum points differ") 
#            return(TRUE)
#          }
        )
        

         
setClassUnion(".CaretHyperspectral", c("Speclib", "Nri", "Specfeat"))

if (!isGeneric("speclib")) {
  setGeneric("speclib", function(spectra, wavelength, ...)
  standardGeneric("speclib"))
}

if (!isGeneric("spectra")) {
  setGeneric("spectra", function(object, ...)
  standardGeneric("spectra"))
}
if (!isGeneric("spectra<-")) {
  setGeneric("spectra<-",function(object, value)
  standardGeneric("spectra<-"))
}

if (!isGeneric("mask")) {
  setGeneric("mask", function(object, ...)
  standardGeneric("mask"))
}
if (!isGeneric("mask<-")) {
  setGeneric("mask<-",function(object, value)
  standardGeneric("mask<-"))
}

if (!isGeneric("attribute")) {
  setGeneric("attribute", function(object)
  standardGeneric("attribute"))
}
if (!isGeneric("attribute<-")) {
  setGeneric("attribute<-", function(object, value)
  standardGeneric("attribute<-"))
}

if (!isGeneric("wavelength")) {
  setGeneric("wavelength", function(object, ...)
  standardGeneric("wavelength"))
}
if (!isGeneric("wavelength<-")) {
  setGeneric("wavelength<-",function(object, value)
  standardGeneric("wavelength<-"))
}

if (!isGeneric("fwhm")) {
  setGeneric("fwhm", function(object, ...)
  standardGeneric("fwhm"))
}
if (!isGeneric("fwhm<-")) {
  setGeneric("fwhm<-",function(object, value)
  standardGeneric("fwhm<-"))
}


if (!isGeneric("distMat3D")) {
  setGeneric("distMat3D",function(x, ...)
  standardGeneric("distMat3D"))
}

if (!isGeneric('HyperSpecRaster')) 
{
  setGeneric('HyperSpecRaster', function(x, wavelength, ...)
  standardGeneric('HyperSpecRaster')) 
}


if (!isGeneric("ncol")) {
  setGeneric("ncol", function(object, ...)
  standardGeneric("ncol"))
}

if (!isGeneric("nrow")) {
  setGeneric("nrow", function(object, ...)
  standardGeneric("nrow"))
}

if (!isGeneric("as.data.frame")) {
  setGeneric("as.data.frame")
}
