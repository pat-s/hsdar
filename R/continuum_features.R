.get_transformation <- function(x)
{
  if (length(x@transformation) == 0)
    return(NA)
  if (x@transformation == "NONE")
    return(NA)
  return(x@transformation)
}



#' Definition of absorption features
#' 
#' Function sets the spectral range of absorption features.
#' 
#' Absorption features are defined as the area between local maxima in the
#' reflectance spectra.  This function adds the information of the feature
#' limits to the \code{Speclib}.  Thus, it is a pre-processing step to isolate
#' features.
#' 
#' @param x Object of class \code{Speclib} containing the band depth or ratio
#' transformed reflectance spectra.
#' @param tol The tolerance of the band depth which defines a wavelength as a
#' start or end point of a feature.  Usually a band depth of 0 or a ratio of 1
#' indicates feature limits, however, better results are achieved if slightly
#' deviating values are tolerated.
#' @param FWL Optional. If passed, result is directly converted into
#' \code{\linkS4class{Specfeat}}. A vector containing one wavelength per
#' feature to be isolated, e.g. the major absorption features. Features which
#' include these specified wavelengths will be isolated.
#' @return The updated \code{\linkS4class{Speclib}} containing additional
#' information about the feature limits. If \code{FWL} is not \code{NULL},
#' result will be of class \code{\linkS4class{Specfeat}}.
#' @author Hanna Meyer and Lukas Lehnert
#' @seealso \code{\link{transformSpeclib}}, \code{\link{specfeat}},
#' \code{\linkS4class{Specfeat}}
#' @examples
#' 
#' % \dontrun{
#' data(spectral_data)
#' 
#' ##Example to define feature limits
#' bd <- transformSpeclib(subset(spectral_data, season == "summer"), 
#'                        method = "sh", out = "bd")
#' 
#' ## Define features
#' features <- define.features(bd)
#' % }
#' 
#' @export define.features
define.features <- function(
                            x,    
                            tol = 1.0e-7,
                            FWL = NULL
                           )
{
  if (!is.speclib(x)) 
    stop("x must be of class 'Speclib'")

  if (is.na(.get_transformation(x)))
    stop("x must be Speclib containing transformed spectra")
  if (x@transformation == "difference")
    stop("Difference values for transformation not useable for feature definition")
  
  if (x@transformation == "ratio") 
  {
    continuumValue = 1.0
  } else {
    continuumValue = 0.0
  }
  
  result <- x
  y      <- spectra(x)
  
  minwl <- min(x@wavelength)
  maxwl <- max(x@wavelength)
  
  
  featureLimits <- NULL
  
  for (i in 1:nrow(y))
  {
    seppoints <- x@wavelength[abs(y[i,]-continuumValue)<=tol]
    seppoints <- seppoints[!is.na(seppoints)]
    
    if (!any(seppoints==minwl))
      seppoints <- c(minwl,seppoints)
    if (!any(seppoints==maxwl))
      seppoints <- c(maxwl,seppoints)
    
    featureLimits[[i]] <- data.frame(ll=seppoints[-length(seppoints)],ul=seppoints[-1])
  }
  attr(result, "featureLimits") <- featureLimits
  if (is.null(FWL)) 
  {
    return(result) 
  } else {
    return(specfeat(result, FWL))
  }
}




#' Function to isolate spectral features
#' 
#' Function isolates specified absorption features previously identified by
#' \code{\link{define.features}}.
#' 
#' 
#' @aliases specfeat plot,Specfeat-method
#' @param x Object of class \code{Speclib} containing the band depth or ratio
#' transformed reflectance spectra with additional information on feature
#' limits calculated by \code{\link{define.features}}. For plot this must be
#' object of class \code{specfeat}.
#' @param FWL A vector containing one wavelength per feature to be isolated,
#' e.g. the major absorption features. Features which include these specified
#' wavelengths will be isolated.
#' @param fnumber Index of feature(s) to be plotted.
#' @param stylebysubset Name of variable to be used as grouping factor. May be
#' selected from attributes table, groups or subgroups and must be convertible
#' to \code{factors}.
#' @param changecol Flag, if line colour should be varied among groups
#' @param changetype Flag, if line styles should be varied among groups
#' @param autolegend Flag if, legend is printed automatically.
#' @param new Create new plot or add data to existing one.
#' @param ...  Further arguments passed to plot function.
#' @return An object of class \code{Specfeat} containing the isolated features.
#' @author Hanna Meyer and Lukas Lehnert
#' @seealso \code{\link{define.features}}, \code{\link{cut_specfeat}},
#' \linkS4class{Specfeat}
#' @keywords classes aplot
#' @examples
#' 
#' % \dontrun{
#' data(spectral_data)
#' 
#' ## Transform speclib
#' bd <- transformSpeclib(spectral_data, method = "sh", out = "bd")
#' 
#' ## Define features automatically
#' features <- define.features(bd)
#' 
#' ##Example to isolate the features around 450nm, 700nm, 1200nm and 1500nm.
#' featureSelection <- specfeat(features, c(450,700,1200,1500))
#' 
#' ## Plot features
#' plot(featureSelection, 1:4)
#' 
#' ## Advanced plotting example
#' plot(featureSelection, 1:4, stylebysubset = "season")
#' 
#' plot(featureSelection, 1:4, stylebysubset = "season", changecol = FALSE, 
#'      changetype = TRUE)
#' % }
#' 
#' @export specfeat
specfeat <- function(
                     x, 
                     FWL
                    )
{
  if (class(x)!="Speclib") 
    stop("x must be of class 'Speclib'")
  
  if (is.null(attr(x, "featureLimits")))
    stop("x does not contain feature limits!\nPlease run define.features(x)")
    
  setmask <- .isMasked(x)
  
  if (setmask)
  {
    dropped <- mask(x)
    x <- interpolate.mask(x)
    for (i in 1:nrow(dropped))
      spectra(x)[,x$wavelength >= dropped[i,1] & x$wavelength <= dropped[i,2]] <- NA
  }
  
  y       <- spectra(x)
  feature <- as(x, 'Specfeat')
  
  usagehistory(feature) <- "Extract feature(s)"
  
  featureLimits <- attr(x, "featureLimits")
  
  featurelist <- NULL
  k=1
  for (i in 1:length(FWL))
  {
    featurelist[[k]]      <- list()
    names(featurelist)[k] <- paste("f",FWL[i],sep="")
    
    for (m in 1:nrow(y))
    {
      ll <- featureLimits[[m]]$ll[featureLimits[[m]]$ll<=FWL[i] & 
                                  featureLimits[[m]]$ul>FWL[i]]
      ul <- featureLimits[[m]]$ul[featureLimits[[m]]$ll<=FWL[i] &
                                  featureLimits[[m]]$ul>FWL[i]]
      wf <- y[m,which(x$wavelength==ll):which(x$wavelength==ul)]
      featurelist[[k]][[m]] <- list(y=as.vector(as.matrix(wf)),x1=ll)
      names(featurelist[[k]])[m] <- idSpeclib(x)[m]
    }
    k=k+1
  }
  feature@features <- featurelist
  feature@featureLimits <- featureLimits
  return(feature)
}



#' Cut absorption features
#' 
#' Function cuts absorption features to a user-specified range.
#' 
#' 
#' @param x An object of class "Specfeat" containing isolated features
#' determined by \code{\link{specfeat}}.
#' @param fnumber A vector of the positions of the features in x to be cut.
#' @param limits A vector containing the start and end wavelength for each
#' fnumber.  The corresponding feature will be cut to this specified range.
#' @param ...  Further arguments passed to generic functions. Currently
#' ignored.
#' @return An object of class \code{\linkS4class{Specfeat}} containing the cut
#' features.
#' @author Hanna Meyer and Lukas Lehnert
#' @seealso \code{\link{define.features}}, \code{\link{specfeat}},
#' \code{\linkS4class{Specfeat}}
#' @examples
#' 
#' data(spectral_data)
#' 
#' ##Example to cut the features around 450nm and 700nm to a specific range
#' ## Transform speclib
#' bd <- transformSpeclib(subset(spectral_data, season == "summer"),
#'                        method = "sh", out = "bd")
#' 
#' ## Define features
#' features <- define.features(bd)
#' 
#' ## Convert speclib to specfeat giving center wavelength of features
#' featureSelection <- specfeat(features, c(450,700,1200,1500))
#' 
#' ## Cut 1st and 2nd feature to [310 nm, 560 nm] and [589 nm, 800 nm]
#' featuresCut <- cut_specfeat(x = featureSelection, fnumber = c(1,2), 
#'                             limits = c(c(310, 560), c(589, 800)))
#' 
#' ## Plot result (1st and 2nd feature)
#' plot(featuresCut, fnumber = 1:2)
#' 
#' @export cut_specfeat
cut_specfeat <- function(
                         x, ..., 
                         fnumber,
                         limits
                        )
{
  if (class(x)!="Specfeat") 
    stop("x must be of class 'Specfeat'")
  
  if (length(limits)!=length(fnumber)*2)
    stop("Error in limits: for each fnumber min and max must be specified")
  
  m = 0
  for (i in fnumber)
  {
    m = m + 1
    for (k in 1:length(x@features[[i]]))
    {
      xval <- which(x@wavelength==x@features[[i]][[k]]$x1)
      xval <- x@wavelength[xval:(length(x@features[[i]][[k]]$y)+xval-1)]
      y    <- xval>=limits[2*(m-1)+1] & xval<=limits[2*(m-1)+2]
      
      if (any(y))
      {
        x@features[[i]][[k]]$y  <- x@features[[i]][[k]]$y[y]
        x@features[[i]][[k]]$x1 <- xval[y][1]
      } else {
        x@features[[i]][[k]]$y  <- NaN
        x@features[[i]][[k]]$x1 <- limits[2*(m-1)+1]
      }      
    }
  }
  return(x)
}

.range.specfeat <- function(x, ..., na.rm = TRUE, finite = FALSE)
{
  apply_getMinMax_specfeat <- function(x)
  {
    minvalue <- get("minvalue", envir= environment_apply)
    maxvalue <- get("maxvalue", envir= environment_apply)
    if (any(is.finite(x$y)))
    {
      if (minvalue > min(x$y, na.rm = TRUE)) 
        assign("minvalue", 
              min(x$y, na.rm = get("na.rm", envir= environment_apply)),
              envir= environment_apply) 
      if (maxvalue < max(x$y, na.rm = TRUE))
        assign("maxvalue", 
              max(x$y, na.rm = get("na.rm", envir= environment_apply)),
              envir= environment_apply) 
    }
  }
  
  x <- x@features
  
  environment_apply <- new.env(parent = .GlobalEnv)
  assign("minvalue", min(x[[1]][[1]][[1]], na.rm = TRUE), environment_apply)
  assign("maxvalue", max(x[[1]][[1]][[1]], na.rm = TRUE), environment_apply)
  assign("na.rm", na.rm, environment_apply)

  
  
  for (i in 1:length(x))
    test <- lapply(x[[i]], FUN = apply_getMinMax_specfeat)
    
  return(c(get("minvalue", envir= environment_apply), 
           get("maxvalue", envir= environment_apply)))
}


setMethod("plot", signature(x = "Specfeat"), 
 function(
                          x,
                          fnumber=1,
                          stylebysubset=NULL,
                          changecol=TRUE,
                          changetype=FALSE,
                          autolegend=TRUE,
                          new=TRUE,
                          ...
                         )
{
  if (class(x) != "Specfeat") 
    stop("x must be of class 'Specfeat'")
  
  Reflectance <- .range.specfeat(x)
  Reflectance <- c(rep.int(Reflectance[1],length(x@wavelength)-1), Reflectance[2])  
  Wavelength  <- x@wavelength
  

  spectra <- c(1:length(x@features[[fnumber[1]]]))
  
  call_dots <- match.call()
  if (any(names(call_dots)=="col"))
  {
    stylebysubset = c(1:length(spectra))
    changecol = TRUE
  }
  if (any(names(call_dots)=="lty"))
  {
    stylebysubset = c(1:length(spectra))
    changetype = TRUE
  }
  stylecol <- rep.int("black",length(spectra))
  styletype <- rep.int("solid",length(spectra))
  
  if (mode(stylebysubset)!="NULL")
  {
    if (! any(c(changecol,changetype)))
      warning("At least on of 'changecol' and 'changetype' should be TRUE")
    if (length(stylebysubset)==1)
    {
      i <- which(names(x@attributes)==stylebysubset)
      if (length(i)==0)
      {
        stop(paste(stylebysubset,"not found in attributes of x"))
      } else {


#' Handling attributes of spectra
#' 
#' Returning and setting attributes of spectra in Speclib or Nri.%
#' \code{setattributes} is a more comfortable and save function to set
#' attributes of speclib using string matching.
#' 
#' 
#' @aliases attribute attribute.speclib attribute<- attribute,Speclib-method
#' attribute<-,Speclib,data.frame-method attribute<-,Speclib,matrix-method
#' attribute,Nri-method attribute<-,Nri,data.frame-method
#' attribute<-,Nri,matrix-method
#' @param object Object of class \code{Speclib} or \code{Nri}.
#' @param value Data frame with \code{nrow(value) == nspectra(object)}, or
#' NULL.
#' @return For \code{attribute<-}, the updated object. \code{attribute} returns
#' a data frame with attribute data.% The return value of \code{setattributes}
#' is an object of class speclib.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}, \code{\linkS4class{Nri}}
#' @keywords utilities
#' @examples
#' 
#' data(spectral_data)
#' 
#' ## Returning attributes
#' attribute(spectral_data)
#' 
#' @export attribute
          attribute <- x@attributes[,i]
      }
      if (is.vector(attribute) | is.factor(attribute))
      {
        lev <- as.factor(as.character(attribute[spectra]))
        lev <- levels(lev)
        if (changecol)
        {
          k = 1
          for (i in lev)
          {
            stylecol[lev==i] <- k
            k = k + 1
          }
        }
        if (changetype)
        {
          k = 1
          if (length(lev)>6)
          {
            ls <- rep.int(c(1:6),ceiling(length(lev)/6))
          } else {
            ls <- c(1:6)
          }
          for (i in lev)
          {
            styletype[lev==i] <- ls[k]
            k = k + 1
          }
        }
      } else {
        warning(paste(stylebysubset,"must point to a vector"))
      }

      
      if (changecol)
        stylecol <- as.numeric(stylecol) 
      if (changetype)
        styletype <- as.numeric(styletype)

    } else {
      if (is.vector(stylebysubset))
      {
        if (changecol)
        {
          if (any(names(call_dots)=="col"))
            stylebysubset = rep.int(as.character(call_dots)[names(call_dots)=="col"],length(stylebysubset))
          stylecol <- stylebysubset
        }
        if (changetype)
        {
          if (any(names(call_dots)=="lty"))
            stylebysubset = rep.int(as.character(call_dots)[names(call_dots)=="lty"],length(stylebysubset))
          styletype <- stylebysubset
        }
      } else {
        if (is.data.frame(stylebysubset))
        {
          if (changecol)
          {
            if (mode(stylebysubset$col)!="NULL")
            {
              stylecol <- stylebysubset$col
            } else {
              warning("Could not determine color in data.frame stylebysubset")
            }
          }
          if (changetype)
          {
            if (mode(stylebysubset$type)!="NULL")
            {
              stylecol <- stylebysubset$type
            } else {
              warning("Could not determine type in data.frame stylebysubset")
            }
          }
        } else {
          warning(paste(stylebysubset,"must be a vector or a data.frame"))   
        }
      }
      autolegend <- FALSE
    }
  } else {
    autolegend <- FALSE
  }
  
  if (autolegend)
  {
    p <- par("oma")
    if (p[4]<4)
    {
      p[4] <- p[4] + 4
      par(oma=p)
    }
  } 
  
  if (new)
    plot(Wavelength,Reflectance,type="n",...)

  p <- par("usr")

  for (i in fnumber)
  {
    m = 1
    for (k in spectra)
    {
      xval <- which(x@wavelength==x@features[[i]][[k]]$x1)
      xval <- x@wavelength[xval:(length(x@features[[i]][[k]]$y)+xval-1)]
      lines(xval,x@features[[i]][[k]]$y, col=stylecol[m], lty=styletype[m])
      m = m + 1
    }
  }
  if (autolegend)
  {
    if (changecol)
    {
      changecol <- c(1:length(lev))
    } else {
      changecol <- rep.int("black",length(lev))
    }
    if (changetype)
    {
      changetype <- c(1:length(lev))
    } else {
      changetype <- rep.int("solid",length(lev))
    }
    par(new=TRUE,xpd=NA)
    legend(x=p[2]+(p[2]-p[1])*0.01,y=p[4],legend=lev,col=changecol, lty=changetype)
  }
}
) 



#' Band depth ratio indices
#' 
#' Calculate band depth ratio indices for objects of class \code{Specfeat}.
#' 
#' Method \code{"bdr"} calculates the normalised band depth ratio as
#' \deqn{bdr=\frac{BD}{Dc},} with \eqn{BD} is the band depth calculated by
#' \code{\link{transformSpeclib}} and Dc is the maximum band depth called band
#' centre.  Method \code{"ndbi"} calculates the the normalised band depth index
#' as \deqn{ndbi= \frac{BD-Dc}{BD+Dc}.} Method \code{"bna"} calculates the band
#' depth normalised to band area as \deqn{bna=\frac{BD}{Da},} where \eqn{Da} is
#' the area of the absorption feature (see \code{\link{feature_properties}}).
#' For further information see Mutanga and Skidmore (2004).
#' 
#' @param x Object of class \code{Specfeat}.
#' @param fnumber Integer. Index of feature to modify.
#' @param index Method to be applied. Currently, \code{"bdr"}, \code{"ndbi"}
#' and \code{"bna"} are available.
#' @return Object of class \code{specfeat} containing the updated version of x.
#' @author Lukas Lehnert and Hanna Meyer
#' @seealso \code{\link{transformSpeclib}}, \code{\link{define.features}},
#' \code{\link{specfeat}}
#' @references Mutanga, O. and Skidmore, A. (2004): Hyperspectral band depth
#' analysis for a better estimation of grass biomass (\var{Cenchrus ciliaris})
#' measured under controlled laboratory conditions. International Journal of
#' applied Earth Observation and Geoinformation, 5, 87-96
#' @keywords utilities
#' @examples
#' 
#' % \dontrun{
#' data(spectral_data)
#' 
#' ## Transform speclib
#' bd <- transformSpeclib(subset(spectral_data, season == "summer"),
#'                        method = "sh", out = "bd")
#' 
#' ## Define features automatically
#' features <- define.features(bd)
#' 
#' ## Isolate the features around 450nm, 700nm, 1200nm and 1500nm and 
#' ## convert to specfeat.
#' featureSelection <- specfeat(features, c(450,700,1200,1500))
#' 
#' ## Plot features
#' plot(featureSelection,1:4)
#' 
#' ## Calculate normalized band depth index for first feature
#' featureSelection_bdri <- bdri(featureSelection, 1, index = "ndbi")
#' 
#' ## Plot result
#' plot(featureSelection_bdri)
#' % }
#' 
#' @export bdri
bdri <- function(
                 x,
                 fnumber,
                 index="ndbi"
                )
{
  make.feature.list <- function(x,fnumber,feature)  
  {
    k=1
    for (i in 1:length(x@features[[fnumber]]))
    {
      x@features[[fnumber]][[i]]$y <- feature$y[k:(feature$len[i]+k-1)]
      k = feature$len[i] + k +1
    }
    return(x)
  }

  getfeaturelengths <- function(feature)
  {
    lenval <- 1:length(feature)*0
    for (i in 1:length(feature))
    {
      lenval[i] <- length(feature[[i]]$y)
    }
    return(lenval)
  }
  
  if (class(x) != "Specfeat") 
    stop("x must be of class 'Specfeat'")
  
  if (!any(index==c("bdr","ndbi","bna")))
    stop(paste("Unknown index '",index,"'",sep=""))
  
  for (i in fnumber)
  {
    lenval  <- getfeaturelengths(x@features[[i]])  
    feature <- as.vector(unlist(x@features[[i]]))
    
    storage.mode(lenval)  <- "integer"
    storage.mode(feature) <- "double"


    external <- .Fortran(index,
                         n=as.integer(length(lenval)),
                         ny=as.integer(length(feature)),
                         lenval=lenval,
                         y=feature,
                         PACKAGE="hsdar"
                        )
    
    external <- list(y=external$y,len=external$lenval)                

    x <- make.feature.list(x,i,external)
  }
  return(x)  
}

.get.feature.wavelength <- function(x)
{
  wl <- wavelength(x)
  lapply(x$features, function(x, wl) lapply(x, function(x, wl) 
    { 
      tmp <- wl[c(which(wl == x$x1):length(wl))]
      return(tmp[1:length(x$y)])
    }, wl), wl)
}

.get.rep.feature.parts <- function(x, speclib)
{  
  res <- lapply(x, function(x, wl)
    {
      res <- lapply(x, function(x, wl)
        { 
          match(wl, x, nomatch = NA)
        }, wl)
      return(matrix(unlist(res), ncol = length(wl), byrow = TRUE))
    }, wavelength(speclib))
  return(list(matches = res, wl = .get.rep.wavelength(res)))
}

.get.rep.wavelength <- function(x)
{
  lapply(x, function(x) apply(x, 2, function(x) all(is.finite(x))))
}

setMethod("as.data.frame", signature(x = "Specfeat"), 
          function(x, row.names = NULL, optional = FALSE, ...)
  {
    wl <- .get.feature.wavelength(x)
    rep <- .get.rep.feature.parts(wl, x)
    res <- matrix(ncol = 0, nrow = nspectra(x))
    for (i in 1:length(x$features))
    {
      res <- cbind(res, t(apply(matrix(1:length(x$features[[i]]), ncol = 1), 1,
            function(k, feat, rep_k, rep_wl)
            {
              feat<- feat[[k]]$y
              return(feat[rep_k[k,rep_wl]])
            }, x$features[[i]], rep[[1]][[i]], rep[[2]][[i]])))
    }
    res <- as.data.frame(res, row.names = row.names, optional = optional, ...)
    names(res) <- paste("V", unlist(lapply(rep[[2]], function(x, wl) wl[x], wavelength(x))), sep = "_")
    return(res)

  }
)

.as.speclib.specfeat <- function(x)
{
  wl <- .get.feature.wavelength(x)
  rep <- .get.rep.feature.parts(wl, x)
  res <- matrix(ncol = 0, nrow = nspectra(x))
  for (i in 1:length(x$features))
  {
    res <- cbind(res, t(apply(matrix(1:length(x$features[[i]]), ncol = 1), 1,
          function(k, feat, rep_k, rep_wl)
          {
            feat<- feat[[k]]$y
            return(feat[rep_k[k,rep_wl]])
          }, x$features[[i]], rep[[1]][[i]], rep[[2]][[i]])))
  }
  wl <- unlist(lapply(rep[[2]], function(x, wl) wl[x], wavelength(x)))
  
  wavelength(x) <- wl
  spectra(x) <- res
  
  x@featureLimits <- list()
  x@features <- list()
  class(x) <- "Speclib"
  return(x)
}
