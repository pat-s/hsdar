#' Best performing model(s) with NRI
#' 
#' Get or mark best performing model(s) between narrow band indices and
#' environmental variables
#' 
#' See details in [glm.nri()] and [glm()].
#' 
#' @aliases nri_best_performance mark_nri_best_performance
#' @param nri Object of class nri
#' @param glmnri Object of class glmnri
#' @param n Number of models to return or mark
#' @param coefficient Name or index of coefficient to plot
#' @param predictor Name or index of term to plot
#' @param abs Use absolute value (e.g. for t-values)
#' @param findMax Find maximum or minimum values
#' @param best Output from `nri_best_performance`
#' @param uppertriang Flag to mark the upper triangle
#' @param ...  Further arguments passed to [glm()] function. These
#' must be the same as used for initial creation of [glm.nri()]. For
#' `mark_nri_best_performance` arguments are passed to
#' [polygon()].
#' @author Lukas Lehnert
#' @seealso [glm.nri()], [glm()]
#' @keywords multivariate
#' @examples
#' 
#' data(spectral_data)
#' 
#' ## Calculate all possible combinations for WorldView-2-8
#' spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
#'                               response_function = FALSE)
#' nri_WV <- nri(spec_WV, recursive = TRUE)
#' 
#' ## Build glm-models
#' glmnri <- glm.nri(nri_WV ~ chlorophyll, preddata = spec_WV)
#' 
#' ## Return best 5 models
#' BM <- nri_best_performance(glmnri, n = 5, coefficient = "p.value")
#' 
#' ## Get nri values for the 5 models
#' nri_BM <- getNRI(nri_WV, BM)
#' 
#' @export nri_best_performance
nri_best_performance <- function(nri,
                                 n = 1,
                                 coefficient = "p.value",
                                 predictor = 2,
                                 abs = FALSE,
                                 findMax = FALSE, ...)
{
  glmnri <- nri@multivariate
  coefficient <- glmnri[[which(names(glmnri)==coefficient)]]

  if (is.character(predictor))
    predictor <- which(dimnames(glmnri[[1]])[[1]]==predictor)
  coefficient <- as.matrix(coefficient, lyr = predictor)

  if (abs)
    coefficient <- abs(coefficient)

  if (findMax)
  {
    q <- quantile(coefficient, probs = 1-n/sum(1:(dim(coefficient)[1]-1)), names = FALSE, na.rm=TRUE)
  } else {
    q <- quantile(coefficient, probs = n/sum(1:(dim(coefficient)[1]-1)), names = FALSE, na.rm=TRUE)
  }

  wl1 <- matrix(data=rep.int(c(1:dim(coefficient)[1]),dim(coefficient)[1]),
                nrow=dim(coefficient)[1], ncol=dim(coefficient)[1], byrow = FALSE)
  wl2 <- matrix(data=rep.int(c(1:dim(coefficient)[1]),dim(coefficient)[1]),
                nrow=dim(coefficient)[1], ncol=dim(coefficient)[1], byrow = TRUE)

  if (findMax)
  {
    minvals <- data.frame(dim1=as.vector(wl1[(q-coefficient)<=0 & !is.nan(coefficient) & lower.tri(coefficient)]),
                          dim2=as.vector(wl2[(q-coefficient)<=0 & !is.nan(coefficient) & lower.tri(coefficient)]),
                          coefficient=as.vector(coefficient[(q-coefficient)<=0 & !is.nan(coefficient) & lower.tri(coefficient)])
                         )
    minvals <- minvals[order(minvals$coefficient, decreasing = TRUE),]
  } else {
    minvals <- data.frame(dim1=as.vector(wl1[(q-coefficient)>=0 & !is.nan(coefficient) & lower.tri(coefficient)]),
                          dim2=as.vector(wl2[(q-coefficient)>=0 & !is.nan(coefficient) & lower.tri(coefficient)]),
                          coefficient=as.vector(coefficient[(q-coefficient)>=0 & !is.nan(coefficient) & lower.tri(coefficient)])
                         )
    minvals <- minvals[order(minvals$coefficient),]
  }
  
  fun_name = if (is.null(attr(glmnri,"function"))) "glm" else attr(glmnri,"function")

  result <- vector("list", n)

  if (attr(glmnri,"is.predictor.nri"))
  {
    formula <- update.formula(attr(glmnri, "call"), . ~ predictor)
    for (i in 1:n)
    {
      glm_data <- data.frame(response = attr(glmnri, "response"),
                             predictor = nri$nri[minvals[i,1],minvals[i,2],]
                            )
      model = eval(parse(text = paste(fun_name,"(formula = formula, data = glm_data, ...)",
                                      sep = "")
                        )
                  )
      result[[i]] <- model
    }
  } else {
    formula <- update.formula(attr(glmnri, "call"), response ~ .)
    for (i in 1:n)
    {
      glm_data <- cbind(data.frame(response = nri$nri[minvals[i,1],minvals[i,2],]),
                        attr(glmnri, "predictors"))
      model = eval(parse(text = paste(fun_name,"(formula = formula, data = glm_data, ...)",
                                      sep = "")
                        )
                  )
      result[[i]] <- model
    }
  }
  minvals <- data.frame(Band_1=nri$wavelength[minvals$dim1], Band_2=nri$wavelength[minvals$dim2])
  if (n>1)
  {
    return(list(Indices=minvals[1:n,], Models=result[c(1:n)]))
  } else {
    return(list(Indices=minvals[1,], Models=result[[1]]))
  }
}




#' Return nri-values
#' 
#' Return normalized ratio index values giving the wavelength
#' 
#' Wavelength can be passed in three ways. As the result of
#' [nri_best_performance()], as a data frame with two columns or as a
#' vector of length 2. In the first two cases, the result will be a data frame
#' (if data frames contain more than one row) with the nri-values of each pair
#' of wavelengths. In the latter case it will be a vector.
#' 
#' @aliases getNRI print.getNRI
#' @param nri Object of class 'Nri'
#' @param wavelength Wavelength values where nri is returned. See details
#' section.
#' @author Lukas Lehnert
#' @seealso [nri()], \code{\linkS4class{Nri}}
#' @keywords multivariate
#' @examples
#' 
#' data(spectral_data)
#' 
#' ## Calculate all possible combinations for WorldView-2-8
#' spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
#'                               response_function = FALSE)
#' nri_WV <- nri(spec_WV, recursive = TRUE)
#' 
#' ## Build glm-models
#' glmnri <- glm.nri(nri_WV ~ chlorophyll, preddata = spec_WV)
#' 
#' ## Return best 5 models
#' BM <- nri_best_performance(glmnri, n = 5, coefficient = "p.value")
#' 
#' ## Get nri values for the 5 models
#' nri_BM <- getNRI(nri_WV, BM)
#' 
#' 
#' @export getNRI
getNRI <- function(nri, wavelength)
{
  returnNRI <- function(B)
  {
    nri_data <- get("nri_data", envir= environment_returnNRI)
    return(as.vector(t(nri_data[B[1],B[2],])))
  }

  if (is.list(wavelength))
  {
    if (!is.null(wavelength$Indices))
    {
      wavelength <- wavelength$Indices
    } else {
      stop("Could not determine bands of indices to return")
    }
  } else if (!is.data.frame(wavelength))
  {
    if (length(wavelength)!=2)
      stop("If wavelength is a vector, it must be of length 2")
    wavelength <- data.frame(B_1=wavelength[1], B_1=wavelength[2])
  }
  wavelength_sub <- data.frame(B1 = match(wavelength[,1], nri$wavelength),
                               B2 = match(wavelength[,2], nri$wavelength))

  environment_returnNRI <- new.env(parent = .GlobalEnv)

  assign("nri_data", nri$nri, environment_returnNRI)

  NRI_subset <- apply(wavelength_sub, MARGIN = 1, FUN = returnNRI)

  if (nrow(wavelength)>1)
  {
    NRI_subset <- as.data.frame(as.matrix(NRI_subset))

    row.names(NRI_subset) <- dimnames(nri$nri)[[3]]
    names(NRI_subset) <- paste("I_", wavelength[,1], ",", wavelength[,2], sep="")
  } else {
    NRI_subset <- as.vector(t(NRI_subset))
  }
  return(NRI_subset)
}

mark_nri_best_performance <- function(best, glmnri, n = nrow(best$Indices), uppertriang = FALSE,...)
{

  range.of.wavelength <- glmnri@fwhm
  if (length(range.of.wavelength) == 1)
  {
    best$Indices$Rangex <- range.of.wavelength
    best$Indices$Rangey <- range.of.wavelength
  } else {
    best$Indices$Rangex <- 0
    best$Indices$Rangey <- 0
    for (i in 1:n)
    {
      best$Indices$Rangex[i] <- range.of.wavelength[which(glmnri@wavelength == best$Indices[i,2])]
      best$Indices$Rangey[i] <- range.of.wavelength[which(glmnri@wavelength == best$Indices[i,1])]
    }
  }
  if (!uppertriang)
  {
    for (i in 1:n)
    {
      polygon(c(best$Indices[i,1]-best$Indices$Rangex[i]/2,
                best$Indices[i,1]-best$Indices$Rangex[i]/2,
                best$Indices[i,1]+best$Indices$Rangex[i]/2,
                best$Indices[i,1]+best$Indices$Rangex[i]/2),
              c(best$Indices[i,2]+best$Indices$Rangey[i]/2,
                best$Indices[i,2]-best$Indices$Rangey[i]/2,
                best$Indices[i,2]-best$Indices$Rangey[i]/2,
                best$Indices[i,2]+best$Indices$Rangey[i]/2),
              ...
            )
    }
  } else {
    for (i in 1:n)
    {
      polygon(c(best$Indices[i,2]-best$Indices$Rangex[i]/2,
                best$Indices[i,2]-best$Indices$Rangex[i]/2,
                best$Indices[i,2]+best$Indices$Rangex[i]/2,
                best$Indices[i,2]+best$Indices$Rangex[i]/2),
              c(best$Indices[i,1]+best$Indices$Rangey[i]/2,
                best$Indices[i,1]-best$Indices$Rangey[i]/2,
                best$Indices[i,1]-best$Indices$Rangey[i]/2,
                best$Indices[i,1]+best$Indices$Rangey[i]/2),
              ...
      )
    }
  }
}
