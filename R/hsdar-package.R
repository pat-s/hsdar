

#' Apply function for class DistMat3D
#' 
#' Apply function to values in a 3-D distance matrix
#' 
#' The specified function is either applied to the distances of all samples
#' (MARGIN = 1) or to all distances for each sample (MARGIN = 3). In the first
#' case, if X would be replaced by an array of same dimensions the return value
#' would be equal if the following code is applied:
#' 
#' `apply(X, MARGIN = c(1,2), FUN)`,
#' 
#' where X is an array (see examples).
#' 
#' @aliases apply.DistMat3D apply,DistMat3D-method
#' @param X Object of class 'DistMat3D'.
#' @param MARGIN A vector giving the subscripts (dimensions) of the
#' DistMat3D-object which the function will be applied over (see details).
#' @param FUN Function to be applied. Matched with [match.fun()].
#' @param ...  Further arguments passed to FUN.
#' @return Depending on the length of the return value of the specified
#' function, objects of classes numeric or matrix are returned.
#' @author Lukas Lehnert
#' @seealso [apply()], [match.fun()],
#' \code{\linkS4class{DistMat3D}}
#' @keywords utilities
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
#' class(nri_WV@nri)
#' 
#' ## Calculate mean value of all samples for all indices 
#' meanIndexVals <- apply(nri_WV@nri, MARGIN = 1, FUN = mean)
#' meanIndexVals
#' 
#' ## Same but for array
#' nri_WV_dat <- as.array(nri_WV@nri)
#' meanIndexVals_arr <- apply(nri_WV_dat, MARGIN = c(1, 2), FUN = mean)
#' 
#' meanSampleVals <- apply(nri_WV@nri, MARGIN = 3, FUN = mean)
#' meanSampleVals_arr <- apply(nri_WV_dat, MARGIN = 3, FUN = mean)
#' 
NULL





#' Apply function for class Speclib
#' 
#' Apply function over all spectra or a subset of spectra
#' 
#' 
#' @aliases apply.Speclib apply,Speclib-method
#' @param X Object of class `Speclib`
#' @param FUN Function to be applied. Matched with [match.fun()].
#' @param byattributes Character string giving the name of the column in the
#' attributes to be used as subsets to apply function FUN on.
#' @param ...  Further arguments passed to FUN.
#' @return Object of class Speclib.
#' @author Lukas Lehnert
#' @seealso [apply()], [match.fun()],
#' \code{\linkS4class{Speclib}}
#' @keywords utilities
#' @examples
#' 
#' data(spectral_data)
#' 
#' mean_spectrum <- apply(spectral_data, FUN = mean)
#' plot(mean_spectrum)
#' 
#' ## Same as above but seperately for both seasons
#' mean_spectra <- apply(spectral_data, FUN = mean, byattributes = "season")
#' plot(mean_spectra, FUN = 1, ylim = c(0,50))
#' plot(mean_spectra, FUN = 2, new = FALSE)
#' attribute(mean_spectra)
#' 
#' 
NULL





#' Hyperspectral samples
#' 
#' Hyperspectral samples from the human larynx
#' 
#' BIANCA
#' 
#' @name cancer_spectra
#' @docType data
#' @format An object of class `Speclib`
#' @author Bianca Regeling, Lukas Lehnert
#' @keywords datasets
NULL





#' Methods for Function `createDataPartition`
#' 
#' Methods for function `createDataPartition` in package \pkg{caret}
#' 
#' 
#' @name caret::createDataPartition-methods
#' @aliases createDataPartition-methods createDataPartition,ANY-method
#' createDataPartition,.CaretHyperspectral-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(y = \".CaretHyperspectral\")")}{ Wrapper method for
#' [createDataPartition()].  \cr Note that
#' `".CaretHyperspectral"` is a class union containing classes \Sexpr{
#' paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ") }. } }
#' @keywords methods
NULL





#' Methods for Function `createFolds` and `createMultiFolds`
#' 
#' Methods for functions `createFolds` and `createMultiFolds` in
#' package \pkg{caret}
#' 
#' 
#' @name caret::createFolds-methods
#' @aliases createFolds-methods createMultiFolds-methods createFolds,ANY-method
#' createMultiFolds,ANY-method createFolds,.CaretHyperspectral-method
#' createMultiFolds,.CaretHyperspectral-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(y = \".CaretHyperspectral\")")}{ Wrapper methods for
#' [createFolds()] and [createMultiFolds()].  \cr Note that
#' `".CaretHyperspectral"` is a class union containing classes \Sexpr{
#' paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ") }. } }
#' @keywords methods
NULL





#' Methods for Function `createResample`
#' 
#' Methods for function `createResample` in package \pkg{caret}
#' 
#' 
#' @name caret::createResample-methods
#' @aliases createResample-methods createResample,ANY-method
#' createResample,.CaretHyperspectral-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(y = \".CaretHyperspectral\")")}{ Wrapper method for
#' [createResample()].  \cr Note that `".CaretHyperspectral"` is
#' a class union containing classes \Sexpr{
#' paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ") }. } }
#' @keywords methods
NULL





#' Methods for Function `featurePlot`
#' 
#' Methods for function `featurePlot` in package \pkg{caret}
#' 
#' 
#' @name caret::featurePlot-methods
#' @aliases featurePlot-methods featurePlot,ANY-method
#' featurePlot,.CaretHyperspectral-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(x = \".CaretHyperspectral\")")}{ Wrapper method for
#' [featurePlot()].  \cr Note that `".CaretHyperspectral"` is a
#' class union containing classes \Sexpr{
#' paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ") }. } }
#' @keywords methods
NULL





#' Methods for Function `gafs`
#' 
#' Methods for function `gafs` in package \pkg{caret}.
#' 
#' 
#' @name caret::gafs
#' @aliases gafs-methods get_gafs gafs,Speclib-method gafs,Nri-method
#' gafs,Specfeat-method
#' @docType methods
#' @param x Object of class `Speclib`, `Nri` or `Specfeat`. For
#' `get_gafs`, `x` must be the output of `gafs` as
#' `Speclib` or `Nri`.
#' @param y A numeric or factor vector containing the outcome for each sample.
#' If missing, the response variable set by [setResponse()] is used.
#' @param cutoff The cutoff value of the correlation coefficients between
#' response variables.
#' @param returnData Logical. If TRUE, the updated object of `x` is
#' returned, otherwise only the result of [gafs()] is returned.
#' @param ...  Further aruments passed to [gafs()].
#' @return If `returnData == TRUE`, an object of class `Speclib` or
#' `Nri`, otherwise an object of class `gafs`. Note that if `x`
#' is an object of class `Specfeat`, the function returns an object of
#' class `Speclib` containing the relevant transformed band values.
#' @author Lukas Lehnert
#' @seealso [gafs()]
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' data(spectral_data)
#' 
#' ## Set response variable (Chlorophyll content)
#' spectral_data <- setResponse(spectral_data, "chlorophyll")
#' 
#' ## Set additional predictor variables from the attributes
#' spectral_data <- setPredictor(spectral_data, "season")
#' 
#' ## Feature selection using genetic algorithms
#' ## Note that this may take some time!
#' gafs_res <- gafs(spectral_data)
#' 
#' get_gafs(gafs_res)
#' }
#' 
NULL





#' Methods for Function `preProcess`
#' 
#' Methods for function `preProcess` in package \pkg{caret}. The function
#' is mainly internally required.
#' 
#' 
#' @name caret::preProcess-methods
#' @aliases preProcess-methods preProcess,ANY-method
#' preProcess,.CaretHyperspectral-method show,.preProcessHyperspectral-method
#' preProcess-class
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(x = \".CaretHyperspectral\")")}{ Wrapper method for
#' [preProcess()].  \cr Note that `".CaretHyperspectral"` is a
#' class union containing classes \Sexpr{
#' paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ") }. } }
#' @keywords methods
NULL





#' Methods for Function `rfe`
#' 
#' Methods for function `rfe` in package \pkg{caret}.
#' 
#' 
#' @name caret::rfe
#' @aliases rfe-methods get_rfe rfe,Speclib-method rfe,Nri-method
#' rfe,Specfeat-method
#' @docType methods
#' @param x Object of class `Speclib`, `Nri` or `Specfeat`. For
#' `get_rfe`, `x` must be the output of `rfe` as `Speclib`
#' or `Nri`.
#' @param y A numeric or factor vector containing the outcome for each sample.
#' If missing, the response variable set by [setResponse()] is used.
#' @param cutoff The cutoff value of the correlation coefficients between
#' response variables.
#' @param returnData Logical. If TRUE, the updated object of `x` is
#' returned, otherwise only the result of [rfe()] is returned.
#' @param ...  Further aruments passed to [rfe()].
#' @return If `returnData == TRUE`, an object of class `Speclib` or
#' `Nri`, otherwise an object of class `rfe`. Note that if `x`
#' is an object of class `Specfeat`, the function returns an object of
#' class `Speclib` containing the relevant transformed band values.
#' @author Lukas Lehnert
#' @seealso [rfe()]
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' data(spectral_data)
#' 
#' ## Set response variable (Chlorophyll content)
#' spectral_data <- setResponse(spectral_data, "chlorophyll")
#' 
#' ## Set additional predictor variables from the attributes
#' spectral_data <- setPredictor(spectral_data, "season")
#' 
#' ## Recursive feature selection
#' ## Note that this may take some time!
#' rfe_res <- rfe(spectral_data)
#' 
#' get_rfe(rfe_res)
#' 
#' plot(get_rfe(rfe_res))
#' }
#' 
NULL





#' Methods for Function `safs`
#' 
#' Methods for function `safs` in package \pkg{caret}.
#' 
#' 
#' @name caret::safs
#' @aliases safs-methods get_safs safs,Speclib-method safs,Nri-method
#' safs,Specfeat-method
#' @docType methods
#' @param x Object of class `Speclib`, `Nri` or `Specfeat`. For
#' `get_safs`, `x` must be the output of `safs` as
#' `Speclib` or `Nri`.
#' @param y A numeric or factor vector containing the outcome for each sample.
#' If missing, the response variable set by [setResponse()] is used.
#' @param cutoff The cutoff value of the correlation coefficients between
#' response variables.
#' @param returnData Logical. If TRUE, the updated object of `x` is
#' returned, otherwise only the result of [safs()] is returned.
#' @param ...  Further aruments passed to [safs()].
#' @return If `returnData == TRUE`, an object of class `Speclib` or
#' `Nri`, otherwise an object of class `safs`. Note that if `x`
#' is an object of class `Specfeat`, the function returns an object of
#' class `Speclib` containing the relevant transformed band values.
#' @author Lukas Lehnert
#' @seealso [safs()]
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' data(spectral_data)
#' 
#' ## Set response variable (Chlorophyll content)
#' spectral_data <- setResponse(spectral_data, "chlorophyll")
#' 
#' ## Set additional predictor variables from the attributes
#' spectral_data <- setPredictor(spectral_data, "season")
#' 
#' ## Supervised feature selection using simulated annealing
#' ## Note that this may take some time!
#' safs_res <- safs(spectral_data)
#' 
#' get_safs(safs_res)
#' 
#' plot(get_safs(safs_res))
#' }
#' 
NULL





#' Set predictor variable(s)
#' 
#' Set predictor variable(s) to be used in functions of package \pkg{caret}.
#' 
#' 
#' @name caret::setPredictor
#' @aliases setPredictor-methods setPredictor
#' setPredictor,.CaretHyperspectral,character-method
#' @docType methods
#' @param x Object of one of the following classes: \Sexpr{
#' paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ") }.
#' @param predictor Character vector. Name of additional predictor variable(s)
#' (from the attributes).
#' @return The updated object.
#' @author Lukas Lehnert
#' @seealso [sbf()]
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' data(spectral_data)
#' 
#' ## Use subset of data
#' x <- spectral_data[c(1:31),] 
#' 
#' ## Set additional predictor variables from the attributes
#' x <- setPredictor(x, "season")
#' }
#' 
NULL





#' Set response variable
#' 
#' Set response variable to be used in functions of package \pkg{caret}.
#' 
#' 
#' @name caret::setResponse
#' @aliases setResponse-methods setResponse
#' setResponse,.CaretHyperspectral,character-method
#' @docType methods
#' @param x Object of one of the following classes: \Sexpr{
#' paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ") }.
#' @param response Character. Name of response variable (from the attributes).
#' @return The updated object.
#' @author Lukas Lehnert
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' data(spectral_data)
#' 
#' ## Use subset of data
#' x <- spectral_data[c(1:31),] 
#' 
#' ## Set response variable (Percentage of green vegetation)
#' x <- setResponse(x, "PV")
#' }
#' 
NULL





#' Methods for Function `sbf`
#' 
#' Methods for function `sbf` in package \pkg{caret}.
#' 
#' 
#' @name caret::sbf
#' @aliases sbf-methods get_sbf sbf,Speclib-method sbf,Nri-method
#' sbf,Specfeat-method
#' @docType methods
#' @param x Object of class `Speclib`, `Nri` or `Specfeat`. For
#' `get_sbf`, `x` must be the output of `sbf` as `Speclib`
#' or `Nri`.
#' @param y A numeric or factor vector containing the outcome for each sample.
#' If missing, the response variable set by [setResponse()] is used.
#' @param cutoff The cutoff value of the correlation coefficients between
#' response variables.
#' @param returnData Logical. If TRUE, the updated object of `x` is
#' returned, otherwise only the result of [sbf()] is returned.
#' @param ...  Further aruments passed to [sbf()].
#' @return If `returnData == TRUE`, an object of class `Speclib` or
#' `Nri`, otherwise an object of class `sbf`. Note that if `x`
#' is an object of class `Specfeat`, the function returns an object of
#' class `Speclib` containing the relevant transformed band values.
#' @author Lukas Lehnert
#' @seealso [sbf()]
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' data(spectral_data)
#' 
#' ## Set response variable (Chlorophyll content)
#' spectral_data <- setResponse(spectral_data, "chlorophyll")
#' 
#' ## Set additional predictor variables from the attributes
#' spectral_data <- setPredictor(spectral_data, "season")
#' 
#' ## Selection by filtering
#' ## Note that this may take some time!
#' sbf_res <- sbf(spectral_data)
#' 
#' get_sbf(sbf_res)
#' 
#' plot(get_sbf(sbf_res))
#' }
#' 
NULL





#' Show caret related parameters
#' 
#' Show caret related parameters in objects of classes \Sexpr{
#' paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ") }.
#' 
#' 
#' @aliases showCaretParameters showCaretParameters,.CaretHyperspectral-method
#' @param x Object of one of the following classes: \Sexpr{
#' paste(hsdar:::.getCaretCompatibleClasses(), collapse = ", ") }.
#' @author Lukas Lehnert
#' @seealso [sbf()]
NULL





#' Methods for Function `train`
#' 
#' Methods for functions `train` and `train.formula` in package
#' \pkg{caret}
#' 
#' 
#' @name caret::train-methods
#' @aliases train-methods train,ANY-method train,.CaretHyperspectral-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(x = \".CaretHyperspectral\")")}{ Wrapper method for
#' [train()].  \cr Note that `".CaretHyperspectral"` is a class
#' union containing classes \Sexpr{ paste(hsdar:::.getCaretCompatibleClasses(),
#' collapse = ", ") }. } \item{list("signature(form = \"formula\", data =
#' \"Speclib\")")}{ Wrapper method for [train.formula()] to be used
#' with objects of class `Speclib`. } }
#' @keywords methods
NULL





#' * Clman class
#' 
#' Class to handle continuum removal objects (extends
#' \code{\linkS4class{Speclib}} class).
#' 
#' The class extends \code{\linkS4class{Speclib}}s and adds two additional
#' slots: \itemize{ \itemcp: Object of class `matrix` containing continuum
#' points for all spectra (rows) and bands (columns).  \itemhull: Object of
#' class `matrix` containing hull lines for all spectra (rows) and bands
#' (columns).  }
#' 
#' @name Clman-class
#' @docType class
#' @note See figure in [hsdar-package()] for an overview of classes
#' in hsdar.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}, [=plot.Speclib::plot()]
#' @keywords classes
NULL





#' Methods to create, manipulate and query objects of class 'Clman'.
#' 
#' Methods to create, manipulate and query objects of class 'Clman'.
#' 
#' 
#' @name clman
#' @aliases initialize,Clman-method spectra,Clman-method
#' spectra<-,Clman,data.frame-method spectra<-,Clman,matrix-method
#' spectra<-,Clman,numeric-method
#' @docType methods
#' @param .Object,object Matrix, numeric or array in cases of creation of
#' 'Clman' objects otherwise object of class 'Clman'.
#' @param value Object of class numeric, matrix or array which is used for
#' replacement of the values in x.
#' @param ...  Arguments passed to [createspeclib()].
#' @return For `spectra<-`, the updated object. Otherwise a matrix
#' returning the spectra in the Clman object.
#' @note The functions to create objects of class `Clman` are mainly
#' internally needed by [transformSpeclib()].
#' @author Lukas Lehnert
#' @seealso [dist.speclib()], [Clman()],
#' [transformSpeclib()], [=plot.Speclib::plot()]
NULL





#' * Clman class
#' 
#' Class to store and handle manual continuum lines
#' 
#' 
#' @name Clman
#' @aliases Clman plot,Clman-method
#' @docType class
#' @param wavelength Vector with corresponding wavelength for each band.
#' @param cp Data frame or matrix containing fix points. Fix points have
#' numbers greater than 0, all other bands are 0.
#' @param hull Data frame or matrix containing linear hull.
#' @param spectra Data frame, matrix of raster object of class
#' 'SpatialGridDataFrame' with spectral data.
#' @param outdatedhull Data frame or matrix containing hull of step before for
#' undo porposes.
#' @param mask Data frame with masked parts in the spectra. See
#' [mask()].
#' @param x Object of class `clman`.
#' @param ispec Name or index of spectrum to be plotted.
#' @param subset Lower and upper spectral limits used for plot.
#' @param numeratepoints Flag if points should be numerated in plot.
#' @param hull.style List of arguments passed to [lines()] to
#' construct the continuum line.
#' @param points.style List of arguments passed to [points()] to
#' construct the continuum points. May be `NULL` to suppress plotting of
#' fix points.
#' @param ...  Further arguments passed to plot.default.
#' @return Object of class \code{\linkS4class{Clman}}.
#' @author Lukas Lehnert and Hanna Meyer
#' @seealso [transformSpeclib()], [=plot.Speclib::plot()]
#' @keywords classes aplot
#' @examples
#' 
#' ## Model spectra using PROSAIL
#' parameter <- data.frame(N = rep.int(c(1, 1.5),2), LAI = c(1,1,3,3))
#' spec <- PROSAIL(parameterList=parameter)
#' 
#' ## Transform spectra
#' spec_clman <- transformSpeclib(spec, method = "sh", out = "raw")
#' 
#' ## Plot clman
#' plot(spec_clman, ispec = 1, subset = c(400, 1000))
#' 
NULL





#' Dimensions of Speclib
#' 
#' Get dimension(s) of Speclib
#' 
#' 
#' @aliases dim.speclib dim,Speclib-method nspectra nbands
#' @param x Object of class `Speclib`.
#' @return Vector of length = 2 or single integer value.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}
#' @keywords utilities
#' @examples
#' 
#' data(spectral_data)
#' 
#' dim(spectral_data)
#' 
NULL





#' * DistMat3D class
#' 
#' Class to store effectively (large) distance matrices (up to 3D).
#' 
#' Object with 3 slots: \itemize{ \itemvalues: Numerical vector containing
#' distance values \itemncol: Number of columns in the 3D-matrix. Number of
#' columns equals always the number of rows \itemnlyr: Number of layers in the
#' 3D-matrix }
#' 
#' @name DistMat3D-class
#' @docType class
#' @note See figure in [hsdar-package()] for an overview of classes
#' in hsdar.
#' @author Lukas Lehnert
#' @seealso [distMat3D()]
#' @keywords classes
NULL





#' Methods to create, manipulate and query objects of class 'DistMat3D'.
#' 
#' Methods to create, manipulate and query objects of class 'DistMat3D'. The
#' following relational operators are defined to compare values between
#' 'DistMat3D'-object(s): `<, <=, ==, >, >=`
#' 
#' 
#' @name distMat3D
#' @aliases distMat3D show,DistMat3D-method as.array,DistMat3D-method
#' as.matrix,DistMat3D-method dim,DistMat3D-method ncol,DistMat3D-method
#' nrow,DistMat3D-method distMat3D,matrix-method distMat3D,numeric-method
#' distMat3D,array-method [,DistMat3D,ANY,ANY-method
#' [,DistMat3D,ANY,ANY,ANY-method [<-,DistMat3D,ANY,ANY-method
#' [<-,DistMat3D,ANY,ANY,ANY-method <,DistMat3D,ANY-method
#' <=,DistMat3D,ANY-method >,DistMat3D,ANY-method >=,DistMat3D,ANY-method
#' ==,DistMat3D,ANY-method <,DistMat3D,DistMat3D-method
#' <=,DistMat3D,DistMat3D-method >,DistMat3D,DistMat3D-method
#' >=,DistMat3D,DistMat3D-method ==,DistMat3D,DistMat3D-method
#' <,ANY,DistMat3D-method <=,ANY,DistMat3D-method >,ANY,DistMat3D-method
#' >=,ANY,DistMat3D-method ==,ANY,DistMat3D-method
#' @docType methods
#' @param x,object Matrix, numeric or array in cases of creation of 'DistMat3D'
#' objects otherwise object of class 'DistMat3D'.
#' @param ncol Number of columns in the new 'DistMat3D' object.
#' @param nlyr Number of layer in the new 'DistMat3D' object.
#' @param lower_tri Flag if only the lower triangle is used.
#' @param lyr Layer in the 'DistMat3D' object to be transformed into matrix.
#' @param value Object of class numeric, matrix or array which is used for
#' replacement of the values in x.
#' @param i,j,n Subscripts to access data.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{DistMat3D}},
#' [=apply.DistMat3D::apply()], \code{\linkS4class{Nri}}
#' @keywords classes
#' @examples
#' 
#' data(spectral_data)
#' 
#' ## Mask channel crossing part (arround 1050 nm) and strong 
#' ## water absorption part (above 1350 nm)
#' mask(spectral_data) <- c(1045, 1055, 1350, 1706)
#' 
#' ## Calculate SAM distances (object of class 'dist')
#' sam_dist <- dist.speclib(subset(spectral_data, season == "summer"))
#' 
#' ## Convert to class 'distMat3D'
#' sam_dist <- distMat3D(as.matrix(sam_dist))
#' 
#' sam_dist
#' 
NULL





#' Manage, analyse and simulate hyperspectral data in R
#' 
#' The \pkg{hsdar} package contains classes and functions to manage, analyse
#' and simulate hyperspectral data. These might be either spectrometer
#' measurements or hyperspectral images through the interface of \pkg{raster}.
#' 
#' \pkg{hsdar} provides amongst others the following functionality. \itemize{
#' \itemData handling: \pkg{hsdar} is designed to handle even large sets of
#' spectra. Spectra are stored in a \code{\linkS4class{Speclib}} containing,
#' amongst other details, the wavelength and reflectance for each spectrum.
#' \pkg{hsdar} further contains functions for
#' [=plot.Speclib::plot()]ting spectral data and
#' [=apply.Speclib::apply()]ing functions to spectra.
#' 
#' \itemData manipulation: A variety of established methods for data
#' manipulation such as filter functions ([smoothSpeclib()]),
#' resampling of bands to various satellite sensors
#' ([spectralResampling()]), continuum removal
#' ([transformSpeclib()]), calculations of derivations
#' ([derivative.speclib()]) and extraction of absorption features
#' ([cut_specfeat()]) are implemented.
#' 
#' \itemData analysis: Supported methods to analyse vegetation spectra are the
#' calculation of red edge parameters ([rededge()]), vegetation
#' ([vegindex()]) and soil ([soilindex()]) indices as well
#' as ndvi-like narrow band indices ([nri()]).  \pkg{hsdar} further
#' enables to perform spectral unmixing of spectra ([unmix()]) by use
#' of endmember spectra.
#' 
#' \itemData simulation: \pkg{hsdar} has implemented the models PROSAIL 5B
#' ([PROSAIL()], Jacquemoud et al. 2009) and PROSPECT 5
#' ([PROSPECT()], Jacquemoud and Baret 1990) to simulate spectra of
#' canopy and plants.
#' 
#' }
#' 
#' Several classes are defined and used in \pkg{hsdar}. Most of the classes are
#' used and respective objects are created internally. However, the following
#' figure gives an overview which class is used at which stage of processing.
#' \if{html\figure{classes.pngwidth="35%" alt="Figure: classes.png"}}
#' \if{latex\figure{classes.pdfoptions: width=10cm}} Note that the asterisk
#' marks all classes for which wrapper functions for the \pkg{caret} package
#' exist.
#' 
#' To see the preferable citation of the package, type
#' `citation("hsdar")`.
#' 
#' @name hsdar-package
#' @aliases hsdar-package hsdar
#' @docType package
#' @section Acknowledgements: Development initially funded by German Federal
#' Ministry of Education and Research (03G0808C) in the scope of the project
#' PaDeMoS as precondition to develop a space-based Pasture Degradation
#' Monitoring System for the Tibetan Plateau.
#' @author Lukas Lehnert, Hanna Meyer, Joerg Bendix
#' @keywords package
NULL





#' HyperSpecRaster* class
#' 
#' Extension of *RasterBrick-class to handle hyperspectral data
#' 
#' Extension of *RasterBrick-class with three additional slots: \describe{
#' \item{list("wavelength")}{ A numeric vector giving the center wavelength for
#' each band.}\item{:}{ A numeric vector giving the center wavelength for each
#' band.} \item{list("fwhm")}{ A numeric vector giving the full-width-half-max
#' values for each band.}\item{ (optional):}{ A numeric vector giving the
#' full-width-half-max values for each band.} \item{list("attributes")}{ A
#' `data.frame` containing additional information for each pixel.}\item{
#' (optional):}{ A `data.frame` containing additional information for each
#' pixel.} } The information in the three slots are used for the convertion to
#' \code{\linkS4class{Speclib}}.
#' 
#' @name HyperSpecRaster-class
#' @aliases HyperSpecRaster-class show,HyperSpecRaster-method
#' @docType class
#' @author Lukas Lehnert
#' @seealso [raster::brick()], \code{\linkS4class{Speclib}}
#' @keywords spatial classes
NULL





#' Handle hyperspectral cubes using raster package
#' 
#' Methods to create and handle objects of class HyperSpecRaster
#' 
#' 
#' @aliases HyperSpecRaster HyperSpecRaster,character,numeric-method
#' HyperSpecRaster,RasterLayer,numeric-method
#' HyperSpecRaster,RasterBrick,numeric-method brick,Speclib,ANY-method
#' HyperSpecRaster,Speclib,ANY-method
#' writeStart,HyperSpecRaster,character-method
#' writeStart,HyperSpecRaster,Speclib-method getValuesBlock,HyperSpecRaster
#' HyperSpecRaster,HyperSpecRaster-method getValuesBlock,HyperSpecRaster-method
#' writeValues,RasterLayer,Speclib-method
#' writeValues,RasterBrick,Speclib-method
#' writeValues,HyperSpecRaster,Speclib-method
#' @param x Raster* object
#' @param wavelength Vector containing wavelength for each band
#' @param fwhm Optional vector containing full-width-half-max values. If length
#' == 1 the same value is assumed for each band. Note that function does not
#' check the integrity of the values
#' @param attributes Optional data.frame containing attributes data
#' @param nrow Optional. Number of rows in HyperspecRaster. If omitted,
#' function will try to get the information from the attributes in Speclib
#' (`attr(x, "rastermeta")`)
#' @param ncol Optional. Number of colums in HyperspecRaster. See nrow above.
#' @param xmn Optional. Minimum coordiante in x-dimension. See nrow above.
#' @param xmx Optional. Maximum coordiante in x-dimension. See nrow above.
#' @param ymn Optional. Minimum coordiante in y-dimension. See nrow above.
#' @param ymx Optional. Maximum coordiante in y-dimension. See nrow above.
#' @param crs Optional. Object of class `'CRS'` giving the coordinate
#' system for HyperspecRaster. See nrow above.
#' @param ... Additional arguments as for [brick()]
#' @param filename Name of file to create
#' @param v Speclib or matrix of values
#' @param start Integer. Row number (counting starts at 1) from where to start
#' writing `v`
#' @return HyperSpecRaster or RasterBrick
#' @author Lukas Lehnert
#' @keywords spatial methods
#' @examples
#'  
#' \dontrun{
#' ## Create raster file using PROSAIL
#' ## Run PROSAIL
#' parameter <- data.frame(N = c(rep.int(seq(0.5, 1.4, 0.1), 6)),
#'                         LAI = c(rep.int(0.5, 10), rep.int(1, 10), 
#'                                 rep.int(1.5, 10), rep.int(2, 10), 
#'                                 rep.int(2.5, 10), rep.int(3, 10)))
#' spectra <- PROSAIL(parameterList = parameter)
#' 
#' ## Create SpatialPixelsDataFrame and fill data with spectra from PROSAIL
#' rows <- round(nspectra(spectra)/10, 0)
#' cols <- ceiling(nspectra(spectra)/rows)
#' grd <- SpatialGrid(GridTopology(cellcentre.offset = c(1,1,1), 
#'                                 cellsize = c(1,1,1), 
#'                                 cells.dim = c(cols, rows, 1)))
#' x <- SpatialPixelsDataFrame(grd, data = as.data.frame(spectra(spectra)))
#' 
#' ## Write data to example file (example_in.tif) in workingdirectory
#' writeGDAL(x, fname = "example_in.tif", drivername = "GTiff")
#' 
#' ## Examples for HyperSpecRaster using file example_in.tif
#' ## Example 1:
#' ## smoothing spectra
#' infile <- "example_in.tif"
#' outfile <- "example_result_1.tif"
#' wavelength <- spectra$wavelength
#' 
#' ra <- HyperSpecRaster(infile, wavelength)
#' tr <- blockSize(ra)
#' 
#' res <- writeStart(ra, outfile, overwrite = TRUE)
#' for (i in 1:tr$n) 
#' {
#'   v <- getValuesBlock(ra, row=tr$row[i], nrows=tr$nrows[i])
#'   v <- smoothSpeclib(v, method="sgolay", n=25)
#'   res <- writeValues(res, v, tr$row[i])
#' }
#' res <- writeStop(res)
#' 
#' ## Example 2:
#' ## masking spectra and calculating vegetation indices
#' outfile <- "example_result_2.tif" 
#' n_veg <- as.numeric(length(vegindex()))
#' res <- writeStart(ra, outfile, overwrite = TRUE, nl = n_veg)
#' for (i in 1:tr$n) 
#' {
#'   v <- getValuesBlock(ra, row=tr$row[i], nrows=tr$nrows[i])
#'   mask(v) <- c(1350, 1450)
#'   v <- as.matrix(vegindex(v, index=vegindex()))
#'   res <- writeValues(res, v, tr$row[i])
#' }
#' res <- writeStop(res)
#' }
#' 
NULL





#' Merge speclibs
#' 
#' Merge 2 `Speclib`s and their attributes data
#' 
#' 
#' @aliases merge,Speclib,Speclib-method
#' @param x 1st Object of class `Speclib` to be merged.
#' @param y 2nd Object of class `Speclib` to be merged.
#' @param ...  Further arguments passed to generic functions. Currently
#' ignored.
#' @return Object of class `Speclib`.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}
#' @keywords utilities
#' @examples
#' 
#' data(spectral_data)
#' sp1 <- spectral_data[c(1:10),]
#' sp2 <- spectral_data[c(11:20),]
#' 
#' speclib_merged <- merge(sp1, sp2)
#' 
NULL





#' Methods for * Nri-class
#' 
#' Methods to handle data in objects of class Nri.
#' 
#' 
#' @aliases $,Nri-method as.matrix,Nri-method as.data.frame,Nri-method
#' show,Nri-method print,Nri-method wavelength,Nri-method [,Nri,ANY,ANY-method
#' [,Nri,ANY,ANY,ANY-method getFiniteNri
#' @param x,object List of class 'Nri'
#' @param na.rm Remove indices containing NA-values. Note that if TRUE, all
#' indices are removed which have at least one NA value.
#' @param named_matrix Flag if column and row names are set to band indices
#' used for the calculation of the nri-values.
#' @param ...  Further arguments passed to generic functions. Currently
#' ignored.
#' @author Lukas Lehnert
#' @seealso [glm.nri()], [glm()]
NULL





#' * Nri class
#' 
#' Class to handle datasets containing normalized ratio indices of spectra.
#' 
#' Object with slots: \itemize{ \itemnri: Object of class
#' \code{\linkS4class{DistMat3D}} containing nri values.  \itemfwhm: Vector or
#' single numerical value giving the full-width-half-max value(s) for each
#' band.  \itemwavelength: Vector with wavelength information.  \itemdimnames:
#' Character vector containing band names used to calculate nri-values.
#' \itemmultivariate: List defining the kind of test/model applied to the data
#' and the model data. Only used after object has passed e.g.
#' [=glm.nri::(g)lm.nri()].  \itemattributes: Data.frame containing
#' additional data \itemusagehistory: Vector giving information on history of
#' usage of the object. }
#' 
#' @name Nri-class
#' @docType class
#' @note See figure in [hsdar-package()] for an overview of classes
#' in hsdar.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}
#' @keywords classes
NULL





#' Plot function for (g)lm.nri and cor.test.nri
#' 
#' Plot values in (generalised) linear modes and correlation tests from narrow
#' band indices
#' 
#' See details in [glm.nri()] and [glm()].
#' 
#' @aliases plot.Nri plot,Nri-method plot,Nri,ANY-method
#' @param x Object to be plotted.
#' @param coefficient Name or index of coefficient to plot.
#' @param predictor Name or index of term to plot.
#' @param xlab Label for x-axis.
#' @param ylab Label for y-axis.
#' @param legend Flag if legend is plotted. If `legend == "outer"` the
#' legend is plotted in the outer margins of the figure. This is useful if both
#' diagonals are used.
#' @param colspace Either "hcl" or "rgb". Colour space to be used for the
#' plots.
#' @param col If colspace == "hcl", the vector is giving the minimum and
#' maximum values of hue (element 1 & 2), chroma (element 3 & 4) and luminance
#' (element 5 & 6). The optional element 7 is used as alpha value. See
#' [hcl()] for further explanation. If colspace == "rgb", a vector of
#' length >=2 giving the colours to be interpolated using
#' [colorRamp()].
#' @param digits Precision of labels in legend.
#' @param range "auto" or a vector of length = 2 giving the range of values to
#' be plotted.
#' @param constraint A character string giving a constraint which values should
#' be plotted. See examples section.
#' @param uppertriang Flag if upper triangle is used for the plot. Note that if
#' `TRUE` the current plot is used instead of starting a new plot
#' @param ...  Further arguments passed to `plot.default`.
#' @return An invisible vector with minimum and maximum values plotted.
#' @author Lukas Lehnert
#' @seealso [nri()], [glm.nri()], [glm()],
#' [=cor.test.nri::cor.test()], [=t.test.nri::t.test()]
#' @keywords aplot
#' @examples
#' 
#' \dontrun{
#' data(spectral_data)
#' 
#' ## Calculate all possible combinations for WorldView-2-8
#' spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
#'                               response_function = FALSE)
#' nri_WV <- nri(spec_WV, recursive = TRUE)
#' 
#' ## Fit generalised linear models between NRI-values and chlorophyll
#' glmnri <- glm.nri(nri_WV ~ chlorophyll, preddata = spec_WV)
#' 
#' ## Plot p-values
#' plot(glmnri, range = c(0, 0.05))
#' ## Plot t-values
#' plot(glmnri, coefficient = "t.value")
#' ## Plot only t-values where p-values < 0.001
#' plot(glmnri, coefficient = "t.value", 
#'      constraint = "p.value < 0.001")
#' 
#' ## Fit linear models between NRI-values and chlorophyll
#' lmnri <- lm.nri(nri_WV ~ chlorophyll, preddata = spec_WV)
#' 
#' ## Plot r.squared
#' plot(lmnri)
#' 
#' ## Example for EnMAP (Attention: Calculation time may be long!)
#' spec_EM <- spectralResampling(spectral_data, "EnMAP", 
#'                               response_function = FALSE)
#' mask(spec_EM) <- c(300, 550, 800, 2500)
#' nri_EM <- nri(spec_EM, recursive = TRUE)
#' glmnri <- glm.nri(nri_EM ~ chlorophyll, preddata = spec_EM)
#' 
#' ## Plot T values in lower and p-values in upper diagonal
#' ## of the plot
#' ## Enlarge margins for legends
#' par(mar = c(5.1, 4.1, 4.1, 5))
#' plot(glmnri, coefficient = "t.value", legend = "outer")
#' plot(glmnri, coefficient = "p.value", uppertriang = TRUE)
#' lines(c(400,1705),c(400,1705))
#' }
#' 
NULL





#' Plot function for class Specfeat
#' 
#' Plot spectra in Specfeat objects
#' 
#' 
#' @aliases plot,Specfeat,ANY-method
#' @param x Object to be plotted
#' @param fnumber Subscript of feature(s) to be plotted
#' @param stylebysubset Name of column in attributes to be used for colour.
#' @param changecol Flag indicating if line colours change according to values
#' in coloumn defined by stylebysubset
#' @param changetype Flag indicating if line types change according to values
#' in coloumn defined by stylebysubset
#' @param autolegend Flag if legend is plotted.
#' @param new Flag if a new plot is started.
#' @param ...  Further arguments passed to `plot.default`
#' @author Lukas Lehnert
#' @seealso [nri()], [glm.nri()], [glm()],
#' [cor.test,Nri-method()], [t.test,Nri-method()]
#' @keywords aplot
#' @examples
#' 
#' ## See examples in specfeat
#' 
NULL





#' Plot speclib
#' 
#' Plot `Speclib` in a new plot or adding it to an existing plot.
#' 
#' The function may work in a couple of modes. The default way is to plot mean
#' values (solid line) of all spectra and the standard deviations within bands.
#' If data is assumed to be continuous the standard deviations are plotted as
#' dashed lines otherwise error bars will indicate standard deviations.
#' 
#' The user has various options to change the way things are looking: With
#' argument `FUN` the name of a function, the ID or the index of a certain
#' spectrum may be specified. Note that if `FUN` is a function, this
#' function will be applied to all spectra. If function should be applied to a
#' subset of spectra, use function [=subset.speclib::subset()] to
#' define rules excluding certain spectra.
#' 
#' By passing a subset, the user may specify a spectral range to plot. Limits
#' for x- and y-axis will be found automatically or may be passed separately.
#' 
#' @aliases plot.Speclib plot,Speclib,ANY-method plot,Clman,ANY-method
#' legendSpeclib
#' @param x Object of class `Speclib`.
#' @param FUN Name of a function (character) or index or ID of single spectrum
#' to plot (integer).
#' @param new If FALSE the plot is added to active existing plot.
#' @param ispec Subscript of spectrum to be plotted.
#' @param subset Vector of length = 2 containing minimum and maximum wavelength
#' to plot.
#' @param numeratepoints Flag if continuum points are numerated and labeled.
#' @param hull.style List of arguments passed to [lines()] to
#' construct the continuum line.
#' @param points.style List of arguments passed to [points()] to
#' construct the continuum points. May be `NULL` to suppress plotting of
#' fix points.
#' @param ...  Further arguments passed to internal plot functions or to
#' [=plot.Speclib::plot()] for objects of class `Speclib` and
#' `Clman`.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}
#' @keywords aplot
#' @examples
#' 
#' data(spectral_data)
#' 
#' ## Set mask for channel crossing and water absorption bands
#' mask(spectral_data) <- c(1040, 1060, 1350, 1450)
#' 
#' ## Simple example
#' plot(spectral_data, legend = list(x = "topleft"))
#' 
#' ## Example with groups
#' plot(spectral_data, bygroups = TRUE, legend = list(x = "topleft"))
#' 
#' ## Example with function
#' par(mfrow = c(2,3))
#' plot(spectral_data, FUN = "min", main = "Minimum of speclib")
#' plot(spectral_data, FUN = "max", main = "Maximum of speclib")
#' plot(spectral_data, FUN = "median", main = "Median of speclib")
#' plot(spectral_data, FUN = "mean", main = "Mean of speclib")
#' plot(spectral_data, FUN = "var", main = "Variance of speclib")
#' 
#' 
NULL





#' Rasterbased methods for spectra
#' 
#' Methods to manipulate and save spectra in Speclibs stored as RasterBrick
#' 
#' 
#' @aliases Raster-methods extract,Speclib,ANY-method
#' writeRaster,Speclib,character-method
#' @param x Speclib with RasterBrick-object for spectra
#' @param y Object of any valid type to define area to extract
#' @param filename Output filename
#' @param ... Additionaly arguments passed to basic funtions in the
#' raster-package
#' @return Speclib
#' @author Lukas Lehnert
#' @keywords spatial methods
NULL





#' Satellite sensor response functions
#' 
#' Satellite sensor response functions for all sensor channels
#' 
#' Please do not access this data directly, since it contains only the response
#' values without any spectral information. To get response functions use
#' function [get.response()], instead.
#' 
#' @name response_functions
#' @aliases response_functions Landsat_4_response Landsat_5_response
#' Landsat_7_response Landsat_8_response Sentinel2A_response Quickbird_response
#' RapidEye_response WV_2_8_response response_functions get_landsat4_response
#' get_landsat5_response get_landsat7_response get_landsat8_response
#' get_quickbird_response get_reflectance get_wv2_4_response get_wv2_8_response
#' get_sentinel2_response
#' @docType data
#' @format An object of class 'data.frame'
#' @note This data is kindly provided by operators of satellites. See
#' \code{\link{hsdardocs}("Copyright")} for copyright information on spectral
#' response functions. \itemize{ \itemQuickbird: Copyright by DigitalGlobe,
#' Inc. All Rights Reserved \itemRapidEye: Copyright by RapidEye AG
#' \itemWorldView-2: Copyright by DigitalGlobe, Inc. All Rights Reserved }
#' @keywords datasets
NULL





#' * Specfeat class
#' 
#' Class to handle spectral feature data.
#' 
#' Class extends `Speclib`-class and adds two additional slots: \itemize{
#' \itemfeatures: List containing the spectra according to the features.
#' \itemfeatureLimits: List containing limits of features defined by
#' [define.features()]. }
#' 
#' @name Specfeat-class
#' @docType class
#' @note See figure in [hsdar-package()] for an overview of classes
#' in hsdar.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}
#' @keywords classes
NULL





#' * Speclib class
#' 
#' Class to store and handle hyperspectral data in R
#' 
#' \subsection{Spectral data The spectral data (usually reflectance values) are
#' stored in an object of class `'.Spectra'`. This object may eiter
#' contain the spectral data as a `RasterBrick` or as a `matrix` with
#' columns indicating spectral bands and rows different samples, respectively.
#' The Speclib-class provides converting routines to and from
#' `RasterBrick`-class allowing to read and write geographic raster data
#' via [raster::brick()] and its extension
#' [HyperSpecRaster-class()]. Since R is in general not intended to
#' be used for VERY large data sets, this functionality should be handled with
#' care. If raster files are large, one should split them in multiple smaller
#' ones and process each of the small files, separately. See the excellent
#' tutorial 'Writing functions for large raster files' available on
#' \url{https://CRAN.R-project.org/package=raster} and section '2.2.2 Speclibs
#' from raster files' in 'hsdar-intro.pdf'.
#' 
#' } \subsection{Spectral information Speclib contains wavelength information
#' for each band in spectral data. This information is used for spectral
#' resampling, vegetation indices and plotting etc. Since spectra can be
#' handled either as continuous lines or as discrete values like for satellite
#' bands, spectral information is handled in two principle ways: \itemize{
#' \itemContinuous spectra: Data of spectrometers or hyperspectral (satellite)
#' sensors. This data is plotted as lines with dotted lines indicating standard
#' deviations by default.  \itemNon-continuous spectra: Data of multispectral
#' satellite sensors. Here, data is plotted as solid lines and error bars at
#' the mean position of each waveband indicating standard deviations by
#' default.  } The kind of data may be chosen by the user by setting the
#' attribute flag `"continuousdata"` (`attr(x,`
#' `"continuousdata")`) or passing `continuousdata = TRUE/FALSE`,
#' when initially converting data to \code{\linkS4class{Speclib}}-class. Take
#' care of doing so, because some functions as [spectralResampling()]
#' may only work correctly with continuous data!
#' 
#' The unit of spectral data must be set initially, when converting data to
#' speclib. Note that the package currently supports only "nm" as unit. This is
#' particularly important for function like [vegindex()], which need
#' to get correct bands out of the spectral data. } \subsection{Technical
#' description An object of class `Speclib` contains the following slots:
#' 
#' \itemize{ \itemwavelength: Vector with wavelength information.  \itemfwhm:
#' Vector or single numerical value giving the full-width-half-max value(s) for
#' each band.  \itemspectra: Object of class '.Spectra' with three slots:
#' \itemize{ \itemfromRaster: logical, indicating if spectral data is read from
#' a RasterBrick-object.  \itemspectra_ma: Matrix with ncol = number of bands
#' and nrow = number. Used if fromRaster == FALSE \itemspectra_ra:
#' RasterBrick-object which is used if fromRaster == TRUE.  } Contains
#' reflectance, transmittance or absorbance values. Handle with function
#' [=spectra.Speclib::spectra()].  \itemattributes: Data frame
#' containing additional data to each spectrum. May be used for linear
#' regression etc. Handle with function
#' [=attribute.speclib::attribute()].  \itemusagehistory: Vector
#' giving information on history of usage of speclib. Handle with function
#' [usagehistory()]. } }
#' 
#' @name Speclib-class
#' @docType class
#' @note See figure in [hsdar-package()] for an overview of classes
#' in hsdar.
#' @author Lukas Lehnert
#' @seealso [=plot.Speclib::plot()], [readGDAL()],
#' [mask()], [idSpeclib()],
#' 
#' [=dim.speclib::dim()], [=spectra.Speclib::spectra()],
#' [=attribute.speclib::attribute()]
#' @keywords classes
NULL





#' Methods to create objects of class Speclib
#' 
#' Methods to create objects of class Speclib from various data types
#' 
#' See details in \code{\linkS4class{Speclib}}.
#' 
#' @name speclib
#' @aliases speclib print,Speclib-method $,Speclib-method
#' [,Speclib,ANY,ANY-method [,Speclib,ANY,ANY,ANY-method show,Speclib-method
#' initialize,Speclib-method is.speclib createspeclib
#' speclib,matrix,numeric-method speclib,SpatialGridDataFrame,numeric-method
#' speclib,numeric,numeric-method speclib,matrix,data.frame-method
#' speclib,SpatialGridDataFrame,data.frame-method
#' speclib,numeric,data.frame-method speclib,matrix,matrix-method
#' speclib,SpatialGridDataFrame,matrix-method speclib,numeric,matrix-method
#' speclib,HyperSpecRaster,ANY-method speclib,RasterBrick,data.frame-method
#' speclib,RasterBrick,matrix-method speclib,RasterBrick,numeric-method
#' speclib,hyperSpec,ANY-method ncol,.Spectra-method nrow,.Spectra-method
#' @docType methods
#' @param spectra Data frame, matrix of raster object of class 'RasterBrick' or
#' 'SpatialGridDataFrame' with spectral data
#' @param x,object Object to be converted to or from Speclib. For conversion to
#' Speclib it can be a of class `'data frame'`, `'matrix'`,
#' `'list'` or `'character string'`. In the latter case x is
#' interpreted as path to raster object and read by `readGDAL`. For
#' conversion from `Speclib` the object must be of class `Speclib`.
#' @param wavelength Vector with corresponding wavelength for each band. A
#' matrix or data.frame may be passed giving the upper and lower limit of each
#' band. In this case, the first column is used as lower band limit and the
#' second as upper limit, respectively.
#' @param fwhm Vector containing full-width-half-max values for each band
#' @param attributes Data frame with additional attributes data.
#' @param transformation Kind of transformation applied to spectral data
#' (character)
#' @param usagehistory Character string or vector used for history of usage
#' @param continuousdata Flag indicating if spectra are quasi continuous or
#' discrete sensor spectra
#' @param wlunit Unit of wavelength in spectra
#' @param xlabel Label of wavelength data to be used for plots etc.
#' @param ylabel Label of spectral signal to be used for plots etc.
#' @param rastermeta List of meta information for SpatialGridDataFrame or
#' HyperSpecRaster. If missing, meta data in speclib is used. Use function
#' [rastermeta()] to create valid objects.
#' @param ...  Further arguments passed to specific (generic) functions or
#' createspeclib
#' @return An object of class `Speclib` containing the following slots is
#' returned:
#' 
#' \itemize{ \itemwavelength: Vector with wavelength information \itemfwhm:
#' Vector or single numerical value giving the full-width-half-max value(s) for
#' each band.  \itemspectra: Object of class '.Spectra' with three slots:
#' \itemize{ \itemfromRaster: logical, indicating if spectral data is read from
#' a RasterBrick-object.  \itemspectra_ma: Matrix with ncol = number of bands
#' and nrow = number. Used if fromRaster == FALSE \itemspectra_ra:
#' RasterBrick-object which is used if fromRaster == TRUE.  } Contains
#' reflectance, transmittance or absorbance values. Handle with function
#' [=spectra.Speclib::spectra()].  \itemattributes: Data frame
#' containing additional data to each spectrum. May be used for linear
#' regression etc. Handle with function
#' [=attribute.speclib::attribute()].  \itemusagehistory: Vector
#' giving information on history of usage of speclib. Handle with function
#' [usagehistory()].  \itemrastermeta: List containing meta
#' information to create *Raster objects from Speclib. Handle with function
#' [rastermeta()]. }
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}, [=plot.Speclib::plot()],
#' [readGDAL()], [mask()],
#' 
#' [idSpeclib()], [=dim.speclib::dim()],
#' [=spectra.Speclib::spectra()],
#' 
#' [=attribute.speclib::attribute()]
#' @keywords classes
#' @examples
#' 
#' data(spectral_data)
#' spectra <- spectra(spectral_data)
#' wavelength <- spectral_data$wavelength
#' 
#' spectra <- speclib(spectra,wavelength)
#' 
NULL





#' Hyperspectral samples
#' 
#' Hyperspectral samples from a FACE experiment in Germany
#' 
#' Data has been sampled during vegetation period 2014 in spring and summer.
#' Measurements were taken with a HandySpec Field portable spectrometer (tec5
#' AG Oberursel, Germany). This device has two channels measuring incoming and
#' reflected radiation simultaneously between 305 and 1705 nm in 1 nm steps.
#' 
#' @name spectral_data
#' @docType data
#' @format An object of class `Speclib`
#' @author Wolfgang A. Obermeier, Lukas Lehnert, Hanna Meyer
#' @keywords datasets
NULL





#' Subsetting `Nri`-objects
#' 
#' Return subsets of `Nri`-objects which meet conditions.
#' 
#' Matchable objects are attributes data. Use column names to identify the
#' respectrive attribute. See [attribute()] to access attributes of a
#' `Nri`. IDs of samples may be accessed using "id.nri" as variable name.
#' 
#' @aliases subset.nri subset,Nri-method
#' @param x Object of class 'Nri'.
#' @param subset Logical expression indicating spectra to keep: missing values
#' are taken as false. See details section.
#' @param ...  Further arguments passed to [agrep()].
#' @return Object of class `Nri`.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Nri}}, [attribute()]
#' @keywords utilities
#' @examples
#' 
#' %   \dontrun{
#' data(spectral_data)
#' 
#' ## Calculate all possible combinations for WorldView-2-8
#' spec_WV <- spectralResampling(spectral_data, "WorldView2-8",
#'                               response_function = FALSE)
#' nri_WV <- nri(spec_WV, recursive = TRUE)
#' 
#' ## Return names of attributes data
#' names(attribute(nri_WV))
#' 
#' ## Devide into both seasons
#' sp_summer <- subset(nri_WV, season == "summer")
#' sp_spring <- subset(nri_WV, season == "spring")
#' 
#' ## Print both Nri-objects
#' sp_summer
#' sp_spring
#' %   }
#' 
NULL





#' Subsetting speclibs
#' 
#' Return subsets of `Speclib`s which meet conditions.
#' 
#' Matchable objects are attributes data. Use column names to identify the
#' respectrive attribute. See [attribute()] to access attributes of a
#' `Speclib`. IDs of spectra may be accessed using "id.speclib" as
#' variable name.
#' 
#' @aliases subset.speclib subset,Speclib-method
#' @param x Object of class 'Speclib'.
#' @param subset Logical expression indicating spectra to keep: missing values
#' are taken as false. See details section.
#' @param ...  Further arguments passed to [agrep()].
#' @return Object of class `Speclib`.
#' @author Lukas Lehnert
#' @seealso \code{\linkS4class{Speclib}}, [attribute()]
#' @keywords utilities
#' @examples
#' 
#' %   \dontrun{
#' data(spectral_data)
#' 
#' ## Return names of attributes data
#' names(attribute(spectral_data))
#' 
#' ## Devide into both seasons
#' sp_summer <- subset(spectral_data, season == "summer")
#' sp_spring <- subset(spectral_data, season == "spring")
#' 
#' ## Plot both speclibs
#' plot(sp_summer, col="darkgreen")
#' plot(sp_spring, col="darkred", new=FALSE)
#' %   }
#' 
NULL





#' import USGS spectra
#' 
#' Import and download spectral data from USGS spectral library
#' 
#' 
#' @aliases USGS_get_available_files USGS_retrieve_files
#' @param url Character passing the url of the data. If NULL, the following URL
#' is used:
#' 'ftp://ftpext.cr.usgs.gov/pub/cr/co/denver/speclab/pub/spectral.library/splib06.library/ASCII/'
#' @param avl List of available files. Typically the result of
#' `USGS_get_available_files`.
#' @param pattern Search pattern to define a subset of all available spectra.
#' @param retrieve Logical. Should the data be downloaded?
#' @param loadAsSpeclib Logical. If TRUE, an object of class "Speclib" is
#' retured
#' @param tol Discrepancy of the wavelength values between different spectra.
#' @author Lukas Lehnert
#' @keywords multivariate
#' @examples
#' 
#' \dontrun{
#' ## Retrieve all available spectra
#' avl <- USGS_get_available_files()
#' 
#' ## Download all spectra matching "grass-fescue"
#' grass_spectra <- USGS_retrieve_files(avl = avl, pattern = "grass-fescue")
#' 
#' plot(grass_spectra)
#' }
#' 
NULL



