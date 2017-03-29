#' nbi_raster
#' 
#' @title Calculates all possible combinations of narrow band indices 
#' @description Calculates all possible combinations of narrow band indices 
#' 
#' @param x Object of class `HyperSpecRaster`.
#' @param weighted Logical indicating if reflectance values should be
#' interpolated to fit wavelength position. If `FALSE` the reflectance
#' values of nearest neighbour to passed position are returned. See
#' [get_reflectance()] for further
#' explanation.
#' @param filename (optional) Filename of the raster file written to disk. 
#' 
#' @return Object of class `RasterBrick`
#' 
#' @details Caution! This function creates a somewhat large raster file. 
#' Example: If your 'HyperSpecRaster' has 126 bands, 7875 indices will be calculated. 
#' File size would be 1,7 GB in native R raster format (.gri).  
#' 
#' This function is to be used with objects of class `HyperSpecRaster`. The equivalent 
#' function for objects of class `Speclib` is [nri()].
#' 
#' \subsection{Raster formats and layer names}{When calculating hundres/thousands of indices
#' you need layer names. Formats like .tif do not support layer names. It is strongly
#' recommended to use the native format of the `raster` package in R (.gri) which preserves layer 
#' names. However, .gri files cannot simply be opened in other applications. To get this working,
#' you need to create an additional `.hdr` file. See examples how to do so.}
#' 
#' @author Patrick Schratz
#' @seealso [vegindex()], [nri()], [get_reflectance()]
#' @examples
#' 
#' data(hyperspecs)
#' nbi <- nbi_raster(hyperspecs, nl = 7875)
#' 
#' # create header for raster file 
#' hdr(nbi, format = "ENVI")
#' 
#' @export
#' 
nbi_raster <- function(x, weighted = TRUE, bnames = "NBI", 
                       filename = NULL, ...) {  
  
  if (!class(x) == "HyperSpecRaster") {
    stop("Input is not of class 'HyperSpecRaster'")
  }
  
  bnames_bak <- bnames
  
  out_ras <- x
  
  if (is.null(filename)) {
    filename_init <- filename
    filename <- paste0(tempdir(), "/nbi")
  }
  
  filename <- trim(filename)
  
  out_ras <- writeStart(out_ras, filename, overwrite = TRUE, ...)
  
  bs <- blockSize(x)
  pb <- pbCreate(bs$n, ...)
  
  for (i in 1:bs$n) {
    
    v <- hsdar::getValuesBlock(x, row = bs$row[i], nrows = bs$nrows[i] )
    
    s <- spectra(v)
    w <- wavelength(v)
    
    # Narrow Band Indices Mon Mar 27 18:00:37 2017 -------------------------------
    position1 <- position2 <- w
    
    # calculation of narrow band indices 
    lapply(seq_along(1:(length(w))), function(u) 
      sapply(seq_along(1:(length(w))), function(i) 
        (get_reflectance(s, w, position1[u], weighted) - get_reflectance(s, w, position2[i], weighted)) /
          (get_reflectance(s, w, position1[u],weighted) + get_reflectance(s, w, position2[i], weighted)))) -> v
    
    ### remove unwanted indices 
    # - self_band indices (e.g.B1_B1)
    # - duplicate indices (e.g. B15_B71 <-> B71_B15). First combination is removed
    # total number of valid indices: (nbands*(nabnds-1)) / 2.         Numbers example: 126*125 = 15750 / 2 = 7875
    
    v <- lapply(seq_along(1:(length(w))), function(i) {
      if (i > 1) {
        v[[i]] <- v[[i]][, 1:(i - 1)] # subset matrices so that we have only one combination of each index (e.g. B3_B1)
      } else {
        v[[i]] <- NULL
      }
    })
    v <- v[-1]
    
    # list 'v' here contains no band1, 1 entry for band 2 (b2_b1), 2 entries for band 3 (b3_b1, b3_b2) etc. 
    
    # convert to matrix for further processing
    v <- as.matrix(v)
    
    
    ### band names
    
    # restore initial bnames vector otherwise it is appended multiple times during the apply call
    bnames <- bnames_bak
    
    # create band names 
    bnames_calc <- lapply(seq_along(1:(dim(v)[1] + 1)), function(z) 
      sapply(seq_along(1:length(x@wavelength)), function(i) 
        paste0("NBI_b", z, "_", "b", i)))
    
    bnames_calc <- lapply(seq_along(1:(length(w))), function(i) {
      if (i > 1) {
        bnames_calc[[i]] <- bnames_calc[[i]][1:(i - 1)] # same as subset approach for indices (see ABOVE)
      } else {
        bnames_calc[[i]] <- NULL
      }
    })
    
    # convert list to vector
    bnames_calc[-1] %>% unlist() -> bnames_calc
    
    # combine matrices into single matrix
    v <- do.call(cbind, v)
    
    # set names of output raster
    names(out_ras) <- bnames_calc
    
    out_ras <- writeValues(out_ras, v, bs$row[i])
    pbStep(pb, i)
  }
  out_ras <- writeStop(out_ras)
  
  # load raster from tempdir if not written to disk
  if (is.null(filename_init)) {
    out_ras <- brick(filename)
  }
  
  return(out_ras)
  
}
