#' nbi
#' 
#' Function calculates all possible combinations of narrow band indices
#' 
#' 
#' @param x Object of class `HyperSpecRaster`.
#' @param weighted Logical indicating if reflectance values should be
#' interpolated to fit wavelength position. If `FALSE` the reflectance
#' values of nearest neighbour to passed position are returned. See
#' [get_reflectance()] for further
#' explanation.
#' @param filename (optional) Filename of the raster file written to disk. Only used if 
#' an object of class `HyperSpecRaster` is provided.
#' 
#' 
#' @return Object of class RasterBrick
#' 
#' @author Patrick Schratz
#' @seealso [vegindex()], [nri()], [get_reflectance()]
#' @examples
#' 
#' data(spectral_data)
#' nbi <- nbi(spectral_data, "NDVI")
#' example data missing here
#' @export
#' 
nbi <- function(x, index = "NBI", weighted = TRUE, bnames = "NBI", 
                filename = '', ...) {  
  
  if (!class(x) == "HyperSpecRaster") {
    stop("Input is not of class 'HyperSpecRaster'")
  }
  
  bnames_bak <- bnames
  
  out_ras <- x
  
  # get nbands
  nlayers <- x@file@nbands^2 / 2
  
  big <- !canProcessInMemory(out_ras, 3)
  filename <- trim(filename)
  if (big & filename == '') {
    filename <- rasterTmpFile()
  }
  if (filename != '') {
    out_ras <- writeStart(out_ras, filename, overwrite = TRUE, ...)
    todisk <- TRUE
  } else {
    vv <- matrix(ncol = nrow(out_ras), nrow = ncol(out_ras))
    todisk <- FALSE
  }
  
  bs <- blockSize(x)
  pb <- pbCreate(bs$n, ...)
  
  if (todisk) {
    
    for (i in 1:bs$n) {
      
      v <- hsdar::getValuesBlock(x, row = bs$row[i], nrows = bs$nrows[i] )
      
      s <- spectra(v)
      w <- wavelength(v)
      
      # Narrow Band Indices Mon Mar 27 18:00:37 2017 -------------------------------
      if (index == "NBI") {
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
      }
      
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
  } else {
    for (i in 1:bs$n) {
      v <- getValuesBlock(x, row = bs$row[i], nrows = bs$nrows[i] )
      
      s <- spectra(v)
      w <- wavelength(v)
      
      # Narrow Band Indices Mon Mar 27 18:00:37 2017 -------------------------------
      if (index == "NBI") {
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
      }
      
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
      
      # cols <- bs$row[i]:(bs$row[i] + bs$nrows[i] - 1)
      # vv[,cols] <- matrix(v, nrow = out_ras@ncols)
      
      # convert to brick
      out_ras <- brick(out_ras, nl = length(bnames_calc))
      # set names of output raster
      names(out_ras) <- bnames_calc
      
      out_ras <- setValues(out_ras, v, bs$row[i])
      
      
      pbStep(pb, i)
    }
    #out_ras <- setValues(out_ras, as.vector(vv))
  }
  pbClose(pb)
  return(out_ras)
  
}
