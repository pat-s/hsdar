#' hsdar_parallel
#' 
#' Get all functions which support parallel execution
#' 
#' Parallel execution is performed via the \pkg{foreach}-package. Care is taken
#' that a function will never run in parallel if the calling function is
#' already using multicore processing.
#' 
#' @return Vector containing supported function names
#' @author Lukas Lehnert
#' @keywords utilities
#' @examples
#' 
#' \dontrun{
#' data(spectral_data)
#' ## Load library
#' library(doMC)
#' ## Register number of workers
#' registerDoMC(3)
#' 
#' supported_functions <- hsdar_parallel()
#' supported_functions
#' 
#' ## Transform speclib using 3 cores
#' bd <- transformSpeclib(spectral_data)
#' }
#' 
#' @export hsdar_parallel
hsdar_parallel <- function()
{
  sort(c("transformSpeclib", "glm.nri"
         ))
}

.process_parallel <- function()
{
  process_parallel <- list(parallel = any(search() == "package:doMC"), dofun = NULL, donestedfun = NULL)
  if (process_parallel[[1]])
  {
    if (.getParallel())
    {   
      loadNamespace("foreach")
      process_parallel$dofun <- if (foreach::getDoParWorkers() > 1) foreach::`%dopar%` else  foreach::`%do%`
      process_parallel$donestedfun <- foreach::`%:%`
    } else {
      process_parallel[[1]] <- FALSE
    }
  }  
  return(process_parallel)
}

.getParallel <- function()
{
  res <- .Fortran("adminparallel",
                  flag    = as.integer(1),
                  process = as.integer(0),
                  PACKAGE = "hsdar"
                 )$process
  return(res == -1)
}
  
  
.restoreParallel <- function()
{
  res <- .Fortran("adminparallel",
                  flag    = as.integer(0),
                  process = as.integer(0),
                  PACKAGE = "hsdar"
                 )
}
