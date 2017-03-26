#' Load additional documents
#' 
#' Access help documents and references for different methods.
#' 
#' 
#' @param doc Name of document to load. Currently, only \code{"References.pdf"}
#' and \code{"Copyright"} are available
#' @author Lukas Lehnert
#' @keywords utilities documentation
#' @examples
#' 
#' \dontrun{
#' ## Open references of hyperspectral vegetation indices (PDF-file)
#' hsdardocs("References.pdf")
#' 
#' ## See copyrights of routines and data used in hsdar-package (ascii-file)
#' hsdardocs("Copyright")
#' }
#' 
#' @export hsdardocs
hsdardocs <- function(doc)
{
if (doc == "References.pdf")
{
  doc <- file.path(system.file(package = "hsdar"), "doc", doc) 
  if (.Platform$OS.type == "windows")
  {
    shell.exec(doc)
  } else {
    system(paste(getOption("pdfviewer"), doc, "&"))
  }
}
if (toupper(doc) == "COPYRIGHT")
{
  doc <- file.path(system.file(package = "hsdar"), "COPYRIGHTS") 
  if (.Platform$OS.type == "windows")
  {
    file.show(doc)
  } else {
    system(paste(getOption("pager"), doc, "&"))
  }
}
if (doc == "hsdar-intro.pdf")
{
  doc <- file.path(system.file(package = "hsdar"), "doc", doc) 
  if (.Platform$OS.type == "windows")
  {
    shell.exec(doc)
  } else {
    system(paste(getOption("pdfviewer"), doc, "&"))
  }
}
}
