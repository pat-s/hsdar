#' @name vegindex_available
#' @title Returns available vegetation indices
#' @description Returns available vegetation indices
#'
#' Used in `vegindex`. 
#' 
#' @internal
#' @export
#' 

vegindex_available <- function() {
  av <- c("NDVI","OSAVI","SAVI","MTVI","NDWI","PWI",
          "MSI", "SRWI","GMI1","GMI2","MCARI","TVI",
          "Vogelmann4","Boochs","Boochs2",
          "CARI","CI","Carter","Carter2","Carter3","Carter4",
          "Carter5","Carter6","Datt","Datt2","Datt3","Datt4",
          "Datt5","Datt6","DD","DDn","D1","D2","EVI","EGFR","EGFN",
          "GI","Gitelson","Gitelson2","Green NDVI","MCARI/OSAVI",
          "MCARI2","MCARI2/OSAVI2","mNDVI","mND705","Maccioni",
          "mREIP","MSAVI","mSR","mSR705","mSR2","MTCI","NDVI2",
          "NDVI3","NPCI","OSAVI2","RDVI","REP_LE","REP_Li",
          "SIPI","SPVI","SR","SR1","SR2","SR3","SR4","SR5","SR6",
          "SR7", "SR8","SRPI","Sum_Dr1","Sum_Dr2","TCARI","TCARI2",
          "TCARI/OSAVI","TCARI2/OSAVI2","Vogelmann","NDLI",
          "Vogelmann2","Vogelmann3","PRI","CAI","NDNI",
          "PSSR", "PSND", "CRI1", "CRI2", "CRI3",
          "CRI4", "MPRI", "PRI*CI2", "CI2", "PSRI", "ClAInt", 
          "TGI", "PRI_norm","PARS","DPI","Datt7","Datt8",
          "GDVI_2","GDVI_3","GDVI_4","LWVI1","LWVI2",
          "DWSI1","DWSI2","DWSI3","DWSI4","DWSI5",
          "SWIR FI", "SWIR LI", "SWIR SI", "SWIR VI"
  )
  return(sort(av))
}  

#' @name return_index
#' @title returns vegetation indeces
#' @description returns vegetation indeces
#' useless since it does not do anything because 'x' is just returned ???
#'
#' Used in `vegindex`. 
#' 
#' @internal
#' @export
#' 
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
