setMethod("[", "Nri",
          function(x, i, ...)
{  
  x@nri <- distMat3D(x@nri[,,i], lower_tri = TRUE)
  if (ncol(attribute(x)) == 1)
  {
    nam <- names(attribute(x))
    tmp <- data.frame(XXX = attribute(x)[i,])
    names(tmp) <- nam
    attribute(x) <- tmp
  } else { 
    attribute(x) <- attribute(x)[i,]
  }
  return(x)  
})
