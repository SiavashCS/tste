"Obj.STE" <-
  function(vecx, trips,n){
    
    matx = vecx;
    dim(matx) = c(n,length(vecx)/n)
    
    if (length(vecx)/n > 1){
    
    nom = exp(-rowSums((matx[trips[,1],] - matx[trips[,2],])^2))
    denom = nom + exp(-rowSums((matx[trips[,1],] - matx[trips[,3],])^2))
    }else{
      nom = exp(-(matx[trips[,1],] - matx[trips[,2],])^2)
      denom = nom + exp(-(matx[trips[,1],] - matx[trips[,3],])^2)
    }
    summary(log(nom/denom))
    return(-sum(log(nom/denom)))
    
  }
