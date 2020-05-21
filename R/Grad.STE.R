"Grad.STE" <-
  function(vecx, trips,n){
  
    p = Obj.STE(vecx,trips,n)
    matx = vecx;
    d = length(vecx)/n
    dim(matx) = c(n,d)
    
    if (d > 1){
    nom = exp(-rowSums((matx[trips[,1],] - matx[trips[,2],])^2))
    denom = nom + exp(-rowSums((matx[trips[,1],] - matx[trips[,3],])^2))
    }else{
      nom = exp(-(matx[trips[,1],] - matx[trips[,2],])^2)
      denom = nom + exp(-(matx[trips[,1],] - matx[trips[,3],])^2)
    }
    ps = nom/denom;
    
    dx1 = 2*(matx[trips[,3],] - matx[trips[,2],])
    dx2 = -2*(matx[trips[,1],] - matx[trips[,2],])
    dx3 = 2*(matx[trips[,1],] - matx[trips[,3],])
    
    dx = matrix(0,nrow = n,ncol = d)
    
    for (r in 1:(length(trips)/3)){
      tempdx = matrix(0,nrow = n,ncol = d);
      tempdx[trips[r,1],] = dx1[r,drop=FALSE];
      tempdx[trips[r,2],] = dx2[r,drop=FALSE];
      tempdx[trips[r,3],] = dx3[r,drop=FALSE];
      
      dx = dx + (1-ps[r])*tempdx
      
    }
    return(dx)
  }
