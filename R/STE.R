"STE" <-
  function(trips, n ,d=2, maxit =1000,report=100, iniX = "rand"){

    if(iniX[1]=="rand"){
      iniX <- mvrnorm(n,rep(0,d),diag(d))
    }

    inivecx <- as.vector(iniX)
    optSTE <- optim(par=inivecx, fn = Obj.STE , gr= Grad.STE ,trips=trips,n=n,method ="BFGS",control = list( trace = TRUE,maxit=maxit,REPORT=1))

  }
