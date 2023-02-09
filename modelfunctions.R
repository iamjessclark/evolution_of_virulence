r0func <- function(b, N, delta, clearance, vulnerability, k, m, intX, incidence){
  virulence <- alpha(vulnerability, intX)
  agespecr0 <- b*(vulnerability*(intX^k))/((clearance/(vulnerability*(intX^m)))+virulence+delta)
  r0_time <- sum(incidence*N*agespecr0, na.rm=T)
  return(r0_time)
}

alpha <- function(vulnerability, intX){
  y <- vulnerability*intX
  return(y)
}

R0present <- function(n, params, variableslist){
  LHSlist <- list()

  for(i in 1:length(variableslist)){
  
    LHSlist[[i]] <- list()
    for(j in 1:length(variableslist[[i]])){
    
    # create LHS values
    r <- randomLHS(n,length(params[,1]))
    parmVals <- lapply(1:length(params[,1]),function(x){
      temp <- params[x,]
      randomSample <- runif(r[,x], min=temp$min, max=temp$max)
    })
    parmVals <- do.call(cbind.data.frame,parmVals)
    names(parmVals) <- params$params
    parmVals$run <- 1:n

      
      contact <- 3
      b <- contact*sum(variableslist[[i]][[j]]$IncNorm*variableslist[[i]][[j]]$proportion, na.rm=T)
      parmVals$R0out <- NA
     
        for(k in 1:nrow(parmVals)){
          
          parmVals[k,ncol(parmVals)] <- r0func(b = b, 
                                               N = variableslist[[i]][[j]]$proportion, 
                                               delta = variableslist[[i]][[j]]$prop.mort, 
                                               clearance = variableslist[[i]][[j]]$ClearNorm, 
                                               vulnerability = variableslist[[i]][[j]]$Vulnerability, 
                                               k = parmVals[k,1], 
                                               m = parmVals[k,2], 
                                               intX = parmVals[k,3], 
                                               incidence = variableslist[[i]][[j]]$IncNorm)
          
          
          
        }
      LHSlist[[i]][[j]] <- parmVals
      
    }
  }
  return(LHSlist)
}
