r0func <- function(b, N, delta, clearance, vulnerability, k, m, intX, incidence){
  virulence <- alpha(vulnerability, intX)
  agespecr0 <- b*(vulnerability*(intX^k))/((clearance/(vulnerability*(intX^m)))+virulence+delta)
  r0_time <- sum(N*agespecr0, na.rm=T)
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

      contact <- ifelse(names(variableslist[[i]][j])=="Measles", 1.2, 
                    ifelse(names(variableslist[[i]][j])=="Meningitis", 0.05, 
                      ifelse(names(variableslist[[i]][j])=="Tuberculosis", 0.2, 
                        ifelse(names(variableslist[[i]][j])=="Ebola", 0.15, 0.05))))
      
    # create LHS values
    r <- randomLHS(n,length(params[,1]))
    parmVals <- lapply(1:length(params[,1]),function(x){
      temp <- params[x,]
      randomSample <- runif(r[,x], min=temp$min, max=temp$max)
    })
    parmVals <- do.call(cbind.data.frame,parmVals)
    names(parmVals) <- params$params
    parmVals$run <- 1:n

      b <- contact*sum(variableslist[[i]][[j]]$IncNorm, na.rm=T)
      parmVals$R0out <- NA
     
        for(k in 1:nrow(parmVals)){
          
          parmVals[k,ncol(parmVals)] <- r0func(b = b, 
                                               N = variableslist[[i]][[j]]$proportion.x, 
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



R0future <- function(n, params, variables, maxR0){
  LHSlist <- list()
  
  for(i in 1:length(variables)){
    
    LHSlist[[i]] <- list()
    for(j in 1:length(variables[[i]])){
      
      contact <- ifelse(names(variableslist[[i]][j])=="Measles", 1.2, 
                        ifelse(names(variableslist[[i]][j])=="Meningitis", 0.05, 
                               ifelse(names(variableslist[[i]][j])=="Tuberculosis", 0.2, 
                                      ifelse(names(variableslist[[i]][j])=="Ebola", 0.15, 0.05))))
      
      k = dplyr::filter(MaxR0, disease==names(variables[[i]][j]) & region == names(variables[i]))$k
      m = dplyr::filter(MaxR0, disease==names(variables[[i]][j]) & region == names(variables[i]))$m
      # create LHS values
      r <- lhs::randomLHS(n,length(params[,1]))
      parmVals <- lapply(1:length(params[,1]),function(x){
        temp <- params[x,]
        randomSample <- runif(r[,x], min=temp$min, max=temp$max)
      })
      parmVals <- do.call(cbind.data.frame,parmVals)
      names(parmVals) <- params$params
      parmVals$run <- 1:n
    
      b <- contact*sum(variables[[i]][[j]]$IncNorm, na.rm=T)
      parmVals$R0out <- NA
      
      for(r in 1:nrow(parmVals)){
        
        parmVals[r,ncol(parmVals)] <- r0func(b = b, 
                                             N = variables[[i]][[j]]$proportion.y, 
                                             delta = variables[[i]][[j]]$prop.mort, 
                                             clearance = variables[[i]][[j]]$ClearNorm, 
                                             vulnerability = variables[[i]][[j]]$Vulnerability, 
                                             k = k,
                                             m = m, 
                                             intX = parmVals[r,1], 
                                             incidence = variables[[i]][[j]]$IncNorm)
        
        
        
      }
        LHSlist[[i]][[j]] <- parmVals
      
    }
  }
  return(LHSlist)
}