for(i in 1:length(diseases2023)){
  for(j in 1:length(diseases2023[[i]])){
    diseases2023[[i]][[j]]$duration <- diseases2023[[i]][[j]]$Prevalence/diseases2023[[i]][[j]]$Incidence
    diseases2023[[i]][[j]]$clearance <- 1/diseases2023[[i]][[j]]$duration
    diseases2023[[i]][[j]]$probdying <- diseases2023[[i]][[j]]$Deaths/diseases2023[[i]][[j]]$Incidence
    diseases2023[[i]][[j]]$IncNorm <- diseases2023[[i]][[j]]$Incidence/mean(diseases2023[[i]][[j]]$Incidence, na.rm = T)
    diseases2023[[i]][[j]]$PrevNorm <- diseases2023[[i]][[j]]$Prevalence/mean(diseases2023[[i]][[j]]$Prevalence, na.rm = T)
    diseases2023[[i]][[j]]$DeathsNorm <- diseases2023[[i]][[j]]$Deaths/mean(diseases2023[[i]][[j]]$Deaths, na.rm = T)
    diseases2023[[i]][[j]]$ClearNorm <- diseases2023[[i]][[j]]$clearance/mean(diseases2023[[i]][[j]]$clearance, na.rm = T)
    diseases2023[[i]][[j]]$Vulnerability <- diseases2023[[i]][[j]]$probdying/mean(diseases2023[[i]][[j]]$probdying, na.rm = T)
    
    diseases2023[[i]][[j]] <-  diseases2023[[i]][[j]] %>% 
                                       left_join(demography_2017[[i]], by = "Age") %>%
                                       left_join(baseline_mort[[i]], by = "Age") 
    
  }
}

diseases2023 <- rapply(diseases2023, f=function(x) ifelse(is.infinite(x),0,x), how="replace" )
diseases2023 <- rapply(diseases2023, f=function(x) ifelse(is.nan(x),0,x), how="replace" )
diseases2023 <- rapply(diseases2023, f=function(x) ifelse(is.na(x),0,x), how="replace" )