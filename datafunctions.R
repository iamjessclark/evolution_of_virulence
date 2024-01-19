load_mortality <- function(file.name){
  mortality <- read.csv(file.name)%>%
    dplyr::select(!"Indicator")
  
  young_mort <- mortality %>%
    filter(Age.Group=="<1 year" | Age.Group=="01-04 years") %>%
    group_by(X) %>%
    summarise(both = sum(both))%>%
    mutate(Age.Group="0-04")
  
  old_mort <- mortality %>%
    filter(Age.Group=="80-84 years" | Age.Group=="85+ years") %>%
    group_by(X) %>%
    summarise(both = sum(both))%>%
    mutate(Age.Group="80+")
  
  mortality <- mortality %>%
    filter(Age.Group!="<1 year" & Age.Group!="01-04 years" & Age.Group!="80-84 years" & Age.Group!="85+ years") %>%
    bind_rows(young_mort)%>%
    bind_rows(old_mort) %>%
    group_by(Age.Group)%>%
    summarise(mortality = sum(both))%>%
    ungroup()%>%
    mutate(proportion = mortality/sum(mortality))
  
  mortality$Age.Group <-  str_replace_all(mortality$Age.Group, " years", "")
  mortality$Age.Group <-  str_replace_all(mortality$Age.Group, "15-19 ", "15-19")
  mortality <- mortality %>% 
    rename(Age = Age.Group, prop.mort = proportion)
  
  return(mortality)  
}

load_demography <- function(df1=NULL, df2=NULL, df3=NULL, df4=NULL, df5=NULL){
  
  if(!is.null(df1)) demodf <- read.csv(df1)
  if(!is.null(df2)) demodf <- bind_rows(demodf, read.csv(df2))
  if(!is.null(df3)) demodf <- bind_rows(demodf, read.csv(df3))
  if(!is.null(df4)) demodf <- bind_rows(demodf, read.csv(df4))
  if(!is.null(df5)) demodf <- bind_rows(demodf, read.csv(df5))
  
  
  demographic_totals <- demodf %>%
    filter(Age == "Total")
  
  demodf$Age <-  str_replace_all(demodf$Age, c("0-4" = "0-04", "05-Sep" = "05-09", "Oct-14" = "10-14"))
  
  demodf$Age <-  str_replace_all(demodf$Age, c("40-044" = "40-44"))
  
  demodf <- demodf %>%
    filter(Age!="Total" & Age!="85+")
  
  demodf <- demodf %>%
    group_by(Age) %>%
    summarise(total = sum(Both.Sexes.Population))%>%
    ungroup()%>%
    mutate(proportion=total/sum(total))
  
  old <- demodf %>%
    filter(Age=="80-84" | Age=="85-89" | Age=="90-94" | Age=="95-99" | Age=="100+")
  
  old[nrow(old)+1,1] <- "80+"
  old[nrow(old),2] <- sum(old$total, na.rm=T)
  old[nrow(old),3] <- sum(old$proportion, na.rm=T)
  old <- old %>%
    filter(Age=="80+")
  
  demodf_return <- bind_rows(demodf, old) %>%
    filter(Age!="80-84" & Age!="85-89" & Age!="90-94" & Age!="95-99" & Age!="100+")
  
  return(demodf_return)
}

load_diseases <- function(diseasefile){
  diseases <- read.csv(diseasefile)
  
  HI_young_diseases <- diseases %>%
    filter(age=="<1 year" | age=="1-04 years") %>%
    group_by(measure, cause, location) %>%
    summarise(val = sum(val))%>%
    mutate(age="0-04")
  
  diseases <- diseases %>%
    filter(age!="<1 year"& age!="1-4 years" ) %>%
    bind_rows(HI_young_diseases)%>%
    select(measure, age, cause, location, val)
  
  diseases$age <-  str_replace_all(diseases$age, " years", "") 
  diseases <- diseases %>% 
            rename(Age = age)
  
  return(diseases)
}

get_location_diseases <- function(filename){
  
  diseases <- load_diseases(filename)
  
  HI_diseases <- diseases %>%
    filter(location == "High-income") %>%
    pivot_wider(names_from = measure, values_from = val)
  
  HI_diseases <- split(HI_diseases, f=HI_diseases$cause)
  
  SEA_diseases <- diseases %>%
    filter(location == "Southeast Asia, East Asia, and Oceania") %>%
    pivot_wider(names_from = measure, values_from = val)
  
  SEA_diseases <- split(SEA_diseases, f=SEA_diseases$cause)
  
  CEstEUcentAs_diseases <- diseases %>%
    filter(location == "Central Europe, Eastern Europe, and Central Asia") %>%
    pivot_wider(names_from = measure, values_from = val)
  
  CEstEUcentAs_diseases <- split(CEstEUcentAs_diseases, f=CEstEUcentAs_diseases$cause)
  
  SSA_diseases <- diseases %>% 
    filter(location == "Sub-Saharan Africa") %>%
    pivot_wider(names_from = measure, values_from = val)
  
  SSA_diseases <- split(SSA_diseases, f=SSA_diseases$cause)
  
  LACaribb_diseases <- diseases %>% 
    filter(location == "Latin America and Caribbean") %>%
    pivot_wider(names_from = measure, values_from = val)
  
  LACaribb_diseases <- split(LACaribb_diseases, f=LACaribb_diseases$cause)
  
  SAsia_diseases <- diseases %>% 
    filter(location == "South Asia") %>%
    pivot_wider(names_from = measure, values_from = val)
  
  SAsia_diseases <- split(SAsia_diseases, f=SAsia_diseases$cause)
  
  NAfMidEast_diseases <- diseases %>% 
    filter(location == "North Africa and Middle East") %>%
    pivot_wider(names_from = measure, values_from = val)
  
  NAfMidEast_diseases <- split(NAfMidEast_diseases, f=NAfMidEast_diseases$cause)
  
  return(list(high_income = HI_diseases, SEA = SEA_diseases, 
              CenEastEUCentAs = CEstEUcentAs_diseases, SSA = SSA_diseases, 
              LatAmerCaribb = LACaribb_diseases, SAsia = SAsia_diseases, 
              NafMidEast = NAfMidEast_diseases))
}

lhs_df <- function(LHSlist, nametemplate){
  
  for(i in 1:length(LHSlist)){
    for(j in 1:length(LHSlist[[i]])){
      
      LHSlist[[i]][[j]]$region <- names(nametemplate)[i]
      LHSlist[[i]][[j]]$disease <- names(nametemplate[[i]])[j]
      
    }
  }
  
  return(do.call(rbind, rrapply::rrapply(LHSlist, classes = "data.frame", how = "flatten")))
  
}

percent.change <- function(new,old){
  value <- ((new-old)/old)*100
  return(value)
}

