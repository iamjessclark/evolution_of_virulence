
  # mortality ----
load_mort_list <- function(){
  SEasiaOceania_mort <- load_mortality("2016.mortality.SEaAs.EaAs.Ocean.csv") 
  SSA_mort <- load_mortality("2016.mortality.SubSahAf.csv")
  SAsia_mort <- load_mortality("2016.mortality.SAsia.csv")
  LatAmericaCarr_mort <- load_mortality("2016.morality.LatinAmericaCaribb.csv") # note spelling mistake
  NAfricaMidEast_mort <- load_mortality("2016.mortality.NAf.MidE.csv")
  CentralEastEU_mort <- load_mortality("2016.mortality.CEu.EEu.CAs.csv")
  HighIncome_mort <- load_mortality("2016.mortality.high.income.csv")
  
  return(list(high_income = HighIncome_mort, 
              SEA = SEasiaOceania_mort, 
              CenEastEUCentAs = CentralEastEU_mort, 
              SSA = SSA_mort, 
              LatAmerCaribb = LatAmericaCarr_mort, 
              SAsia = SAsia_mort,
              NAfMidEast = NAfricaMidEast_mort))
  
}

baseline_mort <- load_mort_list()
  