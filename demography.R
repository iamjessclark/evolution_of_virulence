# demography ----


#### High Income ####
load_2017_demo <- function(){
  high_income_2017 <- load_demography(df1 = "2017.pop.Australasia.csv", 
                                      df2 = "2017.pop.HighIncomeAsiaPacific.csv", 
                                      df3 = "2017.pop.HighIncomeNorthAmerica.csv", 
                                      df4 = "2017.pop.SouthernLatinAmerica.csv", 
                                      df5 = "2017.pop.WesternEurope.csv")
  
  #### South East Asia ####
  
  SE_Asia_2017 <- load_demography(df1 = "2017.pop.SouthAsia.csv", 
                                  df2 = "2017.pop.Oceania.csv", 
                                  df3 = "2017.pop.EastAsia.csv")
  
  #### Sub-Saharan Africa ####
  
  SSA_2017 <- load_demography(df1 = "2017.pop.WesternSubSaharanAfrica.csv", 
                              df2 = "2017.pop.SouthernSubSaharanAfrica.csv", 
                              df3 = "2017.pop.CentralSubsaharanAfrica.csv", 
                              df4 = "2017.pop.EasternSubSaharanAfrica.csv")

  #### South Asia ####
  
  South_Asia_2017 <- load_demography("2017.pop.SouthAsia.csv")

  
  #### Latin America and Caribbean ####
  
  LatAmCaribb_2017 <- load_demography(df1 = "2017.pop.TropicalLatinAmerica.csv", 
                                      df2 = "2017.pop.Caribbean.csv", 
                                      df3 = "2017.pop.AndeanLatinAmerica.csv", 
                                      df4 = "2017.pop.CentralLatinAmerica.csv")
  
  #### North Africa & Middle East ####
  
  NAfricaMidEast_2017 <- load_demography("2017.pop.NorthAfricaMiddleEast.csv")

  #### Central and Eastern EU and Central Asia ####
  
  CentralEastEUCentralAsia_2017 <- load_demography(df1 = "2017.pop.CentralAsia.csv", 
                                                   df2 = "2017.pop.CentralEurope.csv", 
                                                   df3 = "2017.pop.EasternEurope.csv")

  return(list(high_income = high_income_2017, 
              SEA = South_Asia_2017, 
              CenEastEUCentAs = CentralEastEUCentralAsia_2017, 
              SSA = SSA_2017, 
              LatAmerCaribb = LatAmCaribb_2017, 
              SAsia = South_Asia_2017, 
              NAfMidEast = NAfricaMidEast_2017))
}

demography_2017 <- load_2017_demo()

load_2050_demography <- function(){
  high_income_2050 <- load_demography(df1 = "2050.demo.high.income.csv")
  SSA_2050 <- load_demography(df1 = "2050.demo.SubShAf.csv")
  SE_Asia_2050 <- load_demography(df1 = "2050.demo.SEAs.EasAs.Oc.csv")
  LatAmCaribb_2050 <- load_demography("2050.demo.LatAmer.Carrib.csv")
  CentralEastEUCentralAsia_2050 <- load_demography("2050.demo.CenEuEasEuCenAs.csv")
  South_Asia_2050 <- load_demography("2050.demo.SAsia.csv")
  NAfricaMidEast_2050 <- load_demography("2050.demo.NAf.MidEa.csv")

  return(list(high_income = high_income_2050, 
              SEA = South_Asia_2050, 
              CenEastEUCentAs = CentralEastEUCentralAsia_2050, 
              SSA = SSA_2050, 
              LatAmerCaribb = LatAmCaribb_2050, 
              SAsia = South_Asia_2050, 
              NAfMidEast = NAfricaMidEast_2050))  
}

demography_2050 <- load_2050_demography()