lhsdf <- lhsdf %>% 
  filter(disease !="HIV/AIDS") 

lhsdf$region[which(lhsdf$region=="SEA")] <- "Southeast Asia, East Asia, and Oceania" 
lhsdf$region[which(lhsdf$region=="SSA")] <- "Sub-Saharan Africa" 
lhsdf$region[which(lhsdf$region=="CenEastEUCentAs")] <- "Central Europe, Eastern Europe, and Central Asia"
lhsdf$region[which(lhsdf$region=="LatAmerCaribb")] <-  "Latin America and Caribbean" 
lhsdf$region[which(lhsdf$region=="NafMidEast")] <- "North Africa and Middle East"
lhsdf$region[which(lhsdf$region=="SAsia")] <-  "South Asia"
lhsdf$region[which(lhsdf$region=="high_income")] <- "High Income"

lhsdf %>%   
  ggplot()+
  geom_point(aes(x = int_V, y = R0out))+
  facet_grid(disease~region, 
             scales="free", 
             labeller = label_wrap_gen())+
  theme(strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  theme_bw()+
  ylab(bquote(R[0]))+
  xlab("Intrinsic pathogen virulence")

ggsave("R0intrinsicV.pdf")
#save.image("myEnvironment.RData")

# susceptibility to infection ####
susceptibility <- list()

for(i in 1:length(diseases2023)){
  susceptibility[[i]] <- list()
  for(j in 1:length(diseases2023[[i]])){
    
    susceptibility[[i]][[j]] <- diseases2023[[i]][[j]] %>%
      dplyr::select(Age, cause, location, IncNorm)
    
  }
}


susceptibility <- do.call(rbind, rrapply::rrapply(susceptibility, classes = "data.frame", how = "flatten"))
susceptibility <- susceptibility %>%
  mutate(Age = factor(Age, levels = c("0-04", "5-9", "10-14", "15-19", "20-24", 
                                      "25-29", "30-34", "35-39", "40-44", "45-49", 
                                      "50-54", "55-59", "60-64", "65-69", "70-74", 
                                      "75-79", "80+")))

susceptibility %>%
  filter(cause != "HIV/AIDS") %>%
  ggplot() +
  geom_col(aes(x = Age, y = IncNorm, fill = cause), colour="black", alpha = 0.6) + 
  facet_grid(cause ~ location, 
             scales = "free",
             labeller = label_wrap_gen(multi_line = TRUE)) + 
  theme_bw() + 
  scale_fill_npg()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none", 
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  ylab("Susceptibilty to infection")

ggsave("susceptibility.pdf")

# solving for k and m figures ####
intx <- seq(from = 0, to = 1, length.out= 100)

# # this figure will now show y/x^m and b*x^k
# # so selecting the clearance values for each disease and setting
# # not using normalised because mean(clearnorm)=1 for all of them

clearance <- list()
for(i in 1:length(diseases2023)){
  clearance[[i]] <- list()
  for(j in 1:length(diseases2023[[i]])){
    
    df <- as.data.frame(diseases2023[[i]][[j]])
    clearance[[i]][[j]] <- df %>%
      dplyr::select(cause, location, clearance) %>%
      group_by(cause, location) %>%
      summarise(clearance = mean(clearance, na.rm=T))
    
  }
}

clearancedf <- do.call(rbind, rrapply::rrapply(clearance, classes = "data.frame", how = "flatten"))
clearancedf <- clearancedf %>%
  rename(disease = cause, region = location) %>%
  filter(disease != "HIV/AIDS")

kmplot <- expand.grid(intx = intx, k = MaxR0$k, m = MaxR0$m)
kmplot <- MaxR0 %>%
  dplyr::select(k, m, int_V, region, disease) %>%
  left_join(kmplot) %>%
  mutate(xk = intx^k, 
         xm = intx^m) %>%
  filter(disease!="HIV/AIDS") %>%
  mutate(b = ifelse(disease=="Measles", 1.2,
                    ifelse(disease=="Meningitis", 0.05,
                           ifelse(disease=="Tuberculosis", 0.2,
                                  ifelse(disease=="Ebola", 0.15, 0.05)))))#,
# mutate("clearance tradeoff"  = rescale(1/xm),
#        "transmission tradeoff" = 1*xk)

kmplot$region[which(kmplot$region=="SEA")] <- "Southeast Asia, East Asia, and Oceania" 
kmplot$region[which(kmplot$region=="SSA")] <- "Sub-Saharan Africa" 
kmplot$region[which(kmplot$region=="CenEastEUCentAs")] <- "Central Europe, Eastern Europe, and Central Asia"
kmplot$region[which(kmplot$region=="LatAmerCaribb")] <-  "Latin America and Caribbean" 
kmplot$region[which(kmplot$region=="NafMidEast")] <- "North Africa and Middle East"
kmplot$region[which(kmplot$region=="SAsia")] <-  "South Asia"
kmplot$region[which(kmplot$region=="high_income")] <- "High-income"

kmplot <- kmplot %>%
  left_join(clearancedf) %>%
  mutate("clearance" = rescale(clearance/xm), "transmission" = rescale (b*xm))

kmplot <- kmplot[-!is.infinite(kmplot$clearance),]

kmplot %>%
  pivot_longer(cols = c( "clearance","transmission"), names_to = "Trade-off", values_to = "scaled_pars_value") %>%
  ggplot()+
  geom_line(aes(x = intx, y = scaled_pars_value, colour = `Trade-off`), size = 0.87 )+
  facet_grid(disease ~ region,
             labeller = label_wrap_gen(multi_line = TRUE))+
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18))+
  theme_bw()+
  #ylab(bquote(`intrinsic virulence`^`scaling parameter`))+
  ylab("Functional form")+
  xlab("Intrinsic pathogen virulence")+
  scale_colour_manual(values = c("#00A087B2", "#8491B4B2"))
ggsave("kmplot.pdf")

# kmplot %>% 
#   pivot_longer(cols = c("k","m"), names_to = "scaling parameter", values_to = "scaling parameter value") %>% 
#   ggplot()+
#   geom_line(aes(x = intx, y = (intx^`scaling parameter value`), colour = `scaling parameter`), size = 0.87 )+
#   facet_grid(disease ~ region, 
#              labeller = label_wrap_gen(multi_line = TRUE))+
#   theme(strip.text.x = element_text(size = 16),
#         strip.text.y = element_text(size = 16))+
#   theme_bw()+
#   ylab(bquote(`intrinsic virulence`^`scaling parameter`))+
#   xlab("Intrinsic pathogen virulence")+
#   scale_colour_manual(values = c("#00A087B2", "#8491B4B2"))
# ggsave("kmplot.pdf")

# change in intrinsic virulence ####

intrinsicVchange <- MaxR0_future %>%
  dplyr::select(int_V, region, disease, R0out) %>%
  rename(int_V_future = int_V, 
         R0future = R0out) %>%
  left_join(MaxR0)

intrinsicVchange$PercentChangeR0 <- percent.change(intrinsicVchange$R0future, intrinsicVchange$R0out)

intrinsicVchange$region[which(intrinsicVchange$region=="SEA")] <- "Southeast Asia, East Asia, and Oceania" 
intrinsicVchange$region[which(intrinsicVchange$region=="SSA")] <- "Sub-Saharan Africa" 
intrinsicVchange$region[which(intrinsicVchange$region=="CenEastEUCentAs")] <- "Central Europe, Eastern Europe, and Central Asia"
intrinsicVchange$region[which(intrinsicVchange$region=="LatAmerCaribb")] <-  "Latin America and Caribbean" 
intrinsicVchange$region[which(intrinsicVchange$region=="NafMidEast")] <- "North Africa and Middle East"
intrinsicVchange$region[which(intrinsicVchange$region=="SAsia")] <-  "South Asia"
intrinsicVchange$region[which(intrinsicVchange$region=="high_income")] <- "High Income"

intrinsicVchange %>% 
  filter(disease!="HIV/AIDS") %>%
  ggplot() +
  geom_bar(aes(x=disease, y=PercentChangeR0, fill = disease), 
           alpha = 0.6, position="dodge", stat="identity", colour = "black") + 
  geom_text(aes(x=disease, y=PercentChangeR0, label= round(PercentChangeR0, digits = 2)), 
            position=position_dodge(width=0.1), hjust=-0.3)+
  coord_flip() +
  facet_grid(region~., 
             labeller = label_wrap_gen(width = 17), 
             space = "free_y", scales = "free_y")+
  scale_fill_npg()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none", 
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16))+
  ylab("Percent change in R0")+
  xlab("Diseases")
ggsave("R0change percent.pdf")


# age structure change in mortality ####

mortplot <- list()

for(i in 1:length(diseases2023)){
  mortplot[[i]] <- diseases2023[[i]][[1]] %>%
    dplyr::select(Age, location, mortality, proportion.x, proportion.y)
}

mortplot <- do.call(rbind, rrapply::rrapply(mortplot, classes = "data.frame", how = "flatten"))

mortplot <- mortplot %>%
  mutate(Age = factor(Age, levels = c("0-04", "5-9", "10-14", "15-19", "20-24", 
                                      "25-29", "30-34", "35-39", "40-44", "45-49", 
                                      "50-54", "55-59", "60-64", "65-69", "70-74", 
                                      "75-79", "80+")))


mortplot %>%
  rename(`2017` = "proportion.x", `2050` = "proportion.y") %>%
  pivot_longer(cols = c(`2017`, `2050`), names_to = "year", values_to = "popn") %>%
  ggplot() +
  geom_col(aes(x = Age, y = mortality*popn, fill = location), colour="black", alpha = 0.6) + 
  facet_grid(year ~ location, 
             labeller = label_wrap_gen(multi_line = TRUE)) + 
  theme_bw() + 
  scale_fill_npg()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none", 
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  ylab("mortality")
ggsave("mortalityageyears.pdf")

# age structure change in clearance ####

changeclearplot <- list()

for(i in 1:length(diseases2023)){
  changeclearplot[[i]] <- list()
  changeclearplot[[i]][[j]] <- diseases2023[[i]][[j]] %>%
    dplyr::select(Age, location, ClearNorm, proportion.x, proportion.y)
}

changeclearplot <- do.call(rbind, rrapply::rrapply(changeclearplot, classes = "data.frame", how = "flatten"))

changeclearplot <- changeclearplot %>%
  mutate(Age = factor(Age, levels = c("0-04", "5-9", "10-14", "15-19", "20-24", 
                                      "25-29", "30-34", "35-39", "40-44", "45-49", 
                                      "50-54", "55-59", "60-64", "65-69", "70-74", 
                                      "75-79", "80+")))


changeclearplot %>%
  rename(`2017` = "proportion.x", `2050` = "proportion.y") %>%
  pivot_longer(cols = c(`2017`, `2050`), names_to = "year", values_to = "popn") %>%
  ggplot() +
  geom_col(aes(x = Age, y = ClearNorm*popn, fill = location), colour="black", alpha = 0.6) + 
  facet_grid(year ~ location, 
             labeller = label_wrap_gen(multi_line = TRUE)) + 
  theme_bw() + 
  scale_fill_npg()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none", 
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  ylab("clearance")
ggsave("clearanceageyears.pdf")

#save.image("EoVEnvironment2023.RData")

#  age structure change in vulnerability*N ####
changemortplot <- list()

for(i in 1:length(diseases2023)){
  changemortplot[[i]] <- list()
  for(j in 1:length(diseases2023[[i]])){
    changemortplot[[i]][[j]] <- diseases2023[[i]][[j]] %>%
      dplyr::select(Age, location, Vulnerability, proportion.x, proportion.y, cause)
  }
}

changemortplot <- do.call(rbind, rrapply::rrapply(changemortplot, classes = "data.frame", how = "flatten"))

changemortplot <- changemortplot %>%
  mutate(Age = factor(Age, levels = c("0-04", "5-9", "10-14", "15-19", "20-24", 
                                      "25-29", "30-34", "35-39", "40-44", "45-49", 
                                      "50-54", "55-59", "60-64", "65-69", "70-74", 
                                      "75-79", "80+")))


changemortplot %>%
  filter(cause!="HIV/AIDS") %>%
  rename(`2017` = "proportion.x", `2050` = "proportion.y") %>%
  mutate(mort2017 = `2017`*Vulnerability, 
         mort2050 = `2050`*Vulnerability) %>%
  pivot_longer(cols = c(mort2017, mort2050), 
               names_to = "year", 
               values_to = "rate") %>%
  dplyr::select(Age, location, year, rate, cause) %>%
  group_by(year, location, cause) %>%
  summarise(rate = sum(rate)) %>%
  pivot_wider(names_from = year, 
              values_from = rate) %>%
  mutate("Percent Difference" = percent.change(mort2050, mort2017)) %>%
  filter(!is.nan(`Percent Difference`)) %>%
  ungroup() %>%
  ggplot()+
  geom_bar(aes(x=cause, y=`Percent Difference`, fill = cause), 
           alpha = 0.6, position="dodge", stat="identity", colour = "black") + 
  geom_text(aes(x=cause, y=`Percent Difference`, 
                label= round(`Percent Difference`, digits = 2)), 
            position=position_dodge(width=0.1), hjust=-0.3) +
  coord_flip() +
  facet_grid(location~., 
             labeller = label_wrap_gen(width = 17), 
             space = "free_y", scales = "free_y")+
  scale_fill_npg()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none", 
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16))+
  ylab("Percent change in vulnerability to mortality")+
  xlab("Diseases")
ggsave("vulnerabilitychange.pdf")

# vulnerability * present and future X

xs <- MaxR0FutureReps[[1]] %>%
  rename("Future IV" = X_int) %>%
  filter(disease !="HIV/AIDS") %>%
  left_join(MaxR0, by = c("region", "disease")) %>%
  rename("Present IV" = int_V,
         "location" = region, 
         "cause" = disease) %>%
  dplyr::select(location, cause, `Future IV`, `Present IV`) 

xs$location[which(xs$location=="high_income")] <- "High-income"
xs$location[which(xs$location=="SEA")] <- "Southeast Asia, East Asia, and Oceania"
xs$location[which(xs$location=="CenEastEUCentAs")] <- "Central Europe, Eastern Europe, and Central Asia"
xs$location[which(xs$location=="SSA")] <- "Sub-Saharan Africa"
xs$location[which(xs$location=="LatAmerCaribb")] <- "Latin America and Caribbean"
xs$location[which(xs$location=="SAsia")] <- "South Asia"
xs$location[which(xs$location=="NafMidEast")] <- "North Africa and Middle East"

require(RColorBrewer)

changemortplot %>%
  left_join(xs, by = c("location", "cause")) %>%
  filter(!is.na(`Future IV`)) %>%
  mutate(Future = Vulnerability*`Future IV`, 
         Present = Vulnerability*`Present IV`) %>%
  pivot_longer(cols = c(Future, Present), 
               names_to = "Time", values_to = "probability") %>%
  filter(!(cause == "Measles" & Age == "60-64"| cause == "Measles" & Age == "65-69"|
             cause == "Measles" & Age=="70-74"| cause == "Measles" & Age=="75-79"|
             cause == "Measles" & Age == "80+"), 
         !(cause == "Ebola" & Age == "75-79"|cause == "Ebola" & Age == "80+")) %>%
  ggplot()+
  geom_line(aes(x = Age, y = probability,  colour = cause, linetype = Time, group = interaction(cause, Time))) +
  ggh4x::facet_grid2(cause ~ location, 
                     scales = "free_y", 
                     independent = "y", 
                     labeller = label_wrap_gen(width = 17)) +
  theme_bw() +
  theme(axis.text = element_text(angle = 90), 
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  scale_colour_brewer(palette = "Set2")
ggsave("probmort.pdf")
