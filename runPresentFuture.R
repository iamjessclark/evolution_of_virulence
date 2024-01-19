source("setup.R")

# Present ####
min <- c(0,0,0)
max <- c(1,1,1)
params <- c("k", "m", "int_V")
params <- cbind.data.frame(params, min, max)
n=1000000

# Present 
R0_now <- R0present(n=n, 
                    params = params, 
                    variableslist = diseases2023)

lhsdf <- lhs_df(R0_now1, diseases2023) 

MaxR0 <- lhsdf %>% 
  group_by(disease, region) %>%
  filter(R0out == max(R0out) & R0out !=0)

#save.image("EoVEnvironment2023.RData")

# Future ####
f = 100
min <- 0
max <- 1
n=100000
paramsfuture <- "X_int"
paramsfuture <- cbind.data.frame(paramsfuture, min, max)

start = Sys.time()
MaxR0FutureReps <-
  mclapply(1:f, function(l) {
    
    R0_future_reps <- R0future(n=n, 
                               params = paramsfuture, 
                               variables = diseases2023, 
                               maxR0 = MaxR0)
    names(R0_future_reps) <- names(diseases2023)
    for(r in 1:length(R0_future_reps)){
      names(R0_future_reps[[r]]) <- names(diseases2023[[1]])
    }
    
    R0futuredf_reps <- lhs_df(R0_future_reps, diseases2023)
    MaxR0_future_reps <- R0futuredf_reps %>% 
      dplyr::group_by(disease, region) %>%
      dplyr::filter(R0out == max(R0out)& R0out !=0)
    
    MaxR0_future_reps
  }, mc.cores = 15)

end = Sys.time()
print(start-end)
#save.image("EoVEnvironment2023_reps.RData")

