# Author: Bruno S. Carturan

# The goal of the script is to conduct the model sensitivity analysis to initial 
# conditions.

wd <- getwd()
wd_data <- paste(wd,"data",sep="/") 
wd_data_raw <- paste(wd,"data_raw",sep="/") 
wd_Rscripts <- paste(wd,"Rscripts",sep="/")
wd_figures <- paste(wd,"figures",sep="/")

library(deSolve)

source(file = paste(wd_Rscripts,"Functions.R",sep = "/"))

# Import parameters values:
fileName <- "/parameters.initial.dist.51.13.RData"
parameters <- readRDS(file = paste(wd_data_raw,fileName,sep="/"))

# Scenario: this is the same asfor the global sensivity analysis, i.e., the
# "Calibration" = "Resource discontinuity" scenario in the paper.
parameters$Tw <- treatments.Tw$calibration

nb.days <- 23
t <- seq(0.01,nb.days*12, by = 0.01) # in h

# the initial state:
states_df <- t(as.data.frame(state))

# determine the values for B, Rnw and Wn to vary:
# -100%, -50%, 0%, +50%, +100%
percent_change <- c(0,-100,-50,50,100)

changes_l <- list(states_df)
names(changes_l)[1] <- "BaseLine"
count <- 2
for(var in c("B","Rnw","Wn")){
  dfHere <- states_df
  dfHere <- dfHere[-1,]
  for(i in 2:length(percent_change)){
    dfHereHere <- states_df
    dfHereHere[,var] <- states_df[,var] + states_df[,var] * percent_change[i] / 100
    dfHere <- rbind(dfHere,dfHereHere)
  }
  changes_l[[count]] <- dfHere
  names(changes_l)[count] <- var
  count <- count + 1
}

# run the corresponding simulations and place the outputs in a list

outputs_l <- list()
count <- 1
for(var in names(changes_l)){
  statesHere <- changes_l[[var]]
  outputHere <- lapply(X = 1:nrow(statesHere), FUN = function(r){
    # r <- 1
    stateHereHere <- statesHere[r,]
    
    out.1 <- ode(y = stateHereHere, times = t, func = ODE.m, parms = parameters)
    out.2 <- as.data.frame(out.1)
    out.2$CY <- crop.yield.fun(data.ts = out.2,parameters = parameters,convert.to.day = F,
                               modif.tot.amount = T, plot.PS = F) # in g for the total area
    out.2[,1] <- out.2[,1] / 12 # conversion in days
    out.2 <- out.2[out.2[,1] %in% 0:90,]
  })
  if(length(outputHere) == 1){
    names(outputHere) <- "0"
  }else{
    names(outputHere) <- as.character(percent_change[2:length(percent_change)])
  }
  outputs_l[[count]] <- outputHere
  names(outputs_l)[count] <- var
  count <- count + 1
}

# saveRDS(outputs_l,paste0(wd_data_raw,"/SensitivityInitialConditionsData.rds"))
outputs_l <- readRDS(paste0(wd_data_raw,"/SensitivityInitialConditionsData.rds"))
# Bruno is here






# base line results:

out.1 <- ode(y = state, times = t, func = ODE.m, parms = parameters)
out.2 <- as.data.frame(out.1)
out.2$CY <- crop.yield.fun(data.ts = out.2,parameters = parameters,convert.to.day = F,
                           modif.tot.amount = T, plot.PS = F) # in g for the total area
out.2[,1] <- out.2[,1] / 12 # conversion in days
out.2 <- out.2[out.2[,1] %in% 0:90,]
figure3.fun(out.2,parameters = parameters, print = print.fig, 
            wd_figures = wd_figures, total.yield = T,plot.lines = T,
            y.max.H.Wn = y.max.H.Wn, y.max.U.S = y.max.U.S, y.max.B = y.max.B, 
            y.max.Rn = y.max.Rn, y.max.R = y.max.R, y.max.CY = y.max.CY,
            var.goal = var.goal, show.var.goal = T,legend.box = T, # y.max.R = y.max.R,
            nameFile = paste0("Figure.SensibilityInitalCond_",parameters$Tw,"_A=",parameters$A,".jpeg"))


# 


nb.days <- 23
t <- seq(0.01,nb.days*12, by = 0.01) # in h

out.1 <- ode(y = state, times = t, func = ODE.m, parms = parameters)
out.2 <- as.data.frame(out.1)
out.2$CY <- crop.yield.fun(data.ts = out.2,parameters = parameters,convert.to.day = F) # in g for the total area
out.2[,1] <- out.2[,1] / 12 # conversion in days
out.2 <- out.2[out.2[,1] %in% 0:90,]
figure3.fun(out.2,parameters = parameters,y.max.R = NA, print = F, y.max.H.Wn = 40,
            wd_figures = wd_figures)
# 
state_0 <- state
state_0["Wn"] <- 0

out.3 <- ode(y = state_0, times = t, func = ODE.m, parms = parameters)
out.4 <- as.data.frame(out.3)
out.4$CY <- crop.yield.fun(data.ts = out.4,parameters = parameters,convert.to.day = F) # in g for the total area
out.4[,1] <- out.4[,1] / 12 # conversion in days
out.4 <- out.4[out.4[,1] %in% 0:90,]
figure3.fun(out.4,parameters = parameters,y.max.R = NA, print = F, y.max.H.Wn = 40,
            wd_figures = wd_figures)
