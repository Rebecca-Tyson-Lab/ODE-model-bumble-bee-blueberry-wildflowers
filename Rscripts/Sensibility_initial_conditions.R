# Author: Bruno S. Carturan

# The goal of the script is to conduct the model sensitivity analysis to initial 
# conditions.

# Figure produced:
# Figure.Sensitivity_initial_conditions.jpeg

# File produced:
# data_raw/SensitivityInitialConditionsData.rds

wd <- getwd()
wd_data <- paste(wd,"data",sep="/") 
wd_data_raw <- paste(wd,"data_raw",sep="/") 
wd_Rscripts <- paste(wd,"Rscripts",sep="/")
wd_figures <- paste(wd,"figures",sep="/")

library(deSolve)

source(file = paste(wd_Rscripts,"Functions.R",sep = "/"))

print.fig <- F

# Import parameters values:
fileName <- "/parameters.initial.dist.51.13.RData"
parameters <- readRDS(file = paste(wd_data_raw,fileName,sep="/"))

# Scenario: this is the same as for the global sensitivity analysis, i.e., the
# "Calibration" = "Resource discontinuity" scenario in the paper, but we stop
# before the resources become unavailable.
parameters$Tw <- treatments.Tw$calibration
parameters$A <- 600
parameters$theta_a <- 0.1
parameters$theta_q <- parameters$theta_h <- 2

nb.days <- 23
t <- seq(0,nb.days*12, by = 0.01) # in h

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

saveRDS(outputs_l,paste0(wd_data_raw,"/SensitivityInitialConditionsData.rds"))
outputs_l <- readRDS(paste0(wd_data_raw,"/SensitivityInitialConditionsData.rds"))

x_time <- outputs_l$BaseLine$`0`$time
y_totBees <- rowSums(outputs_l$BaseLine$`0`[,c("Wn","S","Hc","Hw","Uc","Uw")])

Ymax_var <- c(100,15,50)
Ymax_totBees <- 70
lwd_lines <- 2

Y_labs <- c("Number of broods (B)","Amount of nest resources (Rnw, g)","Number of workers in the nest (Wn)")
col_lines <- c("bisque4","darkgoldenrod3","grey50")
names(Y_labs) <- names(col_lines) <- names(Ymax_var) <- c("B","Rnw","Wn")

percent_change <- as.character(c(-100,-50,50,100))
lty_lines <- c(3,4,2,5)
names(lty_lines) <- as.character(percent_change)

var_order <- c("B","Wn","Rnw") # so the order can be changed easily
plotResourcesPoly <- F


coeff <- 1.2
if(print.fig){
  jpeg(filename = paste0(wd_figures,"/Figure.Sensitivity_initial_conditions.jpeg"),
       width = 22*coeff, height = 20*coeff, units = "cm", res = 250)
}
layout(matrix(1:9, nrow = 3, byrow = T), heights = c(1,1,1.2))
for(var_i in 1:length(var_order)){
  # var_i <- 1
  varHere <- var_order[var_i]
  outputs_l_here <- outputs_l[[varHere]]
  y_baseLine <- outputs_l$BaseLine$`0`[,varHere]
  
  # *** plot the variable whose initial value is changed ***
  side1 <- 0.5
  xaxt <- "n"
  if(var_i == 3){
    side1 <- 4.5
    xaxt <- "s"
  }
  par(mar = c(side1,5,0.5,0.5))
  plot(0,0, las = 1, col= "white", xlim = c(0,23), ylim = c(0,Ymax_var[varHere]), 
       xlab = "", ylab = "",xaxt=xaxt)
  if(var_i == 3){
    mtext("Time (days)",side = 1, line = 2.5)
  }
  mtext(Y_labs[varHere],side = 2, line = 3)
  
  if(plotResourcesPoly){
    # plot resources availability
    polygon(x = c(0,23,23,0), y = c(0,0,Ymax_var[varHere],Ymax_var[varHere]),border = F,
            col = add.transparency.colour("blueviolet",alpha = 0.20))
    polygon(x =c(0,23,23,0), y = c(0,0,Ymax_var[varHere],Ymax_var[varHere]),
            density = 5,lwd=1.5,col = "black")
  }
  # plot the base line for that variable
  lines(x = x_time, y = y_baseLine, lwd = lwd_lines, col = col_lines[varHere], lty = 1)
  # plot the lines for the different inital conditions
  for(percent_i in 1:length(percent_change)){
    percent_changeHere <- as.character(percent_change[percent_i])
    lty_linesHere <- lty_lines[percent_changeHere]
    y <- outputs_l_here[[percent_changeHere]][,varHere]
    lines(x = x_time, y = y, lwd = lwd_lines, col = col_lines[varHere], lty = lty_linesHere)
  }
  # legend
  if(var_i == 1){
    legend("bottomright",c("base line","-100%","-50%","+50%","+100%"),lty = c(1,lty_lines),
           bty = "n", lwd = 1.5)
  }
  
  # *** plot the corresponding total number of bees ***
  plot(0,0, las = 1, col= "white", xlim = c(0,23), ylim = c(0,Ymax_totBees), 
       xlab = "", ylab = "",xaxt=xaxt)
  if(var_i == 3){
    mtext("Time (days)",side = 1, line = 2.5)
  }
  mtext("Total nb. of adult bees",side = 2, line = 3)
  #
  if(plotResourcesPoly){
    # plot resources availability
    polygon(x = c(0,23,23,0), y = c(0,0,Ymax_var[varHere],Ymax_totBees),border = F,
            col = add.transparency.colour("blueviolet",alpha = 0.20))
    polygon(x =c(0,23,23,0), y = c(0,0,Ymax_var[varHere],Ymax_totBees),
            density = 5,lwd=1.5,col = "black")
  }
  # plot the base line total number workers
  lines(x = x_time, y = y_totBees, lwd = lwd_lines, col = "black", lty = 1)
  # plot the other lines
  for(percent_i in 1:length(percent_change)){
    percent_changeHere <- as.character(percent_change[percent_i])
    lty_linesHere <- lty_lines[percent_changeHere]
    y <- rowSums(outputs_l_here[[percent_changeHere]][,c("Wn","S","Hc","Hw","Uc","Uw")])
    lines(x = x_time, y = y, lwd = lwd_lines, col = "black", lty = lty_linesHere)
  }
  
  # *** now determine the % change in the total number of bees at 23 days ***
  y_23_balseLine <- tail(y_totBees,1)
  y_23s <- sapply(X = 1:length(percent_change), function(percent_i){
    percent_changeHere <- as.character(percent_change[percent_i])
    y <- rowSums(outputs_l_here[[percent_changeHere]][,c("Wn","S","Hc","Hw","Uc","Uw")])
    y_23 <- tail(y,1)
    percent_diff <- (y_23_balseLine - y_23) / y_23_balseLine * 100 * -1
    return(percent_diff)
  })
  plot(x = as.numeric(percent_change), y = y_23s, ylim = range(as.numeric(percent_change)),
       las = 1, xlab = "", ylab = "", pch = 16, cex = 3, xaxt = xaxt)
  if(var_i == 3){
    mtext("% change initial value",side = 1, line = 2.5)
  }
  mtext("% change bee nb. at day 23",side = 2, line = 3)
  abline(a = 0,b = 1, lty = 2, lwd = 2)
  abline(a = 0,b = 0, lty = 2, lwd = 2)
  
  # print the outputs
  y_23_nb <- sapply(X = 1:length(percent_change), function(percent_i){
    percent_changeHere <- as.character(percent_change[percent_i])
    y <- rowSums(outputs_l_here[[percent_changeHere]][,c("Wn","S","Hc","Hw","Uc","Uw")])
    y_23 <- tail(y,1)
   return(y_23)
  })
  names(y_23s) <- names(y_23_nb) <- percent_change
  print(varHere)
  df_change <- as.data.frame(round(rbind(y_23s,y_23_nb),2))
  rownames(df_change) <- c("%","nb")
  print(df_change)
  print(paste0("Base-line: ",round(y_23_balseLine,2)))
}
if(print.fig){
 dev.off() 
}

#  "B"
#      -100    -50    50   100
# %  -21.62 -10.77 10.74 21.46
# nb  41.41  47.14 58.51 64.17
#  "Base-line: 52.83"

#  "Wn"
#     -100   -50    50   100
# %  -4.20 -2.08  2.05  4.07
# nb 50.62 51.74 53.92 54.99
#  "Base-line: 52.83"

#  "Rnw"
#      -100   -50    50   100
# %  -27.87 -8.44  5.89 10.52
# nb  38.11 48.38 55.95 58.40
#  "Base-line: 52.83"



