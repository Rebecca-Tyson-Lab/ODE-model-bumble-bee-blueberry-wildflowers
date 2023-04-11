# Author: Bruno S. Carturan

# The goal of the script is to conduct the calibration and produce the associated figures.

# Files imported:
# --> these are calibrated parameters obtained from previous calibration attempts (Section 2.3.3)
# They are used to provide extra initial parameter points in the calibration procedure.
# parameters.initial.dist.30.59.RData
# parameters.initial.dist.weeds.only.0.61.RData
# parameters.initial.43.62.RData
# parameters.initial.57.93.RData

# Files exported:
# calibration.lossFun_2022-05-26_14-08-09_Dist_44.92.csv: the file contains 16655 parameter points with associated distance
# parameters.initial.dist.44.92.RData: the parameter point with the smallest overall distance in above file, but not selected because of the non acceptableadult bee dynamics in the weeds only scenario
# parameters.initial.dist.51.13.RData: the parameter point selected for the rest of the manuscript

# Figures created:
# Figure.Calibration_distances_all_runs.jpeg
# Figure.Calibration_distances_vectorSelection.jpeg
# Figure.Calibration_calibration_A=1000_Objectives.jpeg ; the "resource discontinuity" scenario
# Figure.Calibration_weeds.only_A=1000_Objectives.jpeg              ; the "weeds only" scenario
# Figure.Calibration_calibration_A=1000_Objectives_notSelected.jpeg
# Figure.Calibration_weeds.only_A=1000_Objectives_notSelected.jpeg

#
wd <- getwd()
wd_data <- paste(wd,"data",sep="/") 
wd_data_raw <- paste(wd,"data_raw",sep="/") 
wd_Rscripts <- paste(wd,"Rscripts",sep="/")
wd_figures <- paste(wd,"figures",sep="/")

source(file = paste(wd_Rscripts,"Functions.R",sep = "/"))

library(deSolve)
# https://tpetzoldt.github.io/deSolve-forcing/deSolve-forcing.html
# https://tpetzoldt.github.io/deSolve-forcing/deSolve-forcing.html#Events
library(lhs)
library(plot3D)
library(rgl)
library(parallel)

print.fig <- F

#
# Calibration -----
# The five parameter sets added initially (Section 2.3.3)

para.orig <- parameters.fun()
parameters <- para.conversion.fun(parameters = para.orig,mg.to.g = T, h.to.d = F)
parameters$Tw <- "calibration" # ApJ.c.w
count <- 1

parameters <- list(parameters)
parameters[[count <- count + 1]] <- 
  readRDS(file = paste(wd_data_raw,"/parameters.initial.dist.30.59.RData",sep=""))
parameters[[count <- count + 1]] <- 
  readRDS(file = paste(wd_data_raw,"/parameters.initial.dist.weeds.only.0.61.RData",sep=""))
parameters[[count <- count + 1]] <- 
  readRDS(file = paste(wd_data_raw,"/parameters.initial.43.62.RData",sep=""))
parameters[[count <- count + 1]] <- 
  readRDS(file = paste(wd_data_raw,"/parameters.initial.57.93.RData",sep=""))

for(i in 1:length(parameters)){
  parameters[[i]]$A <- 1000
  parameters[[i]]$Tw <- treatments.Tw$calibration
}

# The parameters to calibrate:
para.to.calib <- c("K_SHf","delta_B","K_n","delta_Rn","K_WnSnf","K_WnBnf","K_HUf","K_WnSf","K_Bnf",
                   "K_WnS","K_SH","K_HU","K_UWn")

# define the ranges of values for each of these parameters
ranges.calib.l <- list()
ranges.calib.l$delta_Rn <- c(0.001,10)
ranges.calib.l$delta_B <- c(0.0001,0.1) # this should be calibrated from the literature
ranges.calib.l$K_WnSnf <- c(0.1,100) # c(1,50000) # 
ranges.calib.l$K_WnBnf <- c(0.1,100) # c(50,500)
ranges.calib.l$K_HUf <- c(0.1,100)
ranges.calib.l$K_WnSf <- c(0.1,100)
ranges.calib.l$K_SHf <- c(0.1,100) # used to be K_f
ranges.calib.l$K_n <- c(0.1,100)
ranges.calib.l$K_Bnf <- c(0.1,10)
ranges.calib.l$K_WnS <- c(0.1,30) # 1.7
ranges.calib.l$K_SH <- c(0.1,30)  # 20
ranges.calib.l$K_HU <- c(0.1,30)  # 3
ranges.calib.l$K_UWn <- c(0.1,30) # 25

t <- seq(0,65*12, by = 0.01) # in h

n.pt.init <- 3000 # n.pt.init <- 2 # nb of initial points
n.pt.selected.loop <- 30 # 10 # nb of points selected per loop
n.pt.generated.loop <- 30 # 10 # nb of points drawn around each selected point for each loop
n.loop <- 10 # nb of loops
percent.range <- 20 # 10 # the % of the initial range from which new points are drawn (note that the range gets smaller because it is divided by the number of each loop, i.e., 1st, 2nd, 3rd, etc.)


n.pt.init <- 2 # n.pt.init <- 2 # nb of initial points
n.pt.selected.loop <- 1 # 10 # nb of points selected per loop
n.pt.generated.loop <- 1 # 10 # nb of points drawn around each selected point for each loop
n.loop <- 1 # 10 # nb of loops
percent.range <- 20 # 10 # the % of the initial range from which new points are drawn (note that the range gets smaller because it is divided by the number of each loop, i.e., 1st, 2nd, 3rd, etc.)

# time is take in hours
(n.pt.init + n.loop * n.pt.selected.loop * n.pt.generated.loop + n.loop*n.pt.selected.loop + 1)*9/60/60 # 14 second per simulation

numCores <- detectCores()

var.goal <- var.goal.fun()
thetas.calibration <- thetas.calibration.fun()

# if run an a linux machine set parallel = T
DS <- ODE.calibration.fun(t = t, state = state, ranges.calib.l = ranges.calib.l,
                          percent.range = percent.range,n.pt.init = n.pt.init,
                          n.pt.selected.loop = n.pt.selected.loop,
                          var.goal = var.goal,thetas.calibration = thetas.calibration,
                          n.pt.generated.loop = n.pt.generated.loop,n.loop = n.loop,
                          parameters = parameters, write.file = F, wd = wd_data,
                          parallel = F)

# File exported: 
# data_raw/calibration.lossFun_2022-05-26_14-08-09_Dist_44.92.csv
# run with sigma_B = 1.3 and not 0.9 and weight for weeds.only scenario
file.name <- "calibration.lossFun_2022-05-26_14-08-09_Dist_44.92.csv"
DS <- read.csv(paste(wd_data_raw,file.name,sep="/"),header = T)

nrow(DS) # 16655
min(DS$distance)
max(DS$distance)


# Figure showing the distance for all the parameter points generated (not published)------

if(print.fig){
  jpeg(filename = paste(wd_figures,"/Figure.Calibration_distances_all_runs.jpeg",sep=""),
       width = 18,height = 13,units = "cm",res = 600)
}
layout(matrix(1))
par(mar=c(5,5,0.5,0.5))
plot(y = DS$distance, x = (1:nrow(DS)),
     ylab="Distance (from loss function)",
     xlab="Number of the parameter points",
     ylim=c(0,max(DS$distance)),las=1,cex=1, lwd=2,pch=16,
     col=add.transparency.colour("gray60"))
legend("topright",paste0("n = ",nrow(DS)),bty="n")
if(print.fig){
  dev.off()
}
#
# Figure showing distances for each scenario for all points --------
# Determine the weight to give to each scenario based on the number of target 
# they have. The weight for the scenario with the largest number of targets is
# = 1.
var.goal <- var.goal.fun()
nbTargets <- unlist(lapply(X = var.goal,FUN = function(X){sum(!is.na(X[colnames(X) !="day"]))}))
maxTargets <- max(nbTargets)
weights.scenarios <- sqrt(maxTargets/nbTargets)

DS.sort <- DS[order(DS$distance),]
DS.here <- DS.sort # if we select the one with the minimum distance or 


simulationSc <- c("calibration","weeds.only")
layout(matrix(1:length(simulationSc), nrow = 1))
par(mar=c(5,5,1,1))
for(i in 1:length(simulationSc)){
  print(simulationSc[i])
  x <- DS.sort[,paste0("distance.",simulationSc[i])]*weights.scenarios[simulationSc[i]]
  print(x[1])
  hist(x = x,
       main = "",xlab = paste0("distance.",simulationSc[i]))
  segments(x0 = x[1], x1 = x[1], y0 = 0, y1 = 5000, lwd=3, col = "red")
}

range(DS.sort$distance.calibration)*weights.scenarios["calibration"]
range(DS.sort$distance.weeds.only)*weights.scenarios["weeds.only"]


# the parameter point with the smallest overall distance:
parameters <- as.list(DS.sort[1,!colnames(DS.sort) %in% 
                                c("distance","distance.calibration","distance.weeds.only")])
para.points <- as.list(DS.sort[1,colnames(DS.sort) %in% 
                                 c("distance","distance.calibration","distance.weeds.only")])

# The above point does not produce good results for the weeds only scenario, so we 
# take instead this point (Section 3.1):
threshold.weeds <- 17
threshold.calibration <- 35
DS.sort.cut <- DS.sort[DS.sort$distance.calibration < threshold.calibration & 
                         DS.sort$distance.weeds.only < threshold.weeds,]
para.points.alternative <- as.list(DS.sort.cut[1,colnames(DS.here) %in% 
                                                 c("distance","distance.calibration","distance.weeds.only")])

if(print.fig){
  jpeg(filename = paste0(wd_figures,"/Figure.Calibration_distances_vectorSelection.jpeg"),
       width = 18,height = 13, units = "cm",res = 600)
}
par(mar=c(5,5,0.5,0.5))
layout(matrix(1))
plot(x = DS$distance.calibration * weights.scenarios[1],
     y = DS$distance.weeds.only * weights.scenarios[2],
     ylab = "Distance for the weeds only scenario (weighted)",
     xlab = "Distance for the resource discontinuity scenario",
     ylim = c(0,80),
     col=add.transparency.colour("gray60"),
     cex=1,pch=16,las=1,xlim=c(0,130))
points(x = para.points$distance.calibration * weights.scenarios[1], 
       y = para.points$distance.weeds.only * weights.scenarios[2],
       pch=4, col = "red", cex= 2,lwd=2)
legend("bottomright",paste0("n = ",nrow(DS)),bty="n")
segments(x0 = c(threshold.calibration * weights.scenarios[1],0),
         y0 = c(0,threshold.weeds * weights.scenarios[2]),
         x1 = rep(threshold.calibration * weights.scenarios[1],2),
         y1 = rep(threshold.weeds * weights.scenarios[2],2),
         col = "red", lty=2,lwd=2)
points(x = para.points.alternative$distance.calibration * weights.scenarios[1], 
       y = para.points.alternative$distance.weeds.only *  weights.scenarios[2] ,
       pch=4, col = "blue", cex= 2, lwd=2)
if(print.fig){
  dev.off()    
}

# Save the two parameter points ------

# The parameter point with the smallest distance:
parameters <- as.list(DS.sort[1,!colnames(DS.sort) %in% 
                                c("distance","distance.calibration","distance.weeds.only")])
nameFile <- paste0("/parameters.initial.dist.",round(DS.sort$distance[1],2),".RData")
# saveRDS(object = parameters,
#         file = paste(wd_data_raw,nameFile,sep=""))


# the selected parameter point:
parameters <- as.list(DS.sort.cut[1,!colnames(DS.sort.cut) %in% 
                                c("distance","distance.calibration","distance.weeds.only")])
nameFile <- paste0("/parameters.initial.dist.",round(DS.sort.cut$distance[1],2),".RData")
# saveRDS(object = parameters,
#         file = paste(wd_data_raw,nameFile,sep=""))


# Figures population dynamics in each scenario for the selected point ------ 

para.fileName <- "parameters.initial.dist.51.13.RData"
parameters <- readRDS(file = paste(wd_data_raw,para.fileName,sep="/"))

parameters$A
parameters$theta_a
parameters$theta_h
parameters$theta_q

parameters$A * parameters$theta_a * parameters$rho_w
parameters$A * (1-parameters$theta_a) * parameters$rho_c

var.goal <- var.goal.fun()

y.max.H.Wn = 90
y.max.U.S = 2
y.max.B = 120
y.max.Rn = 35
y.max.CY = 700
y.max.R = 15

t <- seq(0,61*12, by = 0.01) # in h

parameters$Tw <- treatments.Tw$calibration
out.1 <- ode(y = state, times = t, func = ODE.m, parms = parameters)
out.2 <- as.data.frame(out.1)
out.2$CY <- crop.yield.fun(data.ts = out.2,parameters = parameters,convert.to.day = F,
                           modif.tot.amount = T, plot.PS = F) # in g for the total area
out.2[,1] <- out.2[,1] / 12 # conversion in days
out.2 <- out.2[out.2[,1] %in% 0:90,]
figure3.fun(out.2,parameters = parameters, print = print.fig, 
            wd_figures = wd_figures, total.yield = T,pch.diff = T,  
            y.max.H.Wn = y.max.H.Wn, y.max.U.S = y.max.U.S, y.max.B = y.max.B, 
            y.max.Rn = y.max.Rn, y.max.R = y.max.R, y.max.CY = y.max.CY,
            var.goal = var.goal, show.var.goal = T,legend.box = T,plot.lines = T,
            nameFile = paste0("Figure.Calibration_",parameters$Tw,"_A=",parameters$A,"_Objectives.jpeg"))

#
parameters$Tw <- treatments.Tw$weeds.only
out.1 <- ode(y = state, times = t, func = ODE.m, parms = parameters)
out.2 <- as.data.frame(out.1)
out.2$CY <- crop.yield.fun(data.ts = out.2,parameters = parameters,convert.to.day = F,
                           modif.tot.amount = T, plot.PS = T) # in g for the total area
out.2[,1] <- out.2[,1] / 12 # conversion in days
out.2 <- out.2[out.2[,1] %in% 0:90,]
figure3.fun(out.2,parameters = parameters, print = print.fig, 
            wd_figures = wd_figures, total.yield = T,plot.lines = T,
            y.max.H.Wn = y.max.H.Wn, y.max.U.S = y.max.U.S, y.max.B = y.max.B, 
            y.max.Rn = y.max.Rn, y.max.R = y.max.R, y.max.CY = y.max.CY,
            var.goal = var.goal, show.var.goal = T,legend.box = T, # y.max.R = y.max.R,
            nameFile = paste0("Figure.Calibration_",parameters$Tw,"_A=",parameters$A,"_Objectives.jpeg"))
#

# Figures population dynamics in each scenario for the non selected point having the smallest distance ------ 
#
para.fileName <- "parameters.initial.dist.44.92.RData"
parameters <- readRDS(file = paste(wd_data_raw,para.fileName,sep="/"))

parameters$Tw <- treatments.Tw$calibration
out.1 <- ode(y = state, times = t, func = ODE.m, parms = parameters)
out.2 <- as.data.frame(out.1)
out.2$CY <- crop.yield.fun(data.ts = out.2,parameters = parameters,convert.to.day = F,
                           modif.tot.amount = T, plot.PS = F) # in g for the total area
out.2[,1] <- out.2[,1] / 12 # conversion in days
out.2 <- out.2[out.2[,1] %in% 0:90,]
figure3.fun(out.2,parameters = parameters, print = print.fig, 
            wd_figures = wd_figures, total.yield = T,pch.diff = T,  
            y.max.H.Wn = y.max.H.Wn, y.max.U.S = y.max.U.S, y.max.B = y.max.B, 
            y.max.Rn = y.max.Rn, y.max.R = y.max.R, y.max.CY = y.max.CY,
            var.goal = var.goal, show.var.goal = T,legend.box = T,plot.lines = T,
            nameFile = paste0("Figure.Calibration_",parameters$Tw,"_A=",parameters$A,"_Objectives_notSelected.jpeg"))

#
parameters$Tw <- treatments.Tw$weeds.only
out.1 <- ode(y = state, times = t, func = ODE.m, parms = parameters)
out.2 <- as.data.frame(out.1)
out.2$CY <- crop.yield.fun(data.ts = out.2,parameters = parameters,convert.to.day = F,
                           modif.tot.amount = T, plot.PS = T) # in g for the total area
out.2[,1] <- out.2[,1] / 12 # conversion in days
out.2 <- out.2[out.2[,1] %in% 0:90,]
figure3.fun(out.2,parameters = parameters, print = print.fig, 
            wd_figures = wd_figures, total.yield = T,plot.lines = T,
            y.max.H.Wn = y.max.H.Wn, y.max.U.S = y.max.U.S, y.max.B = y.max.B, 
            y.max.Rn = y.max.Rn, y.max.R = y.max.R, y.max.CY = y.max.CY,
            var.goal = var.goal, show.var.goal = T,legend.box = T, # y.max.R = y.max.R,
            nameFile = paste0("Figure.Calibration_",parameters$Tw,"_A=",parameters$A,"_Objectives_notSelected.jpeg"))



