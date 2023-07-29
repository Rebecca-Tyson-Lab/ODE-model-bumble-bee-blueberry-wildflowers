# Script related to Carturan et al. 2023. Bumble bee pollination and the 
# wildflower/crop trade-off: When do wildflower enhancements improve crop yield?
# Ecological Modelling

# Author: Bruno S. Carturan

# The goal of the script is to conduct the virtual experiment and produce the associated figures.

# Files imported:
# parameters.initial.dist.51.13.RData: the parameter point selected for the rest of the manuscript


# File exported
# output.experiment_2022-05-27 12-43-12_A=300_51.13.csv
# output.experiment_2022-05-27 10-24-10_A=600_51.13.csv
# output.experiment_2022-05-27 15-02-08_A=900_51.13.csv

# Figures created:
# Figure.Experiment_CY_percent_A=300cont.const.jpeg
# Figure.Experiment_CY_percent_A=600cont.const.jpeg
# Figure.Experiment_CY_percent_A=900cont.const.jpeg
# Figure.CumulatedVar_3_configurations.jpeg
# Figure.Experiment_dynamics_dist_A_600_Tw_ApmM_theta_a_0.1_theta_hq_2.jpeg
# Figure.Experiment_dynamics_dist_A_600_Tw_M_theta_a_0.1_theta_hq_2.jpeg
# Figure.Experiment_dynamics_dist_A_600_Tw_M_theta_a_0.1_theta_hq_5.jpeg

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
library(parallel)

print.fig <- F

# Experiment ---- 
#
para.fileName <- "parameters.initial.dist.51.13.RData" # the point manually selected against parameters.initial.44.92.RData
parameters <- readRDS(file = paste(wd_data_raw,para.fileName,sep="/"))

dist.here <- 51.13 # 
As <- c(600,300,900) 

t.h <- seq(0,61*12, by = 0.01)
theta_a <- seq(0.05,0.4,0.02) 
theta_h <- theta_q <- seq(0.2,10.0,0.2)  
Tw <- c("Ap","ApmM","ApM","M","2ndmM","control")
numCores <- detectCores() # for Linux

# set parallel = T if using a Linux machine

for(i in 1:length(As)){
  print(paste0("A=",As[i],"_",dist.here))
  parameters$A <- As[i]
  output.exp <- experiment.fun(ODE.m = ODE.m,theta_a = theta_a,theta_h = theta_h,
                               theta_q = theta_q,parameters = parameters,t.h = t.h,
                               print.csv = F,
                               wd_data_raw, parallel = F, numCores = numCores,
                               suffix.name.csv = paste0("A=",As[i],"_",dist.here))
}


# Figures contour lines (Figure 5, S4, S5) -------
#
t <- seq(0,61*12, by = 0.01)
para.fileName <- "parameters.initial.dist.51.13.RData" # 
parameters <- readRDS(file = paste(wd_data_raw,para.fileName,sep="/"))

# NOTE: we conducted the simulations first with theta_hq in [0.2,6.0,0.2] and then 
# in [6.2,10.0,0.2]

output.experiment.l <- list()

exp.files <- c( # para.fileName <- "parameters.initial.dist.51.13.RData" #
"output.experiment_2022-07-05 12-43-20_A=300_51.13.csv",
"output.experiment_2022-07-05 10-15-04_A=600_51.13.csv",
"output.experiment_2022-07-05 15-11-05_A=900_51.13.csv")

# the result of the experiment for theta_hq in [6.2,10.0]
exp.files_2 <- c(
"output.experiment_2022-08-24 15-28-31_A=300_51.13.csv",
"output.experiment_2022-08-24 13-46-14_A=600_51.13.csv",
"output.experiment_2022-08-24 17-09-25_A=900_51.13.csv")

output.experiment.l_2 <- list()

for(i in 1:length(exp.files)){
  filename <- exp.files[i]
  fileHere <- read.csv(paste(wd_data_raw,exp.files[i],sep="/"),header = T)
  fileHere <- fileHere[fileHere$theta_a >= 0.05,]
  output.experiment.l[[i]] <- fileHere
  names(output.experiment.l)[i] <- filename
  
  filename <- exp.files_2[i]
  fileHere <- read.csv(paste(wd_data_raw,exp.files_2[i],sep="/"),header = T)
  fileHere <- fileHere[fileHere$theta_a >= 0.05,]
  output.experiment.l_2[[i]] <- fileHere
  names(output.experiment.l_2)[i] <- filename
  
  # need to cbind the datasets by respecting the order of variation of the 
  # the explanatory variables:
  # 1st by Tw, 2nd by theta_a
  ds_1 <- output.experiment.l[[i]]
  ds_2 <- output.experiment.l_2[[i]]
  Tws <- unique(output.experiment.l[[i]]$Tw)
  theta_as <- unique(output.experiment.l[[i]]$theta_a)
  ds_new <- ds_1[1,]
  ds_new <- ds_new[-1,]
  for(Tw_here in Tws){
    for(theta_a_here in theta_as){
      ds_1_here <- ds_1[ds_1$theta_a == theta_a_here & 
                          ds_1$Tw == Tw_here,]
      ds_2_here <- ds_2[ds_2$theta_a == theta_a_here & 
                        ds_2$Tw == Tw_here,]
      ds_new <- rbind(ds_new,rbind(ds_1_here,ds_2_here))
    }
  }
  
  output.experiment.l[[i]] <- ds_new
}

As.num <- c(300,600,900)
As <- as.data.frame(t(paste0("A_",As.num)))
colnames(As) <- paste0("A_",As.num)
names(output.experiment.l) <- As[1,]

for(i in 1:ncol(As)){
  A.here <- As[,i]
  parameters$A <- As.num[i]
  # Results in % AND with constant control (theta_a ~0, theta_hq = 1)
  # --> diff_control_percentage_theta_a_fix is T
  # This is the figure included in the paper
  figure.4Var.contourLine.fun(output = output.experiment.l[[A.here]],total.yield = T,
                              int.cont.lines = 10,
                              col.cont.line = "red",
                              col.grad = c("darkgoldenrod3","white","dodgerblue3"),
                              diff_control_percentage = T,
                              parameters = parameters,responseVariable = "cropYield",
                              nameFile = paste0("Figure.Experiment_CY_percent_A=",gsub("A_","",A.here),"cont.const.jpeg"),
                              diff_control_percentage_theta_a_fix = T,
                              show.bee.CY.diffControl_integrated = F,
                              show.bee.CY.diffControl = T, 
                              arrows.every.x = 5,arrows.every.y = 3,
                              lwdArrows = 5,colArrows = "black", 
                              percentCutnbBeeChange = c(0,10,30,60,100),
                              # percentCutnbBeeChange = c(0,10,100,200,300),
                              # percentCutnbBeeChange = c(20,100,200,300),
                              theta_a_control = 0.05,
                              print = print.fig)
}

#
# Figure that combines the different accumulated flower visits (Figure 8) ------
#
A.s <- c(600,300,900)
Tw.s <- rep(treatments.Tw$Ap,length(A.s))
Tw.long.s <-rep("April",length(A.s))
theta_a.s <- c(0.05,0.05,0.35)
theta_hq.s <- c(6,2,5)
Xaxt.s <- c(rep("n",length(A.s)-1),"s")
legend.position.s <- c("left",rep(NA,length(A.s)-1))
out2.s <- list()
parameters.s <- list()
for(i in 1:length(A.s)){
  parameters$A <- A.s[i]
  parameters$Tw <- Tw.s[i]
  parameters$theta_h <- parameters$theta_q <- theta_hq.s[i]
  parameters$theta_a <- theta_a.s[i]
  
  parameters.s[[i]] <- parameters
  
  out.1 <- ode(y = state, times = t, func = ODE.m, parms = parameters)
  out2.s[[i]] <- as.data.frame(out.1)
}

ymax <- 6e5

if(print.fig){
  jpeg(filename = paste(wd_figures,"/Figure.CumulatedVar_",length(A.s),
                        "_configurations.jpeg",sep=""),
       width = 13,height = 18,units = "cm",res = 600)
}
layout(matrix(1:length(A.s),ncol=1),heights = c(rep(1,length(A.s)-1),1.2)) # wth 3 plots
for(i in 1:length(A.s)){
  crop.yield.fun(data.ts = out2.s[[i]],parameters = parameters.s[[i]],
                 convert.to.day = F,ymax.cumFlowerVisisted = ymax,
                 modif.tot.amount = T,plot.PS = T,print.fig = F,
                 position.legend = legend.position.s[i],only.cum.flowerVisits = T,
                 Xaxt = Xaxt.s[i],las=1)
  legend("topleft",letters[i],bty="s")
  legend("bottomright",sapply(c(bquote(A == .(A.s[i])~"m"^2),
                                bquote("T"[w] == .(Tw.long.s[i])),
                                bquote(theta[a] == .(theta_a.s[i])),
                                bquote(theta[hq] == .(theta_hq.s[i]))),
                              as.expression),bty="n")
}
if(print.fig){
  dev.off()
}

#
# Figures population dynamics to compare effects of ApM vs. M and theta_hq = 2 vs. 5 (Figures 6, 7, S6) -----
#

y.max.H.Wn = 90
y.max.U.S = 2
y.max.B = 130 # 120
y.max.Rn = 35
y.max.R = 15
y.max.CY = 800 # 700

As <- rep(600,3)
Tws <- c("ApmM","M","M") # Figure 6, 7 and S6
theta_as <- rep(0.1,3)
theta_hqs <- c(2,2,5)

t <- seq(0,61*12, by = 0.01) # in h

out.2.l <- list()
for(i in 1:length(As)){
  parameters$A <- As[i]
  parameters$Tw <- Tws[i]
  parameters$theta_h <- parameters$theta_q <- theta_hqs[i]
  parameters$theta_a <- theta_as[i]
  out.1 <- ode(y = state, times = t, func = ODE.m, parms = parameters)
  out.2 <- as.data.frame(out.1)
  out.2$CY <- crop.yield.fun(data.ts = out.2,parameters = parameters,convert.to.day = F,
                             modif.tot.amount = T,plot.PS = F,print.fig = F,
                             position.legend = "left",
                             nameFig = paste0("A_",parameters$A,"_Tw_",parameters$Tw,
                                              "_theta_a_",parameters$theta_a,"_theta_hq_",
                                              parameters$theta_q,".jpeg")) # in g for the total area
  
  out.2[,1] <- out.2[,1] / 12 # conversion in days
  out.2 <- out.2[out.2[,1] %in% 0:90,]
  out.2.l[[i]] <- out.2
}

for(i in 1:length(As)){
  parameters$A <- As[i]
  parameters$Tw <- Tws[i]
  parameters$theta_h <- parameters$theta_q <- theta_hqs[i]
  parameters$theta_a <- theta_as[i]
  figure3.fun(out.2.l[[i]],parameters = parameters, print = print.fig,
              wd_figures = wd_figures,total.yield = T,legend.box = T,legend.position = "topleft",
              y.max.H.Wn = y.max.H.Wn, y.max.U.S = y.max.U.S, y.max.B = y.max.B, 
              y.max.Rn = y.max.Rn, y.max.R = y.max.R, y.max.CY = y.max.CY,
              nameFile = paste0("Figure.Experiment_dynamics_dist_A_",parameters$A,"_Tw_",
                                parameters$Tw,"_theta_a_",parameters$theta_a,"_theta_hq_",
                                parameters$theta_q,".jpeg"))
}

# value CY in each case:
max(out.2.l[[1]]$CY)/1000 # 762.5 kg
max(out.2.l[[2]]$CY)/1000 # 309.3
max(out.2.l[[3]]$CY)/1000 # 222.7

# Figure population dynamics control vs other treatment (extra, not published) -----

y.max.H.Wn = 90
y.max.U.S = 2
y.max.B = 130 # 120
y.max.Rn = 35
y.max.R = 15
y.max.CY = 800 # 700

print <- F

last_day <- 61
t <- seq(0,last_day*12, by = 0.01)
parameters$theta_a <- 0.05
parameters$Tw <- treatments.Tw$control
parameters$A <- 600
parameters$theta_q <- parameters$theta_h <- 1
out.1 <- ode(y = state, times = t, func = ODE.m, parms = parameters)
out.2 <- as.data.frame(out.1)
out.2$CY <- crop.yield.fun(data.ts = out.2,parameters = parameters,
                           convert.to.day = F,
                           modif.tot.amount = T,plot.PS = F) # in g for the total area
out.2[,1] <- out.2[,1] / 12 # conversion in days
out.2 <- out.2[out.2[,1] %in% 0:90,]
figure3.fun(out.2, parameters = parameters,y.max.R = y.max.R, print = print,
            wd_figures = wd_figures,total.yield = T,legend.box = T,legend.position = "topleft",
            y.max.H.Wn = y.max.H.U, y.max.U.S = y.max.U.S, y.max.B = y.max.B, 
            y.max.Rn = y.max.Rn, y.max.CY = y.max.CY,
            nameFile = paste0("Figure.Experiment_dynamics_dist_A_",parameters$A,"_Tw_",
                              parameters$Tw,"_theta_a_",parameters$theta_a,"_theta_hq_",
                              parameters$theta_q,".jpeg"))

# 
parameters$theta_a <- 0.3
parameters$Tw <- treatments.Tw$M
parameters$theta_q <- parameters$theta_q <- 5
out.1 <- ode(y = state, times = t, func = ODE.m, parms = parameters)
out.2 <- as.data.frame(out.1)
out.2$CY <- crop.yield.fun(data.ts = out.2,parameters = parameters,
                           convert.to.day = F,
                           modif.tot.amount = T,plot.PS = F) # in g for the total area
out.2[,1] <- out.2[,1] / 12 # conversion in days
out.2 <- out.2[out.2[,1] %in% 0:90,]
figure3.fun(out.2, parameters = parameters, print = print,
            wd_figures = wd_figures,total.yield = T,legend.box = T,legend.position = "topleft",
            y.max.H.Wn = y.max.H.U, y.max.U.S = y.max.U.S, y.max.B = y.max.B, 
            y.max.Rn = y.max.Rn, y.max.CY = y.max.CY,y.max.R = 15,
            nameFile = paste0("Figure.Experiment_dynamics_dist_A_",parameters$A,"_Tw_",
                              parameters$Tw,"_theta_a_",parameters$theta_a,"_theta_hq_",
                              parameters$theta_q,".jpeg"))
#
# Difference population size control vs. May with theta_hq = 1, 4, 6 and 10 (Figure S7) ------

para.fileName <- "parameters.initial.dist.51.13.RData" #
parameters <- readRDS(file = paste(wd_data_raw,para.fileName,sep="/"))
parameters$A <- 600

last_day <- 61
t <- seq(0,last_day*12, by = 0.01)
Tw_s <- c(treatments.Tw$control,rep(treatments.Tw$M,4))
Theta_hq_s <- c(1,1,4,6,10)
Theta_a_s <- c(0.05,rep(0.35,4))
out.2.l <- list()
for(i in 1:length(Tw_s)){
  parameters$theta_a <-Theta_a_s[i]
  parameters$Tw <- Tw_s[i]
  parameters$theta_q <- parameters$theta_h <- Theta_hq_s[i]
  out.1 <- ode(y = state, times = t, func = ODE.m, parms = parameters)
  out.2 <- as.data.frame(out.1)
  out.2$CY <- crop.yield.fun(data.ts = out.2,parameters = parameters,
                             convert.to.day = F,
                             modif.tot.amount = T,plot.PS = F) # in g for the total area
  out.2[,1] <- out.2[,1] / 12 # conversion in days
  out.2 <- out.2[out.2[,1] %in% 0:90,]
  out.2.l[[i]] <- out.2
  names(out.2.l)[i] <- paste0(Tw_s[i],"_thetaa_",Theta_a_s[i],"_thetahq_",Theta_hq_s[i])
}

adultBees <- c('Wn','S','Hc','Hw','Uc','Uw')
Ymax <- max(unlist(lapply(out.2.l,FUN = function(x){max(rowSums(x[,adultBees]))})))

if(print.fig){
  jpeg(filename = paste0(wd_figures,"/Figure.Experiment_PopSizes_control_May_theta_hq_1_4_6.jpeg"),
       width = 18,height = 12, units = "cm",res = 600)
}
layout(1)
par(mar=c(4.5,5,0.5,0.5))
plot(x = c(0,max(out.2.l[[1]]$time)),y = c(0,Ymax),las=1,col="white",
     xlab="Time (days)",ylab="Number adult bees")
polygon(x = c(30,60,60,30),y = c(0,0,Ymax,Ymax),border = F,
        col = add.transparency.colour("blueviolet",alpha = 0.15))
lineCol <- c("black",
             add.transparency.colour("blue",alpha = 0.2),
             add.transparency.colour("blue",alpha = 0.4),
             add.transparency.colour("blue",alpha = 0.6),
             add.transparency.colour("blue",alpha = 1))
lty <- c(2,rep(1,4))
for(i in length(Tw_s):1){
  lines(x = out.2.l[[i]]$time,y = rowSums(out.2.l[[i]][,adultBees]),lwd=4,
        col=lineCol[i],lty=lty[i])
}
legend("topleft",
       c("control",sapply(X = 2:length(out.2.l),
                          FUN = function(i){paste0("attractiveness = ",Theta_hq_s[i])})),
       bty = "n",lwd = 3,col = lineCol,lty = lty)
if(print){
  dev.off()
}

# THE END
