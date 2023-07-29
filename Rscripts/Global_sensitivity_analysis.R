# Script related to Carturan et al. 2023. Bumble bee pollination and the 
# wildflower/crop trade-off: When do wildflower enhancements improve crop yield?
# Ecological Modelling

# Author: Bruno S. Carturan

# The goal of the script is to conduct the GSA produce the associated figure.

# Files imported:
# parameters.initial.dist.51.13.RData: the parameter point selected for the rest of the manuscript

# File exported
# GSA.LHS.PRCC.ODE.2022-05-28_17-46-32.51.13.RData

# Figure produced
# Figure.GSA.LHS.PRCC.bothVar.jpeg

#
wd <- getwd()
wd_data <- paste(wd,"data",sep="/") 
wd_data_raw <- paste(wd,"data_raw",sep="/") 
wd_Rscripts <- paste(wd,"Rscripts",sep="/")
wd_figures <- paste(wd,"figures",sep="/")

source(file = paste(wd_Rscripts,"Functions.R",sep = "/"))

require(parallel)
require(ODEsensitivity)
require(lhs) # for the LHS-PRCC
require(boot) # for the LHS-PRCC
require(sensitivity) # https://cran.r-project.org/web/packages/sensitivity/sensitivity.pdf
library(ggplot2)
library(latex2exp)
require(deSolve)

print.fig <- F

#
# get the parameters -----
fileName <- "/parameters.initial.dist.51.13.RData"
parameters <- readRDS(file = paste(wd_data_raw,fileName,sep="/"))

# Scenario: this is the same as the "Calibration" = "Resource discontinuity" 
# scenario in the paper, but we stop before the resources become unavailable.
parameters$Tw <- treatments.Tw$calibration # this 
parameters$A <- 600

nb.days <- 23
t <- seq(0,nb.days*12, by = 0.01) # in h

# determine the parameters to include in the GSA
para.not.GSA <- c("Tw","A","n_f","w_f")
para.not.GSA.2 <- c("eta","rho_c","epsilon") # the parameters for which I am not sure
name.para.GSA <- names(parameters)[!names(parameters) %in% c(para.not.GSA,para.not.GSA.2)]
parameters.GSA <- parameters[!names(parameters) %in% c(para.not.GSA,para.not.GSA.2)]
#

# LHS-PRCC ------
# https://cran.r-project.org/web/packages/pse/vignettes/pse_tutorial.pdf

nb.para.pts <- 1000 # we should have N > (4/3)K [11, 2], with N nb simulations, K nb parameters
# source:
# https://trace.tennessee.edu/cgi/viewcontent.cgi?article=2443&context=utk_gradthes
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2570191/
range.percent <- 50 
nboot <- 1000
numCores <- detectCores()

# set parallel to TRUE if using a Linux machine.

GSA <- ODE.LHS.PRCC.GSA.fun(ODE.m = ODE.m, t = t, state = state, 
                            parameters.l = parameters, name.para.GSA = name.para.GSA,
                            nb.para.pts = nb.para.pts, range.percent = range.percent,
                            parallel = F, numCores = numCores, rank = T, semi = F,
                            nboot = nboot, write.file = F, wd = wd_data_raw, suffix = "")

# file produced:
# GSA.LHS.PRCC.ODE.2022-05-28_17-46-32.51.13.RData

# Figures results LHS-PRCC (Figure 9) -----
#
name.file <- "GSA.LHS.PRCC.ODE.2022-05-28_17-46-32.51.13.RData"

GSA <- readRDS(file = paste(wd_data_raw,"/",name.file,sep=""))
GSA <- GSA[c("tot.adult.bees.last.d","yield_kg.last.d")]

para.name.code <- rownames(GSA$yield_kg.last.d$PRCC)
para.name.fig <-        c(expression(theta[a]),
                          expression(theta[q]),
                          expression(theta[h]),
                          expression(K[UWn]),
                          expression(K[WnS]),
                          expression(K[BWn]),
                          expression(K[HU]),
                          expression(K[SH]),
                          expression(rho[B]),
                          expression(rho[w]),
                          expression(delta[H]),
                          expression(delta[S]),
                          expression(delta[U]),
                          expression(delta[Wn]),
                          expression(delta[B]),
                          expression(delta[Rn]),
                          expression(delta[c]),
                          expression(delta[w]),
                          expression(sigma[Wnc]),
                          expression(sigma[Wnw]),
                          expression(sigma[Bc]),
                          expression(sigma[Bw]),
                          expression(K[WnSnf]),
                          expression(K[WnBnf]),
                          expression(K[HUf]),
                          expression(K[WnSf]),
                          expression(K[HSf]),
                          expression(K[n]),
                          expression(K[Bnf]))

names(para.name.fig) <- para.name.code

col.data <- 'black'

if(print.fig){
  jpeg(filename = paste0(wd_figures,"/Figure.GSA.LHS.PRCC.bothVar.jpeg"),
       width = 18,height = 18,units = "cm",res = 600)
}
layout(matrix(1:2,nrow = 1),widths = c(1.38,1))
for(i in 1:length(GSA)){
  side2 <- 6
  if(i == 2){
    side2 <- 0.5
  }
  ds.here <- GSA[[i]]$PRCC
  if(i == 1){
    order.parameters <- rownames(ds.here[order(GSA[[i]]$PRCC$original),]) # keep the order of the 1st variable for the second one
  }
  ds.here <- ds.here[order.parameters,]
  
  par(mar=c(4.5,side2,0.5,0.5))
  plot(y = 1:nrow(ds.here), x = ds.here$original,
       pch = 16, col = "white", cex=2, xlim=c(-1,1),las=1,xlab="PRCC",ylab="",
       yaxt = "n")
  for(j in 1:nrow(ds.here)){
    segments(y0 = j, x0 = -2, y1 =j, x1 = 2, col = "grey90", lwd = 2)
  }
  for(j in c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1)){
    segments(y0 = -2, x0 = j, y1 = nrow(ds.here)+2, x1 = j, col = "grey90", lwd = 2)
  }
  segments(y0 = 0, x0 = 0, y1 = nrow(ds.here)+2, x1 = 0, 
          col = "red", lty = 2, lwd = 3)
  points(y = 1:nrow(ds.here), x = ds.here$original, pch = 16, col = col.data,cex= 1.2)
  segments(y0 = 1:nrow(ds.here), y1 = 1:nrow(ds.here),
           x0 = ds.here$`min. c.i.`, x1 = ds.here$`max. c.i.`,
           col = col.data, lwd=3)
  if(i == 1){
    axis(side = 2,at = 1:nrow(ds.here),labels = para.name.fig[order.parameters],
         las=2)
    mtext(text = "Parameters",side = 2,line = 4.5,cex=1.2)
  }
  #
  par(new=T)
  plot(y = 999, x = 999, pch = 16, cex = 0.1, col = "white", xlim=c(-1,1),
       las=1,ylab="",xlab="",yaxt = "n",xaxt="n")
  #
  legend("topleft",c("a","b")[i])
  legend.here <- c("Bees","Yield")[names(GSA)[i] == names(GSA)]
  legend("bottomright",legend.here)
}
if(print.fig){
  dev.off()
}

# THE END

