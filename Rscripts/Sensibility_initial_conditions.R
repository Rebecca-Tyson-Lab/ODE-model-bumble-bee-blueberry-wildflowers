

# Sensitivity to initial conditions -----

parameters$Tw <- treatments.Tw$ApM
state # initial state

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
