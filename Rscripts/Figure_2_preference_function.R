# Script related to Carturan et al. 2023. Bumble bee pollination and the 
# wildflower/crop trade-off: When do wildflower enhancements improve crop yield?
# Ecological Modelling

# Author: Bruno S. Carturan

# The goal of the script is to produce the figure related to the preference function (Figure 2)

# Figures created:
# Preference_function_theta_p.jpeg

wd <- getwd()
wd_Rscripts <- paste(wd,"Rscripts",sep="/")
wd_figures <- paste(wd,"figures",sep="/")

source(file = paste(wd_Rscripts,"Functions.R",sep = "/"))

print.fig <- F

theta_a.char <- c("0.01","0.10","0.30","0.60","0.70","0.95")
theta_a <- as.numeric(theta_a.char)
theta_h <- theta_q <- theta_hq <- seq(0,10,0.1)
col.l <- c("blue","orange","green","red","purple","brown")

if(print.fig){
  jpeg(filename = paste(wd_figures,"Preference_function_theta_p.jpeg",sep="/"),
       width = 16,height = 12,units = "cm",res = 600)
}
#
par(mar=c(4.5,5,0.5,0.5))
plot(0,0,xlim=c(0,6),ylim=c(0,1),col="white",las=1,xlab="",ylab="")
mtext(bquote("Attractiveness" ~ theta[hq]~"="~(theta[h]+theta[q])/2),
      side = 1,line = 3)
mtext(bquote("Preference for crop vs. wildflowers ="~theta[p]*"("*theta[a]*","*theta[h]*","*theta[q]*")"),
      side = 2,line = 3)
abline(a = 0.5, b = 0, lty =2, lwd = 2, col="gray")
for(i in 1:length(theta_a)){
  lines(x = theta_hq, y = theta_p.fun(theta_a = theta_a[i],
                                      theta_h = theta_h,
                                      theta_q = theta_q),
        lwd = 2, col = col.l[i])
}
theta_legend.l <- c()
for(i in 1:length(theta_a)){
  theta_legend.l <- c(theta_legend.l,bquote(theta[a] == .(theta_a.char[i])))
}
theta_legend <- sapply(theta_legend.l,as.expression)
legend("topright",theta_legend,bty="n", col = col.l,lwd=2)
legend("right",c("Crop flowers preferred","","Wildflowers preferred"),bty="n")
x <- 4
arrows(x0 = x, y0 = 0.53,x1 = x,y1 = 0.65,lwd = 2,length = 0.15)
arrows(x0 = x, y0 = 0.47,x1 = x,y1 = 0.35,lwd = 2,length = 0.15)
#
if(print.fig){
  dev.off()
}

# THE END
