# Author: Bruno S. Carturan

# The goal of the script is to provide the functions for the other R scripts.


# Function that takes as argument a dataframe with the x, y and z variables as columns
# and returns a matrix of z value in a x, y, z.
# ds <- ds.here
# xvar.name <- "attractiveness"
# yvar.name <- "theta_a"
# zvar.name <- "yield_kg"
matrixFromDataset.fun <- function(ds,xvar.name,yvar.name,zvar.name,
                                  only.print.if.duplicated.zvar.not.same=T){
  
  xvar.here <- ds[,xvar.name]
  xvar.here <- sort(unique(xvar.here))
  yvar.here <- ds[,yvar.name]
  yvar.here <- sort(unique(yvar.here))
  
  output <- matrix(NA,nrow = length(yvar.here),ncol = length(xvar.here))
  for(x.c in 1:length(xvar.here)){
    for(y.r in 1:length(yvar.here)){
      
      val.here <- ds[,zvar.name][ds[,xvar.name] == xvar.here[x.c] &
                                  ds[,yvar.name] == yvar.here[y.r]]
      if(length(val.here) > 1){
        val.here.copy <- round(val.here,2)
        if(sum(!duplicated(val.here.copy)) == 1){ # if only the 1st value is not a duplicate, i.e., all the values are the same
          if(!only.print.if.duplicated.zvar.not.same){
            print(paste("These are duplicated values: ",
                        paste(val.here,collapse = " ; "),
                        ". They are all the same, so all good.",sep=""))
          }
        }else{
          print(paste("These are duplicated values: ",
                      paste(val.here,collapse = " ; "),
                      ". They are NOT the same, so NOT good.",sep=""))
        }
        #print("The 1st value was retained.")
        val.here <- val.here[1]
      }
      output[y.r,x.c] <- val.here
    }
  }
  return(output)
}

# function that takes a matrix of the response variable as provided by the model,
# and returns a long format table.
# data.matrix <- CY.m # the dataset of the response variable for each combinations of the explanatory variables, i.e., the one outputed by the model
# data.matrix <- TotH.m
# data.matrix <- CropH.m
# data.matrix <- datavar.m$CY.m
# data.matrix <- datavar.m[[1]]
# time.intervals <- Tw # vectors of the different time intervals, with name(.) = the names as written in
# var.exp.one <- theta_a # values in increasing order of the explanatory variable that changes in each row of the long format table
# var.exp.two <- attractiveness # values in increasing order of the explanatory variable that changes each new time interval
# nameResponseVar <- NULL
# name.var.exp.one <- NULL
# name.var.exp.two <- NULL
responseVarDS.to.LongFormat.fun <- function(data.matrix,time.intervals,
                                            var.exp.one,
                                            var.exp.two,
                                            nameResponseVar = NULL,
                                            name.var.exp.one = NULL,
                                            name.var.exp.two = NULL){
  if(is.null(nameResponseVar)){
    nameResponseVar <- "responseVar"
  }
  if(is.null(name.var.exp.one)){
    name.var.exp.one <- deparse(substitute(var.exp.one)) # give the name of the object entered for var.exp.one
  }
  if(is.null(name.var.exp.two)){
    name.var.exp.two <- deparse(substitute(var.exp.two)) # give the name of the object entered for var.exp.two
  }
  
  output <- NULL
  for(t in 1:length(time.intervals)){  # for each blooming period / interval 
    I.here <- names(time.intervals)[t]
    row.start.I <- which(gsub("=.*","",data.matrix[,1]) == I.here)
    if(t == length(time.intervals)){
      row.finish.I <- nrow(data.matrix)
    }else{
      row.finish.I <- which(gsub("=.*","",data.matrix[,1]) == names(time.intervals)[t+1]) - 1
    }
    # row.finish.I <- row.start.I + ceiling(length(var.exp.one) / ncol(data.matrix)) * length(var.exp.two) - 1
    data.matrix.cut <- data.matrix[row.start.I:row.finish.I,]
    data.matrix.cut.val <- as.vector(t(data.matrix.cut))
    #data.matrix.cut.val <- as.vector(data.matrix.cut)    # if value are entered by column and not by row but I doupt it
    data.matrix.cut.val <- gsub(".*\\[","",data.matrix.cut.val)
    data.matrix.cut.val <- gsub("]]","",data.matrix.cut.val)
    data.matrix.cut.val <- gsub("]","",data.matrix.cut.val)
    data.matrix.cut.val <- gsub(",","",data.matrix.cut.val)
    data.matrix.cut.val <- gsub(" ","",data.matrix.cut.val)
    data.matrix.cut.val <- data.matrix.cut.val[! data.matrix.cut.val %in% c("",",")]
    data.matrix.cut.val <- as.numeric(gsub(" ","",data.matrix.cut.val))
    data.matrix.cut.val <- data.matrix.cut.val[!is.na(data.matrix.cut.val)]
    
    # the data frame to fill:
    output.bis <- data.frame(time.intervals = rep(time.intervals[t],
                                                  length(var.exp.one)*length(var.exp.two)))
    
    output.bis$col_temp <- data.matrix.cut.val
    colnames(output.bis)[ncol(output.bis)] <- nameResponseVar
    
    # check if data was collected as Tw[attractiveness[theta_a]] 
    # i.e., in the long format, normally theta_a should be changing each row, for 
    # each value of attractiveness, and each combination of these two for each 
    # value of Tw. But it is possible that the data was not generated the same
    # way.
    needToReverseColandRow <- nrow(data.matrix.cut) %% length(var.exp.two) > 0 |
                             ncol(data.matrix.cut) %% length(var.exp.one) > 0
    if(needToReverseColandRow){
      print("In responseVarDS.to.LongFormat.fun(): need to reverse theta_a and attractiveness")
    }
  
    output.bis$col_temp <- rep(var.exp.one,length(var.exp.two))
    colnames(output.bis)[ncol(output.bis)] <- name.var.exp.one
    
    output.bis$col_temp <- as.vector(sapply(X = var.exp.two,
                                            FUN = function(x){rep(x,length(var.exp.one))}))
    colnames(output.bis)[ncol(output.bis)] <- name.var.exp.two
    
    if(is.null(output)){
      output <- output.bis
    }else{
      output <- rbind(output,output.bis)
    }
  }
  return(output)
}

# Function that produces the results figures with the three explanatory variables 
# (i.e., thata_a, attractiveness and Tw) and the response variable (i.e., crop 
# yield or nb of individuals) and adds contour lines.
# output: the long-format data set with column = "time.intervals", "yield_kg" (or "individual_nb"), "theta_a", "attractiveness"
# col.grad: the colour gradient to use; either a vector of colour names (e.g., c("firebrick", "darkorchid","dodgerblue3")
#           or the name of the gradient function (e.g., "viridis",)
# reverseColGradient: to reverse the colour gradient
# int.cont.lines: the interval for the contour lines
# col.cont.line: the colour of the contour lines
# col.grad.val: a vector of colours, with names = the corresponding value of the response variable; e.g., 
#               26           26.1        26.2        26.3        ... 
#               "#FDE725FF" "#FDE725FF" "#FDE725FF" "#FDE725FF"   ... 
# min.max.responseVar: the min and max value for the colour scale of the response variable
# division.coef: a division coefficient for the response variable
# label.col.scale: the label for the colour gradient scale

# output <- data.all$datavar.m.L$CY.m.L
# col.grad <- c("firebrick", "darkorchid","dodgerblue3")
# col.grad <- "viridis"
# int.cont.lines <- 10
# reverseColGradient <- T
# col.cont.line <- "white"
# min.max.responseVar <- NULL
# division.coef <- 1
# label.col.scale <- "Yield (kg/ha)"
figure.4Var.contourLine.fun.old <- function(output,col.grad,reverseColGradient=F,
                                        int.cont.lines=10,col.cont.line="white",
                                        min.max.responseVar=NULL,division.coef= 1,
                                        label.col.scale="Colour gradient"){
  
  responseVar <- colnames(output)[! colnames(output) %in% c("time.intervals","theta_a","attractiveness")]
  
  # Tw <- c("April","April through midMay","May","midMay through June","June",
  #         "April through June")
  Tw <- c("April","April through midMay","April through May","May","2nd part of May",
          "no wildflowers")
  
  attractiveness.val.seq <- sort(unique(output$attractiveness))
  int.attractiveness <- attractiveness.val.seq[2] - attractiveness.val.seq[1]
  theta_a.val.seq <- sort(unique(output$theta_a))
  int.theta_a <- theta_a.val.seq[2] - theta_a.val.seq[1]
  
  # modify the response variable
  output[,responseVar] <- output[,responseVar]/division.coef
  
  # Define the colour gradient for the response variable:
  if(is.null(min.max.responseVar)){
    responseVar.seq <- seq(floor(min(output[,responseVar])),ceiling(max(output[,responseVar])) + 1,0.1)
  }else{
    responseVar.seq <- seq(min.max.responseVar[1],min.max.responseVar[2],0.1)
  }
  
  colPaletteFunction <- c("viridis","magma","plasma","inferno")
  
  colPaletFun.fun <- colPaletteFunction.fun()
  
  
  if(length(col.grad) == 1){
    if(col.grad %in% colPaletteFunction){
      col.grad.val <- colPaletFun.fun[[col.grad]](x = length(responseVar.seq))
    }else{
      print("Wrong value for col.grad so palette viridis was used.")
      col.grad.val <- colPaletFun.fun[["viridis"]](x = length(responseVar.seq))
    }
  }else{
    col.grad.fun <- colorRampPalette(col.grad)
    col.grad.val <- col.grad.fun(length(responseVar.seq))
  }
  
  # eventually reverse the gradient
  if(reverseColGradient){
    col.grad.val <- rev(col.grad.val)
  }
  names(col.grad.val) <- responseVar.seq
  
  # Add a column in output for the colour corresponding to the response variable:
  output$responeVar_col <- NA
  for(i in 1:nrow(output)){
    output$responeVar_col[i] <- 
      col.grad.val[as.numeric(names(col.grad.val)) == round(output[,responseVar][i],1)]
  }
  
  fig.m <- matrix(c(1:3,4,5:7,4),nrow = 2,byrow = T)
  count.fig <- 1
  layout(fig.m,widths = c(1.22,rep(1,2),0.5),heights = c(1,1.225))
  for(i in 1:max(fig.m)){
    
    Xaxt <- "n"
    Yaxt <- "n"
    side1 <- 0.5
    side2 <- 0.6
    
    if(i != 4){ # plot the variables
      if(count.fig %in% c(1,4)){
        Yaxt <- "s"
        side2 <- 4
      }
      if(count.fig %in% 4:6){
        Xaxt <- "s"
        side1 <- 5
      }
      
      par(mar=c(side1,side2,2.5,0.6))
      plot(output$theta_a ~ output$attractiveness,col="white",las=1,
           yaxs="i",xaxs="i",xlab="",ylab="",xaxt=Xaxt,yaxt=Yaxt)
      
      mtext(Tw[count.fig],side = 3,line = 0.5)
      
      if(count.fig %in% c(1,4)){
        mtext(bquote(theta[a]),side = 2,line = 2.5)
      }
      if(count.fig %in% 4:6){
        mtext(bquote("attractiveness = "~ (theta[h]+theta[q])/2),side = 1,line = 3.2)
      }
      
      ds.here <- output[output$time.intervals == Tw[count.fig],]
      
      for(j in 1:nrow(ds.here)){
        x.range <- c(ds.here$attractiveness[j], ds.here$attractiveness[j] + int.attractiveness)
        y.range <- c(ds.here$theta_a[j], ds.here$theta_a[j] + int.theta_a)
        col.here <- ds.here$responeVar_col[j]
        polygon(x = c(x.range,rev(x.range)),
                y = c(rep(y.range[1],2),rep(y.range[2],2)),
                border = NA,col = col.here)
      }
      count.fig <- count.fig + 1
      
      # plot contour lines
      ## Get data in matrix form for plotting
      ds.here.m <- matrixFromDataset.fun(ds=ds.here,
                                         xvar.name="attractiveness",
                                         yvar.name="theta_a",
                                         zvar.name=responseVar)
      #
      # https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/contour
      u <- par("usr")
      rect(u[1], u[3], u[2], u[4])
      contour(x = sort(unique(ds.here$attractiveness)),
              y = sort(unique(ds.here$theta_a)),
              #z = t(ds.here.m[nrow(ds.here.m):1,]), # ?! 
              z = t(ds.here.m), 
              col = col.cont.line, lty = "solid", add = TRUE,
              #vfont = c("sans serif", "plain"),
              #levels = seq(floor(min(ds.here.m)),ceiling(max(ds.here.m)),30))
              levels = seq(0,ceiling(max(output[,responseVar])),int.cont.lines))
      
    }else{ # plot the gradient legend
      par(mar=c(8,1,8,5))
      y.here <- as.numeric(names(col.grad.val))
      plot(x = rep(0,length(y.here)), y = y.here ,xaxt="n",yaxt="n",ylab="",xlab="",
           yaxs="i",xaxs="i",col="white")
      axis(side = 4,las=1)
      mtext(label.col.scale,side = 4, line = 3)
      for(j in 1:(length(y.here)-1)){
        x.range <- c(-1,1)
        y.range <- c(y.here[j],y.here[j+1])
        col.here <- col.grad.val[j]
        polygon(x = c(x.range,rev(x.range)),
                y = c(rep(y.range[1],2),rep(y.range[2],2)),
                border = NA,col = col.here)
        
      }
    }
  }
}

# Function that produces the results figures with the three explanatory variables 
# (i.e., thata_a, attractiveness and Tw) and the response variable (i.e., crop 
# yield or nb of individuals) and adds contour lines.
# output <- output.experiment.l$A_600 # read.csv(paste(wd_data_raw,"output.experiment.1.csv",sep="/"),header = T)
# output <- output.exp
# col.grad = c("darkgoldenrod3","white","dodgerblue3") # col.grad <- "viridis"
# int.cont.lines <- 10
# reverseColGradient <- F
# col.cont.line <- "black"
# min.max.responseVar <- NULL
# division.coef <- 1
# label.col.scale <- expression("Yield (kg.ha"^-1*")")
# print <- F
# show.bee.CY.diffControl <- T
# diff_control_percentage <- T
# diff_control_percentage_theta_a_fix <- T # to compare with a control where field cover = A (i.e., theta_a = 0)
# total.yield <- T # if T in kg, if F, in k/ha
# # para.A <- 100 # m2
# t = seq(0,61*12, by = 0.01) # used if argument above is true and value last day is not provided in output
# responseVariable <- "cropYield" # "numberBees" #
# arrows.every.x <- arrows.every.y = 3
# colArrows="red"
# lwdArrows = 3
# theta_a_control = 0.05
# percentCutnbBeeChange <- c(0,5,20,50,100)
figure.4Var.contourLine.fun <- function(output,col.grad,reverseColGradient=F,
                                        int.cont.lines=10,col.cont.line="white",
                                        min.max.responseVar=NULL,division.coef= 1,
                                        label.col.scale=NA,
                                        print = F,nameFile=NA,total.yield = F,
                                        diff_control_percentage = F, parameters,
                                        diff_control_percentage_theta_a_fix=F,
                                        t = seq(0,65*12, by = 0.01),
                                        responseVariable = c('cropYield',"numberBees"),
                                        show.bee.CY.diffControl=F,show.bee.CY.diffControl_integrated = T,
                                        arrows.every.x = 3,arrows.every.y = 3,
                                        colArrows="red",
                                        lwdArrows = 3,theta_a_control= 10^-8,
                                        percentCutnbBeeChange = c(0,10,50,100)){
  
  
  # temporary
  if(responseVariable == "numberBees"){
    total.yield <- F
    diff_control_percentage <- F
    show.bee.CY.diffControl <- F
    diff_control_percentage_theta_a_fix <- F
  }
  
  if(!is.null(output$A[1])){
    para.A <- output$A[1]
  }else{
    para.A <- parameters$A
    print("A was not provided in 'output' so it was taken in parameters$A")
    print("Make sure this is the right value for A.")
  }
  
  # for former datasets that have the older name of the yield column:
  colnames(output)[colnames(output) == "yield_kg.ha.61d"] <- "yield_kg.ha.last.d"

  if(print){
    if(is.na(nameFile)){
      date.time <- Sys.time()
      date.time <- gsub(":","-",date.time)
      nameFile <- paste("Experiment_results_",date.time,".jpeg",sep="")
    }
    path.nameFile <- paste(wd_figures,nameFile,sep="/")
    jpeg(filename = path.nameFile,
         width = 22,height = 15, units = "cm",res = 600)
  }
  
  #
  if(length(int.cont.lines) == 1){
    int.cont.lines <- rep(int.cont.lines,6)
  }
  
  # responseVar <- colnames(output)[!colnames(output) %in% c("Tw","theta_h","theta_q","theta_a")]
  responseVar <- colnames(output)[!colnames(output) %in% c(names(parameters),"last_day")]
  if(length(responseVar) > 1){
    if(responseVariable == 'cropYield'){
      responseVar <- responseVar[grep("yield",responseVar)]
    }else if(responseVariable == 'numberBees'){
      responseVar <- responseVar[grep("bee",responseVar)]
    }
  }
  
  # Tw <- c("April","April through midMay","May","midMay through June","June",
  #         "April through June")
  Tw <- c("April","April through midMay","April through May","May","2nd part of May",
          "no wildflowers")
  names(Tw) <- c("Ap","ApmM","ApM","M","2ndmM","control")
  
  output$attractiveness <- (output$theta_h + output$theta_q) / 2
  
  attractiveness.val.seq <- sort(unique(output$attractiveness))
  int.attractiveness <- attractiveness.val.seq[2] - attractiveness.val.seq[1]
  theta_a.val.seq <- sort(unique(output$theta_a))
  int.theta_a <- theta_a.val.seq[2] - theta_a.val.seq[1]
  
  # 
  output$yield_kg.last.d <-  output$yield_kg.ha.last.d * para.A / 10000
  
  # # kg/ha --> kg
  # if(total.yield){
  #   total.yield.kg <- output$yield_kg.ha.last.d
  #   # total.yield.kg <- total.yield.kg * (para.A * (1 - output$theta_a)) / 10000
  #   output$yield_kg.ha.last.d <-  total.yield.kg * para.A / 10000
  #   
  #   # plot(output$yield_kg.ha.last.d ~ output$theta_a)
  #   # plot(total.yield.kg ~ output$theta_a)
  # }
  
  if(responseVariable == "cropYield"){
    if(total.yield){
      output$responseVar <- output$yield_kg.last.d 
      responseVar <- gsub("ha.","",responseVar)
    }else{
      output$responseVar <- output$yield_kg.ha.last.d
    }
  }else{
    output$responseVar <- output$tot.adult.bees.last.d
  }

  # modify the response variable; NOT USED
  output[,responseVar] <- output[,responseVar]/division.coef
  
  # % difference with control
  # Note that this is done for crop yield and not the total number of bees.
  # Future work should consists in allowing the option for bees too.
  if(diff_control_percentage){
    
    output.control <- output[output$Tw == "control",]
    output.cut <-  output[output$Tw != "control",]
    
    if(diff_control_percentage_theta_a_fix){
      # Need to calculate crop yield in that specific case.
      # Then the values of CY and CY.ha in output.control are replaced.
      
      # check if the values used for theta_a, ha nd q for the contral are already in output.control
      # tot.adult.bees.integ.c <- output.control$tot.adult.bees.integ[output.control$theta_a == theta_a_control & 
      #                                                                 output.control$theta_q == 1 &
      #                                                                 output.control$theta_h == 1]
      # if(length(tot.adult.bees.integ.c) == 0){
      #   # the configuration is not present in output.control --> need to run the corresponding simulation
      #   
      # }
      
      if(!"last_day" %in% colnames(output)){
        last_day <- max(t)/12
        print(paste0("The last day is: ",last_day,". Make sure this is correct."))
      }else{
        last_day <- output$last_day[1]
      }
      t.h <- seq(0,last_day*12, by = 0.01)
      parameters.c <- parameters
      parameters.c$theta_a <- theta_a_control
      parameters.c$Tw <- treatments.Tw$control
      parameters.c$theta_q <- parameters.c$theta_h <- 1
      
      out.1 <- ode(y = state, times = t.h, func = ODE.m, parms = parameters.c)
      out.2 <- as.data.frame(out.1)
      out.2$CY <- crop.yield.fun(data.ts = out.2,parameters = parameters.c,
                                 convert.to.day = F,
                                 modif.tot.amount = T,plot.PS = F) # in g for the total area
      # check --> 
      # out.2$CY[nrow(out.2)] / 1000
      # output.control$yield_kg.last.d[output.control$theta_a == theta_a_control & 
      #                                output.control$theta_q == 1 &
      #                                output.control$theta_h == 1]

      # CY (integrated) --> same value for all the rows because diff_control_percentage_theta_a_fix is T
      yield.control <- out.2$CY[nrow(out.2)] / 1000 # in kg
      output.control$yield_kg.ha.last.d <- yield.control * 10000 / para.A
      output.control$yield_kg.last.d <- yield.control
      
      # integrate the total number of adult bees over the crop blooming period
      time.diff <- diff(out.2$time)[1]
      out.cropBloom <- out.2[out.2$time >= 30*12 & out.2$time <= 60*12,]
      out.cropBloom$tot.adult.bees.integ <- rowSums(out.cropBloom[,c("Wn","S","Hc","Hw","Uc","Uw")])
      out.cropBloom$tot.adult.bees.integ <- cumsum(out.cropBloom$tot.adult.bees.integ)*time.diff
      
      out.cropBloom[,1] <- out.cropBloom[,1] / 12 # conversion in days
      out.cropBloom <- out.cropBloom[out.cropBloom[,1] %in% 0:90,]
      # out.cropBloom$tot.adult.bees.integ[out.cropBloom$time == 60]
      
      # to use below if show.bee.CY.diffControl
      if(show.bee.CY.diffControl){
        out.2.control <- out.2
        
        out.2.control$yield_kg.ha.last.d <- out.2.control$CY /1000 * 10000 / output.control$A[1] # g --> kg/ha
        out.2.control$yield_kg.last.d <- out.2.control$CY /1000 # g --> kg
        
        out.2.control[,1] <- out.2.control[,1] / 12 # conversion in days
        out.2.control <- out.2.control[out.2.control[,1] %in% 0:90,]
      }
    }
    
    # Tw.here <- levels(as.factor(output.cut$Tw)) # levels does not keep the order of the values
    # calculate the % difference with control
    Tw.here <- c("Ap","ApmM","ApM","M","2ndmM")
    new.val <- c()
    for(i in Tw.here){
      new.val.here <- (output.cut[output.cut$Tw == i,responseVar]- output.control[,responseVar]) /
        output.control[,responseVar] * 100
      new.val <- c(new.val,new.val.here)
    }
    output.cut$responseVar <- new.val
    output <- output.cut
  }
  
  # Define the colour gradient for the response variable:
  decimal <- 0.1
  if(max(output$responseVar) > 1000){
    decimal <- 1
  }
  if(is.null(min.max.responseVar)){
    responseVar.seq <- seq(floor(min(output$responseVar)),
                           ceiling(max(output$responseVar)) + 1,decimal)
  }else{
    responseVar.seq <- seq(min.max.responseVar[1],min.max.responseVar[2],decimal)
  }
  
  # colPaletteFunction <- c("viridis","magma","plasma","inferno")
  colPaletFun.fun <- colPaletteFunction.fun()
  
  if(length(col.grad) == 1){ # the name of the colour palette is given
    if(col.grad %in% colPaletteFunction){
      col.grad.val <- colPaletFun.fun[[col.grad]](x = length(responseVar.seq))
    }else{
      print("Wrong value for col.grad so palette viridis was used.")
      col.grad.val <- colPaletFun.fun[["viridis"]](x = length(responseVar.seq))
    }
    # col.grad.fun <- colorRampPalette(col.grad)
    # col.grad.val <- col.grad.fun(length(responseVar.seq))
    # eventually reverse the gradient
    if(reverseColGradient){
      col.grad.val <- rev(col.grad.val)
    }
    names(col.grad.val) <- responseVar.seq
  }else{ # several colour are given
    if(diff_control_percentage){
      
      col.grad.l <- c(col.grad[1],col.grad[2])
      col.grad.length <- length(col.grad)
      col.grad.u <- c(col.grad[col.grad.length-1],col.grad[col.grad.length])
      
      # max.val <- max(responseVar.seq)
      # if(max.val < abs(min(responseVar.seq))){
      #   max.val <- abs(min(responseVar.seq))
      # }
      
      responseVar.seq.l <- responseVar.seq[responseVar.seq < 0]
      responseVar.seq.u <- responseVar.seq[responseVar.seq >= 0]
      
      col.grad.fun.l <- colorRampPalette(col.grad.l)
      col.grad.val.l <- col.grad.fun.l(length(responseVar.seq.l))
      col.grad.fun.u <- colorRampPalette(col.grad.u)
      col.grad.val.u <- col.grad.fun.u(length(responseVar.seq.u))
      
      names(col.grad.val.l) <- responseVar.seq.l
      names(col.grad.val.u) <- responseVar.seq.u
      
      col.grad.val <- c(col.grad.val.l,col.grad.val.u)
      
    }else{
      col.grad.fun <- colorRampPalette(col.grad)
      col.grad.val <- col.grad.fun(length(responseVar.seq))
      # eventually reverse the gradient
      if(reverseColGradient){
        col.grad.val <- rev(col.grad.val)
      }
      names(col.grad.val) <- responseVar.seq
    }
  }
  
  # Add a column in output for the colour corresponding to the response variable:
  output$responeVar_col <- NA
  round <- 1
  if(max(output$responseVar) > 1000){
    round <- 0
  }
  responVarVal <- unique(round(output$responseVar,round))
  for(i in 1:length(responVarVal)){
    output$responeVar_col[round(output$responseVar,round) == responVarVal[i]] <- 
      col.grad.val[round(as.numeric(names(col.grad.val)),round) == responVarVal[i]]
  }
  
  # for(i in 1:nrow(output)){
  #   print(i)
  #   output$responeVar_col[i] <- 
  #     col.grad.val[round(as.numeric(names(col.grad.val)),1) == round(output$responseVar[i],1)]
  # }
  
  # if we want to show the areas where:
  # - CY increase & nb bees increase compared to control
  # - CY increase & nb bees decrease 
  # - CY decrease & nb bees decrease
  # - CY decrease & nb bees increases
  if(show.bee.CY.diffControl){
    
    if(diff_control_percentage_theta_a_fix){ # replace tot.adult.bees.last.d by value in out.2.control
      output.control$tot.adult.bees.last.d <- 
        rowSums(out.2.control[,c("Wn","S","Hc","Hw","Uc","Uw")])[nrow(out.2.control)]
      
      # output.control$tot.adult.bees.last.d[output.control$theta_a == 0.05 & 
      #                                      output.control$theta_q == 1 &
      #                                      output.control$theta_h == 1]
      
      output.control$tot.adult.bees.integ <- out.cropBloom$tot.adult.bees.integ[nrow(out.cropBloom)]
    }

    if(!show.bee.CY.diffControl_integrated){ # if we show the # of adult bees at the end crop blooming season
      varhere <- "tot.adult.bees.last.d"
    }else{ # if show the integrated number of bees at the end of crop blooming season (default)
      varhere <- "tot.adult.bees.integ"
    }

    output$bees_diffControl <- output[,varhere] - rep(output.control[,varhere],
                                                      nrow(output)/nrow(output.control))
    
    # 
    output$bees_diffControl_round <- output$bees_diffControl
    output$bees_diffControl_round[abs(output$bees_diffControl) < 1 ] <- 0
    
    # 
    output$bees_diffControl_round_absPercent <- abs(output$bees_diffControl_round/
                                                    rep(output.control[,varhere],nrow(output)/nrow(output.control))*
                                                    100)
    
    # 100% to lwd arrows: 
    # interval <- (lwdArrows - 0.5)/(length(percentCutnbBeeChange)-2) # identical interval regardless of values in percentCutnbBeeChange
    # interval proportional to values in percentCutnbBeeChange
    interval <- transform.scale.fun(x1 = percentCutnbBeeChange[percentCutnbBeeChange != 0], 
                                    min1 = percentCutnbBeeChange[2],
                                    min2 = 0.5, max2 = lwdArrows)
    
    for(i in 1:(length(percentCutnbBeeChange)-1)){
      output$bees_diffControl_lwd[output$bees_diffControl_round_absPercent >
                                    percentCutnbBeeChange[i]] <- interval[i] # 0.5 + interval*(i - 1)
    }
  }
  
  if(diff_control_percentage){
    if(show.bee.CY.diffControl){
      fig.m <- matrix(c(1:3,1:3,5:6,7,5:6,4),nrow = 4,byrow = T)
      layout(fig.m,widths = c(1.22,rep(1,2)),heights = c(rep(1,3),1.225))
    }else{
      fig.m <- matrix(c(1:3,5:6,4),nrow = 2,byrow = T)
      layout(fig.m,widths = c(1.22,rep(1,2)),heights = c(1,1.225))
    }
  }else{
    fig.m <- matrix(c(1:3,4,5:7,4),nrow = 2,byrow = T)
    layout(fig.m,widths = c(1.22,rep(1,2),0.5),heights = c(1,1.225))
  }
  count.fig <- 1
  for(i in 1:max(fig.m)){
    
    Xaxt <- "n"
    Yaxt <- "n"
    side1 <- 0.5
    side2 <- 0.6
    
    if(i != 4 & i != 7){ # plot the variables
      if(count.fig %in% c(1,4)){
        Yaxt <- "s"
        side2 <- 5
      }
      if(count.fig %in% 4:6){
        Xaxt <- "s"
        side1 <- 5
      }
      
      par(mar=c(side1,side2,2.5,0.6))
      plot(output$theta_a ~ output$attractiveness,col="white",las=1,
           yaxs="i",xaxs="i",xlab="",ylab="",xaxt=Xaxt,yaxt=Yaxt)
      
      mtext(Tw[count.fig],side = 3,line = 0.5)
      
      if(count.fig %in% c(1,4)){
        mtext(bquote("wildflower proportion ( "*theta[a]*")"),side = 2,line = 3.15)
      }
      if(count.fig %in% 4:6){
        mtext(bquote("attractiveness = "~ (theta[h]+theta[q])/2),side = 1,line = 3.2)
      }
      
      # ds.here <- output[output$time.intervals == Tw[count.fig],]
      ds.here <- output[output$Tw == names(Tw[count.fig]),]
      
      for(j in 1:nrow(ds.here)){
        x.range <- c(ds.here$attractiveness[j], ds.here$attractiveness[j] + int.attractiveness)
        y.range <- c(ds.here$theta_a[j], ds.here$theta_a[j] + int.theta_a)
        col.here <- ds.here$responeVar_col[j]
        polygon(x = c(x.range,rev(x.range)),
                y = c(rep(y.range[1],2),rep(y.range[2],2)),
                border = NA,col = col.here)
      }
      
      #
      if(show.bee.CY.diffControl){
        
        ds.here$arrow_x0 <- ds.here$arrow_x1 <- ds.here$attractiveness
        ds.here$arrow_y0 <- ds.here$theta_a
        ds.here$arrow_y1 <- NA
        
        condition <- ds.here$bees_diffControl_round > 0
        ds.here$arrow_y1[condition] <- ds.here$theta_a[condition] + int.theta_a
        
        condition <- ds.here$bees_diffControl_round < 0
        ds.here$arrow_y1[condition] <- ds.here$theta_a[condition] - int.theta_a
        # arrow_y1 and arrow_y0 are reversed here below to give the arrows the same location
        ds.here$arrow_y0[condition] <- ds.here$arrow_y0[condition] + int.theta_a
        ds.here$arrow_y1[condition] <- ds.here$arrow_y1[condition] + int.theta_a
        
        condition <- ds.here$bees_diffControl_round == 0
        ds.here$arrow_y1[condition] <- ds.here$theta_a[condition]
        
        val.x <- unique(ds.here$attractiveness)
        val.x.toKeep <- val.x[(1:length(val.x) + arrows.every.x - 1) %% arrows.every.x == 0]
        val.y <- unique(ds.here$theta_a)
        val.y.toKeep <- val.y[(1:length(val.x) + arrows.every.y - 1) %% arrows.every.y == 0]
        
        ds.here.cut <- ds.here[ds.here$attractiveness %in% val.x.toKeep,]
        ds.here.cut <- ds.here.cut[ds.here.cut$theta_a %in% val.y.toKeep,]
        
        # plot arrows for when a change in nb bees is observed
        y_shift <- int.theta_a /2
        x_shift <- int.attractiveness/2
        
        ds.here.cut.change <- ds.here.cut[ds.here.cut$bees_diffControl_round !=0,]
        if(nrow(ds.here.cut.change) > 0){
          arrows(x0 = ds.here.cut.change$arrow_x0 + x_shift, 
                 x1 = ds.here.cut.change$arrow_x1 + x_shift, 
                 y0 = ds.here.cut.change$arrow_y0 + y_shift,
                 y1 = ds.here.cut.change$arrow_y1 + y_shift,
                 length = 0.06, lwd = ds.here.cut.change$bees_diffControl_lwd,
                 col=colArrows)
        }
        
        # plot points for when no change in nb bees is observed
        ds.here.cut.changeNo <- ds.here.cut[ds.here.cut$bees_diffControl_round ==0,]
        if(nrow(ds.here.cut.changeNo) > 0){
          points(x = ds.here.cut.changeNo$arrow_x0 + x_shift,
                 y = ds.here.cut.changeNo$arrow_y0 + y_shift,
                 lwd = lwdArrows, col=colArrows,pch = 16)
        }
      }
      
      # plot contour lines
      ## Get data in matrix form for plotting
      ds.here.m <- matrixFromDataset.fun(ds=ds.here,
                                         xvar.name="attractiveness",
                                         yvar.name="theta_a",
                                         zvar.name="responseVar")
      #
      # https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/contour
      u <- par("usr")
      rect(u[1], u[3], u[2], u[4])
      contour(x = sort(unique(ds.here$attractiveness)),
              y = sort(unique(ds.here$theta_a)),
              #z = t(ds.here.m[nrow(ds.here.m):1,]), # ?! 
              z = t(ds.here.m), 
              col = col.cont.line, lty = "solid", add = TRUE,
              #vfont = c("sans serif", "plain"),
              #levels = seq(floor(min(ds.here.m)),ceiling(max(ds.here.m)),30))
              levels = seq(floor(min(output$responseVar)),
                           ceiling(max(output$responseVar)),
                           int.cont.lines[count.fig]))
      
      count.fig <- count.fig + 1
      
    }else{
      
      # plot the gradient legend
      if(i == 4){
        if(is.na(label.col.scale)){
          if(total.yield & !diff_control_percentage){
            label.col.scale <- "Yield (kg)"
          }else if(total.yield & diff_control_percentage){
            label.col.scale <- "% yield difference with control"
          }else if(!total.yield & !diff_control_percentage){
            label.col.scale <- bquote("Yield (kg.ha"^-1*")")
          }else if(!total.yield & diff_control_percentage){
            label.col.scale <- bquote("% yield difference with control")
          }
        }
        
        if(diff_control_percentage){
          if(show.bee.CY.diffControl){
            par(mar=c(12.5/1.5,1,9.5/4,1))
          }else{
            par(mar=c(12.5,1,9.5,1)) # larger margins
          }
          
          y.here <- as.numeric(names(col.grad.val))
          plot(y = rep(0,length(y.here)), x = y.here ,xaxt="n",yaxt="n",ylab="",xlab="",
               yaxs="i",xaxs="i",col="white")
          axis(side = 1,las=1)
          mtext(label.col.scale,side = 1, line = 3)
          for(j in 1:(length(y.here)-1)){
            x.range <- c(-1,1)
            y.range <- c(y.here[j],y.here[j+1])
            col.here <- col.grad.val[j]
            polygon(y = c(x.range,rev(x.range)),
                    x = c(rep(y.range[1],2),rep(y.range[2],2)),
                    border = NA,col = col.here)
          }
        }else{ # legend is on the right side
          
          par(mar=c(8,1,8,5))
          y.here <- as.numeric(names(col.grad.val))
          plot(x = rep(0,length(y.here)), y = y.here ,xaxt="n",yaxt="n",ylab="",xlab="",
               yaxs="i",xaxs="i",col="white")
          axis(side = 4,las=1)
          mtext(label.col.scale,side = 4, line = 3)
          for(j in 1:(length(y.here)-1)){
            x.range <- c(-1,1)
            y.range <- c(y.here[j],y.here[j+1])
            col.here <- col.grad.val[j]
            polygon(x = c(x.range,rev(x.range)),
                    y = c(rep(y.range[1],2),rep(y.range[2],2)),
                    border = NA,col = col.here)
            
          }
        }
      }else if(i == 7){ # plot legend arrows
        par(mar=c(12.5/3,1,9.5/3,1))
        plot(x = c(0,100),y = c(0,1),col="white",
             xlab ="",ylab="",xaxt="n",yaxt="n",
             bty="n")
        at <- 100/(length(percentCutnbBeeChange)-1)
        at <- seq(0,100,at)
        axis(side = 1,at = at,labels = percentCutnbBeeChange)
        mtext(text = "% change bees",side = 1,line = 3)
        minY <- 0.2
        
        xat <- (at[2:length(at)] - 
                  at[1:(length(at)-1)])/2
        xat <- at[2:length(at)] - xat
        
        arrows(x0 = xat, x1 = xat, 
               y0 = rep(minY,length(percentCutnbBeeChange)), 
               y1 = rep(0.7,length(percentCutnbBeeChange)),
               length = 0.1, lwd = sort(unique(output$bees_diffControl_lwd)),
               col=colArrows)
        points(x = 0, y = minY, col = colArrows, pch = 16, lwd = lwdArrows)
      }
    }
  }
  if(print){
    dev.off()
  }
}

# function that transform number from one scale/range to another
# x1 <- c(5,20,50,100)
# min2 <- 0.5
# max2 <- 4
# min1=NA
# max1=NA
transform.scale.fun <- function(min1=NA,max1=NA,min2,max2,x1){
  if(is.na(min1)){
    min1 <- min(x1)
  }
  if(is.na(max1)){
    max1 <- max(x1)
  }
  x2 <- min2 + (max2 - min2)*(x1-min1)/(max1-min1)
  return(x2)
}

# Function gbm.perspec() from the package "dismo" modified for additional display options 
gbm.perspec.modif <- function(gbm.object, x = 1, y = 2, pred.means = NULL, x.label = NULL, 
                              x.range = NULL, y.label = NULL, z.label = "fitted value", 
                              y.range = NULL, z.range = NULL, leg.coords = NULL, 
                              ticktype = "detailed", theta = 55, phi = 40, 
                              smooth = "none", mask = FALSE, perspective = TRUE, 
                              display.legend = T, col.lines = "black", lwd.lines = 1, # added from original versions
                              lty.lines=NA,# added from original versions
                              ...){
  
  if (!requireNamespace("gbm")) {
    stop("you need to install the gbm package to use this function")
  }
  requireNamespace("splines")
  gbm.call <- gbm.object$gbm.call
  gbm.x <- gbm.call$gbm.x
  n.preds <- length(gbm.x)
  gbm.y <- gbm.call$gbm.y
  pred.names <- gbm.call$predictor.names
  family = gbm.call$family
  have.factor <- FALSE
  x.name <- gbm.call$predictor.names[x]
  if (is.null(x.label)) {
    x.label <- gbm.call$predictor.names[x]
  }
  y.name <- gbm.call$predictor.names[y]
  if (is.null(y.label)) {
    y.label <- gbm.call$predictor.names[y]
  }
  data <- gbm.call$dataframe[, gbm.x, drop = FALSE]
  n.trees <- gbm.call$best.trees
  if (is.vector(data[, x])) {
    if (is.null(x.range)) {
      x.var <- seq(min(data[, x], na.rm = T), max(data[, 
                                                       x], na.rm = T), length = 50)
    }
    else {
      x.var <- seq(x.range[1], x.range[2], length = 50)
    }
  }
  else {
    x.var <- names(table(data[, x]))
    have.factor <- TRUE
  }
  if (is.vector(data[, y])) {
    if (is.null(y.range)) {
      y.var <- seq(min(data[, y], na.rm = T), max(data[, 
                                                       y], na.rm = T), length = 50)
    }
    else {
      y.var <- seq(y.range[1], y.range[2], length = 50)
    }
  }
  else {
    y.var <- names(table(data[, y]))
    if (have.factor) {
      stop("at least one marginal predictor must be a vector!")
    }
    else {
      have.factor <- TRUE
    }
  }
  pred.frame <- expand.grid(list(x.var, y.var))
  names(pred.frame) <- c(x.name, y.name)
  pred.rows <- nrow(pred.frame)
  if (have.factor) {
    if (is.factor(pred.frame[, 2])) {
      pred.frame <- pred.frame[, c(2, 1)]
      x.var <- y.var
    }
  }
  j <- 3
  for (i in 1:n.preds) {
    if (i != x & i != y) {
      if (is.vector(data[, i])) {
        m <- match(pred.names[i], names(pred.means))
        if (is.na(m)) {
          pred.frame[, j] <- mean(data[, i], na.rm = T)
        }
        else pred.frame[, j] <- pred.means[m]
      }
      if (is.factor(data[, i])) {
        m <- match(pred.names[i], names(pred.means))
        temp.table <- table(data[, i])
        if (is.na(m)) {
          pred.frame[, j] <- rep(names(temp.table)[2], 
                                 pred.rows)
        }
        else {
          pred.frame[, j] <- pred.means[m]
        }
        pred.frame[, j] <- factor(pred.frame[, j], levels = names(temp.table))
      }
      names(pred.frame)[j] <- pred.names[i]
      j <- j + 1
    }
  }
  prediction <- gbm::predict.gbm(gbm.object, pred.frame, n.trees = n.trees, 
                                 type = "response")
  if (smooth == "model") {
    pred.glm <- glm(prediction ~ ns(pred.frame[, 1], df = 8) * 
                      ns(pred.frame[, 2], df = 8), data = pred.frame, 
                    family = poisson)
    prediction <- fitted(pred.glm)
  }
  max.pred <- max(prediction)
  message("maximum value = ", round(max.pred, 2), "\n")
  if (is.null(z.range)) {
    if (family == "bernoulli") {
      z.range <- c(0, 1)
    }
    else if (family == "poisson") {
      z.range <- c(0, max.pred * 1.1)
    }
    else {
      z.min <- min(data[, y], na.rm = T)
      z.max <- max(data[, y], na.rm = T)
      z.delta <- z.max - z.min
      z.range <- c(z.min - (1.1 * z.delta), z.max + (1.1 * 
                                                       z.delta))
    }
  }
  if (have.factor == FALSE) {
    pred.matrix <- matrix(prediction, ncol = 50, nrow = 50)
    if (smooth == "average") {
      pred.matrix.smooth <- pred.matrix
      for (i in 2:49) {
        for (j in 2:49) {
          pred.matrix.smooth[i, j] <- mean(pred.matrix[c((i - 
                                                            1):(i + 1)), c((j - 1):(j + 1))])
        }
      }
      pred.matrix <- pred.matrix.smooth
    }
    if (mask) {
      mask.trees <- gbm.object$gbm.call$best.trees
      point.prob <- gbm::predict.gbm(gbm.object[[1]], 
                                     pred.frame, n.trees = mask.trees, type = "response")
      point.prob <- matrix(point.prob, ncol = 50, nrow = 50)
      pred.matrix[point.prob < 0.5] <- 0
    }
    if (!perspective) {
      image(x = x.var, y = y.var, z = pred.matrix, zlim = z.range)
    }
    else {
      persp(x = x.var, y = y.var, z = pred.matrix, zlim = z.range, 
            xlab = x.label, ylab = y.label, zlab = z.label, 
            theta = theta, phi = phi, r = sqrt(10), d = 3, 
            ticktype = ticktype, mgp = c(4, 1, 0), ...)
    }
  }
  if (have.factor) {
    factor.list <- names(table(pred.frame[, 1]))
    n <- 1
    if (is.null(z.range)) {
      vert.limits <- c(0, max.pred * 1.1)
    }
    else {
      vert.limits <- z.range
    }
    
    if(is.na(lty.lines)){  # this added from the original command
      lty.lines.here <- c(1:length(factor.list))
    }else if(length(lty.lines) == 1){
      lty.lines.here <- rep(lty.lines,length(factor.list))
    }else{
      lty.lines.here <- lty.lines
    }

    plot(pred.frame[pred.frame[, 1] == factor.list[1], 2], 
         prediction[pred.frame[, 1] == factor.list[1]], type = "l", 
         ylim = vert.limits, xlab = y.label, ylab = z.label,
         lwd=lwd.lines, col=col.lines[1], lty= lty.lines.here[1], # added from original function
         ...)
    for (i in 2:length(factor.list)) {
      factor.level <- factor.list[i]
      
      col.lines.here <- col.lines    # this is added from original command
      if(length(col.lines) > 1){
        col.lines.here <- col.lines[i]
      }
      
      lines(pred.frame[pred.frame[, 1] == factor.level, 
                       2], prediction[pred.frame[, 1] == factor.level], 
            lty = lty.lines.here[i], # lty = i,   # modified from original command
            lwd=lwd.lines, col=col.lines.here)
    }
    
    if(display.legend){ # The if(display.legend){} was added from original function
      if (is.null(leg.coords)) {
        x.max <- max(pred.frame[, 2])
        x.min <- min(pred.frame[, 2])
        x.range <- x.max - x.min
        x.pos <- c(x.min + (0.02 * x.range), x.min + (0.3 * 
                                                        x.range))
        y.max <- max(prediction)
        y.min <- min(prediction)
        y.range <- y.max - y.min
        y.pos <- c(y.min + (0.8 * y.range), y.min + (0.95 * 
                                                       y.range))
        legend(x = x.pos, y = y.pos, factor.list, 
               lty = lty.lines.here, # lty = c(1:length(factor.list)), 
               bty = "n",col = col.lines,lwd = lwd.lines)
      }
      else {
        legend(x = leg.coords[1], y = leg.coords[2], factor.list, 
               lty = lty.lines.here, # lty = c(1:length(factor.list)), 
               bty = "n",col = col.lines,lwd = lwd.lines)
      }
    }
  }
}

# function taking a vector of colours names and return a vector of the same colours
#but with more transparency added
add.transparency.colour <- function(colours,alpha=0.35){
  col.rgb <- col2rgb(colours)
  colnames(col.rgb) <- colours
  output <- c()
  for(i in 1:length(col.rgb[1,])){
    output[i] <- rgb(red = col.rgb[1,i],green = col.rgb[2,i],blue = col.rgb[3,i], maxColorValue = 255,alpha = alpha*255)
  }
  names(output) <- names(colours)
  return(output)
}


# dataset <- datavar.m.L$CY.m.L
# var.response <- "yield_kg"
# var.explanatory <- "time.intervals"
# var.explanatory.others <- c("attractiveness","theta_a")
# error <- "SD"
# y.lim <- c(0,300)
# x.lim <- NULL
# x.lab <- bquote(theta[a])
# y.lab <- NULL
# jitter.amount <- 0.2
# length.arrow <- 0.25
plot.pairwise.relationship.fun <- function(dataset,var.response,var.explanatory,
                                           var.explanatory.others,error = "SD",
                                           x.lim = NULL, y.lim = NULL,
                                           x.lab=NULL,y.lab=NULL,jitter.amount=0.2,
                                           xax.t = "s", yax.t = "s",length.arrow = 0.25){
  
  
  if(length(var.explanatory) > 1){
    print("var.explanatory has multiple values, but only the 1st one is used")
    print(var.explanatory[1])
    var.explanatory <- var.explanatory[1]
  }
  
  dataset <- dataset[,c(var.response,var.explanatory,var.explanatory.others)]
  
  if(is.null(y.lim)){
    y.lim <- c(min(dataset[,var.response]),
               max(dataset[,var.response]))
  }
  if(is.null(x.lim)){
    x.lim <- c(min(dataset[,var.explanatory]),
               max(dataset[,var.explanatory]))
  }
  
  if(is.null(y.lab)){
    y.lab <- var.response
  }
  if(is.null(x.lab)){
    x.lab <- var.explanatory
  }
  
  if(is.factor(var.explanatory) | is.character(dataset[1,var.explanatory])){ #
    
    if(!is.factor(var.explanatory)){
      # levels.here <- c("April","April through midMay","May","midMay through June",
      #                  "June","April through June")
      levels.here <- c("April","April through midMay","April through May","May","2nd part of May",
                        "no wildflowers")
      
      dataset[,var.explanatory] <- factor(dataset[,var.explanatory],levels = levels.here)
    }
    cat.here <- levels(dataset[,var.explanatory])
    
    x.val <- 1:length(cat.here)
    y.val.mean <- tapply(X = dataset[,var.response],
                         INDEX = dataset[,var.explanatory],
                         FUN = mean)
    y.val.error <- tapply(X = dataset[,var.response],
                          INDEX = dataset[,var.explanatory],
                          FUN = sd)
    n <- tapply(X = dataset[,var.response],
                INDEX = dataset[,var.explanatory],
                FUN = length)
    if(error == "SE"){
      y.val.error <- y.val.error / sqrt(n)
    }
    
    plot(y.val.mean ~ x.val, col = "white", ylim = y.lim, xlim=c(x.val[1]-0.5,tail(x.val,1)+0.5),
         las = 1, xlab="", ylab="", xaxt="n", yaxt = yax.t)
    axis(side = 1,labels = F)
    text(x =x.val, # https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/
         y = par("usr")[3] - 15, # move the labels to just below the bottom of chart
         labels = cat.here,
         xpd = NA, # change the clipping region
         srt = 35, # rotate the labels at 35 degrees
         adj = 0.965, # adjust the labels to almost 100% right-justified
         cex = 1)
    
    for(i in x.val){
      # all the data points
      points(x = jitter(rep(x.val[i],n[i]), amount = jitter.amount),
             y = dataset[dataset[,var.explanatory] == cat.here[i],var.response],
             col=add.transparency.colour("gray"),pch = 16)
      # the mean
      points(x = i, y = y.val.mean[i], cex = 2, pch = 16)
      # the error
      arrows(x0 = i, x1 = i,
             y0 = y.val.mean[i], y1 = y.val.mean[i] + y.val.error[i],
             angle = 90,length = length.arrow)
      arrows(x0 = i, x1 = i, 
             y0 = y.val.mean[i], y1 = y.val.mean[i] - y.val.error[i],
             angle = 90,length = length.arrow)
    }
    
    mtext(x.lab,side = 1,line = 6)
    
  }else{
    
    x.val <- sort(unique(dataset[,var.explanatory]))
    y.val.mean <- tapply(X = dataset[,var.response],
                         INDEX = dataset[,var.explanatory],
                         FUN = mean)
    y.val.error <- tapply(X = dataset[,var.response],
                          INDEX = dataset[,var.explanatory],
                          FUN = sd)
    n <- tapply(X = dataset[,var.response],
                INDEX = dataset[,var.explanatory],
                FUN = length)
    if(error == "SE"){
      y.val.error <- y.val.error / sqrt(n)
    }
    plot(y.val.mean ~ x.val, col = "white",xlim=x.lim,ylim=y.lim, las = 1,
         xlab="",ylab="", yaxt = yax.t, xaxt = xax.t)
    for(i in 1:length(x.val)){
      points(x = rep(x.val[i],n[i]),
             y = dataset[dataset[,var.explanatory] == x.val[i],var.response],
             col=add.transparency.colour("gray"),pch = 16)
    }
    polygon(x = c(x.val,rev(x.val)),
            y = c(y.val.mean + y.val.error,rev(y.val.mean- y.val.error)),
            col = add.transparency.colour("gray"),lty = 0)
    lines((y.val.mean + y.val.error) ~ x.val,lwd=1)
    lines((y.val.mean - y.val.error) ~ x.val,lwd=1)
    
    lines(y.val.mean ~ x.val,lwd=2)
    
    mtext(x.lab,side = 1,line = 3)
  }
  mtext(y.lab,side = 2,line = 3)
}

# dataset <- data.all$datavar.m.L$CY.m.L
# var.response <- "yield_kg"
# var.explanatory.1 <- "attractiveness" # "time.intervals"
# var.explanatory.2 <- "theta_a"
# var.explanatory.others <- c("time.intervals") # c("attractiveness")
# error <- "SD"
# y.lim <- c(0,300)
# x1.lab <- NULL
# x2.lab <- bquote(theta[a])
# y.lab <- NULL
# jitter.amount <- 0.2
# xax.t <- yax.t <- "s"
# col.cat <- NULL
# legend.show <- T
# legend.position <- "bottomleft"
# theta <- 45
# phi <- 30
# col.gradiant <- NULL
# shade.gradiant <- 0.5
# plot.gradiant.legend <- F
plot.pairwise.covariation.fun <- function(dataset,var.response,
                                          var.explanatory.1,var.explanatory.2,
                                          var.explanatory.others,error = "SD",
                                          y.lim = NULL,
                                          x1.lab=NULL,x2.lab=NULL,y.lab=NULL,jitter.amount=0.2,
                                          xax.t = "s", yax.t = "s",col.cat = NULL,
                                          legend.show = T, legend.position = "bottomleft",
                                          theta = 45, phi = 30,col.gradiant = NULL,
                                          shade.gradiant = 0.5,plot.gradiant.legend = F){
  
  if(length(var.explanatory.1) > 1 | length(var.explanatory.2) > 1){
    print("var.explanatory 1 and/or 2 has multiple values, but only the 1st one is used")
    print(var.explanatory.1[1])
    print(var.explanatory.2[1])
    var.explanatory.1 <- var.explanatory.1[1]
    var.explanatory.2 <- var.explanatory.2[1]
  }
  
  dataset <- dataset[,c(var.response,var.explanatory.1,var.explanatory.2,var.explanatory.others)]
  
  if(is.null(y.lim)){
    y.lim <- c(min(dataset[,var.response]),
               max(dataset[,var.response]))
  }
  
  if(is.null(y.lab)){
    y.lab <- var.response
  }
  if(is.null(x1.lab)){
    x1.lab <- var.explanatory.1
  }
  if(is.null(x2.lab)){
    x2.lab <- var.explanatory.2
  }
  
  is.factor.bool <- is.factor(dataset[,var.explanatory.1]) | 
    is.factor(dataset[,var.explanatory.2])
  is.chara.bool <- is.character(dataset[,var.explanatory.1]) | 
    is.character(dataset[,var.explanatory.2])
  
  if(is.factor.bool | is.chara.bool){ # in case one of the explanatory variable is categorical
    
    if(!is.factor.bool){ # turn the categorical variable into a factor
      factor.var.which.bool <- c(is.character(dataset[,var.explanatory.1]),
                                 is.character(dataset[,var.explanatory.2]))
      var.explanatory.factor <- c(var.explanatory.1,var.explanatory.2)[factor.var.which.bool]
      
      # levels.here <- c("April","April through midMay","May","midMay through June",
      #                  "June","April through June")
      
      levels.here <- c("April","April through midMay","April through May","May","2nd part of May",
                       "no wildflowers")
      
      dataset[,var.explanatory.factor] <- factor(dataset[,var.explanatory.factor],levels = levels.here)
    }
    
    factor.var.which.bool <- c(is.factor(dataset[,var.explanatory.1]),
                               is.factor(dataset[,var.explanatory.2]))
    
    var.explanatory.factor <- c(var.explanatory.1,var.explanatory.2)[factor.var.which.bool]
    var.explanatory.Notfactor <- c(var.explanatory.1,var.explanatory.2)[!factor.var.which.bool]
    
    cat.here <- levels(dataset[,var.explanatory.factor])
    
    x.val <- sort(unique(dataset[,var.explanatory.Notfactor]))
    
    # eventually reverse the x1 and x2 labels
    if(var.explanatory.factor == var.explanatory.1){
      x1.lab.temp <- x1.lab
      x2.lab.temp <- x2.lab
      x1.lab <- x2.lab
      x2.lab <- x1.lab
    }
    
    y.val.mean <- list()
    y.val.error <- list()
    n <- list()
    for(i in 1:length(cat.here)){
      dataset.cut <- dataset[dataset[,var.explanatory.factor] == cat.here[i],]
      y.val.mean[[i]] <- tapply(X = dataset.cut[,var.response],
                                INDEX = dataset.cut[,var.explanatory.Notfactor],
                                FUN = mean)
      y.val.error[[i]] <- tapply(X = dataset.cut[,var.response],
                                 INDEX = dataset.cut[,var.explanatory.Notfactor],
                                 FUN = sd)
      n[[i]] <- tapply(X = dataset.cut[,var.response],
                       INDEX = dataset.cut[,var.explanatory.Notfactor],
                       FUN = length)
      if(!is.na(error) & !is.null(error)){
        if(error == "SE"){
          y.val.error[[i]] <- y.val.error[[i]] / sqrt(n[[i]])
        }
      }
    }
    names(y.val.mean) <- names(y.val.error) <- names(n) <- cat.here
    
    plot(y.val.mean[[1]] ~ x.val, col = "white", ylim = y.lim,
         las = 1, xlab="", ylab="", xaxt=xax.t, yaxt = yax.t)
    
    if(is.null(col.cat)){
      col.cat <- rainbow(n = length(cat.here))
    }
    
    if(!is.null(error) & !is.na(error)){
      for(i in 1:length(cat.here)){
        polygon(x = c(x.val,rev(x.val)),
                y = c(y.val.mean[[i]] + y.val.error[[i]],
                      rev(y.val.mean[[i]] - y.val.error[[i]])),
                col = add.transparency.colour(col.cat[i]),lty = 0)
      }
    }
    
    for(i in 1:length(cat.here)){
      lines(x = x.val, y = y.val.mean[[i]],lwd=2,col=col.cat[i])
    }
    
    if(xax.t == "s"){
      mtext(x1.lab,side = 1,line = 3)
    }
    if(yax.t == "s"){
      mtext(y.lab,side = 2,line = 3)
    }
    
    if(legend.show){
      legend(legend.position, cat.here, col = col.cat, bty="n",lty = 1, lwd = 2)
    }
    
  }else{ # in case both explanatory variables are not categorical
    
    x.val.1 <- sort(unique(dataset[,var.explanatory.1]))
    x.val.2 <- sort(unique(dataset[,var.explanatory.2]))
    
    y.val.mean <- matrix(NA,nrow = length(x.val.1), ncol = length(x.val.2), byrow = T)
    
    for(r in 1:length(x.val.1)){
      for(c in 1:length(x.val.2)){
        y.val.mean[r,c] <- mean(dataset[dataset[,var.explanatory.1] == x.val.1[r] &
                                          dataset[,var.explanatory.2] == x.val.2[c],
                                        var.response])
      }
    }
    
    if(is.null(col.gradiant)){
      col.gradiant <- "lightblue"
      
    }else{
      
      if(length(col.gradiant) == 1){
        
        if(col.gradiant %in% c("viridis","magma","plasma","inferno")){
          
          colPaletFun.fun <- colPaletteFunction.fun()
          color <- rev(colPaletFun.fun[[col.gradiant]](x = 300))
          # y.val.mean.seq <- rev(unique(sort(round(y.val.mean,1))))
          # y.val.col.seq <- colPaletFun.fun[[col.gradiant]](x = length(y.val.mean.seq))
          # col.gradiant <- matrix(NA,nrow = length(x.val.1), ncol = length(x.val.2), byrow = T)
          # for(r in 1:length(x.val.1)){
          #   for(c in 1:length(x.val.2)){
          #     col.gradiant[r,c] <- y.val.col.seq[y.val.mean.seq == round(y.val.mean[r,c],1)]
          #   }
          # }
          plot.gradiant.legend <- F
          
        }else{
          print(paste("The colour gradiant ",col.gradiant," is not in the function colPaletteFunction.fun()",sep=""))
          print("Please choose within viridis, magma, plasma and inferno")
        }
      }else{
        jet.colors <- colorRampPalette(col.gradiant)
        color <- jet.colors(300)
      }
      
      nry <- nrow(y.val.mean)
      ncy <- ncol(y.val.mean)
      # Compute the z-value at the facet centres
      yfacet <- (y.val.mean[-1, -1] + y.val.mean[-1, -ncy] + y.val.mean[-nry, -1] + y.val.mean[-nry, -ncy])/4
      # Recode facet y-values into color indices
      facetcol <- cut(yfacet, 300)
      
      col.gradiant <- color[facetcol]
      
    }

    op <- par(bg = "white")
    graphics::persp(x = x.val.1, y = x.val.2, z = y.val.mean,
                    theta = theta, phi = phi, expand = 0.85, col = col.gradiant,
                    ltheta = 60, shade = shade.gradiant, ticktype = "detailed",
                    xlab = paste("\n\n",x1.lab,sep=""),
                    ylab =paste("\n\n",x2.lab,sep=""),
                    zlab = paste("\n\n",y.lab,sep=""),
                    zlim=y.lim)
    
    if(plot.gradiant.legend){
      ## add color bar
      image.plot(legend.only=T, zlim=range(zfacet), col=color)
    }
  }
}

# Function that returns a list of different colour function
colPaletteFunction.fun <- function(){
  require(viridis)
  colPalFunction <- c("viridis","magma","plasma","inferno")
  listColPalFun <- list()
  for(i in 1:length(colPalFunction)){
    if(colPalFunction[i] == "viridis"){
      listColPalFun[[i]] <- function(x){viridis(x)}
    }else if(colPalFunction[i] == "magma"){
      listColPalFun[[i]] <- function(x){magma(x)}
    }else if(colPalFunction[i] == "plasma"){
      listColPalFun[[i]] <- function(x){plasma(x)}
    }else if(colPalFunction[i] == "inferno"){
      listColPalFun[[i]] <- function(x){inferno(x)}
    }
  }
  names(listColPalFun) <- colPalFunction
  return(listColPalFun)
}

# Function that cleans the datasets, places them into a long format data frame and removes observations
# with attractiveness = 0.
# The function returns a list with the values for attractiveness (0 included), theta_a,
# the names of the variables (e.g., "CY", "TotHc", etc.) and a list "datavar.m.L"
# that contains all the long format datasets for each variable.
# file.data: the object returned by for e.g. read.delim(paste(wd_data_raw,"data90.txt",sep = "/"),header = F,sep = ",")
# file.data <- fsHere.l$fsHere.30
# file.data <- read.delim(paste(wd_data_raw,"/data60_old.txt",sep = ""),header = F,sep = ",")
clean.data.fun <- function(file.data){
  
  output <- list()
  count.l <- 1
  
  # 
  row.initial.attractiveness <- 2
  row.initial.theta_a <- 1
  row.initial.var <- 3
  if(file.data[1,1] == "ta_set=" & file.data[3,1] == "thq_set="){ # another possible organization of the data
    row.initial.attractiveness <- 4
    row.initial.theta_a <- 2
    row.initial.var <- 5
  }
  
  # extract each variable and clean them
  theta_a <- file.data[row.initial.theta_a,]
  theta_a <- gsub(".*\\[","",theta_a)
  theta_a <- gsub("]","",theta_a)
  theta_a <- gsub(",","",theta_a)
  theta_a <- as.numeric(theta_a)
  theta_a <- theta_a[!is.na(theta_a)]
  
  attractiveness <- file.data[row.initial.attractiveness,]
  attractiveness <- gsub(".*\\[","",attractiveness)
  attractiveness <- gsub("]","",attractiveness)
  attractiveness <- gsub(",","",attractiveness)
  attractiveness <- attractiveness[attractiveness != ""]
  attractiveness <- as.numeric(attractiveness)
  attractiveness <- attractiveness[!is.na(attractiveness)]

  output[[count.l]] <- attractiveness
  count.l <- count.l + 1
  output[[count.l]] <- theta_a
  count.l <- count.l + 1
  
  # NOTE: there is potentially duplicates values (it is the case actually). This will be
  # corrected for in matrixFromDataset.fun().
  
  # file.data contains the data for a certain number of variables: yield (CY), crop and total harvesters, etc.
  # e.g., CY, CropH, TotH, Bees)
  vars <- file.data[row.initial.var:nrow(file.data),1]
  rowToKeep <- grepl("=",vars)
  vars <- vars[rowToKeep]
  vars <- gsub("\\=.*","",vars) # keep everything before "="
  vars <- gsub("\\_.*","",vars) # keep everything before "_"
  vars <- unique(vars)
  
  Tw.short <- file.data[row.initial.var:nrow(file.data),1]
  Tw.short <- Tw.short[rowToKeep]
  Tw.short <- gsub("\\=.*","",Tw.short)
  Tw.short <- gsub(".*\\_","",Tw.short) # keep everything after "_"
  Tw.short <- unique(Tw.short)

  # in case there are typos
  vars.expected <- c("CY","TotHc","TotHw","TotS","TotUc","TotUw","TotWn","TotRnc",
                     "TotRnw","TotRc","TotRw","TotB","TotBees")
  vars.typo <- vars[!vars %in% vars.expected]
  
  # in case TotBees is in the dataset
  # vars.typo <- vars.typo[vars.typo != "TotBees"]
  
  vars <- vars[vars %in% vars.expected]
  # note:  Rc and Rw are mg
  output[[count.l]] <- vars
  count.l <- count.l + 1
  
  # Note that we need to consider the 6th or even higher column because some values were shifted
  # nc.max <- 40
  colToKeep <- apply(file.data[row.initial.var:nrow(file.data),],2,function(x){sum(x != "" & x != "," & !is.na(x))}) > 0
  file.data.cut <- file.data[row.initial.var:nrow(file.data),colToKeep]
  # need to shift back to the left the numerical values --> those preceded in a row by an cell not containing numerical values
  file.data.cut.corrected <- file.data.cut
  for(r in 1:nrow(file.data.cut)){
    presence.nb <- grepl("[0-9]",file.data.cut[r,])
    position.toFill <- c()
    for(i in 2:length(presence.nb)){
      if(presence.nb[i-1] == F & presence.nb[i] == T){
        position.toFill <- c(position.toFill,i-1)
        # print(r)
      }
    }
    if(length(position.toFill) > 0){   # slide all the values at the right of the cell that did not have numerical values, starting from the higher column number
      for(i in length(position.toFill):1){
        # in some instances, the cell contain the information that need to be kept, e.g., "CropH_M=[["
        keepContentCell <- grepl("=",file.data.cut.corrected[r,position.toFill[i]]) 
        if(keepContentCell){
          newVal <- paste(file.data.cut.corrected[r,c(position.toFill[i],position.toFill[i]+1)],collapse="")
          file.data.cut.corrected[r,position.toFill[i]:ncol(file.data.cut.corrected)] <-
            cbind(newVal,file.data.cut.corrected[r,(position.toFill[i]+2):ncol(file.data.cut.corrected)],"")
        }else{
          file.data.cut.corrected[r,position.toFill[i]:ncol(file.data.cut.corrected)] <- 
            cbind(file.data.cut.corrected[r,(position.toFill[i]+1):ncol(file.data.cut.corrected)],"")
        }
      }
    }
  }
  
  # correct eventual change in the organization of the data
  # i.e., the name of the variable (e.g., "CY_Ap=") is above the data instead of being included,
  # like: "CY_Ap=285.881717, 285.7727272, etc." --> as the following code was built for
  if(length(grep("=",file.data.cut.corrected[1,1])) == 1 & is.na(file.data.cut.corrected[1,2])){
   
    row.with.vars <- grep("=",file.data.cut.corrected[,1])
    
    for(i in 1:length(row.with.vars)){
      var.here <- file.data.cut.corrected[row.with.vars[i],1]
      val.first.here <- file.data.cut.corrected[row.with.vars[i]+1,1]
      file.data.cut.corrected[row.with.vars[i]+1,1] <- paste(var.here,val.first.here,sep="")
    }
    
    file.data.cut.corrected <- file.data.cut.corrected[-row.with.vars,]
    file.data.cut.corrected <- file.data.cut.corrected[file.data.cut.corrected[,1] != "]",]
  }
  
  # correct the eventual typos
  if(length(vars.typo) > 0){
    for(i in 1:length(vars.typo)){
      
      # find the rows concerned
      rows.concerned <- which(grepl(vars.typo[i],file.data.cut.corrected[,1]))
      # check that these rows are not actually correct 
      # (e.g., if typo is "Tot" instead of "TotHc", all row with "Tot" are being returned)
      rows.ok <- c()
      for(j in 1:length(vars)){
        rows.ok.here <- which(grepl(vars[j],file.data.cut.corrected[,1]))
        rows.ok <- c(rows.ok,rows.ok.here)
      }
      rows.concerned <- rows.concerned[! rows.concerned %in% rows.ok]
      
      # find the 1st row for each vars
      row.first <- tapply(X = 1:length(vars),INDEX = vars,
                          function(x){min(which(grepl(vars[x],file.data.cut.corrected[,1])))})
      row.first <- sort(row.first)
      
      # find the range variable that has the typo
      for(j in 1:length(rows.concerned)){
        rows.concerned.here <- rows.concerned[j]
        var.concerned <- tail(names(row.first)[row.first <= rows.concerned.here],1)
        
        charToCorrect <- file.data.cut.corrected[rows.concerned.here,1]
        charCorrected <- gsub(vars.typo[i],var.concerned,charToCorrect)
        file.data.cut.corrected[rows.concerned.here,1] <- charCorrected
        
        print(paste("Typo: ",charToCorrect," corrected by: ",charCorrected,sep=""))
      }
    }
  }
  
  # check for another type of typo:
  # if a variable name was changed for another variable name.
  # e.g., "TotHw_M" is replaced by "TotHc_M="
  typoTofix <- F
  vars.initial.row <- list()
  # in case "TotBees" and "TotB" 
  TotBees.present <- sum(vars == "TotBees") > 0
  TotB.present <- sum(vars == "TotB") > 0
  for(i in 1:length(vars)){
    vars.initial.row[[i]] <- which(grepl(vars[i],file.data.cut.corrected[,1]))
    # n case "TotBees" and "TotB" are present; grepl will select both if vars[i] = "TotB"
    if(vars[i] == c("TotB") & TotBees.present & TotBees.present){
      TotBees.rows <-  which(grepl("TotBees",file.data.cut.corrected[,1]))
      vars.initial.row[[i]] <- 
        vars.initial.row[[i]][! vars.initial.row[[i]] %in% TotBees.rows]
    }
    if(length(vars.initial.row[[i]]) != 6){
      typoTofix <- T
    }
  }
  names(vars.initial.row) <- vars
  
  if(typoTofix){
    
    # create a data frame with the expected initial row number for each variable
    # at each Tw
    row.nb.init.DF <- data.frame(var =NA,
                                 Tw = NA,
                                 row.init = NA,
                                 nameInDS = NA,
                                 stringsAsFactors = F)
    row.nb.init.DF <- row.nb.init.DF[-1,]
    
    
    row.here <- 1
    for(i in 1:length(vars)){
    # for(i in 1:3){
      
      row.nb.init.DF.here <- data.frame(var = rep(NA,6),
                                        Tw =  rep(NA,6),
                                        row.init = rep(NA,6),
                                        nameInDS = rep(NA,6),
                                        stringsAsFactors = F)
      row.nb.init.DF.here$var <- vars[i]
      row.nb.init.DF.here$Tw <- Tw.short
      row.nb.init.DF.here$row.init[1] <- row.here
      
      # file.data.cut.corrected[row.here,]
      # file.data.cut.corrected[vars.initial.row[[vars[i]]],]
      
      # nb of rows between each initial row
      init.rows <- vars.initial.row[[vars[i]]]
      nb.rows.btw.init.rows <- sapply(X = init.rows[1:(length(init.rows)-1)],
                                      FUN = function(x){
                                        position <- which(x == init.rows,init.rows)
                                        init.rows[position + 1] - init.rows[position]})
      # find the number of rows between the last initial row for this variable and the 1st initial row for the next variable
      row.nb.larger <- c()
      for(j in 1:length(vars)){
        init.rows.here <- vars.initial.row[[vars[j]]]
        row.nb.first.nextVar.here <- init.rows.here[init.rows.here > tail(init.rows,1)]
        if(length(row.nb.first.nextVar.here) > 0){
          row.nb.larger <- c(row.nb.larger,row.nb.first.nextVar.here)
        }
      }
      if(!is.null(row.nb.larger[1])){ # in case this is not the last variable
        first.row.nextVar <- min(row.nb.larger)
        # file.data.cut.corrected[vars.initial.row[[vars[i]]],]
        # file.data.cut.corrected[first.row.nextVar,]
        nb.rows.btw.init.rows <- c(nb.rows.btw.init.rows, first.row.nextVar - tail(init.rows,1))
        
      }else{ # in case this is the last variable
        nb.rows.btw.init.rows <- c(nb.rows.btw.init.rows, nrow(file.data.cut.corrected) - tail(init.rows,1) + 1)
      }

      # file.data.cut.corrected[row.here + sum(nb.rows.btw.init.rows[1:5]),]
      
      # find the correct number of rows between each var (based on majority --> assumes typos are rare)
      six.val.allTheSame <- length(nb.rows.btw.init.rows) == 6 & length(unique(nb.rows.btw.init.rows)) == 1
      six.val.NotallTheSame <- length(nb.rows.btw.init.rows) == 6 & length(unique(nb.rows.btw.init.rows)) > 1
      more.than.sixVal <- length(nb.rows.btw.init.rows) > 6
      less.than.sixVal <- length(nb.rows.btw.init.rows) < 6
      
      if(six.val.allTheSame | more.than.sixVal | less.than.sixVal){ # if there are extra rows --> remove the ones that are in minority
        
        nb.rows.btw.init.rows.t <- table(nb.rows.btw.init.rows)
        correct.nb.btw.rows <- 
          as.numeric(names(nb.rows.btw.init.rows.t)[nb.rows.btw.init.rows.t == 
                                                      max(nb.rows.btw.init.rows.t)])
        
        # 
        row.nb.init.DF.here$row.init[2:6] <- sapply(1:5,
                                                    FUN = function(x){x*correct.nb.btw.rows+row.here})
        
      }else if(six.val.NotallTheSame){
        for(j in 2:6){
          row.nb.init.DF.here$row.init[j] <- row.nb.init.DF.here$row.init[j-1] + nb.rows.btw.init.rows[j - 1]
        }
        correct.nb.btw.rows <- nb.rows.btw.init.rows
      }

      # file.data.cut.corrected[row.nb.init.DF.here$row.init,]
      
      # find the name corresponding to the row number in original dataset
      namesHere <- file.data.cut.corrected[row.nb.init.DF.here$row.init,1]
      namesHere <- gsub("\\=.*","",namesHere) # keep only characters before "="
      row.nb.init.DF.here$nameInDS <- namesHere
      
      #
      if(nrow(row.nb.init.DF) == 0){
        row.nb.init.DF <- row.nb.init.DF.here
      }else{
        row.nb.init.DF <- rbind(row.nb.init.DF,row.nb.init.DF.here)
      }
      
      # 
      row.here <- row.nb.init.DF.here$row.init[6] + tail(correct.nb.btw.rows,1)

      # file.data.cut.corrected[row.nb.init.DF.here$row.init,]
      # View(file.data.cut.corrected[row.nb.init.DF.here$row.init[6]:nrow(file.data.cut.corrected),])
    }
    
    #
    row.nb.init.DF$nameCorrect <- apply(X = row.nb.init.DF[,1:2],MARGIN = 1,FUN = paste, collapse = "_") 
    row.nb.init.DF$issue <- apply(X = row.nb.init.DF[,c("nameInDS","nameCorrect")],
                                  MARGIN = 1, FUN = function(x){x[1] != x[2]})
    # correct term in file.data.cut.corrected
    row.nb.init.DF.cut <- row.nb.init.DF[row.nb.init.DF$issue,,drop = F]
    for(j in 1:nrow(row.nb.init.DF.cut)){
      row.here <- row.nb.init.DF.cut$row.init[j]
      charToCorrect <- file.data.cut.corrected[row.here,1]
      charCorrected <- gsub(row.nb.init.DF.cut$nameInDS,row.nb.init.DF.cut$nameCorrect,charToCorrect)
      file.data.cut.corrected[row.here,1] <- charCorrected
    }
  }
  
  # place the data for each variable in a separate dataset, inside a list
  datavar.m <- list()
  for(i in 1:length(vars)){
    
    row.first <- min(which(grepl(vars[i],file.data.cut.corrected[,1])))
    
    if(i == length(vars)){
      row.last <- nrow(file.data.cut.corrected) 
    }else{
      row.last <- min(which(grepl(vars[i+1],file.data.cut.corrected[,1]))) - 1 
    }
    dS.here <- file.data.cut.corrected[row.first:row.last,]
    colToKeep <- apply(dS.here,2,function(x){sum(x != "" & x != "," & !is.na(x))}) > 0
    datavar.m[[i]] <- dS.here[,colToKeep]
  }
  names(datavar.m) <- paste(vars,".m",sep="")
  
  # intervals for the y and x axes on the figures
  int.attractiveness <- attractiveness[2] - attractiveness[1] # for the figure
  int.theta_a <- theta_a[2] - theta_a[1]
  
  #
  # place each data set in a long format data frame, inside a list: ------
  #
  # Tw.short
  Tw <- c("April","April through midMay","May","midMay through June","June",
          "April through June")
  
  datavar.m.L <- list()
  for(i in 1:length(vars)){
    
    Tw.names <- datavar.m[[i]][gsub("_.*","",datavar.m[[i]][,1]) == vars[i],1]
    Tw.names <- gsub("=.*","",Tw.names)
    
    # check that Tw and Tw.names are in the same order
    Tw.names.cut <- gsub(paste(vars[i],"_",sep=""),"",Tw.names)
    Tw.names.cut <- factor(Tw.names.cut,levels = c("Ap","ApmM","M","mMJ","J","ApJ"))
    Tw.names <- Tw.names[order(Tw.names.cut)]
    
    names(Tw) <- Tw.names
    nameResponseVar <- "Individuals_nb"
    if(vars[i] == "CY"){
      nameResponseVar <- "yield_kg" 
    }else if(vars[i] %in% c("TotRnc","TotRnw","TotRc","TotRw")){
      nameResponseVar <- "resources_mg"
    }
    datavar.m.L[[i]] <- responseVarDS.to.LongFormat.fun(data.matrix = datavar.m[[i]],
                                                        time.intervals = Tw,
                                                        var.exp.one = theta_a, 
                                                        var.exp.two = attractiveness,
                                                        nameResponseVar = nameResponseVar)
  }
  names(datavar.m.L) <- paste(vars,".m.L",sep="")
  
  #
  # remove 0 from attractiveness -----
  datavar.m.L.c <- datavar.m.L
  for(i in 1:length(datavar.m.L)){
    ds.here <- datavar.m.L[[i]]
    datavar.m.L.c[[i]] <- ds.here[ds.here$attractiveness > 0,]
  }
  datavar.m.L <- datavar.m.L.c
  
  output[[count.l]] <- datavar.m.L
  names(output) <- c("attractiveness","theta_a","vars","datavar.m.L")
  return(output)
}

# Function that replace update the following values for Tw:
# "April through June" --> "April through May"
# "June" --> "no wildflowers"
# "midMay through June" --> "2nd part of May"
# data.all.l <- clean.data.fun(fsHere.60)
update.Tw.fun <- function(data.all.l){
  
  for(i in 1:length(data.all.l$datavar.m.L)){
    
    rows.to.change <- data.all.l$datavar.m.L[[i]]$time.intervals == "April through June" 
    data.all.l$datavar.m.L[[i]]$time.intervals[rows.to.change] <- "April through May"
    
    rows.to.change <- data.all.l$datavar.m.L[[i]]$time.intervals == "June" 
    data.all.l$datavar.m.L[[i]]$time.intervals[rows.to.change] <- "no wildflowers"
    
    rows.to.change <- data.all.l$datavar.m.L[[i]]$time.intervals == "midMay through June" 
    data.all.l$datavar.m.L[[i]]$time.intervals[rows.to.change] <- "2nd part of May"
  }
  return(data.all.l)
}

# The preference function
theta_p <- function(theta_a,theta_h,theta_q){
  
  theta_hq <- (theta_h + theta_q)/2
  
  H <- function(x){ # the Heaviside function
    output <- rep(1,length(x))
    output[x <= 0] <- 0
    return(output)
  }
  
  T.f <- function(x){
    output <- atan(x - 1)
    return(output)
  }
  
  output <- (1 - theta_a) - 2/pi * T.f(theta_hq) *
    ((1 - theta_a) * H(T.f(theta_hq)) + theta_a * H(-T.f(theta_hq)))
  
  return(output)
}

# Quantification of crop total yield (NOT USED)
# eta <- 75      # average load size per bee in mg/bee
# m_f <- 0.43    # average resources harvested per flower in mg
# n_f <- 1       # average # of flower visits for fruit set in fruit-1
# A <- 1.77*10^4 # size of the area disc in m2
# K_HU <- 3      # baseline rate of switch from H to U in hr-1
# K_c <- 12      # half saturation of Rc in g/m2
# theta_a <- 0.1 # relative proportion of the field occupied by wildflowers
# w_f <- 2.0     # the weight of a blueberry in g
# data.ts <- time.series.l$`Ap_ta=0.11_thq=5.4` #
crop.yield.tot.fun <- function(data.ts,eta = 0.43*10^-3,m_f = 0.43*10^-3,A = 1.77*10^4,
                               K_HU = 3,K_HUf = 12,theta_a = 0.1, w_f = 2, n_f = 1){
  
  t <- data.ts$days[data.ts$days >= 30 & data.ts$days <= 60]
  Rc <- data.ts$Rc[data.ts$days %in% t]   # g/m2
  Hc <- data.ts$Hc[data.ts$days %in% t]
  
  # Eq. 2.14 & 2.15
  PS_May <- eta/((1-theta_a)*A)*K_HU*Rc/(Rc + K_HUf)*Hc/m_f
  PS_May <- max(cumsum(PS_May)*diff(t)[1])
  
  TCY = (1 - theta_a)*A*w_f*PS_May/n_f # in g
  
  return(TCY)
}

# Quantification of cumulative crop yield over time
# data.ts <- out.1
# convert.to.day <- F
# modif.tot.amount <- T
# position.legend <- "topleft"
# plot.PS <- T
# x.prop.half.y.max <- 1/5
# print.fig <- F
crop.yield.fun <- function(data.ts,convert.to.day=F,parameters,modif.tot.amount=T,
                           plot.PS=F,print.fig = F,nameFig=NA,ymax.cumFlowerVisisted = NA,
                           position.legend = "right", x.prop.half.y.max = 1/5,
                           only.cum.flowerVisits = F,Xaxt = "s",las=0){
  
  eta <- parameters$eta
  m_f <- parameters$m_f # NOT USED
  A <- parameters$A
  K_HU <- parameters$K_HU
  K_HUf <- parameters$K_HUf
  theta_a <- parameters$theta_a
  w_f <- parameters$w_f
  n_f <- parameters$n_f
  Tw <- parameters$Tw
  rho_c <- parameters$rho_c
  
  if(Tw == "ApJ.c.w"){
    Tc <- c(0,90)*12
  }else if(Tw == "calibration"){
    # Tc <- c(0,23)*12
    Tc <- c(0,23,53,90)*12
  }else{
    Tc <- c(30,60)*12
  }
  
  if(sum(class(data.ts) == "data.frame") == 0){
    data.ts <- as.data.frame(data.ts)
  }
  
  time.var <- c("time","days")[c("time","days") %in% colnames(data.ts)]
  
  Rc <- data.ts$Rc   # g/m2
  Hc <- data.ts$Hc
  
  time.diff <- diff(data.ts[,time.var])
  if(length(unique(round(time.diff,2))) > 1){
    print("In crop.yield.fun():")
    print("Time intervals are not regular in data.ts$days. The 1st interval was taken.")
    print(unique(round(time.diff,2)))
    print("The 1st interval was taken.")
  }
  time.diff <- time.diff[1]
  
  if(convert.to.day){
    Tc <- Tc/12
  }
  
  Rc.available <- Rc
  # Rc.available[data.ts[,time.var] < Tc[1]] <- 0
  # Rc.available[data.ts[,time.var] > Tc[2]] <- 0
  
  # number of crop flowers visited per h
  flowerCrop.denisty <- 1000 # /m2
  if(modif.tot.amount){
    # in # flowers/hr ; /1000 because there are 1000 flowers / m2
    PS.t <- eta*K_HU*Rc.available/(Rc.available + K_HUf)*Hc / (rho_c / flowerCrop.denisty)
    PS.t[PS.t < 0 ] <- 0
    # probability (P) of flowers to be selected at least once
    # library(multicool)
    # n <- PS.t # the number of samples
    # k <- flowerCrop.denisty * A * (1 - theta_a) # the total nb of flowers
    # P <- factorial(k) * Stirling2(n, k)/(k^n)
    # 
    # P.t <- 1 / (flowerCrop.denisty * A * (1 - theta_a)) * PS.t
    
  }else{ # previous method
    # PS.t <- eta/((1-theta_a)*A)*K_HU*Rc.available/(Rc.available + K_HUf)*Hc/m_f # in # flowers/m2/hr
  }
  # PS <- eta/((1-theta_a)*A)*K_HU*Rc/(Rc + K_HUf)*Hc/m_f # in # flowers/m2/hr
  
  # integrate
  if(convert.to.day){
    PS <- cumsum(PS.t)*time.diff*12 # in # flowers/m2/time.interval
  }else{
    PS <- cumsum(PS.t)*time.diff # in # flowers/m2/time.interval
  }
  
  # divide by the average number of flower visits required for successful pollination 
  PS <- PS / n_f
  
  # number of new flowers visited
  flower.crop.density <- 1000 # per m2
  fruit.set <- 0.9
  flowers.nb.max <- flower.crop.density * A * (1 - theta_a)
  PS.notCorrected <- PS # for the figures
  PS[PS > flowers.nb.max] <- flowers.nb.max
  
  # total crop yield in g
  if(modif.tot.amount){
    # CY <- w_f*PS/n_f * fruit.set # in g
    CY <- w_f*PS * fruit.set # in g
    
    CY.notCorrected <- w_f*PS.notCorrected/n_f
  }else{
    # CY <- (1 - theta_a)*A*w_f*PS/n_f # in g
  }
  
  if(plot.PS){
    
    if(print.fig){
      jpeg(filename = paste(wd_figures,"/CumulatedVar_",nameFig,sep=""),
           width = 18,height = 13,units = "cm",res = 600)
    }
    
    x.h <- data.ts$time
    x.h.entire <- x.h[x.h %in% 0:(90*12)]
    x.d <- x.h / 12 # conversion to days
    x.d.entire <- x.d[x.d %in% 0:90]
    
    A.here <- 1 # A if we want the results in /m2
    
    lwd <- 3
    
    xaxt.here <- "n"
    
    if(only.cum.flowerVisits){
      # layout(matrix(1, nrow = 1), heights = c(1))
    }else{
      layout(matrix(1:2, nrow = 2), heights = c(1,1.2))
    }
    # plot number of flower visited and cumulative number of flower visited
    # par(mar=c(0.5,4.5,1,4.5))
    if(only.cum.flowerVisits & Xaxt == "s"){
      par(mar=c(4.5,5.5,1,1))
      xaxt.here <- Xaxt
    }else{
      if(only.cum.flowerVisits){
        par(mar=c(0.2,5.5,1,1))
      }else{
        par(mar=c(1,5.5,1,1))
      }
    }
    # plot number of cumulated # flower visited over time
    max.nb.flowers <- 1000 * A / A.here * (1 - theta_a)
    
    if(is.na(ymax.cumFlowerVisisted)){
      ymax.here <- max.nb.flowers
    }else{
      ymax.here <- ymax.cumFlowerVisisted
    }
    
    plot(0,0,col="white", 
         xlim=c(0,max(c(max(x.d)))),
         ylim=c(0,max(c(max(PS.notCorrected/A.here),ymax.here))),
         xaxt=xaxt.here, yaxt = "s",xlab="",las=las,
         # ylab=bquote("Cumulated nb flowers visited .m"^ -2))
         ylab="")
    # axis(side = 2)
    mtext(text = "Number of flowers",side = 2, line = 4,
          col = "black")
    #
    if(only.cum.flowerVisits & Xaxt == "s"){
      mtext(text = "Time (days)",side = 1, line = 2.8,
            col = "black")
    }
    
    # max nb of flowers per m2
    abline(a = max.nb.flowers , b = 0, lwd = 2, lty = 3, col = "black")
    # corrected PS
    # x.h.bloom <- x.h[PS > 0 | (PS > 0 & !duplicated(PS))]
    # PS.corrected <- saturation.logistic.fun(y.max = max.nb.flowers, x = x.h.bloom,
    #                                         x.prop.half.y.max = x.prop.half.y.max)
    lines(x = x.d, y = PS/A.here, lwd=lwd, col="cyan3",lty=1)
    #
    lines(x = x.d, y = PS.notCorrected/A.here, lwd=lwd, col="blue",lty=2)
    #
    if(!is.na(position.legend)[1]){
      legend(position.legend,
             c("cum nb flower visits","nb flowers present","cum nb flowers visited"),
             bty="n",col = c("blue","black","cyan3"), lwd = c(lwd,2,lwd), 
             lty = c(2,3,1))
    }
    #
    if(!only.cum.flowerVisits){
      # plot number of flower visited over time
      par(new = F)
      par(mar=c(4.5,4.5,1,1))
      plot(0,0,col="white", xlim=c(0,max(x.d)),ylim=c(0,max(PS.t/A.here)),
           xaxt="s", yaxt="s", ylab="",xlab= "Time (days)")
      # axis(side = 1, labels = )
      # mtext(bquote("Nb flowers visited .m"^ -2),side = 4, line = 3)
      mtext(bquote("Nb flowers visits"),side = 2, line = 3,col = "black")
      lines(x = x.d, y = PS.t/A.here, lwd=2, col="red")
    }

    if(print.fig){
      dev.off()
    }
  }
  # print(paste("CY",CY,"theta_a",theta_a,"A",A,"w_f",w_f,"PS",PS))
  return(CY)
}

# 
# y.max <- max.nb.flowers
# x <- x.h.bloom
# x.prop.half.y.max <- 1/2 # t get te x correspnding to 1/2 max.y
saturation.logistic.fun <- function(y.max,x,x.prop.half.y.max){
  
  growthRate <- -log(1/3)/(x.prop.half.y.max * (max(x) - min(x)))
  
  output <- logistic.fun(y.max = 2 * y.max, 
                         x.0 = min(x), x = x, 
                         growth.rate = growthRate) - y.max
  return(output)
}

# logistic function
logistic.fun <- function(y.max,x.0,x,growth.rate){
  output <- y.max / (1 + exp(-growth.rate*(x - x.0)))
  return(output)
}

# Function that plots the time series for the bees, resources and crop yield
# list.datasets <- time.series.l
# list.datasets <- out.2
# print <- F
# total.yield <- T
# y.max.H.Wn <- NA
# y.max.R <- NA
# y.max.Rn <- NA
# y.max.B <- NA
# y.max.U.S <- NA
# y.max.CY <- NA
# var.goal <- NA
# show.var.goal <- F
# legend.box <- T
# Rcw.kg.ha <- F
# lwd <- 3
# plot.lines <- T
figure3.fun <- function(list.datasets,parameters,print = F,nameFile=NA,wd_figures,
                        y.max.H.Wn = NA, y.max.U.S = NA, y.max.B = NA,
                        y.max.R = NA, y.max.Rn = NA, y.max.CY = NA,lwd=3,
                        convert.to.day.CY=F,total.yield=F,Rcw.kg.ha=F,
                        var.goal = NA,show.var.goal=F,legend.box = F,
                        legend.position = "topright",pch.diff = F,
                        plot.lines=T){
  
  Tw <- parameters$Tw
  ta <- parameters$theta_a
  th <- parameters$theta_h
  tq <- parameters$theta_q
  A <- parameters$A
  eta <- parameters$eta
  m_f <- parameters$m_f # NOT USED
  K_HU <- parameters$K_HU
  K_c <- parameters$K_c
  w_f <- parameters$w_f
  n_f <- parameters$n_f
  
  show.var.goal.2 <-  show.var.goal & Tw %in% c("calibration","weeds.only",
                                                "noResources","resourcesDiscontWF",
                                                "resourcesDiscontCF")
  if(show.var.goal.2){
    if(is.na(var.goal)[1]){
      var.goal <- var.goal.fun()
    }
    var.goal.here <- var.goal[[Tw]]
  }
  
  if(print){
    if(is.na(nameFile)){
      nameFile <- paste("Dynamics_system_Tw=",Tw,"_theta_a=",ta,"_theta_h=",th,
                        "_theta_q=",tq,".jpeg",sep="")
    }
    path.nameFile <- paste(wd_figures,nameFile,sep="/")
    jpeg(filename = path.nameFile,
         width = 22,height = 15, units = "cm",res = 600)
  }
  
  side.2.large <- 4.8
  line.2 <- 2.5
  
  if("list" %in% class(list.datasets)){
    name.file.here <- paste(Tw,"_ta=",ta,"_thq=",thq,sep = "")
    ds.here <- list.datasets[[name.file.here]]
  }else if("data.frame" %in% class(list.datasets)){
    ds.here <- list.datasets
  }else if("matrix" %in% class(list.datasets)){
    ds.here <- as.data.frame(list.datasets)
  }
  
  colnames(ds.here)[colnames(ds.here) == "time"] <- "days"
  
  layout(matrix(1:6,nrow=2,byrow = T),heights = c(1,1.09))
  x.max <- max(ds.here$days) + 10
  
  # plot Hc, Hw, Wn ------
  # variables <- c("Hc","Hw","Uc","Uw")
  variables <- c("Hc","Hw","Wn")
  if(is.na(y.max.H.Wn)){
    y.max <- ceiling(max(ds.here[,variables]))
    
    if(show.var.goal.2 & Tw == "calibration"){ # the objectives are potentially larger than the simulated values
      val.here <- as.matrix(var.goal.here[,c("Hcw","Wn")])
      val.here <- c(val.here)
      val.here <- max(val.here[!is.na(val.here)])
      if(val.here > y.max){
        y.max <- val.here
      }
    }
    
  }else{
    y.max <- y.max.H.Wn
  }
  # col.lines <- rep(c("blueviolet","darkgoldenrod3"),2)
  col.lines <- c("blueviolet","darkgoldenrod3","grey50")
  # lty.lines <- c(1,1,2,2)
  lty.lines <- c(1,1,1)
  #
  par(mar=c(1,side.2.large,0.5,0.5))
  plot(ds.here$Hc,x = ds.here$days, las = 0,col= "white", 
       ylim = c(0,y.max), xlim = c(0,x.max),xlab = "",ylab = "", xaxt = "n")
  # mtext("Time (days)",side = 1, line = 2.5)
  mtext("Number of individuals",side = 2, line = line.2)
  # crop
  if(Tw == "ApJ.c.w"){
    x.cf <- c(0,90,90,0)
  }else if(Tw %in% c("calibration","resourcesDiscontWF")){ # 1st blooming period
    x.cf <- c(0,23,23,0)
  }else if(Tw %in% c("weeds.only","noResources","resourcesDiscontWF")){
    x.cf <- c(0,0,0,0)
  }else{
    x.cf <- c(30,60,60,30)
  }

  # WF
  if(Tw == "Ap"){
    x.wf <- c(0,30,30,0)
  }else if(Tw == "ApmM"){
    x.wf <- c(0,45,45,0)
  }else if(Tw == "M"){
    x.wf <- c(30,60,60,30)
  }else if(Tw == "mMJ"){
    x.wf <- c(45,90,90,45)
  }else if(Tw == "J"){
    x.wf <- c(60,90,90,60)
  }else if(Tw %in% c("ApJ","ApJ.c.w")){
    x.wf <- c(0,90,90,0)
  }else if(Tw %in% c("2ndmM")){
    x.wf <- c(45,60,60,45)
  }else if(Tw %in% c("ApM")){
    x.wf <- c(0,60,60,0)
  }else if(Tw %in% c("control")){
    x.wf <- c(0,0,0,0)
  }else if(Tw %in% c("calibration")){
    x.wf <- c(0,23,23,0)
  }else if(Tw %in% c("weeds.only","noResources")){
    x.wf <- c(0,0,0,0)
  }
  
  if(Tw %in% c("weeds.only","noResources")){
    y.max.poly <- 0
  }else{
    y.max.poly <- y.max
  }
  
  polygon(x = x.cf, y = c(0,0,y.max.poly,y.max.poly),border = F,
          col = add.transparency.colour("blueviolet",alpha = 0.20))
  polygon(x = x.wf, y = c(0,0,y.max.poly,y.max.poly),density = 5,lwd=1.5,col = "black")
  
  if(Tw %in% c("calibration","resourcesDiscontWF")){ # second blooming period
    polygon(x = c(53,90,90,53), y = c(0,0,y.max.poly,y.max.poly),border = F,
            col = add.transparency.colour("blueviolet",alpha = 0.20))
    polygon(x = c(53,90,90,53), y = c(0,0,y.max.poly,y.max.poly),density = 5,lwd=1.5,col = "black")
  }
  #
  if(plot.lines){
    lines(x = ds.here$days, y = ds.here[,variables[1]], lwd = lwd, col = col.lines[1],
          lty = lty.lines[1])
    lines(x = ds.here$days, y = ds.here[,variables[2]], lwd = lwd, col = col.lines[2],
          lty = lty.lines[2])
    lines(x = ds.here$days, y = ds.here[,variables[3]], lwd = lwd, col = col.lines[3],
          lty = lty.lines[3])
  }
  # lines(x = ds.here$days, y = ds.here[,variables[4]], lwd = 2, col = col.lines[4],
  #       lty = lty.lines[4])
  #
  if(legend.box){
    bty.legend <- "o"
  }else{
    bty.legend <- "n"
  }
  
  # if(Tw %in% c("J","mMJ")){
  #   position.legend <- "topleft"
  # }
  if(Tw %in% c("ApJ","ApJ.c.w","calibration") & legend.box){
    bty.legend <- "o"
  }
  legend(legend.position,variables,bty=bty.legend,lwd = lwd,col=col.lines,lty=lty.lines)
  
  # show objective calibration
  if(show.var.goal.2 & Tw == "calibration"){
    if(pch.diff){
      pch.here <- c(1,4,3,2)[1:length(var.goal.here[1,c("Hcw","Wn")])]
    }else{
      pch.here <- 1
    }
    points(x = rep(var.goal.here$day[1],2), 
           y = var.goal.here[1,c("Hcw","Wn")],
           col=col.lines[c(1,3)],pch = pch.here,cex=3,lwd=3)
  }
  
  # plot Uc, Uw and S ------
  variables <- c("Uc","Uw","S")
  if(is.na(y.max.U.S)){
    y.max <- ceiling(max(ds.here[,variables]))
    
    if(show.var.goal.2 & Tw == "calibration"){ # the objectives are potentially larger than the simulated values
      val.here <- as.matrix(var.goal.here[,c("Ucw","S")])
      val.here <- c(val.here)
      val.here <- max(val.here[!is.na(val.here)])
      if(val.here > y.max){
        y.max <- val.here
      }
    }
    
  }else{
    y.max <- y.max.U.S
  }
  # col.lines <- c("grey50","firebrick")
  col.lines <- c("blueviolet","darkgoldenrod3","firebrick")
  lty.lines <- c(2,2,1)
  
  par(mar=c(1,side.2.large,0.5,0.5))
  plot(ds.here$Hc,x = ds.here$days, las = 0,col= "white", 
       ylim = c(0,y.max), xlim = c(0,x.max),xlab = "",ylab = "", xaxt = "n")
  # mtext("Time (days)",side = 1, line = 2.5)
  mtext("Number of individuals",side = 2, line = line.2)
  # 
  if(Tw %in% c("weeds.only","noResources")){
    y.max.poly <- 0
  }else{
    y.max.poly <- y.max
  }
  
  polygon(x = x.cf, y = c(0,0,y.max.poly,y.max.poly),border = F,
          col = add.transparency.colour("blueviolet",alpha = 0.20))
  polygon(x = x.wf, y = c(0,0,y.max.poly,y.max.poly),density = 5,lwd=1.5,col = "black")

  if(Tw == "calibration"){
    polygon(x = c(53,90,90,53), y = c(0,0,y.max.poly,y.max.poly),border = F,
            col = add.transparency.colour("blueviolet",alpha = 0.20))
    polygon(x = c(53,90,90,53), y = c(0,0,y.max.poly,y.max.poly),density = 5,lwd=1.5,col = "black")
    
  }
  #
  if(plot.lines){
    lines(x = ds.here$days, y = ds.here[,variables[1]], lwd = lwd, col = col.lines[1],
          lty = lty.lines[1])
    lines(x = ds.here$days, y = ds.here[,variables[2]], lwd = lwd, col = col.lines[2],
          lty = lty.lines[2])
    lines(x = ds.here$days, y = ds.here[,variables[3]], lwd = lwd, col = col.lines[3],
          lty = lty.lines[3]) 
  }
  legend(legend.position,variables,bty=bty.legend,lwd = lwd,col=col.lines,lty=lty.lines)
  
  # 
  if(show.var.goal.2 & Tw == "calibration"){
    if(pch.diff){
      pch.here <- c(1,4,3,2)[1:length(var.goal.here[1,c("Ucw","S")])]
    }else{
      pch.here <- 1
    }
    points(x = jitter(rep(var.goal.here$day[1],2)), 
           y = var.goal.here[1,c("Ucw","S")],
           pch=pch.here,col=col.lines[c(1,3)],cex=3,lwd=3)
  }
  
  # plot B and total nb of adult bees -----
  adult.bees <- c("Hc","Hw","Wn","S","Uc","Uw")
  ds.here$adult.bees <- rowSums(ds.here[,adult.bees])
  
  variables <- c("B","adult.bees")
  if(is.na(y.max.B)){
    y.max <- ceiling(max(ds.here[,variables]))
    
    if(show.var.goal.2 ){ # the objectives are potentially larger than the simulated values
      if(Tw == "calibration"){
        W.tot <- var.goal.here[,c("Hcw","Wn","Ucw","S")] # to estimate W.tot, even if it was not used as a objective itself
        W.tot <- rowSums(W.tot)
        W.tot <- W.tot[!is.na(W.tot)]
        
        val.here <- as.matrix(var.goal.here[,c("B")])
        val.here <- c(val.here)
        val.here <- max(val.here[!is.na(val.here)])
        val.here <- max(c(W.tot,val.here))
      }else if(Tw %in% c("weeds.only","noResources")){
        val.here <- as.matrix(var.goal.here[,c("B","W.tot")])
        val.here <- c(val.here)
        val.here <- max(val.here[!is.na(val.here)])
      }
      if(val.here > y.max){
        y.max <- val.here
      }
    }
    
  }else{
    y.max <- y.max.B
  }
  col.lines <- c("bisque4","black")
  lty.lines <- c(1,1)
  par(mar=c(1,side.2.large,0.5,0.5))
  plot(ds.here[,variables[1]], x = ds.here$days, las = 0,col= "white", 
       xlim = c(0,x.max), ylim=c(0,y.max), xlab = "",ylab = "",xaxt="n")
  # mtext("Time (days)",side = 1, line = 2.5)
  mtext("Number of individuals",side = 2, line = line.2)
  # 
  if(Tw %in% c("weeds.only","noResources")){
    y.max.poly <- 0
  }else{
    y.max.poly <- y.max
  }
  
  polygon(x = x.cf, y = c(0,0,y.max.poly,y.max.poly),border = F,
          col = add.transparency.colour("blueviolet",alpha = 0.20))
  polygon(x = x.wf, y = c(0,0,y.max.poly,y.max.poly),density = 5,lwd=1.5,col = "black")
  if(Tw == "calibration"){
    polygon(x = c(53,90,90,53), y = c(0,0,y.max.poly,y.max.poly),border = F,
            col = add.transparency.colour("blueviolet",alpha = 0.20))
    polygon(x = c(53,90,90,53), y = c(0,0,y.max.poly,y.max.poly),density = 5,lwd=1.5,col = "black")
  }
  #
  if(plot.lines){
    lines(x = ds.here$days, y = ds.here[,variables[1]], lwd = lwd, col = col.lines[1],
          lty = lty.lines[1])
    lines(x = ds.here$days, y = ds.here[,variables[2]], lwd = lwd, col = col.lines[2],
          lty = lty.lines[2])
  }
  legend(legend.position,c("B","Tot bees"),bty=bty.legend,lwd = lwd,col=col.lines,lty=lty.lines)
  
  if(show.var.goal.2){
    if(Tw == "calibration"){
      
      if(pch.diff){
        pch.here <- c(1,4,3,2)[1:length(var.goal.here[1,c("Ucw","S")])]
      }else{
        pch.here <- 1
      }
      
      # plot W.tot
      pch.here <- 1
      var.goal.here.2 <- var.goal.here
      var.goal.here.2$W.tot.2 <- var.goal.here.2$W.tot
      if(is.na(var.goal.here$W.tot[1])){
        var.goal.here.2$W.tot.2[1] <- sum(var.goal.here.2[1,c("Hcw","Ucw","Wn","S")])
      }else{
        var.goal.here.2$W.tot.2[1] <- var.goal.here$W.tot[1]
      }
      var.goal.here.2 <- var.goal.here.2[!is.na(var.goal.here.2$W.tot.2),]
      points(x = var.goal.here.2$day,
             y = var.goal.here.2$W.tot.2,
             pch=pch.here, col=col.lines[2], cex=3, lwd=3)
      
      # plot B
      if(pch.diff){
        pch.here <- 4
      }
      points(x = var.goal.here$day, 
             y = var.goal.here$B,
             pch=pch.here,col=col.lines[1],cex=3,lwd=3)
    }else if(Tw %in% c("weeds.only","noResources")){
      # plot W.tot
      # points(x = jitter(var.goal.here$day), 
      #        y = var.goal.here$W.tot,
      #        pch=1,col=col.lines[2],cex=3,lwd=3)
      # plot B
      # points(x = var.goal.here$day, 
      #        y = var.goal.here$B,
      #        pch=1,col=col.lines[1],cex=3,lwd=3)
      # plot for both, because objectives are the same
      points(x = var.goal.here$day, 
             y = var.goal.here$B,
             pch=4,col="red",cex=3,lwd=3)
    }
  }
  
  # plot Rc and Rw -------
  variables <- c("Rc","Rw")
  y.label <- bquote("Amount of landscape resources (g.m"^-2*")")
  if(!Rcw.kg.ha){ # convert from g/m to kg
    # ds.here[,variables] <- ds.here[,variables] * A * (1 - ta) / 1000
    ds.here$Rc <- ds.here$Rc * A * (1 - ta) / 1000
    ds.here$Rw <- ds.here$Rw * A * ta / 1000
    
    y.label <- "Amount of landscape resources (kg)"
    
    if(ceiling(max(ds.here[,variables])) <= 1){
      ds.here[,variables] <- ds.here[,variables] * 1000
      y.label <- "Amount of landscape resources (g)"
    }
  }
  
  if(is.na(y.max.R)){
    y.max <- ceiling(max(ds.here[,variables]))
  }else{
    y.max <- y.max.R
  }
  col.lines <- c("blueviolet","darkgoldenrod3")
  lty.lines <- c(1,1)
  par(mar=c(4,side.2.large,0.5,0.5))
  plot(ds.here$Hc,x = ds.here$days, las = 0,col= "white", 
       ylim = c(0,y.max), xlim = c(0,x.max),xlab = "",ylab = "", xaxt = "s")
  mtext("Time (days)",side = 1, line = 2.5)
  mtext(y.label,side = 2, line = line.2)
  #
  if(Tw %in% c("weeds.only","noResources")){
    y.max.poly <- 0
  }else{
    y.max.poly <- y.max
  }
  
  polygon(x = x.cf, y = c(0,0,y.max.poly,y.max.poly),border = F,
          col = add.transparency.colour("blueviolet",alpha = 0.20))
  polygon(x = x.wf, y = c(0,0,y.max.poly,y.max.poly),density = 5,lwd=1.5,col = "black")
  if(Tw == "calibration"){
    polygon(x = c(53,90,90,53), y = c(0,0,y.max.poly,y.max.poly),border = F,
            col = add.transparency.colour("blueviolet",alpha = 0.20))
    polygon(x = c(53,90,90,53), y = c(0,0,y.max.poly,y.max.poly),density = 5,lwd=1.5,col = "black")
  }
  #
  if(plot.lines){
    lines(x = ds.here$days, y = ds.here[,variables[1]], lwd = lwd, col = col.lines[1],
          lty = lty.lines[1])
    lines(x = ds.here$days, y = ds.here[,variables[2]], lwd = lwd, col = col.lines[2],
          lty = lty.lines[2]) 
  }
  legend(legend.position,variables,bty=bty.legend,lwd = lwd,col=col.lines,lty=lty.lines)
  
  # plot Rnc and Rnw --------
  variables <- c("Rnc","Rnw")
  if(is.na(y.max.Rn)){
    y.max <- ceiling(max(ds.here[,variables]))
  }else{
    y.max <- y.max.Rn
  }
  col.lines <- c("blueviolet","darkgoldenrod3")
  lty.lines <- c(1,1)
  par(mar=c(4,side.2.large,0.5,0.5))
  plot(ds.here$Hc,x = ds.here$days, las = 0,col= "white", 
       ylim = c(0,y.max), xlim = c(0,x.max),xlab = "",ylab = "", xaxt = "s")
  mtext("Time (days)",side = 1, line = 2.5)
  mtext("Amount of nest resources (g)",side = 2, line = 2.3)
  #
  if(Tw %in% c("weeds.only","noResources")){
    y.max.poly <- 0
  }else{
    y.max.poly <- y.max
  }
  
  polygon(x = x.cf, y = c(0,0,y.max.poly,y.max.poly),border = F,
          col = add.transparency.colour("blueviolet",alpha = 0.20))
  polygon(x = x.wf, y = c(0,0,y.max.poly,y.max.poly),density = 5,lwd=1.5,col = "black")

  if(Tw == "calibration"){
    polygon(x = c(53,90,90,53), y = c(0,0,y.max.poly,y.max.poly),border = F,
            col = add.transparency.colour("blueviolet",alpha = 0.20))
    polygon(x = c(53,90,90,53), y = c(0,0,y.max.poly,y.max.poly),density = 5,lwd=1.5,col = "black")
  }
  #
  if(plot.lines){
    lines(x = ds.here$days, y = ds.here[,variables[1]], lwd = lwd, col = col.lines[1],
          lty = lty.lines[1])
    lines(x = ds.here$days, y = ds.here[,variables[2]], lwd = lwd, col = col.lines[2],
          lty = lty.lines[2])
  }
  legend(legend.position,variables,bty=bty.legend,lwd = lwd,col=col.lines,lty=lty.lines)
  
  # crop yield --------
  variables <- c("CY")
  var.present <- variables %in% colnames(ds.here)
  var.present.null <- F
  if(var.present){
    if(sum(ds.here[,variables[1]]) == 0){ # if there are only 0s --> CY was not outputed
      var.present.null <- T
    }else{
      # CY is in g
      CY <- ds.here[,variables[1]]
    }
  }
  if(!var.present | var.present.null){
    print("Calculate CY")
    # CY is in g
    CY <- crop.yield.fun(data.ts = ds.here, parameters = parameters, 
                         convert.to.day = convert.to.day.CY, 
                         modif.tot.amount = T) # in g for the total area
  }
  
  # g --> kg
  CY <- CY / 1000       # g --> kg for the total area
  
  # kg --> kg/ha
  if(!total.yield){
    # CY <- CY * 10000 / (A * (1 - ta))  # kg for total area --> kg/ha
    CY <- CY * 10000 / A  # kg for total area --> kg/ha
  }

  if(is.na(y.max.CY)){
    y.max <- y.max <- max(CY)
    y.max[y.max < 0.001] <- 0
  }else{
    y.max <- y.max.CY
  }
  col.lines <- c("blue3")
  lty.lines <- c(1,1)
  par(mar=c(4,side.2.large,0.5,0.5))
  plot(ds.here$Hc,x = ds.here$days, las = 0,col= "white", 
       ylim = c(0,y.max), xlim = c(0,x.max),xlab = "",ylab = "", xaxt = "s")
  mtext("Time (days)",side = 1, line = 2.5)
  if(total.yield){
    mtext("Crop yield (kg)",side = 2, line = line.2)
  }else{
    mtext(bquote("Crop yield (kg.ha"^-1*")"),side = 2, line = line.2)
  }
  #
  if(Tw  %in% c("weeds.only","noResources")){
    y.max.poly <- 0
  }else{
    y.max.poly <- y.max
  }
  
  polygon(x = x.cf, y = c(0,0,y.max.poly,y.max.poly),border = F,
          col = add.transparency.colour("blueviolet",alpha = 0.20))
  polygon(x = x.wf, y = c(0,0,y.max.poly,y.max.poly),density = 5,lwd=1.5,col = "black")
  
  if(Tw == "calibration"){
    polygon(x = c(53,90,90,53), y = c(0,0,y.max.poly,y.max.poly),border = F,
            col = add.transparency.colour("blueviolet",alpha = 0.20))
    polygon(x = c(53,90,90,53), y = c(0,0,y.max.poly,y.max.poly),density = 5,lwd=1.5,col = "black")
  }
  #
  CY[CY < 0.001] <- 0
  if(plot.lines){
    lines(x = ds.here$days, y = CY, lwd = lwd, col = col.lines[1],
          lty = lty.lines[1])
  }

  if(print){
    dev.off()
  }
}


# The preference function
# S.noCF.prop : the proportion of bees scouting in the crop field when not in bloom
theta_p.fun <- function(theta_a,theta_h,theta_q,Ti.c=NA,Ti.w=NA,t=NA,
                        S.noCF.prop = 0.1, parameters = NA){
  
  if(!is.na(parameters[1])){
    theta_a <- parameters$theta_a
    theta_h <- parameters$theta_h
    theta_q <- parameters$theta_q
  }else{
    theta_a <- as.numeric(theta_a)
    theta_h <- as.numeric(theta_h)
    theta_q <- as.numeric(theta_q)
  }
  theta_hq <- (theta_h + theta_q)/2
 
  H <- function(x){ # the Heaviside function
    output <- rep(1,length(x))
    output[x <= 0] <- 0
    return(output)
  }
  
  T.f <- function(x){
    output <- atan(x - 1)
    return(output)
  }
  
  output <- (1 - theta_a) - 2/pi * T.f(theta_hq) *
            ((1 - theta_a) * H(T.f(theta_hq)) + theta_a * H(-T.f(theta_hq)))
  
  # if theta_p is not constant and depends on the present of crop and wildflowers 
  if(!is.na(Ti.c)[1] & !is.na(Ti.w)[1] & !is.na(t)[1]){ 
    
    output <- rep(output,length(t))
    
    wf.bloom <- t >= Ti.w[1] & t <= Ti.w[2]
    cf.bloom <- t >= Ti.c[1] & t <= Ti.c[2]
    
    output[ wf.bloom & !cf.bloom ] <- S.noCF.prop  
    output[ !wf.bloom & cf.bloom ] <- 1 - S.noCF.prop 
  }
  
  return(output)
}

# Function that return the parameters of the ODE model it their original unites
parameters.fun <- function(){
  
  para.orig<-list(theta_a = 0.1, 
                  theta_q = 2,
                  theta_h = 2,
                  Tw = "Ap",             # "Ap", "ApmM", "M", "mMJ", "J", "ApJ", "2ndmM", "ApM", "control"
                  eta = 75,              # load size per bee in mg/bee
                  m_f = 0.43,            # amount of resources harvested / flower in mg # NOT USED
                  n_f = 1,               # nb flower visits for successful fruit set
                  w_f = 2,               # weight of a blue berry in g
                  A = 1.77*10^4,         # size of the circular field in m2
                  K_UWn = 25,            # switch rate from U to Wn in h-1
                  K_WnS = 1.7,           # switch rate from Wn to S in h-1
                  K_BWn = 1/21/12, # 4.0*10^-3, # 2.69*10^-3,    # switch rate from B to Wn in h-1
                  K_HU = 3,              # switch rate from H to U in h-1
                  K_SH = 20,             # switch rate from S to H in h-1
                  rho_B = 0.79, # 0.38, # 0.32,  # birth rate of brood in brood.h-1
                  rho_c = 330, # 264,    # crop resource production rate in mg.m-2.h-1      
                  rho_w = 40, # 8,       # WF resource production rate in mg.m-2.h-1        
                  delta_H = 3*10^-3,     # death rate of H in h-1
                  delta_S = 3*10^-3,     # death rate of S in h-1
                  delta_U = 3*10^-3,     # death rate of U in h-1
                  delta_Wn = 1.5*10^-3,  # death rate of Wn in h-1
                  delta_B = 0.004, # 0.08, #0.004, # 1.6*10^-2,   # death rate of B in h-1
                  delta_Rn = 0.01, # 0.131*10^-3,      # augmented death rate of broods in h-1           TO CALIBRATE
                  delta_c = 0.02,     # decay rate of crop in h-1.flower-1
                  delta_w = 0.02,     # decay rate of WF in h-1.flower-1
                  sigma_Wnc = 8.3,       # consumption rate of Rnc by Wn in mg.bee-1.h-1    
                  sigma_Wnw = 8.3,       # consumption rate of Rnw by Wn in mg.bee-1.h-1     
                  sigma_Bc = 1.3, # 0.9,        # consumption rate of Rnc by B in mg.brood-1.h-1    
                  sigma_Bw = 1.3, # 0.9,        # consumption rate of Rnc by Wn in mg.brood-1.h-1   
                  # K_nc = 3.5*10^3, # 0.5,     # half saturation of Rnc in g                      TO CALIBRATE
                  # K_nw = 0.5*10^3, # 3.5,     # half saturation of Rnw in g                      TO CALIBRATE
                  K_WnSnf = 10,
                  K_WnBnf = 10,
                  # K_c = 12*10^3, # 2          # half saturation of Rc in g.m-2                   TO CALIBRATE
                  # K_w = 2*10^3, # 0.5         # half saturation of Rw in g.m-2                   TO CALIBRATE
                  K_HUf = 10,
                  K_WnSf = 10,
                  K_SHf = 10,             # was K_f before;  TO CALIBRATE
                  K_n = 10,              # ??? TO CALIBRATE
                  # K_B = 10, # 160,             # half saturation of B in brood                    TO CALIBRATE
                  K_Bnf = 1,            # half saturation that reduces the production rate of broods as a function of Rn TO CALIBRATE
                  epsilon = 0.01 ) # 10^-6)       # density of resources in weeds in g.m-2
  return(para.orig)
}

# state variable and initial values
state <- c(B = 50,
           Hc = 0,
           Hw = 0,
           Rc = 0.0,    # in g.m-2
           Rw = 0,    # in g.m-2
           Rnc = 0, # in g
           Rnw = 5, # 0.5*1, # in g
           S = 0,
           Uc = 0,
           Uw = 0,
           Wn = 5)

# The different treatments for Tw
col.n <- c("Ap","ApmM","ApM","M","2ndmM","control","calibration","weeds.only",
           "ApJ.c.w","mMJ","noResources","resourcesDiscontWF","resourcesDiscontCF")
treatments.Tw <- data.frame(t(col.n),stringsAsFactors = F)
colnames(treatments.Tw) <- col.n
rm(col.n)

# Function that takes a list of the parameter values and returns a data frame with
# +/- range.percent values for each parameter
# range.percent <- 10
# para.l <- parameters.GSA
para.range.fun <- function(para.l,range.percent = 10,return.data.frame = T){
  output <- 
    lapply(X = 1:length(para.l),function(x){
      val.here <- para.l[[x]]
      out.l <- val.here - val.here * range.percent / 100
      out.u <- val.here + val.here * range.percent / 100
      return(c(out.l,out.u))
    })
  if(return.data.frame){
    output <- do.call(what = cbind,args = output)
    output <- as.data.frame(output)
    colnames(output) <- names(para.l)
    row.names(output) <- c("lower","upper")
  }else{
    names(output) <- names(para.l)
  }
  return(output)
}

# Function that converts the parameters to g instead of mg and day-1 instead of hour-1
# Note that day have 12 hours
# Note that the conversion from h-1 to d-1 does not yield the same dynamics. 
para.conversion.fun <- function(parameters,mg.to.g=T,h.to.d=T){
  
  if(h.to.d){
    para.rates.h <- c("delta_Rn","K_UWn","K_WnS","K_BWn","K_HU","K_SH","rho_B",
                      "rho_c","rho_w","delta_H","delta_S","delta_U","delta_Wn",
                      "delta_B","delta_c","delta_w","sigma_Wnc","sigma_Wnw",
                      "sigma_Bc","sigma_Bw")
    for(i in 1:length(para.rates.h)){
      parameters[[para.rates.h[i]]] <- parameters[[para.rates.h[i]]] * 12
    }
  }
  
  if(mg.to.g){
    para.mass.mg <- c("eta","m_f","rho_c","rho_w","sigma_Wnc","sigma_Wnw","sigma_Bc",
                      "sigma_Bw")
    for(i in 1:length(para.mass.mg)){
      parameters[[para.mass.mg[i]]] <- parameters[[para.mass.mg[i]]] * 10^-3
    }
  }
  return(parameters)
}


# Function that represents the ODE model
# t <- seq(0,70*12, by = 0.01) # time in h
# state # a vector of the initial values for each variable
# parameters <- para.conversion.fun(parameters = para.orig,mg.to.g = T, h.to.d = F) # list of the parameters and their value
ODE.m <- function(t,state,parameters){
  
  # theta_p <- theta_p.fun(theta_a = parameters[["theta_a"]],
  #                        theta_h = parameters[["theta_h"]],
  #                        theta_q = parameters[["theta_q"]])
  
  # Function that returns the time interval of the wildflower blooming season (Ti)
  Ti.fun <- function(parameters,in.h = T,Ti.w.correction = 0){
    
    # Tw = "Ap", "ApmM", "M", "mMJ", "J", "ApJ", "2ndmM", "ApM", "control"
    Tw.here <- parameters[["Tw"]]
    
    nb.h.in.d <- 1
    if(in.h){
      nb.h.in.d <- 12
    }
    
    if(Tw.here %in% c("Ap")){
      Ti.w <- c(0,30-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("ApmM")){
      Ti.w <- c(0,45-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("M")){
      Ti.w <- c(30,60-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("mMJ")){
      Ti.w <- c(45,90-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("J")){
      Ti.w <- c(60,90-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("ApJ","ApJ.c.w")){
      Ti.w <- c(0,90-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("2ndmM")){
      Ti.w <- c(45,60-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("ApM")){
      Ti.w <- c(0,60-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("control")){
      Ti.w <- c(0,0)*nb.h.in.d
    }else if(Tw.here %in% c("calibration","resourcesDiscontWF")){
      # Ti.w <- c(0,23-Ti.w.correction)*nb.h.in.d
      Ti.w <- c(0,23-Ti.w.correction,53,90-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("weeds.only","noResources","resourcesDiscontCF")){
      Ti.w <- c(0,0)*nb.h.in.d
    }
    
    if(Tw.here %in% c("ApJ.c.w")){
      Ti.c <- c(0,90)*nb.h.in.d
    }else if(Tw.here %in% c("calibration","resourcesDiscontCF")){
      # Ti.c <- c(0,23)*nb.h.in.d
      Ti.c <- c(0,23,53,90)*nb.h.in.d
    }else if(Tw.here %in% c("weeds.only","noResources","resourcesDiscontWF")){
      Ti.c <- c(0,0)*nb.h.in.d
    }else{
      Ti.c <- c(30,60)*nb.h.in.d
    }
    
    Ti <- list(Ti.c,Ti.w)
    names(Ti) <- c("Ti.c","Ti.w")
    
    return(Ti)
  }
  
  # 
  Xi.fun <- function(t,Ti,percent.max = NA,blooming.time = T){
    # output <- rep(0,length(t))
    # output[t >= Ti[1] & t <= Ti[2]] <- 1
    # print(t)
    output <- 0
    
    if(blooming.time){
      
      if(t >= Ti[1] & t <= Ti[2]){
        if(!is.na(percent.max)){
          mid.range.val <- floor((Ti[2] + Ti[1])/2)
          mid.range.val.l <- mid.range.val - percent.max*(Ti[2]-Ti[1])/2/100
          mid.range.val.u <- mid.range.val + percent.max*(Ti[2]-Ti[1])/2/100

          if(t < mid.range.val.l){
            output <- 1 - (mid.range.val.l - t)/(mid.range.val.l - Ti[1])
          }else if(t >= mid.range.val.u){
            output <- (Ti[2] - t)/(Ti[2] - mid.range.val.u)
          }else{
            output <- 1
          }
        }else{
          output <- 1
        }
      }else if(length(Ti) > 2){ # for the "calibration" scenario
        
        if(t >= Ti[3] & t <= Ti[4]){
          
          if(!is.na(percent.max)){
            mid.range.val <- floor((Ti[4] + Ti[3])/2)
            mid.range.val.l <- mid.range.val - percent.max*(Ti[4]-Ti[3])/2/100
            mid.range.val.u <- mid.range.val + percent.max*(Ti[4]-Ti[3])/2/100
            
            if(t < mid.range.val.l){
              output <- 1 - (mid.range.val.l - t)/(mid.range.val.l - Ti[3])
            }else if(t >= mid.range.val.u){
              output <- (Ti[4] - t)/(Ti[4] - mid.range.val.u)
            }else{
              output <- 1
            }
          }else{
            output <- 1
          }
        }
      }
      
    }else{ # for the crop to stop producing any resources at the end of Ti
      
      if(t >= Ti[2] & length(Ti) == 2){
        output <- 1000
      }else if(length(Ti) > 2){
        if(t >= Ti[2] & t <= Ti[3]){
          output <- 1000
        }
      }
    }

    # print(output)
    # print(c(Ti,round(t,2),round(output,2)))
    return(output)
  }
  
  # Ti <- Ti.fun(parameters,in.h = T)
  # Ti.c <- Ti["Ti.c"]
  # Ti.w <- Ti["Ti.w"]
  
  Ti.c <- Ti.fun(parameters,in.h = T)[["Ti.c"]]
  Ti.w <- Ti.fun(parameters,in.h = T,Ti.w.correction = 4)[["Ti.w"]]
  
  # rho_c.hat <- parameters[["rho_c"]]*10^-3*Xc.fun(t) # 10^-3 to convert rho_c in g
  # rho_w.hat <- parameters[["rho_w"]]*10^-3*(parameters[["epsilon"]] + Xw.fun(t,Ti)) # same as above
  
  # rho_c.hat <- parameters[["rho_c"]]*Xc.fun(t) # 
  # rho_w.hat <- parameters[["rho_w"]]*(parameters[["epsilon"]] + Xw.fun(t,Ti)) # same as above
  
  # convert the sigma_ in g.bee-1.h-1
  # sigma_Wnc.g <- parameters[["sigma_Wnc"]]
  # sigma_Wnw.g <- parameters[["sigma_Wnw"]]
  # sigma_Bc.g <- parameters[["sigma_Bc"]]
  # sigma_Bw.g <- parameters[["sigma_Bw"]]
  
  with(as.list(c(state,parameters)),{

    # rho_c.hat <- Xc(t)*rho_c
    # rho_w.hat <- (Xw(t) + epsilon)*rho_w
    percent.max <- NA
    Xc.t <- Xi.fun(t,Ti.c,percent.max = percent.max)
    Xw.t <- Xi.fun(t,Ti.w,percent.max = percent.max)
    
    Xc.t.postBloom <- Xi.fun(t,Ti.c,blooming.time = F) # so that flower do not contain any resource outside of Tw
    Xw.t.postBloom <- Xi.fun(t,Ti.w,blooming.time = F) # NOT USED

    theta_p <- theta_p.fun(theta_a = theta_a, theta_h = theta_h, theta_q = theta_q,
                           Ti.c = Ti.c,Ti.w = Ti.w,t = t,S.noCF.prop = 0)
    # theta_p <- theta_p.fun(theta_a = theta_a, theta_h = theta_h, theta_q = theta_q)
    
    # Eq. 2.8
    dB <- rho_B*(Rnc+Rnw)/(Rnc+Rnw+K_Bnf) - K_BWn*B - (delta_B + delta_Rn/(1+(Rnc+theta_q*Rnw)/K_n))*B
    # previous version with the reduction the brood production rate as a function of Rn
    # dB <- rho_B - K_BWn*B - (delta_B + delta_Rn/(1+(Rnc+theta_q*Rnw)/K_n))*B
    # Bruno's modification:
    # dB <- rho_B*0.95 - K_BWn*B - theta_Rn/(1+(Rnc+theta_q*Rnw)/K_n)*B

    # previous version with  K_nc + K_nw vs. K_n :
    # dB <- rho_B - K_BWn*B - (delta_B + theta_Rn/(1+(Rnc+theta_q*Rnw)/(K_nc + K_nf)))*B
    
    # eq. 2.4
    dHc <- K_SH*theta_p*Rc / (theta_p*Rc+(1-theta_p)*Rw + K_SHf) * S -
           K_HU*Rc/(Rc+K_HUf)*Hc - delta_H*Hc
    # previous version with  K_c & K_w vs. K_HUf
    # dHc <- K_SH*theta_p*Rc / (theta_p*Rc+(1-theta_p)*Rw + K_f) * S -
    #        K_HU*Rc/(Rc+K_c)*Hc - delta_H*Hc
    # previous version with  K_c + K_w vs. K_f :
    # dHc <- K_SH*theta_p*Rc / (theta_p*Rc+(1-theta_p)*Rw + K_c + K_w) * S -
    #        K_HU*Rc/(Rc+K_c)*Hc - delta_H*Hc
    
    dHw <- K_SH*(1-theta_p)*Rw / (theta_p*Rc+(1-theta_p)*Rw + K_SHf) * S -
           theta_h*K_HU*Rw/(Rw+K_HUf)*Hw - delta_H*Hw
    # previous version with  K_c & K_w vs. K_HUf
    # dHw <- K_SH*(1-theta_p)*Rw / (theta_p*Rc+(1-theta_p)*Rw + K_f) * S -
    #        theta_h*K_HU*Rw/(Rw+K_w)*Hw - delta_H*Hw
    # previous version with  K_c + K_w vs. K_f :
    # dHw <- K_SH*(1-theta_p)*Rw / (theta_p*Rc+(1-theta_p)*Rw + K_c + K_w) * S -
    #        theta_h*K_HU*Rw/(Rw+K_w)*Hw - delta_H*Hw
    
    # Eq. 2.10
    dRc <- Xc.t*rho_c - eta/((1-theta_a)*A)*K_HU*Rc/(Rc+K_HUf)*Hc - (delta_c + Xc.t.postBloom)*Rc
    
    # dRc <- Xc.t*rho_c - eta/((1-theta_a)*A)*K_HU*Rc/(Rc+K_HUf)*Hc - delta_c*Rc
    # previous version with K_c and K_w vs. KHUf
    # dRc <- Xc.t*rho_c - eta/((1-theta_a)*A)*K_HU*Rc/(Rc+K_c)*Hc - delta_c*Rc
    # option to include (1-theta_a):
    # dRc <- Xc.t*rho_c *(1-theta_a) - eta/((1-theta_a)*A)*K_HU*Rc/(Rc+K_c)*Hc - delta_c*Rc

    # dRw <- (Xw.t + epsilon)*rho_w - eta/(theta_a*A)*K_HU*theta_h*Rw/(Rw+K_HUf)*Hw - (delta_w + Xw.t.postBloom)*Rw
    
    dRw <- (Xw.t + epsilon)*rho_w - eta/(theta_a*A)*K_HU*theta_h*Rw/(Rw+K_HUf)*Hw - delta_w*Rw
    # previous version with K_c and K_w vs. KHUf
    # dRw <- (Xw.t + epsilon)*rho_w - eta/(theta_a*A)*K_HU*theta_h*Rw/(Rw+K_w)*Hw - delta_w*Rw
    # option to include theta_a:
    # dRw <- (Xw.t + epsilon)*rho_w*theta_a - eta/(theta_a*A)*K_HU*theta_h*Rw/(Rw+K_w)*Hw - delta_w*Rw
    
    # Eq. 2.9
    dRnc <- eta*K_UWn*Uc - sigma_Wnc*Rnc/(Rnc+K_WnBnf)*Wn - sigma_Bc*Rnc/(Rnc+K_WnBnf)*B
    # previous version with K_nc and K_nw vs. K_WnBnf
    # dRnc <- eta*K_UWn*Uc - sigma_Wnc*Rnc/(Rnc+K_nc)*Wn - sigma_Bc*Rnc/(Rnc+K_nc)*B
    
    dRnw <- eta*K_UWn*Uw - sigma_Wnw*Rnw/(Rnw+K_WnBnf)*Wn - sigma_Bw*Rnw/(Rnw+K_WnBnf)*B
    # previous version with K_nc and K_nw vs. K_WnBnf
    # dRnw <- eta*K_UWn*Uw - sigma_Wnw*Rnw/(Rnw+K_nw)*Wn - sigma_Bw*Rnw/(Rnw+K_nw)*B
    
    # Eq. 2.5
    dS <- K_WnS*(
          theta_p*Rc/((Rc+K_WnSf)*(1+Rnc/K_WnSnf)) + (1-theta_p)*Rw/((Rw+K_WnSf)*(1+Rnw/K_WnSnf))
          )*Wn -
          K_SH*(theta_p*Rc+(1-theta_p)*Rw)/(theta_p*Rc+(1-theta_p)*Rw + K_SHf)*S -
          delta_S*S
    # previous version with K_c and K_w vs. K_WnSf AND K_nc and K_nw vs. K_WnSnf
    # dS <- K_WnS*(
    #        theta_p*Rc/((Rc+K_c)*(1+Rnc/K_nc)) + (1-theta_p)*Rw/((Rw+K_w)*(1+Rnw/K_nw))
    #        )*Wn -
    #        K_SH*(theta_p*Rc+(1-theta_p)*Rw)/(theta_p*Rc+(1-theta_p)*Rw + K_f)*S -
    #        delta_S*S
    # previous version with theta vs. thetap - 1 (and vice versa) and K_c + K_w vs. K_f
    # dS <- K_WnS*(
    #       (1-theta_a)*Rc/((Rc+K_c)*(1+Rnc/K_nc)) + theta_a*Rw/((Rw+K_w)*(1+Rnw/K_nw))
    #       )*Wn -
    #       K_SH*(theta_p*Rc+(1-theta_p)*Rw)/(theta_p*Rc+(1-theta_p)*Rw + K_c + K_w)*S -
    #       delta_S*S
    
    # Eq; 2.6
    dUc <- K_HU*Rc/(Rc+K_HUf)*Hc - K_UWn*Uc - delta_U*Uc
    # previous version with  K_c & K_w vs. K_HUf
    # dUc <- K_HU*Rc/(Rc+K_c)*Hc - K_UWn*Uc - delta_U*Uc
    
    dUw <- theta_h*K_HU*Rw/(Rw+K_HUf)*Hw - K_UWn*Uw - delta_U*Uw
    # previous version with  K_c & K_w vs. K_HUf
    # dUw <- theta_h*K_HU*Rw/(Rw+K_w)*Hw - K_UWn*Uw - delta_U*Uw
    
    # Eq. 2.7
    dWn <- K_BWn*B + K_UWn*(Uc+Uw) - (delta_Wn + delta_Rn/(1+(Rnc+theta_q*Rnw)/K_n))*Wn - 
           K_WnS*
      (theta_p*Rc/((Rc+K_WnSf)*(1+Rnc/K_WnSnf)) + 
         (1-theta_p)*Rw/((Rw+K_WnSf)*(1+Rnw/K_WnSnf)))*Wn
    # previous version with with K_c and K_w vs. K_WnSf AND K_nc and K_nw vs. K_WnSnf AND with the extra mortality due to low Rn
    
    # dWn <- K_BWn*B + K_UWn*(Uc+Uw) - delta_Wn*Wn - 
    #        K_WnS*(theta_p*Rc/((Rc+K_c)*(1+Rnc/K_nc)) + (1-theta_p)*Rw/((Rw+K_w)*(1+Rnw/K_nw)))*Wn
    # previous version with theta vs. thetap - 1 (and vice versa) 
    # dWn <- K_BWn*B + K_UWn*(Uc+Uw) - delta_Wn*Wn - 
    #        K_WnS*((1-theta_a)*Rc/((Rc+K_c)*(1+Rnc/K_nc)) + theta_a*Rw/((Rw+K_w)*(1+Rnw/K_nw)))*Wn
    
    # return the rate of change
    list(c(dB,dHc,dHw,dRc,dRw,dRnc,dRnw,dS,dUc,dUw,dWn))
  })
}

# Not sure what this function is for. It is not used anywhere.
ODE.m.Rc <- function(t,state,parameters){
  
  # theta_p <- theta_p.fun(theta_a = parameters[["theta_a"]],
  #                        theta_h = parameters[["theta_h"]],
  #                        theta_q = parameters[["theta_q"]])
  
  # Function that returns the time interval of the wildflower blooming season (Ti)
  Ti.fun <- function(parameters,in.h = T,Ti.w.correction = 0){
    
    # Tw = "Ap", "ApmM", "M", "mMJ", "J", "ApJ", "2ndmM", "ApM", "control"
    Tw.here <- parameters[["Tw"]]
    
    nb.h.in.d <- 1
    if(in.h){
      nb.h.in.d <- 12
    }
    
    if(Tw.here %in% c("Ap")){
      Ti.w <- c(0,30-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("ApmM")){
      Ti.w <- c(0,45-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("M")){
      Ti.w <- c(30,60-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("mMJ")){
      Ti.w <- c(45,90-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("J")){
      Ti.w <- c(60,90-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("ApJ","ApJ.c.w")){
      Ti.w <- c(0,90-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("2ndmM")){
      Ti.w <- c(45,60-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("ApM")){
      Ti.w <- c(0,60-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("control")){
      Ti.w <- c(0,0)*nb.h.in.d
    }else if(Tw.here %in% c("calibration")){
      # Ti.w <- c(0,23-Ti.w.correction)*nb.h.in.d
      Ti.w <- c(0,23-Ti.w.correction,53,90-Ti.w.correction)*nb.h.in.d
    }else if(Tw.here %in% c("weeds.only","noResources")){
      Ti.w <- c(0,0)*nb.h.in.d
    }
    
    if(Tw.here %in% c("ApJ.c.w")){
      Ti.c <- c(0,90)*nb.h.in.d
    }else if(Tw.here %in% c("calibration")){
      # Ti.c <- c(0,23)*nb.h.in.d
      Ti.c <- c(0,23,53,90)*nb.h.in.d
    }else if(Tw.here %in% c("weeds.only","noResources")){
      Ti.c <- c(0,0)*nb.h.in.d
    }else{
      Ti.c <- c(30,60)*nb.h.in.d
    }
    
    Ti <- list(Ti.c,Ti.w)
    names(Ti) <- c("Ti.c","Ti.w")
    
    return(Ti)
  }
  
  Xi.fun <- function(t,Ti,percent.max = NA,blooming.time = T){
    # output <- rep(0,length(t))
    # output[t >= Ti[1] & t <= Ti[2]] <- 1
    # print(t)
    output <- 0
    
    if(blooming.time){
      
      if(t >= Ti[1] & t <= Ti[2]){
        if(!is.na(percent.max)){
          mid.range.val <- floor((Ti[2] + Ti[1])/2)
          mid.range.val.l <- mid.range.val - percent.max*(Ti[2]-Ti[1])/2/100
          mid.range.val.u <- mid.range.val + percent.max*(Ti[2]-Ti[1])/2/100
          
          if(t < mid.range.val.l){
            output <- 1 - (mid.range.val.l - t)/(mid.range.val.l - Ti[1])
          }else if(t >= mid.range.val.u){
            output <- (Ti[2] - t)/(Ti[2] - mid.range.val.u)
          }else{
            output <- 1
          }
        }else{
          output <- 1
        }
      }else if(length(Ti) > 2){ # for the "calibration" scenario
        
        if(t >= Ti[3] & t <= Ti[4]){
          
          if(!is.na(percent.max)){
            mid.range.val <- floor((Ti[4] + Ti[3])/2)
            mid.range.val.l <- mid.range.val - percent.max*(Ti[4]-Ti[3])/2/100
            mid.range.val.u <- mid.range.val + percent.max*(Ti[4]-Ti[3])/2/100
            
            if(t < mid.range.val.l){
              output <- 1 - (mid.range.val.l - t)/(mid.range.val.l - Ti[3])
            }else if(t >= mid.range.val.u){
              output <- (Ti[4] - t)/(Ti[4] - mid.range.val.u)
            }else{
              output <- 1
            }
          }else{
            output <- 1
          }
        }
      }
      
    }else{ # for the crop to stop producing any resources at the end of Ti
      
      if(t >= Ti[2] & length(Ti) == 2){
        output <- 1000
      }else if(length(Ti) > 2){
        if(t >= Ti[2] & t <= Ti[3]){
          output <- 1000
        }
      }
    }
    
    # print(output)
    # print(c(Ti,round(t,2),round(output,2)))
    return(output)
  }
  
  Ti.c <- Ti.fun(parameters,in.h = T)[["Ti.c"]]
  Ti.w <- Ti.fun(parameters,in.h = T,Ti.w.correction = 4)[["Ti.w"]]
  
  with(as.list(c(state,parameters)),{

    percent.max <- NA
    Xc.t <- Xi.fun(t,Ti.c,percent.max = percent.max)
    Xw.t <- Xi.fun(t,Ti.w,percent.max = percent.max)
    
    Xc.t.postBloom <- Xi.fun(t,Ti.c,blooming.time = F) # so that flower do not contain any resource outside of Tw
    Xw.t.postBloom <- Xi.fun(t,Ti.w,blooming.time = F) # NOT USED
    
    theta_p <- theta_p.fun(theta_a = theta_a, theta_h = theta_h, theta_q = theta_q,
                           Ti.c = Ti.c,Ti.w = Ti.w,t = t,S.noCF.prop = 0)

    # dRc <- Xc.t*rho_c - (delta_c + Xc.t.postBloom)*Rc
    
    dRc <- Xc.t*rho_c
    dRw <- rho_c
    
    # dRw <- (Xw.t + epsilon)*rho_w - delta_w*Rw

    list(c(dRc,dRw))
  })
}

# Calculate the euclidean distance between two vectors
euclidian.dist.fun <- function(x=c(),y=c()){
  if(length(x) != length(y)){
    print("vectors have different length")
  }else{
    squaresDiff <- sapply(x - y,function(z){z*z}) 
    return(sqrt(sum(squaresDiff)))
  }
}

# The loss function used to calibrate the model; it calculates the euclidean distance between
# the vectors of aimed and simulated values.
# output.ODE <- out # a data frame of the out put of the ODE, with colnames being the names of the variables
# var.goal <- var.goal[[1]]      # a vector of the values of the variable to consider, with nale being the name of the variables
loss.fun <- function(output.ODE,var.goal = NA){
  
  if(is.na(var.goal)[1]){ # for the switch point only --> not used
    
    var.goal <- data.frame(day = c(23,33,43,53),
                           Wn = c(23,NA,NA,NA),
                           S = c(2,NA,NA,NA),
                           Hcw = c(13,NA,NA,NA),
                           Ucw = c(2,NA,NA,NA),
                           W.tot = c(40,10,10,NA),
                           B = c(114,20,20,48))
    # var.goal <- c(23,13,2,2,114)
    # # 40 workers at the switch point (time = 23 day), from Duchateau and Velthuis 1988 
    # names(var.goal) <- c("Wn","Hcw","S","Ucw","B")
  }
  
  col.here <- c("B","Hc","Hw","Uc","Uw","Wn","S")
  workers <- c("Hc","Hw","Uc","Uw","Wn","S")
  out.cut <- output.ODE[output.ODE[,1] %in% var.goal$day,col.here]
  out.cut$Hcw <- out.cut$Hc + out.cut$Hw
  out.cut$Ucw <- out.cut$Uc + out.cut$Uw
  out.cut$W.tot <- rowSums(out.cut[,workers])
  
  # 
  var.day.l <- list()
  # var.day.l[[1]] <- c("Wn","S","Hcw","Ucw","W.tot","B")
  for(i in 1:nrow(var.goal)){
    var.day.l[[i]] <- colnames(var.goal)[c(F,!is.na(var.goal[i,2:ncol(var.goal)]))]
  }
  names(var.day.l) <- var.goal$day
  
  # var.day.l[[2]] <- c("W.tot","B")
  # var.day.l[[3]] <- c("W.tot","B")
  # var.day.l[[4]] <- c("B")
  # names(var.day.l) <- c(23,33,43,53)
  
  var.goal.c <- NA
  var.got.c <- NA
  for(i in 1:length(var.day.l)){
    new.var.goal <- var.goal[var.goal$day == as.numeric(names(var.day.l)[i]),
                             var.day.l[[i]]]
    if(is.na(var.goal.c)[1]){
      var.goal.c <- new.var.goal
    }else{
      var.goal.c <- cbind(var.goal.c,new.var.goal)
    }
    #
    new.var.got <- out.cut[i,var.day.l[[i]]]
    if(is.na(var.got.c)[1]){
      var.got.c <- new.var.got
    }else{
      var.got.c <- cbind(var.got.c,new.var.got)
    }
  }

  distance <- euclidian.dist.fun(var.got.c,var.goal.c)
  #
  return(distance)
}

# Function that returns that expected values of the different population
# at different times and the ones obtained with the calibrated parameters.
# It also returns the euclidian distance.
# TO UPDATE
var.gaol.comparison.fun <- function(output,var.goal = NA,print.csv=F,wd){
  
  if(is.na(var.goal)){
    var.goal <- as.data.frame(t(c(23,13,2,2,114))) # 40 workers at the switch point (time = 23 day), from Duchateau and Velthuis 1988 
    colnames(var.goal) <- c("Wn","Hcw","S","Ucw","B")
  }

  var.goal$tot.workers <- rowSums(var.goal[,c("Wn","Hcw","S","Ucw")])
  var.goal$tot.workers.33d <- 1
  Wn <- sum(output[output$time == time.goal[1],c("Wn")])
  S <- sum(output[output$time == time.goal[1],c("S")])
  H <- sum(output[output$time == time.goal[1],c("Hc","Hw")])
  U <- sum(output[output$time == time.goal[1],c("Uc","Uw")])
  B <- sum(output[output$time == time.goal[1],c("B")])
  tot.W <- sum(output[output$time == time.goal[1],c("Hc","Hw","Uc","Uw","S","Wn")])
  tot.W.33 <- sum(output[output$time == time.goal[2],c("Hc","Hw","Uc","Uw","S","Wn")])
  row.toAdd <-c(Wn,H,S,U,B,tot.W,tot.W.33)
  var.goal <- rbind(var.goal,row.toAdd)
  var.goal <- round(var.goal,1)
  var.goal$distance <- c(0,
                         round(loss.fun(output.ODE = output,var.goal = var.goal),2))
  if(print.csv){
    write.csv(var.goal,paste(wd_data_raw,"/var.gaol.comparison.dist_",
                             round(loss.fun(output.ODE = output,var.goal = var.goal),2),".csv",sep=""),
              row.names = F)
  }
  return(var.goal)
}

# Function that sample data points in a parameter space whose boundaries are 
# defined by a list of ranges of parameters.
# n <- n.pt.loop # the number of points to select
# range.val.l <- ranges.calib.l # the list of range of value for each parameters
LHS.fun <- function(range.val.l,n){
  ## create hypercube
  lh <- data.frame(randomLHS(n = n, k = length(range.val.l)))
  names(lh) <- names(range.val.l)
  
  ## convert parameters to required scale
  for (i in 1:ncol(lh)){
    par <- names(lh)[i]
    lh[,par] <- qunif(lh[,i], 
                      min = range.val.l[[par]][1], 
                      max = range.val.l[[par]][2]) ## continuous
  }
  return(lh)
}

# Function that conduct the calibration procedure, doing to following steps:
# 1) run the simulation with the values contained in 'parameters'
# 2) generate and run the 'n.pt.init' simulation by sampling points in the parameter
# space defined using the intervals in 'ranges.calib.l' and LHC sampling.
# 3) Select the 'n.pt.selected.loop' best parameter points (using the minimum distance
# returned by the loss.fun()); shuffle the values in the column and run the corresponding
# simulations; then generate 'n.pt.generated.loop' points within centered
# intervals whose length = the interval in 'ranges.calib.l' (centered around the 
# value of the parameter points) x percent.range / 100 / i, with 'i' being the iterated 
# number of the loop. 
# 4) is repeated 'n.loop' times.
# The weight for two scenarios = sqrt(nb obj in scenario with highest # obj / nb obj in scenario with lowest # obj)
# var.goal <- NA      # a vector of the values of the variable to consider, with nale being the name of the variables
# thetas.calibration <- NA # the value
# t <- seq(0,70*12, by = 0.01) # time in h
# state # a vector of the initial values for each variable
# n.pt.init <- 3 # the initial number of parameter points selected
# n.pt.selected.loop <- 2  # the number of parameter points selected afterward for each for eahc loop
# n.pt.generated.loop <- 2  # the number of parameter points generated around the selected points for each loop
# n.loop <- 2      # the number of sampling for loops executed
# ranges.calib.l   # list of the range of the parameters considered for calibration
# write.file <- F
# wd <- wd_data
# name.csv <- "calibration.lossFun.csv"
# parallel <- F
# weight.weeds.vs.calib <- 1 # NOT USED ANYMORE to weight the distance of the "weeds.only" scenario relatively to the distance of the "calibration" scenario
ODE.calibration.fun <- function(t = seq(0,70*12, by = 0.01),state,
                                var.goal=NA,thetas.calibration=NA,
                                ranges.calib.l,n.pt.init = 100, n.pt.selected.loop = 10, 
                                n.pt.generated.loop = 10, n.loop = 4,
                                parameters = NA,percent.range=10,
                                wd=NA,write.file=F,name.csv = NA,parallel = F,
                                numCores =1,n.pt.loop.eachScenario = 0){
  
  if(is.na(thetas.calibration)[1]){
    print("thetas.calibration was NA so the valuesgiven by thetas.calibration.fun() are used")
    thetas.calibration <- thetas.calibration.fun()
  }
  if(is.na(var.goal[1])){
    print("var.goal was NA so the valuesgiven by var.goal.fun() are used")
    var.goal <- var.goal.fun()
  }
  
  # Determine the weight to give to each scenario based on the number of target 
  # they have. The weight for the scenario with the largest number of targets is
  # = 1.
  nbTargets <- unlist(lapply(X = var.goal,FUN = function(X){sum(!is.na(X[colnames(X) !="day"]))}))
  maxTargets <- max(nbTargets)
  weights.scenarios <- sqrt(maxTargets/nbTargets)
  
  name.para.change <- names(ranges.calib.l)
  
  # nb.l.parameters = the number of initial extra parameter points
  nb.l.parameters <- 1
  if(!is.na(parameters)[1]){
    if(length(parameters[[1]]) > 1){ # there are multiple parameter lists in "parameters"
      nb.l.parameters <- length(parameters)
    }
  }
  
  # 
  tot.nb.row <- (nb.l.parameters + n.pt.init + 
    n.loop * (n.pt.selected.loop * n.pt.generated.loop + 
                n.pt.selected.loop + n.pt.loop.eachScenario + 
                (n.pt.selected.loop + n.pt.loop.eachScenario) * (n.pt.selected.loop - 1) / 2)) * 
    length(var.goal)
  
  if(class(var.goal) != "list"){
    print("var.goal must be a list of dtatframe(s) with name(s) being the corresponding Tw")
  }
  
  ## get parameter list
  # para is a list of parameter value that is uses to provide values for the 
  # parameters that do not change value in the procedure.
  if(is.na(parameters)[1]){
    parameters <- parameters.fun()
    para <- para.conversion.fun(parameters,mg.to.g = T,h.to.d = F)
  }else{
    if(nb.l.parameters == 1){
      para <- parameters
    }else{
      para <- parameters[[1]]
    }
  }
  
  #
  para.dist.df <- data.frame(matrix(NA,nrow = 1, ncol = length(para)))
  colnames(para.dist.df) <- names(para)
  para.dist.df <- para.dist.df[-1,]
  
  print(paste("Number of simulations to complete: ",tot.nb.row,sep=" "))
  print("Simulation of paramater points initial selected:")
  print(paste(round(nrow(para.dist.df)/tot.nb.row*100,1),"%",sep=" "))
  
  tot.nb.row <- tot.nb.row / length(var.goal) # there is only one row regardless of the number of scenarios considered (i.e., length(var.goal))
  
  # First run the simulation with parameters provided in "parameters"
  
  if(nb.l.parameters == 1){
    parameters <- list(parameters)
  }
  
  parameters <- do.call(rbind,parameters)
  
  if(parallel){
    s <- mclapply(X = 1:nrow(parameters), FUN = ODE.change.para.fun, DF = parameters,
                  para = para,name.para.change = name.para.change, 
                  var.goal = var.goal, thetas.calibration = thetas.calibration, 
                  t=t, state=state, mc.cores = numCores, 
                  weights.scenarios = weights.scenarios)
  }else{
    s <- lapply(X = 1:nrow(parameters), FUN = ODE.change.para.fun, DF = parameters, 
                para = para, var.goal=var.goal, thetas.calibration = thetas.calibration,
                t = t, state = state,
                name.para.change = name.para.change,
                weights.scenarios = weights.scenarios)
  }
  para.dist.df <- do.call(rbind.data.frame, s)
  
  print(paste("Distance paremeters:",round(para.dist.df$distance,1),sep = " "))
  
  min.dist <- round(min(para.dist.df$distance),2)
  
  print(paste(round(nrow(para.dist.df)/tot.nb.row*100,1),"% ; min.distance =",min.dist,sep=" "))
  
  # Now run the simulations for the lh
  # Generate the set of points by sampling in the parameter space from total ranges
  lh <- LHS.fun(ranges.calib.l,n.pt.init)
  
  if(parallel){
    # numCores <- detectCores()
    s <- mclapply(X = 1:nrow(lh), FUN = ODE.change.para.fun, DF = lh, para = para,
                  name.para.change = name.para.change, 
                  var.goal = var.goal, thetas.calibration = thetas.calibration,
                  t=t, state=state,mc.cores = numCores,
                  weights.scenarios = weights.scenarios)
    # s <- parLapply(X = 1:2, FUN = ODE.change.para.fun,mc.cores = 5)
  }else{
    s <- lapply(X = 1:nrow(lh), FUN = ODE.change.para.fun, DF = lh, para = para,
                var.goal=var.goal, thetas.calibration = thetas.calibration,
                t=t, state=state,
                name.para.change = name.para.change,
                weights.scenarios = weights.scenarios)
  }
  new.rows <- do.call(rbind.data.frame, s)
  para.dist.df <- rbind(para.dist.df,new.rows)
  min.dist <- round(min(para.dist.df$distance),2)
  
  print(paste(round(nrow(para.dist.df)/tot.nb.row*100,1),"% ; min.distance =",min.dist,sep=" "))
  
  # if(write.file){
  #   write.csv(para.dist.df,paste(wd,name.csv,sep="/"),row.names = F)
  # }
  
  # add new parameter points
  for(i in 1:n.loop){
    
    print(paste("Simulation of paramater points round ",i,":",sep=""))
    
    # select the n.pt.loop best points
    para.dist.df.best <- para.dist.df[order(para.dist.df$distance),]
    para.dist.df.best <- para.dist.df.best[1:n.pt.selected.loop,]
    
    # add best points in each scenario separately
    if(n.pt.loop.eachScenario > 0){
      
      newParaPoints <- lapply(X = 1:length(var.goal),FUN = function(x){
        scenarioHere <- names(var.goal)[x]
        parBestScenario <- para.dist.df[order(para.dist.df[,paste0("distance.",scenarioHere)]),]
        parBestScenario <- parBestScenario[1:n.pt.loop.eachScenario,]
        })
      
      para.dist.df.best.add <- do.call(rbind,newParaPoints)
      para.dist.df.best <- rbind(para.dist.df.best,para.dist.df.best.add)
      
      # remove duplicated rows
      para.dist.df.best <- para.dist.df.best[!duplicated(para.dist.df.best),]
    }
    
    # generate mid-distance parameter points for each pair of parameter points
    # in para.dist.df.best
    para.dist.df.best.midPts <- paraPt.midDist.fun(para.dist.DS = para.dist.df.best,
                                                   name.para.change = name.para.change)
    # para.dist.df.best.midPts$distance <- NA # not necessary
    if(parallel){
      s <- mclapply(X = 1:nrow(para.dist.df.best.midPts), FUN = ODE.change.para.fun,
                    DF = para.dist.df.best.midPts[,name.para.change],para = para, 
                    name.para.change = name.para.change, t=t,
                    var.goal = var.goal, thetas.calibration = thetas.calibration,
                    state=state, mc.cores = numCores,
                    weights.scenarios = weights.scenarios)
      
    }else{
      s <- lapply(X = 1:nrow(para.dist.df.best.midPts), FUN = ODE.change.para.fun,
                  DF = para.dist.df.best.midPts[,name.para.change], para = para, 
                  name.para.change = name.para.change,
                  var.goal = var.goal, thetas.calibration = thetas.calibration,
                  state=state,t=t,
                  weights.scenarios = weights.scenarios)

    }
    new.rows <- do.call(rbind.data.frame, s)
    para.dist.df <- rbind(para.dist.df,new.rows)
    min.dist <- round(min(para.dist.df$distance),2)
    
    print(paste(round(nrow(para.dist.df)/tot.nb.row*100,1),"% ; min.distance =",min.dist,sep=" "))
    
    # if(write.file){
    #   write.csv(para.dist.df,paste(wd,name.csv,sep="/"),row.names = F)
    # }
    
    # update para.dist.df.best
    para.dist.df.best <- para.dist.df[order(para.dist.df$distance),]
    para.dist.df.best <- para.dist.df.best[1:n.pt.selected.loop,]
    
    # add best points in each scenario separately
    if(n.pt.loop.eachScenario > 0){
      
      newParaPoints <- lapply(X = 1:length(var.goal),FUN = function(x){
        scenarioHere <- names(var.goal)[x]
        parBestScenario <- para.dist.df[order(para.dist.df[,paste0("distance.",scenarioHere)]),]
        parBestScenario <- parBestScenario[1:n.pt.loop.eachScenario,]
      })
      
      para.dist.df.best.add <- do.call(rbind,newParaPoints)
      para.dist.df.best <- rbind(para.dist.df.best,para.dist.df.best.add)
      
      # remove duplicated rows
      para.dist.df.best <- para.dist.df.best[!duplicated(para.dist.df.best),]
    }
    
    # Shuffle the value in each column (random genetic algorithm)
    # then run the simulations
    para.dist.df.best.shuffle <- para.dist.df.best
    para.dist.df.best.shuffle[,name.para.change] <- 
      apply(X = para.dist.df.best.shuffle[,name.para.change], MARGIN = 2, 
            FUN = function(x){x[sample(x = 1:nrow(para.dist.df.best.shuffle),
                                       size = nrow(para.dist.df.best.shuffle),
                                       replace = F)]})
    # para.orig <- parameters.fun()
    # para <- parameters
    # para <- para.conversion.fun(para.orig,mg.to.g = T,h.to.d = F)
    if(parallel){
      s <- mclapply(X = 1:nrow(para.dist.df.best.shuffle), FUN = ODE.change.para.fun,
                    DF = para.dist.df.best.shuffle[,name.para.change],para = para, 
                    name.para.change = name.para.change, 
                    var.goal = var.goal, thetas.calibration = thetas.calibration,
                    t = t,
                    state = state,mc.cores = numCores,
                    weights.scenarios = weights.scenarios)
    }else{
      s <- lapply(X = 1:nrow(para.dist.df.best.shuffle), FUN = ODE.change.para.fun,
                  DF = para.dist.df.best.shuffle[,name.para.change], para = para, 
                  name.para.change = name.para.change, 
                  var.goal = var.goal, thetas.calibration = thetas.calibration,
                  t = t, state=state,
                  weights.scenarios = weights.scenarios)
    }
    new.rows <- do.call(rbind.data.frame, s)
    para.dist.df <- rbind(para.dist.df,new.rows)
    min.dist <- round(min(para.dist.df$distance),2)
    
    print(paste(round(nrow(para.dist.df)/tot.nb.row*100,1),"% ; min.distance =",min.dist,sep=" "))
    
    # if(write.file){
    #   write.csv(para.dist.df,paste(wd,name.csv,sep="/"),row.names = F)
    # }
    
    # update para.dist.df.best
    para.dist.df.best <- para.dist.df[order(para.dist.df$distance),]
    para.dist.df.best <- para.dist.df.best[1:n.pt.selected.loop,]
    
    # for each point/row in para.dist.df.best:
    # - generate new ranges around each parameter value
    # - LHS n.loop new parameter points
    for(j in 1:nrow(para.dist.df.best)){
      
      pt.here <- para.dist.df.best[j,]
      
      # define the new range for each parameter around the value
      ranges.calib.l.new <- lapply(X = name.para.change,
                                   FUN = function(x){
                                     range.here <- ranges.calib.l[[x]]
                                     length.range <- (range.here[2] - range.here[1])*percent.range/100/i
                                     mid.val <- pt.here[x]
                                     min.val <- mid.val - length.range/2
                                     max.val <- mid.val + length.range/2
                                     if(min.val < range.here[1]){
                                       min.val <- range.here[1]
                                       max.val <- min.val + length.range
                                     }else if(max.val > range.here[2]){
                                       max.val <- range.here[2]
                                       min.val <- max.val - length.range
                                     }
                                     names(max.val) <- names(min.val) <- NULL
                                     output <- unlist(c(min.val,max.val))
                                   })
      names(ranges.calib.l.new) <- name.para.change
      
      # sample new parameter points 
      lh <- LHS.fun(ranges.calib.l.new,n.pt.generated.loop)
      
      # Run the corresponding simulations
      ## get parameter list
      # para.orig <- parameters.fun()
      # para <- parameters
      # para <- para.conversion.fun(para.orig,mg.to.g = T,h.to.d = F)
      
      if(parallel){
        s <- mclapply(X = 1:nrow(lh), FUN = ODE.change.para.fun, DF = lh, para = para,
                      name.para.change = name.para.change, 
                      var.goal = var.goal, thetas.calibration = thetas.calibration,
                      t = t, 
                      state = state, mc.cores = numCores,
                      weights.scenarios = weights.scenarios)
        
      }else{
        s <- lapply(X = 1:nrow(lh), FUN = ODE.change.para.fun, DF = lh, para = para,
                    name.para.change = name.para.change,
                    var.goal=var.goal, thetas.calibration = thetas.calibration,
                    t = t, 
                    state = state,
                    weights.scenarios = weights.scenarios)
        
      }
      new.rows <- do.call(rbind.data.frame, s)
      para.dist.df <- rbind(para.dist.df,new.rows)
      min.dist <- round(min(para.dist.df$distance),2)
      
      print(paste(round(nrow(para.dist.df)/tot.nb.row*100,1),"% ; min.distance =",min.dist,sep=" "))
      
      # if(write.file){
      #   write.csv(para.dist.df,paste(wd,name.csv,sep="/"),row.names = F)
      # }
    }
  }
  
  if(is.na(name.csv)){
    date.time <- Sys.time()
    name.csv <- paste("calibration.lossFun_",date.time,"_Dist_",min.dist,".csv",sep="")
    name.csv <- gsub(":","-",name.csv)
    name.csv <- gsub(" ","_",name.csv)
    
    write.csv(para.dist.df,paste(wd,name.csv,sep="/"),row.names = F)
  }
  return(para.dist.df)
}

# Function that take a dataframe of parameter values and measure of distance
# and generates mid-distance parameter points for each pair of parameter points
# in the dataset.
# para.dist.DS <- para.dist.df.best
paraPt.midDist.fun <- function(para.dist.DS,name.para.change=NA){
  
  all.comb.pairs <- combn(x = 1:nrow(para.dist.DS),m = 2)
  if(!is.na(name.para.change)[1]){
    para.num <- name.para.change
  }else{
    para.num <- colnames(para.dist.DS)[colnames(para.dist.DS) != "Tw"]
  }
  
  output <- 
    apply(X = all.comb.pairs, MARGIN = 2,
          FUN = function(x){
            para.dist.DS.cut <- para.dist.DS[x,]
            new.row <- para.dist.DS.cut[1,]
            new.row[,para.num] <- apply(X = para.dist.DS.cut[,para.num], 
                                        MARGIN = 2, FUN = mean)
            return(new.row)
          })
  
  output <- do.call(rbind.data.frame, output)
  return(output)
}

# Function to run using lapply, parallel::mclapply or parallel::parLapply
# # It is used in ODE.calibration.fun() and experiment.fun()
# x <- 1
# DF <- output.exp
# DF <- lh
# DF <- parameters
# para <- parameters[[1]]
# name.para.change <- paraToChange # c("theta_a","theta_q","theta_h","Tw")
# var.goal <- NA
# t <- seq(0,61*12, by = 0.01)
# calc.dist <- F
# weight.weeds.vs.calib <- 1
ODE.change.para.fun <- function(x,DF,para,name.para.change,
                                var.goal = NA, thetas.calibration = NA,
                                t,state,
                                calc.dist=T,
                                weights.scenarios = NA){
  
  # select the row (= parameter point) corresponding to x
  pt.here <- DF[x,,drop=F]
  # update parameter values
  para[name.para.change] <- pt.here[,name.para.change]
  #
  if(calc.dist){ # for the model calibration
    # run ODE model
    # if(class(var.goal) == "data.frame"){
    #   var.goal <- list(var.goal)
    # }
    # run simulations for each scenario in var.goal
    s <- lapply(X = 1:length(var.goal), function(X){
      scenarioHere <- names(var.goal)[X]
      para$Tw <- scenarioHere
      para$theta_a <- thetas.calibration$theta_a[thetas.calibration$scenario == scenarioHere]
      para$theta_h <- thetas.calibration$theta_h[thetas.calibration$scenario == scenarioHere]
      para$theta_q <- thetas.calibration$theta_q[thetas.calibration$scenario == scenarioHere]
      
      t.here <- seq(0,(max(var.goal[[X]]$day)+1)*12,0.01)
      out <- NULL
      while(is.null(out) | sum(class(out) != "try-error") == 0){
        out <- try(ode(y = state, times = t.here, func = ODE.m, parms = para))
      }
      # determine the distance with loss function
      out <- as.data.frame(out)
      out[,1] <- out[,1] / 12 # conversion in days
      out <- out[out[,1] %in% 0:90,]
      
      # new.row <- para.dist.df[1,]
      new.row <- data.frame(matrix(NA,nrow = 1, ncol = length(para)))
      colnames(new.row) <- names(para)
      char.para <- c("Tw") # so that unlisting does not produce only character values
      new.row[1,! colnames(new.row) %in% char.para] <- unlist(para[!names(para) %in% char.para])
      new.row[1,colnames(new.row) %in% char.para] <- unlist(para[names(para) %in% char.para])
      
      # calculate the euclidian distance from var.goal 
      dist <- loss.fun(output.ODE = out, var.goal = var.goal[[X]])
      new.row$distance <- dist
      return(new.row)
    })
    new.row <- s[[1]]
    if(length(s) > 1){  # sum the distance if more than one scenario was used
      # new.row$distance <- sum(do.call(rbind,s)$distance)
      s.cut <- do.call(rbind,s)[,c("Tw","distance")]
      dist.weight <- unlist(lapply(X = names(weights.scenarios),
                                   FUN = function(X){s.cut$distance[s.cut$Tw == X] *
                                       weights.scenarios[names(weights.scenarios) == X]}))
      new.row$distance <- sum(dist.weight)
      for(i in 1:length(s)){
        new.row$dist.new <- do.call(rbind,s)$distance[i]
        colnames(new.row)[colnames(new.row) == "dist.new"] <- paste0("distance.",names(var.goal)[i])
      }
    }
  }else{ # for the experiment
    # run ODE model
    out <- NULL
    while(is.null(out) | sum(class(out) != "try-error") == 0){
      out <- try(ode(y = state, times = t, func = ODE.m, parms = para))
    }
    # determine the distance with loss function
    out <- as.data.frame(out)
    out$CY <- crop.yield.fun(data.ts = out, parameters = para, convert.to.day = F, 
                             modif.tot.amount = T) # in g for the total area
    
    # integrate the total number of adult bees over the crop blooming period
    time.diff <- diff(out$time)
    out.cropBloom <- out[out$time >= 30*12 & out$time <= 60*12,]
    out.cropBloom$tot.adult.bees.integ <- rowSums(out.cropBloom[,c("Wn","S","Hc","Hw","Uc","Uw")])
    out.cropBloom$tot.adult.bees.integ <- cumsum(out.cropBloom$tot.adult.bees.integ)*time.diff[1]
    out.cropBloom[,1] <- out.cropBloom[,1] / 12 # conversion in days
    out.cropBloom <- out.cropBloom[out.cropBloom[,1] %in% 0:90,]
    
    out[,1] <- out[,1] / 12 # conversion in days
    out <- out[out[,1] %in% 0:90,]

    out$CY <- out$CY / 1000       # g --> kg for the total area
    # CY <- CY * 10000 / (parameters$A * (1 - para$theta_a)) # kg for crop area --> kg/ha
    out$CY <- out$CY * 10000 / para$A # kg for total area --> kg/ha
    
    # new.row <- para.dist.df[1,]
    new.row <- data.frame(matrix(NA,nrow = 1, ncol = length(para)))
    colnames(new.row) <- names(para)
    char.para <- c("Tw") # so that unlisting does not produce only character values
    new.row[1,! colnames(new.row) %in% char.para] <- unlist(para[!names(para) %in% char.para])
    new.row[1,colnames(new.row) %in% char.para] <- unlist(para[names(para) %in% char.para])
    # new.row$yield_kg.ha.last.d <- out$CY[out$time == max(t/12)]
    
    new.row$yield_kg.ha.last.d <- out$CY[nrow(out)]
    new.row$tot.adult.bees.integ <- out.cropBloom$tot.adult.bees.integ[nrow(out.cropBloom)] # integrated just over the crop blooming season
    
    # add the total number of adult bees for the GSA
    new.row$tot.adult.bees.last.d <- 
      rowSums(out[,c("Wn","S","Hc","Hw","Uc","Uw")])[nrow(out)]
  }
  return(new.row)
  # return(list(new.row))
}

# Function that that generate one simulation for each combination of the values 
# of the parameters theta_a, theta_hq and Tw.
# A data frame is returned with the crop yeild in kg/ha or kg
# t.h <- t <- seq(0,61*12, by = 0.01)
# theta_a <- c(0.1) # seq(0.02,0.4,0.02)
# theta_h <- theta_q <- c(1) # seq(0.1,6.0,0.1)
# Tw <- c("control")
# # Tw <- c("Ap","ApmM","ApM","M","2ndmM","control")
# print.csv <- F
experiment.fun <- function(ODE.m,theta_a,theta_h,theta_q,parameters,t.h,print.csv = T,
                           wd_data_raw,parallel=F,numCores=1,suffix.name.csv=""){
  
  # create the data frame that contains the yields at the end of the simulation
  # for all possible combination of variables
  output.exp <- data.frame(
    Tw = unlist(lapply(X=Tw,FUN = function(x){rep(x,length(theta_a)*length(theta_h))})),
    theta_a = rep(unlist(lapply(X=theta_a,FUN = function(x){rep(x,length(theta_h))})),length(Tw)),
    theta_h = rep(theta_h,length(Tw)*length(theta_a)),
    theta_q = rep(theta_q,length(Tw)*length(theta_a)),
    A = parameters$A)
  
  # output.exp$yield_kg.ha.last.d <- NA
  
  if(print.csv){
    date.time <- Sys.time()
    name.csv <- paste("output.experiment_",date.time,"_",suffix.name.csv,".csv",sep="")
    name.csv <- gsub(":","-",name.csv)
    write.csv(output.exp,paste(wd_data_raw,name.csv,sep="/"),row.names = F)
  }
  
  para <- parameters
  paraToChange <- c("theta_a","theta_q","theta_h","Tw")
  
  # now run the simulations for the lh
  if(parallel){
    require(parallel)
    # numCores <- detectCores()
    s <- mclapply(X = 1:nrow(output.exp), FUN = ODE.change.para.fun, DF = output.exp,
                  para = para, name.para.change = paraToChange, t=t.h, 
                  state=state,calc.dist = F, mc.cores = numCores)
    # s <- parLapply(X = 1:2, FUN = ODE.change.para.fun,mc.cores = 5)
  }else{
    s <- lapply(X = 1:nrow(output.exp), FUN = ODE.change.para.fun, DF = output.exp, 
                para = para, state=state, name.para.change = paraToChange, t=t.h,
                calc.dist = F)
  }
  new.rows <- do.call(rbind.data.frame,s)
  new.rows$last_day <- max(t.h)/12
  
  # output.exp$yield_kg.ha.last.d <- new.rows$yield_kg.ha.last.d
  
  # for(i in 1:nrow(output.exp)){
  #   print(paste("Simulations done:",round(i/nrow(output.exp)*100,1),"%",sep=" "))
  #   para[paraToChange] <- output.exp[i,paraToChange]
  #   out <- NULL
  #   while(is.null(out) | sum(class(out) != "try-error") == 0){
  #     out <- try(ode(y = state, times = t.h, func = ODE.m, parms = para))
  #   }
  #   out.2 <- as.data.frame(out)
  #   out.2[,1] <- out.2[,1] / 12 # conversion in days
  #   out.2 <- out.2[out.2[,1] %in% 0:90,]
  #   # figure3.fun(out.2, parameters = para, print = F,wd_figures = wd_figures)
  #   
  #   CY <- crop.yield.fun(data.ts = out.2, parameters = para, convert.to.day = F) # in g for the total area
  #   CY <- CY / 1000       # g --> kg for the total area
  #   CY <- CY * 10000 / parameters$A # kg for total area --> kg/ha
  #   out.2$CY <- CY
  #   output.exp[i,]$yield_kg.ha.last.d <- out.2$CY[out.2$time == 61]
  #   
  #   write.csv(output.exp,paste(wd_data_raw,"output.experiment.csv",sep="/"),row.names = F)
  # }
  if(print.csv){
    write.csv(new.rows,paste(wd_data_raw,name.csv,sep="/"),row.names = F)
  }
  return(new.rows)
}

# Function that conduct a global sensitivity analysis using the Latin Hypercube
# # Sampling and Partial423Rank Correlation Coefficient (LHS-PRCC).
# parameters.l <- parameters
# nb.para.pts <- 10
# range.percent <- 50
# parallel <- F
# numCores <- 1
# rank <- T   # to do a Partial RANK Correlation Coefficients
# semi <- F   # to do a SEMI-Partial Correlation Coefficients
# nboot <- 20
# write.file <- T
# suffix <- NA
ODE.LHS.PRCC.GSA.fun <- function(ODE.m, t, state, parameters.l, name.para.GSA, 
                                 nb.para.pts=100, range.percent = 10,parallel=F,
                                 numCores=1,rank=T,semi=F, nboot = 1000,
                                 write.file = F, wd = wd_data, suffix = NA){
  
  require(sensitivity)
  
  # define the range of value for each parameter: +/- 10%
  range.para <- para.range.fun(para.l = parameters.l[name.para.GSA], 
                               range.percent = range.percent,
                               return.data.frame = F)
  
  # sample parameter points in the parameter space:
  lh <- LHS.fun(range.val.l = range.para,n = nb.para.pts)
  
  # run the simulations for each parameter point:
  if(parallel){
    s <- mclapply(X = 1:nrow(lh), FUN = ODE.change.para.fun, DF = lh, 
                  para = parameters.l, t=t, state=state,calc.dist = F,
                  name.para.change = name.para.GSA, mc.cores = numCores)
  }else{
    s <- lapply(X = 1:nrow(lh), FUN = ODE.change.para.fun, DF = lh, 
                para = parameters.l, t = t, state = state, calc.dist = F,
                name.para.change = name.para.GSA)
  }
  s <- do.call(rbind,s)
  
  if(is.na(suffix)){
    suffix <- Sys.time()
    suffix <- gsub(":","-",suffix)
    suffix <- gsub(" ","_",suffix)
  }
  suffix2 <- "/GSA.LHS."
  
  if(write.file){
    write.csv(s,paste0(wd,suffix2,suffix,".csv"),row.names = F)
  }
  
  print(paste("ODE simulation for LHS:",paste0(suffix2,suffix),"are done"))
  
  x <- s[,name.para.GSA]
  y <- s$yield_kg.ha.last.d
  # transform CY/ha to CCY for the size of the field
  y <-  y * parameters.l$A/10000
  result <- pcc(X = x, y = y, rank = rank, semi = semi, nboot = nboot)
  
  if(rank & !semi){
    suffix2 <- "/GSA.LHS.PRCC."
  }else if(!rank & !semi){
    suffix2 <- "/GSA.LHS.PCC."
  }else if(!rank & semi){
    suffix2 <- "/GSA.LHS.SPCC."
  }else if(rank & semi){
    suffix2 <- "/GSA.LHS.SPRCC."
  }
  
  if(write.file){
    saveRDS(object = result, 
            file = paste0(wd,suffix2,"yield_kg.ha.last.d.",suffix,".RData"))
  }
  
  print(paste("PCC for yeild :",paste0(suffix2,suffix),"are done"))
  
  
  result.l <- list(result) # results with yield_kg.ha.last.d
  
  y <- s$tot.adult.bees.last.d
  result <- pcc(X = x, y = y, rank = rank, semi = semi, nboot = nboot)
  
  if(write.file){
    saveRDS(object = result, 
            file = paste0(wd,suffix2,"tot.adult.bees.last.d.",suffix,".RData"))
  }
  
  print(paste("PCC for nb bees :",paste0(suffix2,suffix),"are done"))
  
  result.l[[2]] <- result # results with yield_kg.ha.last.d
  
  names(result.l) <- c("yield_kg.last.d","tot.adult.bees.last.d")
  
  if(write.file){
    saveRDS(object = result.l, file = paste0(wd,suffix2,"ODE.",suffix,".RData"))
  }
  return(result.l)
}


# Returns a list of two dataframes, one for each of the scenarios in the calibration, 
# with the expected population sizes (i.e., the targets).
var.goal.fun <- function(){
  
  # var.goal <- data.frame(day = c(23,53,63),
  #                        Wn = c(23,NA,NA),
  #                        S = c(2,NA,NA),
  #                        Hcw = c(13,NA,NA),
  #                        Ucw = c(2,NA,NA),
  #                        W.tot = c(NA,15,NA),
  #                        B = c(114,15,43))
  
  # Case with two scenarios:
  var.goal <- data.frame(day = c(23,53,63),
                         Wn = c(23,NA,NA),
                         S = c(2,NA,NA),
                         Hcw = c(13,NA,NA),
                         Ucw = c(2,NA,NA),
                         W.tot = c(NA,15,NA),
                         B = c(114,15,43))
  
  var.goal <- list(var.goal)
  var.goal[[2]] <-  data.frame(day = c(33,43,53),
                               Wn = c(NA,NA,NA),
                               S = c(NA,NA,NA),
                               Hcw = c(NA,NA,NA),
                               Ucw = c(NA,NA,NA),
                               W.tot = c(15,15,15),
                               B = c(15,15,15))
  
  names(var.goal) <- c(treatments.Tw$calibration,
                       treatments.Tw$weeds.only)
  
  # case with three scenarios
  # var.goal <- data.frame(day = c(23,53,63),
  #                        Wn = c(NA,NA,NA),
  #                        S = c(NA,NA,NA),
  #                        Hcw = c(NA,NA,NA),
  #                        Ucw = c(NA,NA,NA),
  #                        W.tot = c(40,15,NA),
  #                        B = c(114,15,43))
  
  # var.goal <- list(var.goal)
  # var.goal[[2]] <-  data.frame(day = c(33,43,53),
  #                              Wn = c(NA,NA,NA),
  #                              S = c(NA,NA,NA),
  #                              Hcw = c(NA,NA,NA),
  #                              Ucw = c(NA,NA,NA),
  #                              # W.tot = c(15,15,15),
  #                              # B = c(15,15,15))
  #                              W.tot = rep(20,3),
  #                              B = rep(20,3))
  # 
  # var.goal[[3]] <-  data.frame(
  #                              # day = c(33,43,53),
  #                              day = c(23,53),
  #                              Wn = c(NA,NA),
  #                              S = c(NA,NA),
  #                              Hcw = c(NA,NA),
  #                              Ucw = c(NA,NA),
  #                              W.tot = c(0,0),
  #                              B = c(0,0))
  # 
  # names(var.goal) <- c(treatments.Tw$calibration,
  #                      treatments.Tw$weeds.only,
  #                      treatments.Tw$noResources)
  return(var.goal)
}

var.goal.4scenarios.fun <- function(){
  
  # resourcesDiscontWF :
  var.goal <- data.frame(day = c(23,53,63),
                         Wn = c(NA,NA,NA),
                         S = c(NA,NA,NA),
                         Hcw = c(NA,NA,NA),
                         Ucw = c(NA,NA,NA),
                         W.tot = c(40,20,NA),
                         B = c(114,20,43))
  
  var.goal <- list(var.goal)
  
  # resourcesDiscontCF
  var.goal[[2]] <-  var.goal[[1]]
  var.goal[[2]][,c("W.tot","B")] <-  var.goal[[2]][,c("W.tot","B")] - 5
  
  # treatments.Tw$weeds.only
  var.goal[[3]] <-  data.frame(day = c(33,43,53),
                               Wn = c(NA,NA,NA),
                               S = c(NA,NA,NA),
                               Hcw = c(NA,NA,NA),
                               Ucw = c(NA,NA,NA),
                               W.tot = c(15,15,15),
                               B = c(15,15,15))
  # noResources
  var.goal[[4]] <-  data.frame(
                               # day = c(33,43,53),
                               day = c(23,53),
                               Wn = c(NA,NA),
                               S = c(NA,NA),
                               Hcw = c(NA,NA),
                               Ucw = c(NA,NA),
                               W.tot = c(0,0),
                               B = c(0,0))

  names(var.goal) <- c(treatments.Tw$resourcesDiscontWF,
                       treatments.Tw$resourcesDiscontCF,
                       treatments.Tw$weeds.only,
                       treatments.Tw$noResources)
  return(var.goal)
}

# The values of the thetas (a, q and h) for the different scenarios of the calibration
thetas.calibration.fun <- function(){
  
  # case with two scenarios:
  output <- data.frame(scenario = c(treatments.Tw$calibration,
                                    treatments.Tw$weeds.only),
                       theta_a = c(0.1,0.1),
                       theta_q = c(2,2),
                       theta_h = c(2,2))
  
  # Case with the three scenarios
  # output <- data.frame(scenario = c(treatments.Tw$calibration,
  #                                   treatments.Tw$weeds.only,
  #                                   treatments.Tw$noResources),
  #                      theta_a = c(0.1,0.999,0.001),
  #                      theta_q = c(2,2,2),
  #                      theta_h = c(2,2,2))
  return(output)
}

thetas.calibration.4scenarios.fun <- function(){
  
  #
  output <- data.frame(scenario = c(treatments.Tw$resourcesDiscontWF,
                                    treatments.Tw$resourcesDiscontCF,
                                    treatments.Tw$weeds.only,
                                    treatments.Tw$noResources),
                       theta_a = c(0.999,0.001,0.999,0.001),
                       theta_q = c(2,2,5,5),
                       theta_h = c(2,2,5,5))
  return(output)
}






