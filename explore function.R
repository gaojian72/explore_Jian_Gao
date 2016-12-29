library('ggplot2')
library('plyr')
library('grid')
library(reshape2)

is.binary <- function(v) {
  #The is.binary function determines whether a vector is binary
  #Input: a vector
  #Output: TRUE or FALSE
  x <- unique(v)  
  #check all the distinct and put those in a vector x
  length(x) - sum(is.na(x)) == 2L 
  #check to see if x only contains 2 distinct values
}

checkswitch<-list("on","off","grid")
# This is a check switch for make sure wheter the switch is on off or grid.
dataframe4=dataframe[sapply(dataframe,is.binary)]
## Set dataframe4 as the dataframe concerns the binary elements 
dataframe=diamonds
## Suppose that the test dataframe is diamonds

## subfunction1
subfunction1<-function(dataframe){ 
  # This function is a frequency table for every categorical and logical variable.
  # parameter: data_frame
  # type: any data frame
  # return: frequancy tables
  # type: list
  dataframe1=dataframe[sapply(dataframe,is.factor)]
  dataframe2=dataframe[sapply(dataframe,is.numeric)]
  dataframe3=dataframe[sapply(dataframe,is.logical)]
  dataframe4=dataframe[sapply(dataframe,is.binary)]
  s1=summary(dataframe1)
  s2=summary(dataframe3)
  list(s1,s2)
}
## e.g
subfunction1(diamonds)

## subfunction2
subfunction2<-function(dataframe){
  # This function is a summary statistics table for each numerical variable
  # parameter: data_frame
  # type: any data frame 
  # return: summary table
  # type: list 
  dataframe1=dataframe[sapply(dataframe,is.factor)]
  dataframe2=dataframe[sapply(dataframe,is.numeric)]
  dataframe3=dataframe[sapply(dataframe,is.logical)]
  dataframe4=dataframe[sapply(dataframe,is.binary)]
  summary(dataframe2)
}
## e.g
subfunction2(diamonds)

## subfunction3
subfunction3<-function(dataframe){
  # this function can accept any dataframe as a parameter and returns a dataframe 
  # that contains each pair of column names in the first column in a single string
  # separated by a -, e.g. for the variables x and y, the string is ?x-y?.
  # and calculate their corresponding r-square in the second column.
  # parameter: data_frame
  # type: any data frame
  # return: a two column data frame
  # type: data frame
  dataframe1=dataframe[sapply(dataframe,is.factor)]
  dataframe2=dataframe[sapply(dataframe,is.numeric)]
  dataframe3=dataframe[sapply(dataframe,is.logical)]
  dataframe4=dataframe[sapply(dataframe,is.binary)]
  a=colnames(dataframe2)
  Variable_Pairs=c()
  R_Square=c()
  for (i in 1:(length(a)-1)){
    for (j in (i+1):length(a)){
      # from the second column to the last column
      Variable_Pairs = c(Variable_Pairs,paste(a[i],a[j],sep='-'))
      # name1-name2
      R_Square=c(R_Square,summary(lm(formula = dataframe2))$r.square)
    }
  }
  return(data.frame(Variable_Pairs,R_Square))
}
## e.g
subfunction3(diamonds)  
## A data frame that contains each pair of column names in
## the first column(name the column "Variable Pairs") and the 
## addociated r-square value in the second column(name the column "R-Square")

## subfunction4
subfunction4<-function(dataframe,threshold){
  # this function can accept any dataframe as a parameter and returns a dataframe 
  # that contains each pair of column names in the first column in a single string
  # separated by a -, e.g. for the variables x and y, the string is ?x-y?.
  # and calculate their corresponding Pearson correlation coefficient in the second column.
  # And choose correlation coefficient (Pearson) for all coefficients whose absolute value 
  # is greater than the correlation threshold
  # parameter: data_frame (data frame)
  #            threshold (numeric)  
  # return: a two column data frame
  # type: data frame
  dataframe1=dataframe[sapply(dataframe,is.factor)]
  dataframe2=dataframe[sapply(dataframe,is.numeric)]
  dataframe3=dataframe[sapply(dataframe,is.logical)]
  dataframe4=dataframe[sapply(dataframe,is.binary)]
  a=colnames(dataframe2)
  Variable_Pairs=c()
  Pearson_Exceeds_Threshold=c()
  for (i in 1:(length(a)-1)){
    for (j in (i+1):length(a)){
      Variable_Pairs = c(Variable_Pairs,paste(a[i],a[j],sep='-'))
      f1=cor(dataframe2[i],dataframe2[j],method="pearson")
      Pearson_Exceeds_Threshold=c(Pearson_Exceeds_Threshold,f1)
    }
  }
  d1<-data.frame(Variable_Pairs,Pearson_Exceeds_Threshold)
  d1[! abs(d1$Pearson_Exceeds_Threshold) < threshold,]
}
## e.g
subfunction4(diamonds,0.7)
## A data frame contains each pair of colunm names in 
## the first column(name the column "Variable Pairs") and 
## correlation coefficient (Pearson) for all coefficients whose
## absolute value is greater than the correlation threshold 
## in the second column

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##plot histogram and bar 
plot_density_count <- function(data_frame,switch,vector){
  # This function works like this: If the plot switch parameter is “on” or “grid”,
  # then plot a pair of blue histograms with a vertical red line at the mean (one 
  # using counts and the other density) for every numerical variable at each number 
  # of bins integer specified in the bin vector parameter. If the plot switch is set 
  # to “grid”, there should be a grid for each count-bin combination and a separate 
  # grid for each density-bin size combination. For example, given 5 numeric variables 
  # and a vector of three bin number integers, the function should generate 30 individual 
  # plots or a total of 6 grid plots (with each grid plot containing 5 subplots).
  
  # parameters: data_frame (type:data frame,range:(0,1))
  #             switch (type:char,range:"on""off""grid")
  #             vector (type:vector,range:integers)
  
  # return: plots
  num <- data_frame[,sapply(data_frame,is.numeric)]    # extract numeric var from data frame
  
  # "on" condition
  if(switch == "on"){
    # use for loops to plot each pair of plots with different vars and different bins
    for(j in 1:length(vector)){
      for(i in 1:ncol(num)){
        # plot histogram for count and density
        p1 <- ggplot(num,aes(x=num[i]),color = "blue")+
          geom_histogram(fill="blue",bins = vector[j])+
          ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean(num[,i]),col="red")
        
        p2 <- ggplot(num,aes(x=num[i],..density..))+
          geom_histogram(fill="blue",bins = vector[j])+
          ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean(num[,i]),col="red")
        # create a new page
        grid.newpage()
        # split the screen into 2 parts.
        pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
        # add title into specific location
        title <- paste(colnames(num[i]),vector[j],sep=" bin=")
        grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        # print plots in pairs
        print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
        print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
        
      }
    }
    
  }
  # other conditions
  else{
    # "grid" condition
    if(switch == "grid"){
      # use for loops to plot each grid(in each grid,subplots has the same var.) of plots with different vars and different bins
      for(j in 1:length(vector)){
        grid.newpage()
        his_count <-list()     # initial a list for count-plots
        his_density <- list()  # initial a list for density-plots
        for(i in 1:ncol(num)){
          # add plot to the count list
          his_count[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(fill="blue", bins=vector[j])+ 
            labs(title= paste(vector[j], "bins"))
        }
        # plot subplots into one plot
        multiplot(plotlist = his_count, cols = 2)  
        
        for(i in 1:ncol(num)){
          # add plot to the density list
          his_density <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(aes(y= ..density..), fill="blue", bins=vector[j])+ 
            labs(title= paste(vector[j], "bins"))
        }
        # plot subplots into one plot
        multiplot(plotlist = his_density, cols = 3)  
      }
    }
  }
}
# check
plot_density_count(diamonds,"grid",c(30,70))
##If the plot switch parameter is "on" or "grid" then plot a pair of 
##blue histograms with a vertical red line at the mean for every numerical
##variable at each number of bins integer specified in the bin vector parameter.


explore<-function(dataframe,switch,threshold,vector){
  ## This function called explore that accepts the dataframe,
  ## a plot switch that can accept three values: off, on or grid
  ## a threshold cut-off value between 0 and 1
  ## a optional vector 
  ## Parameter:dataframe, switch, threshold value, vector c
  ## Return:a frequency table for every factor variables:
  ##        a summary statistics table for each numerical variables;
  ##        a dataframe with associated two column datas r-square
  ##        a dataframe with associated two column datas correlation coefficient which absolute value < threshold
  ##        a histogram(if switch on or grid)
  ##        a bar plot(if switch on or grid)
  result<-list(subfunction1(dataframe),subfunction2(dataframe),subfunction3(dataframe),
               subfunction4(dataframe,threshold))
  plot_density_count(dataframe,switch,vector)
  print(result)
}

#e.g
explore(diamonds,"grid",0.7,c(30,70))

