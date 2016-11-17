##Prof G - ---
##Prof G - title: "Assigenment 7"
##Prof G - output: html_document
##Prof G - ---

# Jian Gao
# Group C

##Prof G - Something is missing in this file. 
##Prof G - dataexpl function does not exist.

## My idea for this question is that first of all clarify whether the variable 
## of the dataframe is factor variables,
## run to the subfunction1, if the variable of the dataframe is factor variables, 
## run to the subfunction2,3,4

library('ggplot2')
library('plyr')
library('grid')


dataframe<-data.frame(diamonds)
dataframe1=dataframe[sapply(dataframe,is.factor)]
dataframe2=dataframe[sapply(dataframe,is.numeric)]
dataframe3=dataframe[sapply(dataframe,is.logical)]
is.binary <- function(v) {
#The is.binary function determines whether a vector is binary
#Input: a vector
#Output: TRUE or FALSE
  x <- unique(v)  
  #check all the distinct and put those in a vector x
  length(x) - sum(is.na(x)) == 2L 
  #check to see if x only contains 2 distinct values
}
dataframe4=dataframe[sapply(dataframe,is.binary)]
checkswitch<-list("on","off","grid")
## represent some basic imformation's name very strong conncetion 
## with rest part of the projct!!!


##Prof G - try naming your functions better to give
##Prof G - a hint about what they do. subfunction1 is 
##Prof G - not very descriptive.
## subfunction1
subfunction1<-function(dataframe){ 
 # This function is a frequency table for every categorical and logical variable.
  # parameter: data_frame
  # type: any data frame
  # return: frequancy tables
  # type: list
  summary(dataframe1)
  summary(dataframe3)
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
  a=colnames(dataframe2)
  Variable_Pairs=c()
  R_Square=c()
  for (i in 1:(length(a)-1)){
    for (j in (i+1):length(a)){
      Variable_Pairs = c(Variable_Pairs,paste(a[i],a[j],sep='-'))
      R_Square=c(R_Square,summary(lm(dataframe2[,i] ~ dataframe2[,j]))$r.square)
    }
  }
  data.frame(Variable_Pairs,R_Square)
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

##plot histogram and bar 
plot_count<-function(dataframe,switch,vector){
  # This function works like this: If the plot switch parameter is ?on? or ?grid?,
  # then plot a pair of blue histograms with a vertical red line at the mean (one 
  # using counts and the other density) for every numerical variable at each number 
  # of bins integer specified in the bin vector parameter. If the plot switch is set 
  # to ?grid?, there should be a grid for each count-bin combination and a separate 
  # grid for each density-bin size combination. For example, given 5 numeric variables 
  # and a vector of three bin number integers, the function should generate 30 individual 
  # plots or a total of 6 grid plots (with each grid plot containing 5 subplots).  
  # parameters: data_frame (type:data frame,range:(0,1))
  #             switch (type:char,range:"on""off""grid")
  #             vector (type:vector,range:integers)
  
  # return: plots
  a=colnames(dataframe2)
  dataframe5=data.frame(dataframe1,dataframe3,dataframe4) 
  b=colnames(dataframe5)
  if(switch[1]==checkswitch[[1]] || switch[1]==checkswitch[[3]]){
    par(mfrow=c(3,3))
    for(i in 1:length(a)){     
      mk=mean(dataframe2[,i])
      mx=max(dataframe2[,i])
      mi=min(dataframe2[,i])
      h=hist(dataframe2[,i], xlab="Density", xlim=c(mi-0.5, mx+0.5), ylab="Count", col="lightblue", main=paste(colnames(dataframe2[i]),"Histogram"))
      abline(v=mk,col="red",lwd=2)      
    }
    par(mfrow=c(2,2))
    for(j in 1:length(b)){
      #pdf(paste(colnames(dataframe1[,j]),"_Bar.pdf"))
      bar=barplot(table(dataframe1[,j]), ylab="Counts", col="gray", main=paste(colnames(dataframe1[j]),"Gray Bar Plot"))
      #dev.off()
    }
  }  
  else if(switch[1]==checkswitch[[3]]){
    par(mfrow=c(3,3))
    for(j in 1:length(vector)){
      mk=mean(dataframe2[,i])
      mx=max(dataframe2[,i])
      mi=min(dataframe2[,i])
      grid.newpage()
      h1=list()
      d=list()
      for(i in 1:length(a)){
        h1[[i]]=ggplot(dataframe2, aes_string(a[i]),color="lightblue")+
                geom_vline(xintercept=mk,color="red")+xlab(colnames(dataframe2)[i])+xlim(mi-0.5, mx+0.5)+
                geom_histogram(fill="blue",bins=vector[j])+
                labs(title=paste(vector[j], "bins"))
        d=ggplot(dataframe2,aes_string(a[i]),color="lightblue")+
          geom_vline(xintercept=mk,color="red")+xlab(colnames(dataframe2)[i])+xlim(mi-0.5, mx+0.5)+
          geom_histogram(aes(y=..density..),fill="blue",bins=vector[j])+labs(title=paste(vector[j],"bins"))
      }
      h=list(multiplot(plotlist=h1,cols=2),multiplot(plotlist=d,cols=2))
    }
  } 
  else{
    h=c()
    bar=c()
  }
  rhis=list(h)
  rbar=list(bar)
  print(h)
  print(bar)
}
## e.g
plot_count(diamonds,"on",c(30,70))
##If the plot switch parameter is "on" or "grid" then plot a pair of 
##blue histograms with a vertical red line at the mean for every numerical
##variable at each number of bins integer specified in the bin vector parameter.


explore<-function(dataframe,switch,threshold,vector){
## This function called explore that accepts the dataframe,
## a plot switch that can accept three values: off, on or grid
## a threshold cut-off value between 0 and 1
  ##Prof G - What does the vector do?
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
  plot_count(dataframe,switch,vector)
  print(result)
}






## Think about the ways things could go wrong and write some
## defensive code to gracefully handle exceptions.
## (for example, what if the first parameter is not a dataframe?)

#test
dataexpl(diamonds,vector=-1)
#test
dataexpl(diamonds,threshold=c(-1,1))
#test
dataexpl(diamonds,"ongrid")
#test
dataexpl(kk)



