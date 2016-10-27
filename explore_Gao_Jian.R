---
title: "Assigenment 7"
output: html_document
---

# Jian Gao
# Group C


## My idea for this question is that first of all clarify whether the variable 
## of the dataframe is factor variables,
## run to the subfunction1, if the variable of the dataframe is factor variables, 
## run to the subfunction2,3,4

library('ggplot2')
library('plyr')
dataframe<-data.frame(diamonds)
dataframe<-data.frame(mtcars)

explore<-function(dataframe,switch,threshold,vector){
  dataframe1=dataframe[sapply(dataframe,is.factor)]
  dataframe2=dataframe[sapply(dataframe,is.numeric)]
  checkswitch<-list("on","off","grid")

## subfunction1 
  result1=summary(dataframe1)
## A frequency table for every categorical and logical variables

## subfunction2
  result2=summary(dataframe2)
## A summary statistics table for each numerical variables

## subfunction3
  a=colnames(dataframe2)
  Variable_Pairs=c()
  R_Square=c()
  for (i in 1:(length(a)-1)){
    for (j in (i+1):length(a)){
      Variable_Pairs = c(Variable_Pairs,paste(a[i],a[j],sep='-'))
      R_Square=c(R_Square,summary(lm(dataframe2[,i] ~ dataframe2[,j]))$r.square)
    }
  }
  result3<-data.frame(Variable_Pairs,R_Square)
## A data frame that contains each pair of column names in
## the first column(name the column "Variable Pairs") and the 
## addociated r-square value in the second column(name the column "R-Square")

## subfunction4
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
  result4<-d1[! abs(d1$Pearson_Exceeds_Threshold) < threshold,]
## A data frame contains each pair of colunm names in 
## the first column(name the column "Variable Pairs") and 
## correlation coefficient (Pearson) for all coefficients whose
## absolute value is greater than the correlation threshold 
## in the second column

##plot histogram and bar 
  a=colnames(dataframe2)
  b=colnames(dataframe1)
  if(switch[1]==checkswitch[[1]] || switch[1]==checkswitch[[3]]){
    par(mfrow=c(3,3))
    for(i in 1:length(a)){
      pdf(paste(colnames(dataframe2[,i]),"_Htg.pdf")) ## save as pdf file. 
      mk=mean(dataframe2[,i])
      mx=max(dataframe2[,i])
      mi=min(dataframe2[,i])
      h=hist(dataframe2[,i], xlab="Density", xlim=c(mi-0.5, mx+0.5), ylab="Count", col="lightblue", main=paste(colnames(dataframe2[i]),"Histogram"))
      abline(v=mk,col="red",lwd=2)
      dev.off()
    }
    par(mfrow=c(2,2))
    for(j in 1:length(b)){
      pdf(paste(colnames(dataframe1[,j]),"_Bar.pdf"))
      bar=barplot(table(dataframe1[,j]), ylab="Counts", col="gray", main=paste(colnames(dataframe1[j]),"Gray Bar Plot"))
      dev.off()
    }
  }
  else if(switch[1]==checkswitch[[3]]){
    par(mfrow=c(3,3))
    for(i in 1:length(a)){
      pdf(paste(colnames(dataframe2[,i]),"_Htg.pdf"))
      mk=mean(dataframe2[,i])
      mx=max(dataframe2[,i])
      mi=min(dataframe2[,i])
      h=hist(dataframe2[,i], xlab="Density", xlim=c(mi-0.5, mx+0.5), ylab="Count", col="lightblue", main=paste(colnames(dataframe2[i]),"Histogram"))
      abline(v=mk,col="red",lwd=2)
      grid(NULL, NULL, lwd = 2, lty="dotted", col="red")
      dev.off()
    }
  }
  else{
    h=c()
    bar=c()
  }
  rhis=list(h)
  rbar=list(bar)
##If the plot switch parameter is "on" or "grid" then plot a pair of 
##blue histograms with a vertical red line at the mean for every numerical
##variable at each number of bins integer specified in the bin vector parameter.

  result<-list(result1,result2,result3,result4,rhis,rbar)
  print(result)
}













