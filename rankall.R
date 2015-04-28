source("rankhospital.R")

rankall<-function(outcome,num="best")
{
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states<-subset(data,select=State)

  statesName <- states[, 1]
  orderedStates<-states[order(statesName),]
  
  uniqueOrderedStates<-unique(orderedStates)
  numberOfStates<-length(uniqueOrderedStates)
  
  x<-matrix(nrow=numberOfStates,ncol=2,dimnames = list(uniqueOrderedStates,c("hospital", "state")))
  
  ris<-vector(mode="character",1)
  indice<-1
  while(indice<=numberOfStates)
  {
    ris<-rankhospital(uniqueOrderedStates[indice], outcome, num)
    x[indice,1] = ris[1]
    x[indice,2] = uniqueOrderedStates[indice]
    indice<-indice+1
  }
  
  rit<-data.frame(x)
  return(rit)
}
