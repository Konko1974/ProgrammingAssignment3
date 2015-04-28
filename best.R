source("base.R")
best<-function(state,outcome)
{
  ris<-base(state,outcome)
  
  return(ris[1,1])
}