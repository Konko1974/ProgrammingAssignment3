source("base.R")
rankhospital<-function(state, outcome,num="best")
{
  hospitals<-base(state, outcome)
  
  if (num=="best")
  {
    return(hospitals[1,1])
  }
  
  lastRow<-nrow(hospitals)
  if (num=="worst")
  {
    index<-lastRow

    while(hospitals[index,2] == "Not Available")
    {
      index<-index-1
    }
    return(hospitals[index,1])
  }
  
  index<-as.numeric(num)
  if ((index>0)&&(index<=lastRow))
  {
    return(hospitals[index,1])
  }
  else
  {
    return(NA)
  }
  
}