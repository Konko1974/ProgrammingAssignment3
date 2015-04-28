base<-function(state,outcome)
{
  outcomeColumn<-0
  
  ##check parametrs
  parameterOk<-0
  if (outcome=="heart attack")
  {
    outcomeColumn<-11
    parameterOk<-1
  }
  if (outcome=="heart failure")
  {
    outcomeColumn<-17
    parameterOk<-1
  }
  if (outcome=="pneumonia")
  {
    outcomeColumn<-23
    parameterOk<-1
  }
  if (parameterOk==0)
  {
    stop("invalid outcome")
  }
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  stateList<-subset(data,State==state,select=State)
  if(summary(stateList)[1] == "Length:0          ")
  {
    stop("invalid state")
  }
  
  outcomeColumnName<-colnames(data)[outcomeColumn]
  hospitalNameColumn<-2
  hospitalNameColumnName<-colnames(data)[hospitalNameColumn]
  
  hospitals<-subset(data,State==state,select=c(hospitalNameColumnName,outcomeColumnName))
  values <- as.numeric(hospitals[, 2])
  hospitalsName <- hospitals[, 1]
  orderedHospitals<-hospitals[order(values,hospitalsName),]
  
  return(orderedHospitals)
}