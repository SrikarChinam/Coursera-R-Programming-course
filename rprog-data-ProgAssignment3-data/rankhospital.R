rankhospital <- function(state, outcome, num= "best"){

  #read file and ensure the inputs are valid
  dtfile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!state %in% unique(dtfile$State)) {
    stop("invalid state")
  }
  kind <- c("heart attack", "heart failure", "pneumonia")  # kind contains the kind of diseases
  if (!outcome %in% kind) {
    stop("invalid outcome")
  }
    #Apply filters to data file
  col <- c(11,17,23) #corresponding column numbers with kind
  reqcolnum <- col[kind==outcome]
  newfile<- dtfile[state==dtfile$State,]  #take data of the desired state
  newfile[,reqcolnum]<-as.numeric(newfile[,reqcolnum])  #make data numeric
  newfile<- newfile[!is.na(newfile[,reqcolnum]),]   # remove NA values
  newfile<-newfile[order(newfile[,reqcolnum],newfile[,2]),]  #order by value and by name
  numrow<- nrow(newfile)  #calcualte number of rows

  
  # final output display
  if (is.numeric(num)) {
    if (num>numrow) {
      return (NA)
      stop
    }
  }  
  if (num=="best") {
    return (newfile[1,2])
  }
  else if (num =="worst") {
    return( newfile[numrow,2])
  }
    else {
      return (newfile[num,2])
    }   
}