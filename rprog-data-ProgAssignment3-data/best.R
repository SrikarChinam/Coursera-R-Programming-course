best <- function(state,outcome){
  #read file and ensure the inputs are valid
  dtfile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!state %in% unique(dtfile$State)) {
    stop("invalid state")
  }
  kind <- c("heart attack", "heart failure", "pneumonia")  # kind contains the kind of diseases
  if (!outcome %in% kind) {
    stop("invalid outcome")
  }
  #find the minimum value of the 30 day mortality rate
  col <- c(11,17,23) #corresponding column numbers with kind
  reqcolnum <- col[kind==outcome]
  newfile<- dtfile[state==dtfile$State,]
  reqcol<- as.numeric(newfile[,reqcolnum])
  minval <- min(reqcol, na.rm=TRUE)
  
  #select hospital based on the minval criteria in alphabetical order
  names <- newfile[minval==reqcol,2]
  newnames <- sort(names)
  return (newnames[1])
  
}