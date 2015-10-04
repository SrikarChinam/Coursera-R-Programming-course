rankall <- function(outcome, num = "best") {
  
  #read file and ensure the inputs are valid
  dtfile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  kind <- c("heart attack", "heart failure", "pneumonia")  # kind contains the kind of diseases
  if (!outcome %in% kind) {
    stop("invalid outcome")
  }
  
  #create a list of states and initialize a character array to hold the
  #required hospital names
  state <- levels(factor(dtfile[, 7]))
  hospital <- vector(mode="character") 
  
  for (i in seq(state)) {
    hospital[i] <- rankhospital(state[i], outcome, num)
  }
  data.frame(hospital, state)
  
}