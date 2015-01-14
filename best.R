## Questions 2 for Assignment 3


best <- function(state, outcome) {
  #Invalid outcome input type
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  #Get index for our given outcome string.
  if (outcome == "heart attack") { index = 11}
  else if (outcome == "heart failure") { index = 17}
  else index = 23
  
  #Read and coerce our dataset while suppressing warnings and removing NA's.
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  #Invalid state input or no observations
  if (!is.element(state, unique(data$State))) { 
    stop("invalid state")
  }  
  
  # clear up missing data and cast indexed data to numeric
  data[,index] <- suppressWarnings(as.numeric(data[,index]))
  data <- na.omit(data)
  
  
  #Slice our data by the given state and sort it by outcome and hospital name.
  sub <- subset(data, State==state)
  sub <- sub[order(sub[,index], na.last=TRUE),2]
  sub <- na.omit(sub)
  
  #Get hospital name with the lowest 30-day mortality rate.
  sub[1]
}
