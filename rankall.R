rankall <- function(outcome, num = "best") {
  ocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
    stop("invalid outcome")
  }
  
  if (outcome == "heart attack") {
    OutcomeCode <- 11
  } else if (outcome == "heart failure") {
    OutcomeCode <- 17
  } else if (outcome == "pneumonia") {
    OutcomeCode <- 23
  }
  
  HospitalStates <- ocm$State
  Hospitals <- ocm$Hospital.Name
  Outcome <- ocm[,OutcomeCode]
  df <- cbind(HospitalStates, Hospitals, Outcome)
  
  dfwona <- subset(df, df[,3] != "Not Available")
  
  # order the data frame alphabetically by Hospital names
  d <- dfwona[order(dfwona[,2]),]
  
  # again order the data frame based on mortality rate
  df <- d[order(as.numeric(d[,3])),]
  
  States <- sort(unique(df[,1]))
  hospitals <- vector()
  for (state in States) {
    d <- subset(df, df[,1] == state)
    
    if (num != "best" && num != "worst" && num > nrow(d)) {
      hospName <- "<NA>"
    } else {
      if (num == "best") {
        hospName <- d[[1,2]]
      } else if (num == "worst") {
        hospName <- d[[nrow(d),2]]
      } else {
        hospName <- d[[num,2]]
      }
    }
    hospitals <- append(hospitals, hospName)
  }
  
  retval <- data.frame(hospital=hospitals, state=States)
  return(retval)
}