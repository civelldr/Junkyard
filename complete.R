complete <- function(directory, id = 1:332) {
  
  #init some vectors
  nobs <- c()
  ids <- c()
  
  #grab only requested filenames
  file.names <- list.files(path = directory, full.names=T)[id]
  
  for (i in 1:length(file.names)) {
    
    data <- read.csv(file.names[i])
    n <- nrow(data[complete.cases(data),])
    id <- head(data$ID, n=1)
    
    # build onto the vectors for the output data frame
    nobs = c(nobs,n)
    ids = c(ids,id)
  }
  
  data.frame(id=ids, nobs=nobs)
  
}
