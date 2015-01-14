pollutantmean <- function(directory, pollutant, id = 1:332) {
  file.names <- list.files(path = directory, full.names=T)
  dat <- data.frame()
  for (i in id) {
    dat <- rbind(dat, read.csv(file.names[i]))
  }
  
  mean(dat[,pollutant], na.rm=TRUE)
}