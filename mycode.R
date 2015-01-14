myFunction <- function(){
  x <- rnorm(100)
  mean(x)
}

second <- function(x) {
  x + rnorm(length(x))
}

###### commands from Quiz 1
x <- c(17, 14, 4, 5, 13, 12, 10)
y <- x[x > 10] <- 4
data = read.csv("c:/Users/dpitchmen/Coursera/hw1_data.csv")
names(data)
head(data,2)
nrow(data)
tail(data,n=2)
summary(data)
subsetData <- subset(data, Ozone > 31 & Temp > 90 )
mean(x=data$Ozone)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  file.names <- list.files(path = directory, full.names=T)
  dat <- data.frame()
  for (i in id) {
    dat <- rbind(dat, read.csv(file.names[i]))
  }
  
  mean(dat[,pollutant], na.rm=TRUE)
}

testFxn <- function(id = 1:5) {
  for (i in id) {
    print(i)
  }
}

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

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  file.names <- list.files(path = directory, full.names=T)
  
  cr <- c()
  
  for (i in 1:length(file.names)) {
    
    data <- read.csv(file.names[i])
    data <- data[complete.cases(data),]
    id <- head(data$ID, n=1)
    
    if (nrow(data) > threshold) {
      c <- cor(data$sulfate, data$nitrate)
      cr <- c(cr,c)
    }
  
  }
  
  cr
  
}

########### week 3 stuff

library(datasets)
data(iris)

#### data cleaning quiz 2
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
data <- read.csv(file=url)
sqldf("select distinct AGEP from acs", drv='SQLite')

url <- "http://biostat.jhsph.edu/~jleek/contact.html"
con <- url(url)
htmlCode <- readLines(con)
htmlCode[10]
sapply(htmlCode[c(10,20,30,100)], nchar)



