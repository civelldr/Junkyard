###### commands from Quiz 1

x <- c(17, 14, 4, 5, 13, 12, 10)
y <- x[x > 10] <- 4
data = read.csv("c:/Users/dpitchmen/Coursera/hw1_data.csv")
names(data)
head(data,2)
nrows(data)
tail(data,n=2)
summary(data)
subsetData <- subset(data, Ozone > 31 & Temp > 90 )

# swirl
y <- x[!is.na(x) & x > 0]               # only the non-NA values in x that are > 0
my_data <- sample(c(y, z), 100)         # sample 100 values at random from vectors y and z
vect <- c(foo = 11, bar = 2, norf = NA) # named elements
tapply(flags$population, flags$landmass, summary) # summarize the population data by landmass

## data frame summary
dim(plants)
obj <- object.size(plants)
print(obj,units="Mb")
str(plants) 
summary(plants)
table(plants$Active_Growth_Period)

flips <- sample(c(0,1), size=100, replace=TRUE, prob=c(0.3, 0.7))
rbinom(1,size=100, prob=0.7) # number of heads in 100 tosses
rbinom(100,size=1, prob=0.7) # vector of coin flips where p of rolling 1 is 0.7
rnorm(n=10,mean=100,sd=25) # 10 random numbers from a dist with mean of 100 and sd of 25
my_pois <- replicate(100, rpois(5,10)) # 100 replicates of 5 numbers from a poisson dist with mean of 10

# dates
t3 <- "October 17, 1986 08:24"
t4 <- strptime(t3, "%B %d, %Y %H:%M")

testFxn <- function(id = 1:5) {
  for (i in id) {
    print(i)
  }
  
## best parser! 
install.packages("data.table")

DT <- fread(input = "ss06pid.csv", sep = "auto", header="auto")
  
## JSON file
sink(file = "c:\\users\\dpitchmen\\desktop\\cc122subset.csv")
C122Subset  ## JSON FILE
sink()
  
## CC122 data
myCols <- rep("NULL",42)
myCols[c(2,3,4,24)] = c("character","character","character","numeric")
Raw <- read.table(file='C:\\Users\\DPitchmen\\Desktop\\CC122-DLBCL-HEMA.csv', sep = ',', header = T, colClasses = myCols, strip.white = T)
write.table(Raw, file= "c:\\users\\dpitchmen\\desktop\\cc122subset.csv", sep = ",")

## mysql and UCSC web facing mysql db
library(RMySQL)
ucscDb <- dbConnect(MySQL(), user="genome", host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb, "show databases;")
dbDisconnect(ucscDb)

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



