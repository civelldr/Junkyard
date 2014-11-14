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

runApp("App-1", host = "10.130.6.76", port=8080)
library(ggplot2)
qplot(data=mpg, displ, hwy, color=drv, geom=c("point","smooth"))
qplot(data=mpg, displ, hwy, color=drv, geom=c("point","smooth"), method=lm)

g <- ggplot(data = mpg, aes(displ, hwy))
g + geom_point() + geom_smooth(method=lm) + facet_grid(.~drv)
g + geom_point(color = "steelblue", size=4, alpha=0.5) + geom_smooth(method=lm) + facet_grid(.~drv) + xlab("DISPLACEMENT")
g + geom_point(aes(color = drv), size=4, alpha=0.5) + geom_smooth(method=lm) + xlab("DISPLACEMENT")

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
}
