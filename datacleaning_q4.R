# Quiz 4
setwd("c:/Users/dpitchmen/Coursera/")
acsData <- read.csv(file="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")
nameslist <- strsplit(x = names(acsData), split="wgtp")
nameslist[[123]]

gdp <- read.csv("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", skip = 4)
gdp2 <- select(gdp, X, X.1, X.3, X.4)
names(gdp2) = c("Country", "Rank", "CountryName", "GDP")
gdp3 <- head(gdp2, 190)
mean(gdp3$GDP)

length(grep("^United", gdp3$CountryName))

gdpedu_fiscalEnd <- gdpedu[grep("iscal", gdpedu$Special.Notes),])
gdpedu_fiscalJune <- gdpedu_fiscalEnd[grep("une ", gdpedu_fiscalEnd$Special.Notes),]

library(quantmod)
amzn <- getSymbols("AMZN",auto.assign=FALSE)
sampleTimes <- index(amzn)
length(sampleTimes)
bool1 <- year(sampleTimes) == 2012
length(sampleTimes[bool1]) # 250
bool2 <- weekdays(sampleTimes)=="Monday"
length(sampleTimes[bool1 & bool2]) # 47