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
Em <- aggregate(Emissions ~ year + fips, data = SCCNEI_Veh, FUN=function(x) c(med=median(x), sum=sum(x), mean=mean(x)))

cellLine <- read.csv("cellLine_With_Tissue.csv", header=T, na.strings="")

mutDB <- merge(mutDB, ERBB3Mut, all=T)
subset <- select(mutDB, cellline, ERBB3, RAS, BRAF, EGFR, PIK3CA, Site_Primary, Histology, Hist_Subtype1, tissue)
HER3Mutations <- filter(subset, !is.na(ERBB3))


write.csv(HER3Mutations, "HER3Mutations_31OCT14.csv", row.names = F)

mutDB[,c(1,4,6)] # take only the cell.line, erbb3 and ras columns

hema <- read.csv(file = "HEMA_PLOT_SUB.csv", header=T)

png(file="CombinedCohorts_Platelets.png", width=900, height=600)
  ggplot(hema, aes(x=factor(ElapsedDays_E), y=LBRES7, group=Current_Dose, color=Current_Dose)) + 
    geom_point(size=3) + 
    geom_line(aes(group=factor(Subject)), size=1) + 

    ylim(0,700) + 
dev.off()

  ggplot(dlbcl001_comp, aes(x=VISIT_TP, y=Pct_Chg_p4EBP1, group=Dose_Level, color=Dose_Level)) + 
    geom_point(size=4, alpha=0.6, position = position_jitter(w = 0.1)) + 
    # geom_line(aes(group=factor(Subject_Number)), size=1) +
    scale_x_discrete(limits=unique(dlbcl001$VISIT_TP)) +
    labs(x="Visit Name / Timepoint", y="% change in p4EBP1 from baseline") +
    ggtitle("CC-122-DLBCL001 Arm A") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(jsonlite)

setwd(dir = "\\\\ussfrspnetfls01\\users\\<CENSORED>\\Dev")
mycols <- rep("NULL", 42)
mycols[c(2,3,4,24)] = c("character", "character", "character", "numeric")
CC122Data <- read.table(file = "CC_122_DLBCL_001_Data_Dumps_24SEP2014_HEMA.csv", 
                        header = T, 
                        sep= ',', 
                        strip.white = T,
                        colClasses = mycols)
CC122JSON <- toJSON(CC122Data, pretty=T)
sink("CC122.json")
cat(CC122JSON)
sink()

### dplyr http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
filter(flights, month == 1, day == 1); filter(flights, month == 1 | month == 2)
