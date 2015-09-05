   
   
   f_g <- gather(f, position, value, -c(ElapsedHours, DateFmt))
   f_mlt <- melt(f, id.vars = c("ElapsedHours", "DateFmt"), variable.name = "position")

 
 
  file.names <- list.files(path = directory, full.names=T)
  dat <- data.frame()
  for (i in 1:length(file.names)) {
    dat <- rbind(dat, read.csv(file.names[i]))
  }
  
 
 #####
 


regex <- "Cycle\\s+(\\d+)\\s+Day\\s+(\\d+)"
m <- regexec(regex, l$VisitName)
matchesList <- regmatches(l$VisitName, m)
l$Cycle <- as.numeric(unlist(lapply(matchesList, FUN=function(x){x[2]})))
l$Day <- as.numeric(unlist(lapply(matchesList, FUN=function(x){x[3]})))

match## Using library(stringr), much nicer
library(stringr)
matchList2 <- str_match_all(l$VisitName, regex)
l$Cycle <- as.numeric(unlist(lapply(matchesList, FUN=function(x){x[2]})))
l$Day <- as.numeric(unlist(lapply(matchesList, FUN=function(x){x[3]})))
##################

test <- eval[eval$SubjectID %in% c("xxx","yyy","zzz"), ]
test$ResultedDate_Fmt <- as.Date(test$ResultedDate, "%m/%d/%Y")
 ## ALT DATE ###
test$ResultedDate_Fmt2 <- parse_date_time(test$ResultedDate, "%m%d%Y %H%M")
 
maxDate <- sqldf('select distinct SubjectID, VisitName, Timepoint, Cohort, max(ColDateFmt) as ColDateFmt 
  from test group by SubjectID, VisitName, Timepoint, Cohort')
maxDate <- aggregate(ResultedDate_Fmt ~ SubjectID + VisitName + Timepoint + Cohort, data=test, FUN=max)

### dplyr stuff ####
MapInfo <- affyOutput %>% select(Chr, MapInfo) %>% distinct
MapInfo_oldWay <- distinct(affyOutput[c("Chr", "MapInfo")])

# verbose dplyr
dupSNPs <- arrange(dupSNPs, Chr, MapInfo)
# dplyr chain
alt <- dupSNPs %>% arrange(Chr, MapInfo)



### base graphics ###
plot(mpg ~ hp, data = mtcars, pch = 16, cex = .9)

### ggplot2 ###
# with qplot()
qplot(mpg, hp, data = mtcars, size = I(2))
# or with ggplot() + geom_point()
ggplot(mtcars, aes(mpg, hp), size = 2) + geom_point()
# or another solution:
ggplot(mtcars, aes(mpg, hp)) + geom_point(size = 2)


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
aggregate(len ~ dose + supp, data=tg, FUN=mean)

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

## mysql and UCSC web facing mysql db
library(RMySQL)
ucscDb <- dbConnect(MySQL(), user="genome", host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb, "show databases;")
dbDisconnect(ucscDb)

### dplyr http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
filter(flights, month == 1, day == 1); filter(flights, month == 1 | month == 2)

##### tabulate
xtabs( ~ Analyte + DilutionFactor, data=allData)  # or wrap the whole thing in ftable()
                    DilutionFactor
Analyte              10000x 1000x 100x 10x 1x
  CXCL4_50               82    82   82  82  0
  S100A8_127             82    82   82   0  0
  Adiponectin_1           0    82    0   0  0
  PARC_79                 0     0   82   0  0
  Leptin_3                0     0   82   0  0
  PAI_1_6                 0     0   82   0  0
  sICAM_16                0     0   82   0  0
  sVCAM_92                0     0   82   0  0
  TIMP_1_86               0     0   82   0  0
  
  addmargins(xtabs( ~ Analyte + DilutionFactor, data=allData))  # tally up the rows
  
## R E G R E S S I O N ########
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

regression through the origin and get the slope treating y as the outcome/dep and x as the regressor/indep.
f <- lm(y ~ 0 + x)  ## regular, but  f <- lm(y ~ 0 + x) to fit through origin
summary(f)

>>
Coefficients:
  Estimate Std. Error t value Pr(>|t|)
x   0.8263     0.5817   1.421    0.189
<<

so that is:  
y = mx + 0  # 0 is int at origin
y = 0.8263*x

plot(x,y)
##
data(mtcars)
f2 <- lm(mtcars$mpg ~ mtcars$wt)
plot(mtcars$wt, mtcars$mpg)
abline(f2, col="red")
>>
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  37.2851     1.8776  19.858  < 2e-16 ***
mtcars$wt    -5.3445     0.5591  -9.559 1.29e-10 ***
<<
mpg = -5.3445*wt + 37.2851

cov(mtcars$mpg, mtcars$wt) / (sd(mtcars$wt) * sd(mtcars$mpg)) == cor(mtcars$mpg, mtcars$wt)

## Norm
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xn <- (x - mean(x)) / sd(x)

## 
# adjusted estimate for the expected change in mpg comparing 8 cylinders to 4.
f <- lm(data=mtcars, mpg ~ factor(cyl) + wt)

#fit1: mpg as the outcome & cylinders as a factor variable and weight as confounder. 
#fit2: mpg as the outcome that considers the interaction between number of cylinders (as a factor variable) and weight. 
fit1 <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
fit2 <- lm(mpg ~ factor(cyl) + wt + interaction(factor(cyl), wt), data = mtcars)
comp <- anova(fit1, fit2) # null is that both models are the same
comp$Pr




