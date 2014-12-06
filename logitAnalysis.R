library(kernlab)
library(dplyr)
data(spam)

# Perform the subsampling
set.seed(3435)
spam <- mutate(spam, trainIndicator = rbinom(nrow(spam), size = 1, prob = 0.5))
table(spam$trainIndicator)

trainSpam <- filter(spam, trainIndicator == 1)
testSpam <- filter(spam, trainIndicator == 0)

# now we'll work with the training set
table(trainSpam$type)

#plots
plot(trainSpam$capitalAve ~ trainSpam$type) # not useful, try log10 to the y axis 
plot(log10(trainSpam$capitalAve) ~ trainSpam$type) #more capital letters in spam
plot(log10(trainSpam[, 1:4] + 1)) # matrix plot with first 4 predictors  ** TRY THIS WITH METABIO2.CSV

#clustering
hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)

hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)

# Statistical prediction/modeling Statistical prediction/modeling
trainSpam$numType = as.numeric(trainSpam$type) - 1  # turn out type into a 0 or 1 (w/o -1 it's actually 1 & 2)
costFunction <- function(x, y) {
  sum(x != (y > 0.5))
} 

cvError = rep(NA, 55)
library(boot)
for (i in 1:55) {
  lmFormula = reformulate(names(trainSpam)[i], response = "numType")
  glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
  cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}
## Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]

## Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)
## Get predictions on the test set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])
## Classify as `spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

## Classification table
table(predictedSpam, testSpam$type)

#predictedSpam nonspam spam
#nonspam    1346  458
#spam         61  449

## Error rate
(61 + 458)/(1346 + 458 + 61 + 449)
