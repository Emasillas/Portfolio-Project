library(tidyverse)
getwd()
setwd("C:/Users/Obayomi/Documents/Statistical analysis with R")

COPD <- read.csv("COPD_student_dataset.csv", header = TRUE, sep = ',')
hist(COPD$MWT1Best)
hist(COPD$MWT1Best, main = "Histogram of MWT1Best", xlab = "MWT1Best", breaks = 12)

subset(COPD, MWT1Best >650)
subset(COPD, MWT1Best >650 | MWT1Best<150)

hist(COPD$FEV1, main = "Histogram of FEV1", xlab = "FEV1")

list("Summary" = summary(COPD$MWT1Best), "Mean" = mean(COPD$MWT1Best, na.rm=TRUE
    ), "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), "Range" = range(
    COPD$MWT1Best, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$MWT1Best, 
    na.rm=TRUE))

list("Summary" = summary(COPD$FEV1), "Mean" = mean(COPD$FEV1, na.rm=TRUE
), "Standard Deviation" = sd(COPD$FEV1, na.rm=TRUE), "Range" = range(
  COPD$FEV1, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$FEV1, 
  na.rm=TRUE))

# calculate the correlation coefficient and have a look at the scatterplot of the two variables.
plot(COPD$FEV1, COPD$MWT1Best, xlab = "FEV1", ylab = "MWT1Best")

cor.test(COPD$FEV1, COPD$MWT1Best, use = "complete.obs", method = "pearson")

cor.test(COPD$FEV1, COPD$MWT1Best, use = "complete.obs", method = "spearman")

# To Compare with AGE varaible

hist(COPD$AGE, main = "Histogram of AGE", xlab = "AGE")

list("Summary" = summary(COPD$AGE), "Mean" = mean(COPD$AGE, na.rm=TRUE
), "Standard Deviation" = sd(COPD$AGE, na.rm=TRUE), "Range" = range(
  COPD$AGE, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$AGE, 
                                                       na.rm=TRUE))

plot(COPD$AGE, COPD$MWT1Best, xlab = "AGE", ylab = "MWT1Best")

cor.test(COPD$AGE, COPD$MWT1Best, use = "complete.obs", method = "pearson")

cor.test(COPD$AGE, COPD$MWT1Best, use = "complete.obs", method = "spearman")

# Linear Regression Model

MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data = COPD)

summary(MWT1Best_FEV1) #Getting the summary

confint(MWT1Best_FEV1) #To view 95% Confidence Interval
par(mfrow=c(1,1)) #To view all 4 plots in one output
plot(MWT1Best_FEV1)

#Linear regression model for MWT1Best and AGE
MWT1Best_AGE <- lm(MWT1Best~AGE, data = COPD)
summary(MWT1Best_AGE)
confint(MWT1Best_AGE)
plot(MWT1Best_AGE)
predictedVals <- predict(MWT1Best_AGE) #Get predicted values for the model
residualVals <- residuals(MWT1Best_AGE)#Get residual values for the model

hist(residualVals, main = "Histogram of Residual Values", xlab = "Residual Values")

#Multiple Regression
MWT1Best_FEV1_AGE <- lm(MWT1Best~FEV1+AGE, data = COPD)
summary(MWT1Best_FEV1_AGE)
confint(MWT1Best_FEV1_AGE)

#Including FVC
hist(COPD$FVC, main = "Histogram of FVC", xlab = "FVC")
list("Summary" = summary(COPD$FVC), "Mean" = mean(COPD$FVC, na.rm=TRUE
), "Standard Deviation" = sd(COPD$FVC, na.rm=TRUE), "Range" = range(
  COPD$FVC, na.rm=TRUE), "Inter-Quartile Range" = IQR(COPD$FVC,na.rm=TRUE))

plot(COPD$FVC, COPD$MWT1Best, xlab = "FVC", ylab = "MWT1Best")

cor.test(COPD$FVC, COPD$MWT1Best, use = "complete.obs", method = "pearson")

cor.test(COPD$FVC, COPD$MWT1Best, use = "complete.obs", method = "spearman")

#Linear Regression for FVC
MWT1Best_FVC <- lm(MWT1Best~FVC, data = COPD)
predictedVals_FVC <- predict(MWT1Best_FVC)
residualVals_FVC <- residuals(MWT1Best_FVC)
summary(MWT1Best_FVC)
confint(MWT1Best_FVC)
plot(MWT1Best_FVC)

#Multiple Regression of FVC and AGE
MWT1Best_FVC_AGE <- lm(MWT1Best~FVC+AGE, data = COPD)
summary(MWT1Best_FVC_AGE)
confint(MWT1Best_FVC_AGE)

#Multiple Regression of FEV1, FVC and AGE
MWT1Best_FEV1_FVC_AGE <- lm(MWT1Best~FEV1+FVC+AGE, data = COPD)
summary(MWT1Best_FEV1_FVC_AGE)
confint(MWT1Best_FEV1_FVC_AGE)

#Checking for Collinearity
cor.test(COPD$FEV1, COPD$FVC, use="complete.obs", method="spearman")  

#Pairwise Correlation
my_data <- COPD[,c("AGE", "PackHistory", "FEV1", "FEV1PRED", "FVC", "CAT", "HAD", "SGRQ")]
cor_matrix <- cor(my_data) #Create a correlation matrix of the variable that are to be analyzed
cor_matrix # View the correlation matrix
round(cor_matrix,2) # Round the values to 2 decimal places

pairs(~AGE+PackHistory+FEV1+FEV1PRED+FVC+CAT+HAD+SGRQ, data = COPD) # Command to produce correlation plot

CrossTable(COPD$hypertension, COPD$IHD) # Examine associations

# Check for Collinearlity
imcdiag()

#Interactions between binary variables
COPD$hypertension <- as.integer(COPD$hypertension)
COPD$AtrialFib <- as.integer(COPD$AtrialFib)
DAF <- COPD$hypertension * COPD$AtrialFib #Creating a new variable DAF
r1 <- lm(MWT1Best~factor(hypertension)+factor(AtrialFib)+factor(hypertension * AtrialFib), data=COPD) #Linear regression
summary(r1)
confint(r1)