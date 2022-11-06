getwd()
setwd("C:/Users/Obayomi/Documents/Statistical analysis with R")
h <- read.csv(file = "C:/Users/Obayomi/Documents/Statistical analysis with R/final-diabetes-data-for-R-_csv_2.csv", header=TRUE, sep=',')
dim(h)
colnames(h)
chol <- h$chol
gender <- as.factor(h$gender)
dm <- as.factor(h$dm)
t <- table(gender) # store the tabulation for further manipulation
addmargins(t) # this will sum up the gender totals to give an overall total and print the results
round(prop.table(t),digits=3) # get proportions rounded to 3dp
dm2 <- factor(dm, exclude=NULL) # make new factor from the old one
table(dm2) # display the counts including the missings (NAs)
summary(chol)
height <- h$height
weight <- h$weight
summary(height)
summary(weight)
height.si <- height*0.0254
weight.si <- weight*0.453592
bmi <- weight.si/height.si^2
summary(bmi)
bmi_categorised <- ifelse(bmi < 18.5, "underweight",
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal",
                                 ifelse(bmi > 25 & bmi <=30, "overweight",
                                        ifelse(bmi > 30, "obese", NA))))
#Confirm if bmi_categorised worked
table(bmi_categorised, exclude = NULL)

#Frequencies of dm by bmi_categorised
dm_by_bmi_categorised <- table(bmi_categorised, dm2, exclude = NULL)
dm_by_bmi_categorised
# with the row percentages 
round(100 * prop.table(dm_by_bmi_categorised, margin = 1), digits = 1)

#Categorising age
age <- h$age
age_categorised <- ifelse(age < 45, "under 45",
                          ifelse(age >=45 & age <= 64, "45-64",
                                 ifelse(age >=65 & age <=74, "65-74",
                                        ifelse(age >= 75, "75 or over", NA))))
table(age_categorised, exclude = NULL)
#Frequencies of gender with age_categorised
gender_by_age_categorised <- table(age_categorised, gender, exclude = NULL)
gender_by_age_categorised
#With row percentages
round(100 * prop.table(gender_by_age_categorised), digits = 1)
# displaying the age frequencies by gender 
round(100 * prop.table(gender_by_age_categorised, margin = 2), digits = 1)
# optional extra check for the extra cautious! 
head(cbind(age_categorised, age))

#Logistic Regression

m <- glm(dm~1, family = binomial(link = logit))
summary(m)
table(m$y)
m <- glm(dm ~ gender, family=binomial (link=logit))
summary(m)
m <- glm(dm~age, family = binomial(link = logit))
summary(m)
# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm)
dm_by_age

# output the frequencies of diabetes status by age 
freq_table <- prop.table(dm_by_age, margin = 1)
freq_table

# calculate the odds of having diabetes 
odds <- freq_table[, "yes"]/freq_table[, "no"]

# calculate the log odds 
logodds <- log(odds)

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds)

location <- h$location
location <- as.factor(location)
dm_table <- table(dm, location)
dm_table
round(100 * prop.table(dm_table, margin = 2), digits = 1)

i <- glm(dm~location, family = binomial(link = logit))
summary(i)

levels(location)

i$coefficients
exp(i$coefficients)

#Multiple Logistic Regression
d <- density(age)
plot(d,main = "") # gives warnings but the “main” argument suppresses the ugly default title 

#Cholesterol
summary(chol)
chol.no.na <- chol[is.na(chol)==0]
d <- density(chol.no.na)
plot(d,main = "")

#HDL
HDL <- h$hdl
summary(HDL)
HDL.no.na <- HDL[is.na(HDL)==0]
d1 <- density(HDL.no.na)
plot(d1,main = "")

#BMI
summary(bmi)
bmi.no.na <- bmi[is.na(bmi)==0]
d2 <- density(bmi.no.na)
plot(d2,main = "")

#Gender
gender <- as.factor(h$gender)
dm_by_gender <- table(gender,dm)
dm_by_gender_prop <- prop.table(dm_by_gender, margin = 1)
dm_by_gender_prop
#odds of having diabetes by gender
odds_gender <- dm_by_gender_prop[,"yes"]/dm_by_gender_prop[,"no"]
#calculate log odds
logodds_gender <- log(odds_gender)
#plot the log odds of having diabetes by gender
dotchart(logodds_gender)
plot(as.factor(names(logodds_gender)), logodds_gender)

#Age
dm_by_age
dm_by_age_prop <- prop.table(dm_by_age, margin = 1)
odds_age <- dm_by_age_prop[,"yes"]/dm_by_age_prop[,"no"]
logodds_age <- log(odds_age)
# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_age_prop), logodds_age)

#Age categorised
age_categorised <- factor(age_categorised, levels = c("under 45", "45-64", "65-74", "75 or over"))
dm_by_age_grouped <- table(age_categorised, dm)
age_grouped_prop <- prop.table(dm_by_age_grouped, margin = 1)
odds_age_grouped <- age_grouped_prop[,"yes"]/age_grouped_prop[,"no"]
logodds_age_grouped <- log(odds_age_grouped)
dotchart(logodds_age_grouped)

#Cholesterol And Diabetes
dm_by_chol <- table(chol, dm)
dm_by_chol_prop <- prop.table(dm_by_chol, margin = 1)
odds_chol <- dm_by_chol_prop[,"yes"]/dm_by_chol_prop[,"no"]
logodds_chol <- log(odds_chol)
plot(rownames(dm_by_chol_prop), logodds_chol, xlim=c(150, 300))

#Cholesterol categorised
chol_categorised <- ifelse(chol < 200, "healthy",  
                           ifelse(chol < 240, "borderline high", 
                                  ifelse(chol >= 240, "high", NA)))
chol_categorised <- factor(chol_categorised, levels = c("healthy", "borderline high", "high"))
dm_by_chol_categorised <- table(chol_categorised, dm)
dm_by_chol_categorised_prop <- prop.table(dm_by_chol_categorised, margin = 1)
odds_chol_categorised <- dm_by_chol_categorised_prop[, "yes"]/dm_by_chol_categorised_prop[, "no"]
logodds_chol_categorised <- log(odds_chol_categorised)
dotchart(logodds_chol_categorised)

#BMI and Diabetes
bmi_categorised <- factor(bmi_categorised, levels = c("underweight", "normal", "overweight","obese"))
dm_by_bmi_categorised <- table(bmi_categorised, dm)
dm_by_bmi_categorised_prop <- prop.table(dm_by_bmi_categorised, margin = 1)
odds_bmi_categorised <- dm_by_bmi_categorised_prop[, "yes"]/dm_by_bmi_categorised_prop[, "no"]
logodds_bmi_categorised <- log(odds_bmi_categorised)
dotchart(logodds_bmi_categorised)

# Multiple Regression
m1 <- glm(dm ~ age + gender + bmi, family=binomial (link=logit))
summary(m1)
exp(confint(m1))

insurance <- h$insurance
m2 <- glm(dm ~ age + chol + insurance, family = binomial(link = logit))
summary(m2)
exp(confint(m2))
levels(insurance)
insurance <- as.factor(insurance)
exp(m2$coefficients)

#Model Fit
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit))

# check your model 
summary(full_model)

# run a null model 
null_model <- glm(dm ~ 1, family=binomial (link=logit)) 

# check 
summary(null_model)

# calculate McFadden's R-square
R2 <- 1-logLik(full_model)/logLik(null_model)

# print it 
R2

#CStatistics
# install a package 
install.packages("DescTools")
library(DescTools)
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 
# check your model 
summary(full_model)
# generate the c-statistic 
Cstat(full_model)

# H-L test 

# install package "ResourceSelection" 
install.packages("ResourceSelection")
library(ResourceSelection)
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit))
full_model$y
# run Hosmer-Lemeshow test 
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10) 
HL
# plot the observed vs expected number of cases for each of the 10 groups 
plot(HL$observed[,"y1"], HL$expected[,"yhat1"])
# plot the observed vs expected number of noncases for each of the 10 groups 
plot(HL$observed[,"y0"], HL$expected[,"yhat0"])

# plot observed vs. expected prevalence for each of the 10 groups 
plot(x = HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]), 
     y = HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"]))

# install package("generalhoslem") 
install.packages("generalhoslem")
library(generalhoslem)
# run Hosmer-Lemeshow test 
logitgof(obs = full_model$y, exp = fitted(full_model), g = 10)


#To test deviance

# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit))

# analyse table of deviance 
anova(full_model, test = "Chisq")

#Backward Elimination
##### Make the variables and run the models #####

dm <- as.factor(h[,"dm"])
insurance <- as.factor(h[,"insurance"])# let's say 0=none, 1=gov, 2=private 
fh <- as.factor(h[,"fh"]) # 1=FH, 0=no FH 
smoking <- as.factor(h[,"smoking"]) # 1,2,3 
chol <- h[,'chol'] 
hdl <- h[,'hdl'] 
ratio <- h[,'ratio'] 
location <- as.factor(h[,'location']) 
age <- h[,'age'] 
gender <- as.factor(h[,'gender']) 
frame <- as.factor(h[,'frame']) 
systolic <- h[,'bp.1s'] 
diastolic <- h[,'bp.1d'] 

model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic, family = binomial(link = logit))
summary(model) 

anova(model, test = "Chisq")

#Dropping BP variables
model1 <- glm(dm ~ age + bmi + chol + hdl, family = binomial(link = logit))
summary(model1)

# strange that systolic and diastolic are not significant... 

cor.test(systolic, hdl) # not significant 

cor.test(systolic, bmi) # significant

cor.test(systolic, chol) # very significant

cor.test(systolic, age) # extremely significant

#Dropping age instead of systolic BP
model2 <- glm(dm ~ systolic + bmi + chol + hdl, family = binomial(link = logit))
summary(model2)

#Adding all
model3 <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic + gender + location + frame + smoking, family = binomial(link = logit))
summary(model3)
