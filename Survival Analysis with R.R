setwd("C:/Users/Obayomi/Documents/Statistical analysis with R")
HF <- read.csv("simulated-HF.csv", header = TRUE, sep = ',')
dim(HF)
head(HF)
install.packages("survival")
library(survival) # this is the cornerstone command for survival analysis in R
library(ggplot2)
gender <- as.factor(HF$gender)
fu_time <- HF$fu_time # continuous variable (numeric)
death <- HF$death # binary variable (numeric)
km_fit <- survfit(Surv(fu_time, death) ~ 1)

plot(km_fit)

summary(km_fit, times = c(1:7,30,60,90*(1:10)))

km_gender_fit <- survfit(Surv(fu_time, death) ~ gender)

plot(km_gender_fit)

survdiff(Surv(fu_time, death) ~ gender, rho = 0)
age <- HF$age

age_65plus <- ifelse(HF$age>=65,1,0) # dichotomise age
table(age_65plus, exclude = NULL) # inspect the numbers - always a good idea
table(age,age_65plus, exclude = NULL) # check - an even better idea...

survdiff(Surv(fu_time, death) ~ age_65plus, rho = 0)

install.packages("survminer")

library(survminer)
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = HF) # take variables straight from HF
summary(cox)
ethnicgroup <- as.factor(HF$ethnicgroup)
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

levels(ethnicgroup)<-c(levels(ethnicgroup),"8") # add level 8 to the factor
ethnicgroup[is.na(ethnicgroup)] <- "8" # Change NA to "None"
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

t <- table(gender, exclude=NULL)
addmargins(t) # adds the total (a "sum" column)
round(100*prop.table(t),digits=1) # get %s rounded to 1dp

copd <- as.factor(HF$copd)
t1 <- table(copd, exclude=NULL)
addmargins(t1) # adds the total (a "sum" column)
round(100*prop.table(t1),digits=1) # get %s rounded to 1dp

prior_dnas <- HF$prior_dnas
t2 <- table(prior_dnas, exclude = NULL)
addmargins(t2)
round(100*prop.table(t2), digits = 1)

cox1 <- coxph(Surv(fu_time, death) ~ age + gender + copd + prior_dnas + ethnicgroup)
summary(cox1)

quintile <- as.factor(HF$quintile)
cox2 <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile + ethnicgroup)
summary(cox2)

table(quintile, exclude=NULL)
t3 <- table(quintile,death)
t3
round(100*prop.table(t3,1),digits=1)
quintile <- relevel(quintile, ref = 2)

quintile_5groups <- HF$quintile
quintile_5groups[quintile_5groups==0] <- 5
quintile_5groups <- as.factor(quintile_5groups)
table(quintile_5groups, exclude=NULL)

cox3 <- coxph(Surv(fu_time, death) ~ age + gender + copd + quintile_5groups + ethnicgroup)
summary(cox3)
quintile_5groups[quintile_5groups==0] <- NA # set the zeroes to missing

cox4 <- coxph(Surv(fu_time, death) ~ age + gender + copd + ethnicgroup)
summary(cox4)

#Checking Proportionality Assumption
cox.zph(fit, transform = "km", global = TRUE)
fit <- coxph(Surv(fu_time, death) ~ copd) # fit the desired model
temp <- cox.zph(fit) # apply the cox.zph function to the desired model
print(temp)
plot(temp)
km_fit <- survfit(Surv(fu_time, death) ~ gender)
autoplot(km_fit)
plot(km_fit, xlab = "time", ylab = "Survival probability") # label the axes 

#Deviance Residual
res.cox <- coxph(Surv(fu_time, death) ~ age)
ggcoxdiagnostics(res.cox, type = "dfbeta", linear.predictions = FALSE, ggtheme = theme_bw())

res.cox <- coxph(Surv(fu_time, death) ~ age) 
ggcoxdiagnostics(res.cox, type = "deviance", linear.predictions = FALSE, ggtheme = theme_bw())                

ggcoxfunctional(Surv(fu_time, death) ~ age + log(age) + sqrt(age), data = HF)

fit <- coxph(Surv(fu_time, death) ~ gender + tt(gender)) # "tt" is the time-transform function
summary(fit)
