library("tidyverse")
library("readr")
rm(list = ls()) #removes all variables previously stored
library("Hmisc") #import

Covid19 <- read.csv("C:/Users/Obayomi/Videos/R Programming/Datasets/COVID19_line_list_data.csv")

describe(Covid19) #Hmisc command

#cleaned up death column
Covid19$death_dummy <- as.integer(Covid19$death !=0)

#death rate
sum(Covid19$death_dummy)/ nrow(Covid19)

#AGE
#claim: people who die are older
dead = subset(Covid19, death_dummy == 1)
alive = subset(Covid19, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# Is this statistically significant?
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.95)
# if p-value < 0.05, we reject the null hypothesis
# here p-value ~ 0, so we reject the null hypothesis
# and conclude that this is statistically significant.

#GENDER
#claim: gender has no effect
men = subset(Covid19, gender == "male")
women = subset(Covid19, gender == "female")
mean(men$death_dummy, na.rm = TRUE)#8.5%
mean(women$death_dummy, na.rm = TRUE)#3.7%

# Is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)
#99% confidence: men have 0.8% to 8.8% higher chances of death
# p-value = 0.002 < 0.05, so this is statistically significant
