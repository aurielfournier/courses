#biometry
#homework 4
#ancova
#Auriel Fournier
# April 28, 2014


library(Hmisc) #for the correlation stuff
library(AICcmodavg)#for AIC ranking
library(ggplot2) #for graphing
library(reshape) #for reshaping/melting 

#set working directory
setwd("C:/Users/avanderlaar/Dropbox/R/Biometry_HW_4")

#read in homework file
hw4 = read.csv('hw4.csv')

#the assignment says that meal_amount is categorical, so this makes it a factor
hw4$meal_amount = as.factor(hw4$meal_amount)



#assumption of homogeneity of variance
ggplot()+  geom_boxplot(data=hw4, aes(x=meal_amount, y=duckling_weight))
hw4$age_f = as.factor(hw4$age)
ggplot()+  geom_boxplot(data=hw4, aes(x=age_f, y=duckling_weight))
#ok, so there is a problem here since variance is inscreasing with age and the spread is different


ggplot() +  geom_histogram(data=hw4, aes(x=duckling_weight), position=position_dodge()) 
#ok so we have a skewed distribution

hw4$duck2 = hw4$duckling_weight^(1/4)

ggplot() +  geom_histogram(data=hw4, aes(x=duck2), position=position_dodge()) 
#this is still funky, but is a bit better

#One Way ANOVAS
lm1 = lm(data=hw4, duck2~age) #is significant
summary(lm1)
lm2 = lm(data=hw4, duck2~meal_amount) # is not significant
summary(lm2)

#Look at the interaction between the two predictors
lm3 = lm(data=hw4, age~meal_amount) # not significant 
summary(lm3)

#linearity assumptions
ggplot() + geom_point(data=hw4, aes(x=age, y=duckling_weight, colour=meal_amount))+
  geom_line(data=hw4, aes(x=age, y=duckling_weight, colour=meal_amount))

lm4 = lm(data=hw4, duck2 ~ age+meal_amount)
summary(lm4)

#check for paralell slopes
lm5 = lm(data=hw4, duck2~ age*meal_amount )
summary(lm5) 


