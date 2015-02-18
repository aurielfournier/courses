#Biometry Lab = Mancova
setwd("~/Downloads")
#graphing package 
library(ggplot2)
#read in the data
part = read.csv("partridge.csv", header=TRUE)
#subset what we are interested in
part = part[,c("treatmen", "longev", "thorax")]
#testing assumptions
plot(part)
boxplot(part)
hist(part$longev) #nicely normally distributed
hist(part$treatmen) #uniform distribution
hist(part$thorax) #slightly skewed to the right
part$longev10 = log10(part$longev)
hist(part$longev10)

#this is just a one way anova to look at the difference between longevity and mating partner treatment
lm1 = lm(data=part, longev10~treatmen)
summary(lm1) #so there is a significant difference between the different treatments (number of mating partners broken into categories)
#look at residuals
plot(lm1)
#homogeneity of variance assumption, looking for interaction

lm2 = lm(data=part, treatmen~thorax)
summary(lm2) # this is not significant, so throax does not explain treatmen, so we can continue

#linearity assumptions
ggplot()+
  geom_point(data=part, aes(x=longev, y=thorax, group=treatmen, colour=treatmen))

#ancova looking at the impact of mating partner treatment and thorax lenght on longetivy 
lm3 = lm(data=part, longev10 ~ treatmen+thorax)
summary(lm3)

#check for paralell slopes
lm4 = lm(data=part, longev10~ treatmen*thorax )
summary(lm4) #thorax is highly significant

#since there is an interaction, the F value increases, teh Rsquared value increases and the pvalue decreases in lm4
#compared to lm1, so it was beneficial to do, thsi is not always the case

