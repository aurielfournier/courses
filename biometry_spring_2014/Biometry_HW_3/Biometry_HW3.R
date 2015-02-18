# Biometry Homework # 3
#Auriel Fournier
# Spring 2014
# Multiple Regression

library(Hmisc) #for the correlation stuff
library(AICcmodavg)#for AIC ranking
library(ggplot2) #for graphing
library(reshape) #for reshaping/melting 

setwd("C:/Users/avanderlaar/Dropbox/R/Biometry_HW_3")
hw3 = read.csv("biometryhw3.csv", header=T)

#subset just what we are interested in
hw3 = subset(hw3, hw3$stream=="Bear"|hw3$stream=="Cave"|hw3$stream=="Falling")
#subset just the variables we are interested in, ignoring the ones that have missing values
hw3 = subset(hw3, select=c("depth","substrate","velocity","totdenarea"))

#visual examination
#puts the data into long form, instead of wide form
mel = melt(hw3)

#assumption of homogeneity of variance
ggplot()+  geom_boxplot(data=mel, aes(x=variable, y=value))

#assumption of normality
ggplot() +  geom_histogram(data=mel, aes(x=value, fill=variable), position=position_dodge()) +facet_wrap(~variable, scales="free")

#depth is skewed
#substrate is ok
#velocity is skewed heavily
#totdenarea is skewed heavily

hw3s = data.frame(hw3)
#transform to get close to a normal distribution
hw3s$depth4 = hw3s$depth^(1/4)
hw3s$velocity4 = hw3$velocity^(1/4)
hw3s$totdenarea4 = hw3$totdenarea^(1/4)

hw3s = subset(hw3s, select=c("depth4","cover4","velocity4", "totdenarea4","canopy","do","spc","substrate","temp"))
melts = melt(hw3s)

#look at everything again
ggplot() +  geom_histogram(data=melts, aes(x=value, fill=variable), position=position_dodge()) +facet_wrap(~variable,scales="free")
#these look much better, though spc is still a problem child

#pearson's correlation 
cor = rcorr(as.matrix(hw3s), type="pearson")

#because of the correlation
#do
#spc
#cover
#velocity

#subset just what we are actually going to use
hw3f = subset(hw3s, select=c("temp","depth4","canopy","substrate","totdenarea4"))

#candidate models put into a list
cand.mod = list()
cand.mod[[1]] = lm(totdenarea4 ~ temp+depth4+canopy+substrate, data=hw3f)
cand.mod[[2]] = lm(totdenarea4 ~ temp+depth4+canopy,  data=hw3f)
cand.mod[[3]] = lm(totdenarea4 ~ temp+depth4+substrate, data=hw3f)
cand.mod[[4]] = lm(totdenarea4 ~ temp+canopy+substrate, data=hw3f)
cand.mod[[5]] = lm(totdenarea4 ~ depth4+canopy+substrate, data=hw3f)
cand.mod[[6]] = lm(totdenarea4 ~ temp+depth4, data=hw3f)
cand.mod[[7]] = lm(totdenarea4 ~ temp+canopy, data=hw3f)
cand.mod[[8]] = lm(totdenarea4 ~ temp+substrate, data=hw3f)
cand.mod[[9]] = lm(totdenarea4 ~ canopy+substrate, data=hw3f)
cand.mod[[10]] = lm(totdenarea4 ~ depth4+canopy, data=hw3f)
cand.mod[[11]] = lm(totdenarea4 ~ depth4+substrate, data=hw3f)
cand.mod[[12]] = lm(totdenarea4 ~ substrate, data=hw3f)
cand.mod[[13]] = lm(totdenarea4 ~ depth4, data=hw3f)
cand.mod[[14]] = lm(totdenarea4 ~ canopy, data=hw3f)
cand.mod[[15]] = lm(totdenarea4 ~ temp, data=hw3f)


#list of the names for the models
names = c("model 1", "model 2", "model 3", "model 4", "model 5", "model 6", "model 7","model 8","model 9", "model 10", "model 11", "model 12", "model 13", "model 14","model 15")

#generates AIC table
aictab(cand.mod, names, sort=T)   

#look in more detail at the top three models
summary(cand.mod[[8]]) # substrate is significant, and the R^2 is .1252, so not great
summary(cand.mod[[3]]) # substrate is significant again, but not hte others, R^2 .1542, so also not great

par(mfrow=c(2,4))
plot(cand.mod[[8]])
#the residuals don't look to bad, 
plot(cand.mod[[3]])
#these also look pretty god, 
