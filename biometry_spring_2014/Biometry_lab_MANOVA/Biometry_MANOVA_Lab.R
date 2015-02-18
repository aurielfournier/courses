#MANOVA Lab
#Biometry

library(ggplot2)
library(devtools)
library(digest)
library(gridExtra)

#set working directory
setwd("~/Downloads")

#read in data
data = read.csv("manova_lab.csv",header=T)

#subset out the boston mountains
boston = subset(data, data$stream=="Bear"|data$stream=="Cave"|data$stream=="Falling")

#subset only variables of interest
boston = subset(boston, select=c("stream","velocity","volume"))

#determine number of levels in each factor
unique(boston$stream) # Three Levels (Bear, Cave, Falling)


#check assumptions

plot(boston$velocity, boston$volume)
#this data has issues

par(mfrow=c(1,2))
hist(boston$velocity)
hist(boston$volume)
#data is heavily skewed

boston$vel4 = boston$velocity^(1/4)
boston$vol4 = boston$volume^(1/4)

#histograms
p1 = ggplot() +  geom_histogram(data=boston, aes(x=vel4, fill=stream), position=position_dodge())
#this is fine except for those 0's
#so maybe we'd need to consider a GLM and using a different distribution, but we're just going 
#to go with it
p2= ggplot() +   geom_histogram(data=boston, aes(x=vol4, fill=stream), position=position_dodge())
#this looks much improved
grid.arrange(p1,p2,ncol=1)

#box plots to look at distribution of variances
p3 = ggplot()+  geom_boxplot(data=boston, aes(x=stream, y=vel4, fill=stream))
#alrightly
p4 = ggplot()+  geom_boxplot(data=boston, aes(x=stream, y=vol4, fill=stream))
#looks good
grid.arrange(p3, p4, ncol=1)

univol = lm(vol4 ~ stream, data=boston) #so cave stream is significant # poor R^2
univel = lm(vel4 ~ stream, data=boston) #no variables significant, awful R^2

#look at residual plots
par(mfrow=c(4,2))
plot(univol)
plot(univel)
#everything looks good to me

#PCA
princomp(boston$vol4, boston$vel4)


#RUN THE MANOVA
manova = manova(cbind(vol4,vel4) ~ stream, data=boston)

summary(manova) #it's significant! horray

#pull out the residuals, check them out
res = manova$residuals
plot(res)
#not to shabby, a little bit of clumping, but overall it's fine

