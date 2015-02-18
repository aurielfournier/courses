#power analysis from Lab 3 of Biometry Spring 2014

setwd("C:/Users/avanderlaar/Dropbox/R/Biometry/Biometry_Lab2_Power_Analysis")

library(pwr)
library(compute.es)


trout = read.csv('Trout.csv', header=TRUE)
beetle = read.csv('Beetle.csv', header=TRUE)
crayfish = read.csv('Crayfish.csv', header=TRUE)

#two sided t-test power analysis of trout

#calculate effect size
e.s.trout = mean(trout$c.growth) - mean(trout$e.growth)

#gives you the current power of your test
pwr.t.test(n=NULL, d=e.s.trout, sig.level=0.05, power=0.8, alternative="two.sided")
#this will throw an error
#Error in uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+07)) : 
#f() values at end points not of opposite sign
#this means that you need a really tiny sample size

#try changing d (effect size) so that it is much smaller
pwr.t.test(n=NULL, d=5, sig.level=0.05, power=0.8, alternative="two.sided")
#and now it runs, because we actually need some samples now
#note that n is the sample size in each group

#beetle data

head(beetle)

#total # of beetles
totalb = beetle$Bright.Red + beetle$Not.BR

#proportion of Bright.Red
pro.br = beetle$Bright.Red/totalb

#proportion of Not.Br
pro.not = beetle$Not.BR/totalb

#effect size
e.s.beetle = pro.br-pro.not

#power analysis for two proportions
pwr.2p.test(h=e.s.beetle , n=NULL, sig.level=0.5, power=0.8, alternative="two.sided")

#crayfish data

summary(crayfish)


cray05 = crayfish[crayfish$Year==2005,]
cray06 = crayfish[crayfish$Year==2006,]
cray07 = crayfish[crayfish$Year==2007,]



pwr.anova.test(k=3 , n=NULL , f=e.s.crayfish , sig.level=0.5, power=0.8)
