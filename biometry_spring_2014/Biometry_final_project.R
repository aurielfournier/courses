# Effects of Wetland Impoundment Management and Vegetation Community on Sora Density in Missouri During Fall Migration
## Key Function & Detection
#set working directory - mac  
#setwd("~/Dropbox/R/Distance")  
#set working directory - windows  
setwd("C:/Users/avanderlaar/Dropbox/R/Distance")

#load required libraries
library(unmarked)
library(AICcmodavg)
library(ggplot2)
library(reshape2)
library(lattice)
library(sjPlot) 
library(grid)

# Key Function
#This set of models look at whether we should be using a hazard, hazard or halfnormal key function

# read in the sora observations
#this is a file with a row for every impoundment surveyed, and a set of 13 columns for each survey period. (13 because of the 0-12 cutpoints). 
sorakey <- read.csv("2013r3_sora.csv", header = T)
# read in the covariate data #organized by impoundment.
# a row for each impoundment and all of the covariate information summarized in columns
covkey <- read.csv("2013r3_cov.csv", header = T)

#these are the cutpoints, or the bins that the observations are summarized in. 
cutpt = as.numeric(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))  #the fartherest distance is 12

# Unmarked Data Frame
umfkey = unmarkedFrameGDS(y = sorakey, numPrimary = 8, siteCovs = covkey, survey = "line", 
                          dist.breaks = cutpt, unitsIn = "m", tlength = covkey$effortm)

#our models that we're trying. 
#lambda is abundance
#phi is availability
#p is detection
#mixture can be NB (Negative Binomial) or P (Poisson)
#data should be the unmarkedFrameGDS file generated above
#keyfun can be uniform, hazard or halfnorm

nullu = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1, data = umfkey, 
                  keyfun = "uniform", mixture = "NB", se = T)

nullhaz = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1, data = umfkey, 
                    keyfun = "hazard", mixture = "NB", se = T)

nullexp = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1, data = umfkey, 
                    keyfun = "exp", mixture = "NB", se = T)

nullhalf = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1, data = umfkey, 
                     keyfun = "halfnorm",mixture = "NB", se = T)

#creates the list and generates the AIC table
listkey = fitList(nullu, nullhaz, nullexp)
modelkey = modSel(listkey)
modelkeyd = as(modelkey,"data.frame")
write.csv(modelkeyd, "modelkey.csv")



#### Looks at how covariates impact Detection
# lambda is abundance
# phi is availability 
# p is detection
#since we're just looking at detection we'll plug things in just for p


# read in the sora observations, set up the same as in the Key Function model 
soradetect <- as.matrix(read.csv("2013r3_sora.csv", header = T))
# read in the covariate data #organized by impoundment.
covdetect <- read.csv("2013r3_cov.csv", header = T)
covdetectsub <- as.matrix(covdetect[,c("waterp", "woodp", "int", "waterd")])


#Standardize Variables
standard <- apply(covdetectsub,1,function(x)(x/mean(x))/sd(x))
transpose = as.data.frame(t(standard))

covsdetect = cbind(transpose, covdetect[,c("habtype", "dda", "dua", "dista", "effortm")])
covsdetect[4,1:4] = 0

cutpt = as.numeric(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))  #the fartherest distance is 12

# Unmarked Data Frame
umfdetect = unmarkedFrameGDS(y = soradetect, numPrimary = 8, siteCovs = covsdetect, 
                             survey = "line", dist.breaks = cutpt, unitsIn = "m", 
                             tlength =covsdetect$effortm)


nulldetect = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~1, data = umfdetect, 
                       keyfun = "hazard", mixture = "NB", se = T)

globaldetect = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~int + 
                           waterd + habtype + woodp + waterp+ dda + dua + dista - 1, 
                         data = umfdetect, keyfun = "hazard", mixture = "NB", se = T)

adetect = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~int + waterd + 
                      waterp + woodp - 1, data = umfdetect, 
                    keyfun = "hazard", mixture = "NB", se = T)

bdetect = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~int + waterd + 
                      waterp - 1, data = umfdetect, keyfun = "hazard", mixture = "NB", se = T)

cdetect = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~int + waterd - 
                      1, data = umfdetect, keyfun = "hazard", mixture = "NB", se = T)

ddetect = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~int - 1, 
                    data = umfdetect, keyfun = "hazard", mixture = "NB", se = T)

hdetect = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~habtype - 
                      1, data = umfdetect, keyfun = "hazard", mixture = "NB", se = T)

idetect = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~habtype + 
                      int - 1, data = umfdetect, keyfun = "hazard", mixture = "NB", se = T)

jdetect = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~habtype + 
                      int + waterd - 1, data = umfdetect, keyfun = "hazard", mixture = "NB", 
                    se = T)

kdetect = gdistsamp(lambdaformula = ~1, phiformula = ~1,pformula = ~habtype + int + dua - 1, 
                    data = umfdetect, keyfun = "hazard", mixture = "NB", se = T)

ldetect = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~habtype + int + dda - 1, 
                    data = umfdetect, keyfun = "hazard", mixture = "NB", se = T)

mdetect = gdistsamp(lambdaformula = ~1, phiformula = ~1, pformula = ~habtype + int + dista - 1, 
                    data = umfdetect, keyfun = "hazard", mixture = "NB", se = T)

listdetect = fitList(nulldetect, adetect, bdetect, cdetect, ddetect, 
                     hdetect, idetect, jdetect, kdetect, ldetect, mdetect)
modeldetect = modSel(listdetect)
modeldetectd = as(modeldetect,"data.frame")
write.csv(modeldetectd, "modeldetect.csv")

# 2012 Round 2

#read in the sora observations
sora12r2 <- as.matrix(read.csv('2012r2_sora.csv', header=TRUE))
#read in the covariate data #organized by impoundment.
cov12r2 <- read.csv('2012r2_cov.csv', header=TRUE)

cov12r2sub <- as.matrix(cov12r2[,c("waterp", "woodp", "int", "waterd")])


#Standardize Variables
standard <- apply(cov12r2sub,1,function(x)(x/mean(x))/sd(x))
transpose = as.data.frame(t(standard))

cov12r2 = cbind(transpose, cov12r2[,c("habtype", "dda", "dua", "dista", "effortm")])

#sora12r2[,13:36]
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12

#Unmarked Data Frame
umf12r2 = unmarkedFrameGDS(y=sora12r2, 
                           numPrimary=3,
                           siteCovs = cov12r2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12r2$effortm,
)

##models (all the models for this and the following rounds are the same)
null12r2 = gdistsamp(lambdaformula = ~1,
                     phiformula = ~1, 
                     pformula = ~1, 
                     data = umf12r2, keyfun = "hazard",
                     mixture="NB",se = T)

global12r2 = gdistsamp(lambdaformula = ~int+habtype+dda+dua+dista-1, 
                       phiformula = ~1,
                       pformula = ~  1, 
                       data = umf12r2, 
                       keyfun = "hazard", 
                       mixture="NB",
                       se = T)

a12r2 = gdistsamp(lambdaformula = ~int+waterd-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf12r2, keyfun = "hazard", mixture="NB",se = T)



d12r2 = gdistsamp(lambdaformula = ~int-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf12r2, keyfun = "hazard", mixture="NB",se = T)

 
 h12r2 = gdistsamp(lambdaformula = ~habtype-1, 
                   phiformula = ~1, 
                   pformula = ~  1,
                   data = umf12r2, keyfun = "hazard", mixture="NB",se = T)
 
 i12r2 = gdistsamp(lambdaformula = ~habtype+int-1, 
                   phiformula = ~1, 
                   pformula = ~  1,
                   data = umf12r2, keyfun = "hazard", mixture="NB",se = T)

j12r2 = gdistsamp(lambdaformula = ~habtype+int+waterd-1, 
                  phiformula = ~1, 
                  pformula = ~ 1,
                  data = umf12r2, keyfun = "hazard", mixture="NB",se = T)

k12r2 = gdistsamp(lambdaformula = ~habtype + int + dua - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf12r2, keyfun = "hazard", mixture = "NB", se = T)

l12r2 = gdistsamp(lambdaformula = ~habtype + int + dda - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf12r2, keyfun = "hazard", mixture = "NB", se = T)

 m12r2 = gdistsamp(lambdaformula = ~habtype + int + dista - 1, 
                   phiformula = ~1, 
                   pformula = ~  1, 
                   data = umf12r2, keyfun = "hazard", mixture = "NB", se = T)


list12r2 = fitList(null12r2, global12r2, a12r2, d12r2,  h12r2, i12r2, j12r2, k12r2, l12r2)
model12r2 = modSel(list12r2)


# 2012 Round 3
#read in the sora observations
sora12r3 <- read.csv('2012r3_sora.csv', header=T)
#read in the covariate data #organized by impoundment.
cov12r3 <- read.csv('2012r3_cov.csv', header=T)

cov12r3sub <- as.matrix(cov12r3[,c("waterp", "woodp", "int", "waterd")])


#Standardize Variables
standard <- apply(cov12r3sub,1,function(x)(x/mean(x))/sd(x))
transpose = as.data.frame(t(standard))

cov12r3 = cbind(transpose, cov12r3[,c("habtype", "dda", "dua", "dista", "effortm")])

#the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12
#Unmarked Data Frame
umf12r3 = unmarkedFrameGDS(y=sora12r3, 
                           numPrimary=3,
                           siteCovs = cov12r3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov12r3$effortm,
)

null12r3 = gdistsamp(lambdaformula = ~1,
                     phiformula = ~1, 
                     pformula = ~ 1, 
                     data = umf12r3, keyfun = "hazard", mixture="NB",se = T)

global12r3 = gdistsamp(lambdaformula = ~int+waterd+dda+dua+dista-1, 
                       phiformula = ~1,
                       pformula = ~ 1, 
                       data = umf12r3, keyfun = "hazard", mixture="NB",se = T)

a12r3 = gdistsamp(lambdaformula = ~int+waterd-1, 
                  phiformula = ~1, 
                  pformula = ~ 1,
                  data = umf12r3, keyfun = "hazard", mixture="NB",se = T)


b12r3 = gdistsamp(lambdaformula = ~int+waterd-1, 
                  phiformula = ~1, 
                  pformula = ~ 1,
                  data = umf12r3, keyfun = "hazard", mixture="NB",se = T)


d12r3 = gdistsamp(lambdaformula = ~int-1, 
                  phiformula = ~1, 
                  pformula = ~ 1,
                  data = umf12r3, keyfun = "hazard", mixture="NB",se = T)


i12r3 = gdistsamp(lambdaformula = ~int-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf12r3, keyfun = "hazard", mixture="NB",se = T)


k12r3 = gdistsamp(lambdaformula = ~int + dua - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf12r3, keyfun = "hazard", mixture = "NB", se = T)

l12r3 = gdistsamp(lambdaformula = ~int + dda - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf12r3, keyfun = "hazard", mixture = "NB", se = T)

m12r3 = gdistsamp(lambdaformula = ~int + dista - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf12r3, keyfun = "hazard", mixture = "NB", se = T)


list12r3 = fitList(null12r3, global12r3, a12r3, b12r3, d12r3, i12r3,  k12r3, l12r3, m12r3)
model12r3 = modSel(list12r3)


# 2013 Round 1

sora13r1 <- as.matrix(read.csv('2013r1_sora.csv', header=T))
#read in the covariate data #organized by impoundment.
cov13r1 <- read.csv('2013r1_cov.csv', header=T)

cov13r1sub <- as.matrix(cov13r1[,c("waterp", "woodp", "int", "waterd")])


#Standardize Variables
standard <- apply(cov13r1sub,1,function(x)(x/mean(x))/sd(x))
transpose = as.data.frame(t(standard))

covs13r1 = cbind(transpose, cov13r1[,c("habtype", "dda", "dua", "dista", "effortm")])

#the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12
#Unmarked Data Frame
umf13r1 = unmarkedFrameGDS(y=sora13r1, 
                           numPrimary=8,
                           siteCovs = cov13r1,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r1$effortm,
)

null13r1 = gdistsamp(lambdaformula = ~1,
                     phiformula = ~1, 
                     pformula = ~ 1, 
                     data = umf13r1, keyfun = "hazard", mixture="NB",se = TRUE
)

global13r1 = gdistsamp(lambdaformula = ~int+waterd+dua+dda+dista+habtype-1, 
                       phiformula = ~1,
                       pformula = ~  1, 
                       data = umf13r1, keyfun = "hazard", mixture="NB",se = TRUE)

a13r1 = gdistsamp(lambdaformula = ~int+waterd-1, 
                  phiformula = ~1, 
                  pformula = ~ 1,
                  data = umf13r1, keyfun = "hazard", mixture="NB",se = TRUE)

d13r1 = gdistsamp(lambdaformula = ~int-1, 
                  phiformula = ~1, 
                  pformula = ~ 1,
                  data = umf13r1, keyfun = "hazard", mixture="NB",se = TRUE)

e13r1 = gdistsamp(lambdaformula = ~short-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r1, keyfun = "hazard", mixture="NB",se = TRUE)


h13r1 = gdistsamp(lambdaformula = ~habtype-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r1, keyfun = "hazard", mixture="NB",se = TRUE)

i13r1 = gdistsamp(lambdaformula = ~habtype+int-1, 
                  phiformula = ~1, 
                  pformula = ~ 1,
                  data = umf13r1, keyfun = "hazard", mixture="NB",se = TRUE)

j13r1 = gdistsamp(lambdaformula = ~habtype+int+waterd-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r1, keyfun = "hazard", mixture="NB",se = TRUE)

k13r1 = gdistsamp(lambdaformula = ~habtype + int + dua - 1, 
                  phiformula = ~1, 
                  pformula = ~ 1, 
                  data = umf13r1, keyfun = "hazard", mixture = "NB", se = TRUE)

l13r1 = gdistsamp(lambdaformula = ~habtype + int + dda - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf13r1, keyfun = "hazard", mixture = "NB", se = TRUE)

m13r1 = gdistsamp(lambdaformula = ~habtype + int + dista - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf13r1, keyfun = "hazard", mixture = "NB", se = TRUE)


list13r1 = fitList(null13r1, global13r1, a13r1, d13r1, h13r1, i13r1, j13r1, k13r1, l13r1, m13r1)
model13r1 =modSel(list13r1)


# 2013 Round 2

#read in the sora observations
sora13r2 <- as.matrix(read.csv('2013r2_sora.csv', header=T))
#read in the covariate data #organized by impoundment.
cov13r2 <- read.csv('2013r2_cov.csv', header=T)

cov13r2sub <- as.matrix(cov13r2[,c("waterp", "woodp", "int", "waterd")])


#Standardize Variables
standard <- apply(cov13r2sub,1,function(x)(x/mean(x))/sd(x))
transpose = as.data.frame(t(standard))

covs13r2 = cbind(transpose, cov13r2[,c("habtype", "dda", "dua", "dista", "effortm")])
covs13r2[4,1:4]=0
sora13r2 = sora13r2[c(2:11,13:22),]
cov13r2 = covs13r2[c(2:11,13:22),]


# #the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12

#Unmarked Data Frame
umf13r2 = unmarkedFrameGDS(y=sora13r2, 
                           numPrimary=8,
                           siteCovs = cov13r2,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r2$effortm,
)

null13r2 = gdistsamp(lambdaformula = ~1,
                     phiformula = ~1, 
                     pformula = ~  1, 
                     data = umf13r2, keyfun = "hazard", mixture="NB",se = TRUE
)

global13r2 = gdistsamp(lambdaformula = ~int+habtype+dda+dua+dista+waterd-1, 
                       phiformula = ~1,
                       pformula = ~ 1, 
                       data = umf13r2, keyfun = "hazard", mixture="NB",se = TRUE)

a13r2 = gdistsamp(lambdaformula = ~int+waterd-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r2, keyfun = "hazard", mixture="NB",se = TRUE)


d13r2 = gdistsamp(lambdaformula = ~int-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r2, keyfun = "hazard", mixture="NB",se = TRUE)


h13r2 = gdistsamp(lambdaformula = ~habtype-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r2, keyfun = "hazard", mixture="NB",se = TRUE)

i13r2 = gdistsamp(lambdaformula = ~habtype+int-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r2, keyfun = "hazard", mixture="NB",se = TRUE)

j13r2 = gdistsamp(lambdaformula = ~habtype+int+waterd-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r2, keyfun = "hazard", mixture="NB",se = TRUE)

k13r2 = gdistsamp(lambdaformula = ~habtype + int + dua - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf13r2, keyfun = "hazard", mixture = "NB", se = TRUE)

l13r2 = gdistsamp(lambdaformula = ~habtype + int + dda - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf13r2, keyfun = "hazard", mixture = "NB", se = TRUE)

m13r2 = gdistsamp(lambdaformula = ~habtype + int + dista - 1, 
                  phiformula = ~1, 
                  pformula = ~ 1, 
                  data = umf13r2, keyfun = "hazard", mixture = "NB", se = TRUE)


list13r2 = fitList(null13r2, global13r2, a13r2, d13r2, h13r2, i13r2, j13r2, k13r2, l13r2, m13r2)
model13r2 = modSel(list13r2)


# 2013 Round 3

#read in the sora observations
sora13r3 <- as.matrix(read.csv("2013r3_sora.csv", header=T))
#read in the covariate data #organized by impoundment.
cov13r3 <- read.csv('2013r3_cov.csv', header=T)

cov13r3sub <- as.matrix(cov13r3[,c("int", "waterd", "waterp", "woodp")])


#Standardize Variables
standard <- apply(cov13r3sub,1,function(x)(x/mean(x))/sd(x))
transpose = as.data.frame(t(standard))

covs13r3 = cbind(transpose, cov13r3[,c("habtype", "dda", "dua", "dista", "effortm")])


# #the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12
#Unmarked Data Frame
umf13r3 = unmarkedFrameGDS(y=sora13r3, 
                           numPrimary=8,
                           siteCovs = cov13r3,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r3$effortm,
)

null13r3 = gdistsamp(lambdaformula = ~1,
                     phiformula = ~1, 
                     pformula = ~  1, 
                     data = umf13r3, keyfun = "hazard", mixture="NB",se = T)

global13r3 = gdistsamp(lambdaformula = ~int+waterd++dda+dua+dista+short+habtype-1, 
                       phiformula = ~1,
                       pformula = ~  1, 
                       data = umf13r3, keyfun = "hazard", mixture="NB",se = T)

a13r3 = gdistsamp(lambdaformula = ~int+waterd-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r3, keyfun = "hazard", mixture="NB",se = T)


d13r3 = gdistsamp(lambdaformula = ~int-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r3, keyfun = "hazard", mixture="NB",se = T)

e13r3 = gdistsamp(lambdaformula = ~short-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r3, keyfun = "hazard", mixture="NB",se = T)


h13r3 = gdistsamp(lambdaformula = ~habtype-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r3, keyfun = "hazard", mixture="NB",se = T)

i13r3 = gdistsamp(lambdaformula = ~habtype + int -1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r3, keyfun = "hazard", mixture="NB",se = T)

j13r3 = gdistsamp(lambdaformula = ~habtype + int + waterd -1, 
                  phiformula = ~1, 
                  pformula = ~ 1,
                  data = umf13r3, keyfun = "hazard", mixture="NB",se = T)

k13r3 = gdistsamp(lambdaformula = ~habtype + int + dua - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf13r3, keyfun = "hazard", mixture = "NB", se = T)

l13r3 = gdistsamp(lambdaformula = ~habtype + int + dda - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf13r3, keyfun = "hazard", mixture = "NB", se = T)

m13r3 = gdistsamp(lambdaformula = ~habtype + int + dista - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf13r3, keyfun = "hazard", mixture = "NB", se = T)

list13r3 = fitList(null13r3, global13r3, a13r3,  d13r3, h13r3, i13r3, j13r3, k13r3, l13r3, m13r3)
model13r3 = modSel(list13r3)


# 2013 Round 4

sora13r4 <- as.matrix(read.csv('2013r4_sora.csv', header=T))
#read in the covariate data #organized by impoundment.
cov13r4 <- read.csv('2013r4_cov.csv', header=T)

cov13r4sub <- as.matrix(cov13r4[,c("waterp", "woodp", "int", "waterd")])


#Standardize Variables
standard <- apply(cov13r4sub,1,function(x)(x/mean(x))/sd(x))
transpose = as.data.frame(t(standard))

covs13r4 = cbind(transpose, cov13r4[,c("habtype", "dda", "dua", "dista", "effortm")])
cov13r4 = covs13r4[1:5,]
sora13r4 = sora13r4[1:5,]
# #the distance bins
cutpt = as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)) #the fartherest distance is 12
#Unmarked Data Frame
umf13r4 = unmarkedFrameGDS(y=sora13r4, 
                           numPrimary=8,
                           siteCovs = cov13r4,
                           survey="line", 
                           dist.breaks=cutpt,  
                           unitsIn="m", 
                           tlength=cov13r4$effortm,
)

null13r4 = gdistsamp(lambdaformula = ~1,
                     phiformula = ~1, 
                     pformula = ~  1, 
                     data = umf13r4, keyfun = "hazard", mixture="NB",se = TRUE)

global13r4 = gdistsamp(lambdaformula = ~int+habtype+waterd+dda+dua+dista-1, 
                       phiformula = ~1,
                       pformula = ~  1, 
                       data = umf13r4, keyfun = "hazard", mixture="NB",se = TRUE)

a13r4 = gdistsamp(lambdaformula = ~int+waterd-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r4, keyfun = "hazard", mixture="NB",se = TRUE)

d13r4 = gdistsamp(lambdaformula = ~int-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r4, keyfun = "hazard", mixture="NB",se = TRUE)

h13r4 = gdistsamp(lambdaformula = ~habtype-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r4, keyfun = "hazard", mixture="NB",se = TRUE)

i13r4 = gdistsamp(lambdaformula = ~habtype+int-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r4, keyfun = "hazard", mixture="NB",se = TRUE)

j13r4 = gdistsamp(lambdaformula = ~habtype+int+waterd-1, 
                  phiformula = ~1, 
                  pformula = ~  1,
                  data = umf13r4, keyfun = "hazard", mixture="NB",se = TRUE)

k13r4 = gdistsamp(lambdaformula = ~habtype + int + dua - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf13r4, keyfun = "hazard", mixture = "NB", se = TRUE)

l13r4 = gdistsamp(lambdaformula = ~habtype + int + dda - 1, 
                  phiformula = ~1,
                  pformula = ~  1, 
                  data = umf13r4, keyfun = "hazard", mixture = "NB", se = TRUE)

m13r4 = gdistsamp(lambdaformula = ~habtype + int + dista - 1, 
                  phiformula = ~1, 
                  pformula = ~  1, 
                  data = umf13r4, keyfun = "hazard", mixture = "NB", se = TRUE)


list13r4 = fitList(null13r4, global13r4, a13r4,  d13r4,h13r4, i13r4, j13r4, k13r4, l13r4, m13r4)
model13r4 = modSel(list13r4)





################################
# Real Data Frames
################################

real12r2 = data.frame(int=cov12r2$int,
                      habtype=cov12r2$habtype)

real12r3 = data.frame(int=cov12r3$int,
                      habtype=cov12r3$habtype,
                      waterd = cov12r3$waterd,
                      dista = cov12r3$dista,
                      dua = cov12r3$dua,
                      dda = cov12r3$dda)

real13r1 = data.frame(int=cov13r1$int,
                      habtype = cov13r1$habtype)

real13r2 = data.frame(int=cov13r2$int,
                      habtype = cov13r2$habtype,
                      dua = cov13r2$dua)

real13r3 = data.frame(int=cov13r3$int,
                      habtype=cov13r3$habtype,
                      dua=cov13r3$dua)

real13r4 = data.frame(int=cov13r4$int,
                      habtype = cov13r4$habtype,
                      dua=cov13r4$dua,
                      waterd=cov13r4$waterd)

############################
## Predictor Data Frames
############################

int12r2 <- data.frame(int = rep(seq(min(cov12r2$int), max(cov12r2$int), length=nrow(cov12r2)),times=1),
                      habtype=factor(x=c("ms"), levels = c("pe", "ms")))

hab12r2 <- data.frame(int = mean(cov12r2$int),
                      habtype=rep(unique(cov12r2$habtype), each=nrow(cov12r2), times=1))

int12r3 <- data.frame(int = rep(seq(min(cov12r3$int), max(cov12r3$int), length=nrow(cov12r3)), each=nrow(cov12r3)),
                      habtype =factor(x=c("ms"), levels = c("pe", "ms")),
                      waterd = mean(cov12r3$waterd), 
                      dua = rep(unique(cov12r3$dua),each=nrow(cov12r3)*5),
                      dda = rep(unique(cov12r3$dda), each=nrow(cov12r3)*4),
                      dista = rep(unique(cov12r3$dista), each=nrow(cov12r3)*5))

water12r3 <- data.frame(int =mean(cov12r3$int),
                        habtype=rep(unique(cov12r3$habtype), each=nrow(cov12r3)),
                        waterd = rep(seq(min(cov12r3$waterd),max(cov12r3$waterd),length=nrow(cov12r3)), each=nrow(cov12r3)), 
                        dua="a",
                        dda="a",
                        dista="a")

hab12r3 <- data.frame(int = mean(cov12r3$int),
                      habtype=c("ms", "pe"),
                      waterd = mean(cov12r3$waterd), 
                      dua=c("a","g"),
                      dda="a",
                      dista="a")

dudddisth12r3 <- data.frame(int = mean(cov12r3$int),
                            habtype=rep(unique(cov12r3$habtype), each=nrow(cov12r3)),
                            waterd = mean(cov12r3$waterd), 
                            dua=rep(unique(cov12r3$dua),each=nrow(cov12r3)*5),
                            dda=rep(unique(cov12r3$dda), each=nrow(cov12r3)*4),
                            woodp=mean(cov12r3$woodp),
                            waterp=mean(cov12r3$waterp),
                            dista=rep(unique(cov12r3$dista), each=nrow(cov12r3)*5))

int13r1 <- data.frame(int = rep(seq(min(cov13r1$int),max(cov13r1$int), length=nrow(cov13r1)),times=1),
                      habtype=rep(c("ms","pe","up"),each=nrow(cov13r1)))

hab13r1 <- data.frame(int = mean(cov13r1$int),
                      habtype=factor(x=c("ms", "pe"), levels = c("pe", "ms", "up")))

int13r2 <- data.frame(int = rep(seq(min(cov13r2$int),max(cov13r2$int), length=nrow(cov13r2))),
                      habtype=rep(c("ms","pe","up"),each=nrow(cov13r2)),
                      dua="a")

hab13r2 <- data.frame(int = mean(cov13r2$int),
                      habtype=c("pe", "ms", "pe","ms"))

du13r2 <- data.frame(int = mean(cov13r2$int),
                     habtype=rep(unique(cov13r2$habtype)),
                     dua=rep(unique(cov13r2$dua), each=3))

int13r3 <- data.frame(int = rep(seq(min(cov13r3$int),max(cov13r3$int), length=nrow(cov13r3))),
                      habtype=rep(c("ms","pe"),each=nrow(cov13r3)),
                      dua = "f")

hab13r3 <- data.frame(int = mean(cov13r3$int),
                      habtype=unique(cov13r3$habtype),
                      dua = c("f", "h"))

du13r3 <- data.frame(int = mean(cov13r3$int),
                     habtype=rep(unique(cov13r3$habtype)),
                     dua=rep(unique(cov13r3$dua), each=2))

int13r4 <- data.frame(int = seq(min(cov13r4$int),max(cov13r4$int), length=nrow(cov13r4)),
                      habtype=rep(c("ms","pe","up"),each=nrow(cov13r4)),
                      waterd =mean(cov13r4$waterd),  
                      dua="a")

water13r4 <- data.frame(int =mean(cov13r4$int),
                        habtype=rep(unique(cov13r4$habtype), each=nrow(cov13r4)),
                        waterd =seq(min(cov13r4$int),max(cov13r4$int), length=nrow(cov13r4)),  
                        dua="a")

hab13r4 <- data.frame(int = mean(cov13r4$int),
                      habtype=unique(cov13r4$habtype),
                      waterd =mean(cov13r4$waterd),  
                      dua="a")

du13r4 <- data.frame(int = mean(cov13r4$int),
                     habtype=rep(unique(cov13r4$habtype)),
                     dua=rep(unique(cov13r4$dua), each=3),
                     waterd=mean(cov13r4$waterd))

########################
# Predictions!
########################

#####2012 Round 2 #############
lm12r2 <- predict(i12r2, type="lambda", newdata=real12r2, appendData=T)
lm12r2$round <- rep("2012r2")
pint12r2<- predict(i12r2, type="lambda", newdata=int12r2, appendData=T)
pint12r2$round <- rep("2012r2")
phab12r2 <-predict(i12r2, type="lambda", newdata=hab12r2, appendData=T)
phab12r2$round <- rep("2012r2")
#####2012 Round 3##################
lm12r3 <- predict(global12r3, type="lambda", newdata=real12r3, appendData=T)
lm12r3$round <- rep("2012r3")
pint12r3 <- predict(global12r3, type="lambda", newdata=int12r3, appendData=T)
pint12r3$round <-rep("2012r3")
phab12r3 <- predict(global12r3, type="lambda", newdata=hab12r3, appendData=T)
phab12r3$round <-rep("2012r3")
pdd12r3 <- predict(global12r3, type="lambda", newdata=dudddisth12r3, appendData=T)
pdd12r3$round <-rep("2012r3")
pdu12r3 <- predict(global12r3, type="lambda", newdata=dudddisth12r3, appendData=T)
pdu12r3$round <-rep("2012r3")
pdist12r3 <- predict(global12r3, type="lambda", newdata=dudddisth12r3, appendData=T)
pdist12r3$round <-rep("2012r3")
pwater12r3 <- predict(global12r3, type="lambda", newdata=water12r3, appendData=T)
pwater12r3$round <-rep("2012r3")
#######2013 Round 1###########
lm13r1 <- predict(i13r1, type="lambda", newdata=real13r1, appendData=T)
lm13r1$round <- rep("2013r1")
pint13r1 <- predict(i13r1, type="lambda", newdata=int13r1, appendData=T)
pint13r1$round <- rep("2013r1")
phab13r1 <- predict(i13r1, type="lambda", newdata=hab13r1, appendData=T)
phab13r1$round <- rep("2013r1")
########2013 Round 2##########
lm13r2 <- predict(k13r2, type="lambda", newdata=real13r2, appendData=T)
lm13r2$round <- rep("2013r2")
pint13r2 <- predict(i13r2, type="lambda", newdata=int13r2, appendData=T)
pint13r2$round <- rep("2013r2")
phab13r2<- predict(k13r2, type="lambda", newdata=hab13r2, appendData=T)
phab13r2$round <- rep("2013r2")
pdu13r2 <- predict(k13r2, type="lambda", newdata=du13r2, appendData=T)
pdu13r2$round <- rep("2013r2")
########2013 Round 3#########
lm13r3 <- predict(k13r3, type="lambda", newdata=real13r3, appendData=T)
lm13r3$round <- rep("2013r3")
phab13r3<- predict(i13r3, type="lambda", newdata=hab13r3, appendData=T)
phab13r3$round <- rep("2013r3")
pint13r3 <- predict(i13r3, type="lambda", newdata=int13r3, appendData=T)
pint13r3$round <- rep("2013r3")
pdu13r3 <- predict(k13r3, type="lambda", newdata=du13r3, appendData=T)
pdu13r3$round <- rep("2013r3")
#########2013 ROund 4##########
lm13r4 <- predict(j13r4, type="lambda", newdata=real13r4, appendData=T)
lm13r4$round <- rep("2013r4")
pint13r4<- predict(j13r4, type="lambda", newdata=int13r4, appendData=T)
pint13r4$round <- rep("2013r4")
phab13r4 <- predict(j13r4, type="lambda", newdata=hab13r4, appendData=T)
phab13r4$round <- rep("2013r4")
pwater13r4 <- predict(j13r4, type="lambda", newdata=water13r4, appendData=T)
pwater13r4$round <- rep("2013r4")
pdu13r4 <- predict(k13r4, type="lambda", newdata=du13r4, appendData=T)
pdu13r4$round <- rep("2013r4")

##########################
# Merge Data Together
##########################

#rbind is a function that binds things together by their rows (cbind is the matching column function)
phab12r2 = subset(phab12r2, select=c("Predicted", "SE", "habtype", "round"))
phab13r1 = subset(phab13r1, select=c("Predicted", "SE", "habtype", "round"))
phab13r2 = subset(phab13r2, select=c("Predicted", "SE", "habtype", "round"))
phab13r3 = subset(phab13r3, select=c("Predicted", "SE", "habtype", "round"))
phab13r4 = subset(phab13r4, select=c("Predicted", "SE", "habtype", "round"))
habrbind = rbind(phab12r2, phab13r1, phab13r2,phab13r3, phab13r4)

pint12r2 = subset(pint12r2, select=c("Predicted", "SE", "int", "round", "habtype"))
pint12r3 = subset(pint12r3, select=c("Predicted", "SE", "int", "round", "habtype"))
pint13r1 = subset(pint13r1, select=c("Predicted", "SE", "int", "round", "habtype"))
pint13r2 = subset(pint13r2, select=c("Predicted", "SE", "int", "round", "habtype"))
pint13r3 = subset(pint13r3, select=c("Predicted", "SE", "int", "round", "habtype"))
pint13r4 = subset(pint13r4, select=c("Predicted", "SE", "int", "round", "habtype"))
intrbind = rbind(pint12r2, pint12r3, pint13r1,  pint13r2, pint13r3, pint13r4)

pwater13r4 = subset(pwater13r4, select=c("Predicted", "SE", "waterd", "round","habtype"))
waterrbind = rbind(pwater13r4)

pdu13r2 = subset(pdu13r2, select=c("Predicted", "SE", "dua", "round","habtype"))
pdu13r3 = subset(pdu13r3, select=c("Predicted", "SE", "dua", "round", "habtype"))
pdu13r4 = subset(pdu13r4, select=c("Predicted", "SE", "dua", "round","habtype"))
durbind = rbind(pdu13r2, pdu13r3, pdu13r4)
durbind[52:58,1] = c(0,0,0,0,0,0,0)
durbind[52:58,2] = c(0,0,0,0,0,0,0)
durbind[52,3] = "b"
durbind[53,3] = "d"
durbind[54,3] = "e"
durbind[55,3] = "f"
durbind[56,3] = "g"
durbind[57,3] = "h"
durbind[58,3] = "i"
durbind[52:58,4] = "2013r4"
durbind[52:58,5] = "ms"


#color blind color palette
cbPalette <- c("#cc4c02","#8c2d04","#8c96c6","#88419d", "#810f7c","#4d004b")

##################################
# Habitat Graph
##################################
realh12r2 = subset(lm12r2, select=c("Predicted", "SE", "habtype", "round"))
realh12r3 = subset(lm12r3, select=c("Predicted", "SE", "habtype", "round"))
realh13r1 = subset(lm13r1, select=c("Predicted", "SE", "habtype", "round"))
realh13r2 = subset(lm13r2, select=c("Predicted", "SE", "habtype", "round"))
realh13r3 = subset(lm13r3, select=c("Predicted", "SE", "habtype", "round"))
realh13r4 = subset(lm13r4, select=c("Predicted", "SE", "habtype", "round"))
realhrbind = rbind(realh12r2, realh12r3, realh13r1, realh13r2, realh13r3, realh13r4)
meltrealh = melt(realhrbind)
castrealh = cast(meltrealh, round ~ variable | habtype , mean)
habmsreal = castrealh$ms
habpereal = castrealh$pe
habmsreal$habtype = "ms"
habpereal$habtype = "pe"
habreal = rbind(habpereal, habmsreal)
habreal[12,1] = "2012r3"
habreal[12,2] = 0
habreal[12,3] = 0
habreal[12,4] = "pe"
habreal$title = c("2012 Round 2", "2013 Round 1", "2013 Round 2", "2013 Round 3", "2013 Round 4", "2012 Round 2", "2012 Round 3", "2013 Round 1", "2013 Round 2", "2013 Round 3", "2013 Round 4","2012 Round 3" )
habreal$hab = c("perennial emergent","perennial emergent","perennial emergent","perennial emergent","perennial emergent","moist soil","moist soil","moist soil","moist soil","moist soil","moist soil","perennial emergent")

ggplot(habreal, aes(x=title, y=Predicted, fill=hab),) + 
  geom_bar( position=position_dodge(), stat="identity", colour="black", size=.8)+
  geom_errorbar(aes(ymin=Predicted-SE, ymax=Predicted+SE),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9),
                colour="black") +
  xlab("Round of Surveys Fall 2013") +
  ylab("Sora per Hectare") +
  ggtitle("The Effect of Habitat Type\non Sora Density Across Rounds") +
  theme(plot.title = element_text(colour="black",size=40), #plot title
        axis.text.x = element_text(colour="black", size=20), #x axis labels
        axis.text.y = element_text(colour="black",size=20), #y axis labels
        axis.title.x = element_text(colour="black", size=30, vjust=-.5), #x axis title
        axis.title.y = element_text(colour="black",size=30), #y axis title
        legend.text = element_text(colour="black", size=30), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="white"), #legend background color
        legend.position = "top",
        legend.direction= "horizontal",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "white" ), #plot background color
        panel.background = element_rect(fill = "white"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA))+ #x axis grid line color
  guides(fill=guide_legend(title="Habitat Type")) #retitles the legend


##############################
## Interspersion Graph
##############################

reali12r2 = subset(lm12r2, select=c("Predicted", "SE", "int", "round", "habtype"))
reali12r3 = subset(lm12r3, select=c("Predicted", "SE", "int", "round", "habtype"))
reali13r1 = subset(lm13r1, select=c("Predicted", "SE", "int", "round", "habtype"))
reali13r2 = subset(lm13r2, select=c("Predicted", "SE", "int", "round", "habtype"))
reali13r3 = subset(lm13r3, select=c("Predicted", "SE", "int", "round", "habtype"))
reali13r4 = subset(lm13r4, select=c("Predicted", "SE", "int", "round", "habtype"))
realirbind = rbind(reali12r2, reali12r3, reali13r1, reali13r2, reali13r3, reali13r4)
realirbind = subset(realirbind, habtype=="ms")
realirbind$title = ifelse(realirbind$round=="2012r2","2012 Round 2", 
                          ifelse(realirbind$round=="2012r3","2012 Round 3",
                                 ifelse(realirbind$round=="2013r1", "2013 Round 1",
                                        ifelse(realirbind$round=="2013r2", "2013 Round 2",
                                               ifelse(realirbind$round=="2013r3", "2013 Round 3",
                                                      ifelse(realirbind$round=="2013r4", "2013 Round 4", NA))))))


intrbind = subset(intrbind, habtype=="ms")
intrbind$hab = ifelse(intrbind$habtype=="ms","moist soil", "perennial emergent")
intrbind$title = ifelse(intrbind$round=="2012r2","2012 Round 2", 
                        ifelse(intrbind$round=="2012r3","2012 Round 3",
                               ifelse(intrbind$round=="2013r1", "2013 Round 1",
                                      ifelse(intrbind$round=="2013r2", "2013 Round 2",
                                             ifelse(intrbind$round=="2013r3", "2013 Round 3",
                                                    ifelse(intrbind$round=="2013r4", "2013 Round 4", NA))))))

ggplot() +
  geom_line(data=intrbind, aes(x=int, y=Predicted, group=title, colour=title))+
  geom_point(data=realirbind, aes(x=int, y=Predicted, group=title, colour=title))+
  scale_y_continuous(limits = c(0, 250))+
  scale_x_continuous(limits = c(0, 1))+
  xlab("Interspersion") +
  ylab("Sora per Hectare") +
  scale_colour_manual(values=cbPalette)+
  theme(plot.title = element_text(colour="black",size=40), #plot title
        axis.text.x = element_text(colour="black", size=20), #x axis labels
        axis.text.y = element_text(colour="black",size=20), #y axis labels
        axis.title.x = element_text(colour="black", size=30, vjust=-.5), #x axis title
        axis.title.y = element_text(colour="black",size=30), #y axis title
        legend.text = element_text(colour="black", size=20), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="azure3"), #legend background color
        legend.position = "top",
        legend.direction= "horizontal",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "azure3" ), #plot background color
        panel.background = element_rect(fill = "azure3"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA),
        plot.margin = unit(c(3,3,3,3), "line"))+
  guides(col = guide_legend(nrow = 2))

###############################
# Water Graph
##############################
waterrbind = subset(waterrbind, habtype=="ms")
waterrbind$hab = c("moist soil","moist soil","moist soil","moist soil","moist soil")
waterrbind$title = c("2013 Round 4","2013 Round 4","2013 Round 4","2013 Round 4","2013 Round 4")

ggplot(waterrbind, aes(x=waterd, y=Predicted, group=title)) +
  geom_line(aes(colour=round), size=2)+
  xlab("Water Depth") +
  ylab("Sora per Hectare") +
  scale_colour_manual(values=cbPalette)+
  theme(plot.title = element_text(colour="black",size=40), #plot title
        axis.text.x = element_text(colour="black", size=20), #x axis labels
        axis.text.y = element_text(colour="black",size=20), #y axis labels
        axis.title.x = element_text(colour="black", size=30, vjust=-.5), #x axis title
        axis.title.y = element_text(colour="black",size=30), #y axis title
        legend.text = element_text(colour="black", size=20), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="azure3"), #legend background color
        legend.position = "top",
        legend.direction= "horizontal",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "azure3" ), #plot background color
        panel.background = element_rect(fill = "azure3"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA),
        plot.margin = unit(c(3,3,3,3), "line"))+
  guides(col = guide_legend(nrow = 2))

################################
## Draw Up Date Graph
################################

durbind = subset(durbind, habtype=="ms")
durbind$hab = ifelse(durbind$habtype=="ms","moist soil", "perennial emergent")

durbind$title = ifelse(durbind$round=="2013r2", "2013 Round 2",
                       ifelse(durbind$round=="2013r3", "2013 Round 3",
                              ifelse(durbind$round=="2013r4", "2013 Round 4", NA)))

durbind$dunum = (ifelse(durbind$dua=="a", "None",
                        ifelse(durbind$dua=="b", "January",
                               ifelse(durbind$dua=="c", "February",
                                      ifelse(durbind$dua=="d", "March", 
                                             ifelse(durbind$dua=="e","April",
                                                    ifelse(durbind$dua=="f","July",
                                                           ifelse(durbind$dua=="g", "August",
                                                                  ifelse(durbind$dua=="h","September",
                                                                         ifelse(durbind$dua=="i","October",NA))))))))))

durbind$dunum = factor(durbind$dunum, levels=c("None","January", "February", "March", "April", 
                                               "July", "August", "September", "October"))
ggplot() + 
  geom_bar(data=durbind, aes(x=title, y=Predicted, fill=dunum, width=.5),position=position_dodge(), stat="identity", colour="black")+
  xlab("Month of Water Draw Up") +
  ylab("Sora per Hectare") +
  scale_colour_manual(values=cbPalette)+
  theme(plot.title = element_text(colour="black",size=40), #plot title
        axis.text.x = element_text(colour="black", size=20), #x axis labels
        axis.text.y = element_text(colour="black",size=20), #y axis labels
        axis.title.x = element_text(colour="black", size=30, vjust=-.5), #x axis title
        axis.title.y = element_text(colour="black",size=30), #y axis title
        legend.text = element_text(colour="black", size=20), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="azure3"), #legend background color
        legend.position = "top",
        legend.direction= "horizontal",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "azure3" ), #plot background color
        panel.background = element_rect(fill = "azure3"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA),
        plot.margin = unit(c(3,3,3,3), "line"))+
  guides(col = guide_legend(nrow = 2))

ggplot() + 
  geom_bar(data=durbind, aes(x=dunum, y=Predicted, fill=title, width=.5),position=position_dodge(), stat="identity", colour="black")+
  xlab("Month of Water Draw Up") +
  ylab("Sora per Hectare") +
  scale_colour_manual(values=cbPalette)+
  theme(plot.title = element_text(colour="black",size=40), #plot title
        axis.text.x = element_text(colour="black", size=20), #x axis labels
        axis.text.y = element_text(colour="black",size=20), #y axis labels
        axis.title.x = element_text(colour="black", size=30, vjust=-.5), #x axis title
        axis.title.y = element_text(colour="black",size=30), #y axis title
        legend.text = element_text(colour="black", size=20), #legend text
        legend.title = element_blank(),#legend title
        legend.background = element_rect(fill="azure3"), #legend background color
        legend.position = "top",
        legend.direction= "horizontal",
        legend.key = element_blank(),
        plot.background = element_rect(fill = "azure3" ), #plot background color
        panel.background = element_rect(fill = "azure3"), #panel background color
        panel.grid.major.y= element_line(colour="black"), #y axis grid line color
        panel.grid.major.x = element_line(colour=NA),
        panel.grid.minor = element_line(colour=NA),
        plot.margin = unit(c(3,3,3,3), "line"))+
  guides(col = guide_legend(nrow = 2))


#######################################
# Anova on Habitat Type
#######################################

#the lm() function allows us to do a variety of linear models
# Predicted ~ habtype is the equation. With the response on teh left and the predictor on teh right
#the variable after the column is the name of the prediction data frame we just created
# then you take that and run it through the aov() function to get an anova table

haba12r2 = lm(Predicted ~ habtype, lm12r2)
#there is only one habitat type in 2012 ROund 3, so we are not running an ANOVA
haba13r1 = lm(Predicted ~ habtype, lm13r1)
haba13r2 = lm(Predicted ~ habtype, lm13r2)
haba13r3 = lm(Predicted ~ habtype, lm13r3)
haba13r4 = lm(Predicted ~ habtype, lm13r4)


summary(haba12r2) #significant difference
#again, only one habitat type, so no Anova for 2012 r3
summary(haba13r1) #significant difference
summary(haba13r2) #no significant difference
summary(haba13r3) #no significant difference
summary(haba13r4) #no significant difference

###################################
# ANOVA on Draw Up Date
###################################

dua12r3 <- lm(Predicted ~ dua, lm12r3) #significant difference for month H and I
dua13r2 <- lm(Predicted ~ dua, lm13r2) #significant difference for month D adn F
dua13r3 <- lm(Predicted ~ dua, lm13r3) #significant difference for month B C E G H I
dua13r4 <- lm(Predicted ~ dua, lm13r4) #no significant difference 

summary(dua12r3)
summary(dua13r2)
summary(dua13r3)
summary(dua13r4)

###################################
# Regression on Interspersion
###################################

intr12r2 <- lm(Predicted ~ int, lm12r2) #significantly different then 0
intr12r3 <- lm(Predicted ~ int, lm12r3) #not significantly different
intr13r1 <- lm(Predicted ~ int, lm13r1) #not significantly different
intr13r2 <- lm(Predicted ~ int, lm13r2) #not significantly different
intr13r3 <- lm(Predicted ~ int, lm13r3) #not significantly different 
intr13r4 <- lm(Predicted ~ int, lm13r4) #not significantly different 

summary(intr12r2)
summary(intr12r3)
summary(intr13r1)
summary(intr13r2)
summary(intr13r3)
summary(intr13r4)

#################################
# Regression on Water Depth
#################################

waterr13r4 <- lm(Predicted ~ waterd, lm13r4) #significantly different 

summary(waterr13r4)
