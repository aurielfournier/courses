#Biometry Logistic Regression
#Auriel Fournier

library(reshape)
library(Hmisc)
library(ggplot2)
library(AICcmodavg)#for AIC ranking
library(rms)
library(ds)
setwd("~/Downloads")

#diving beetle
h = read.csv("h_sulphuria.csv", header=T)

hsub = subset(x=h, select=c("shed_urban", "riparian_forest","h_sulphuria","shed_area"))
hmelt = melt(hsub, id.vars="h_sulphuria")


ggplot() + geom_point(data=hmelt, aes(x=h_sulphuria, y=value, group=variable, colour=variable))

#pearson's correlation
cor = rcorr(as.matrix(hsub), type="pearson")
cor

#lrm() is the logisitc regression model
cand.mod = list()
cand.mod[[1]] = glm(h_sulphuria ~ shed_urban+shed_area, data=hsub, family=binomial)
cand.mod[[2]] = glm(h_sulphuria ~ shed_urban+riparian_forest+shed_area,  data=hsub, family=binomial)
cand.mod[[3]] = glm(h_sulphuria ~ riparian_forest*shed_urban*shed_area, data=hsub, family=binomial)

#list of the names for the models
names = c("model 1", "model 2", "model 3")

#generates AIC table
aictab(cand.mod, names, sort=T)   
model = lrm(h_sulphuria ~ shed_area, data=hsub, x=T, y=T)
model1 = lrm(h_sulphuria ~ shed_urban+shed_area, data=hsub, x=T, y=T)
model2 = lrm(h_sulphuria ~ shed_urban+riparian_forest+shed_area,  data=hsub, x=T, y=T)
model3 = lrm(h_sulphuria ~ riparian_forest*shed_urban*shed_area, data=hsub, x=T, y=T)


#burying beetle
n = read.csv("n_americanus.csv", header=T)

cand.modn = list()
cand.modn[[1]] = glm(americanus ~ forest, data=n, family=binomial)
cand.modn[[2]] = glm(americanus ~ grassland,  data=n, family=binomial)
cand.modn[[3]] = glm(americanus ~ soil_dom, data=n, family=binomial)
cand.modn[[4]] = glm(americanus ~ deltachilu + tomentosus, data=n, family=binomial)
cand.modn[[5]] = glm(americanus ~ forest + as.factor(soil_dom), data=n, family=binomial)
cand.modn[[6]] = glm(americanus ~ forest + as.factor(soil_dom)+ deltachilu,data=n, family=binomial)

#list of the names for the models
names = c("model 1", "model 2", "model 3", "model 4", "model 5", "model 6")

#generates AIC table
aictab(cand.modn, names, sort=T) 
