#Biometry Multiple Regression Lab
#April 3 2014

library(foreign) #lets you read in systat files
library(Hmisc) #for the correlation stuff

stoden = read.systat("STO_DEN.syd")
multreg = read.systat("Multiple_Regression.syd")
write.csv(stoden, "sto_den.csv")
write.csv(multreg, "multiple_regression.csv")
multregs = multreg[,c("pool", "qdep", "pred", "cray_dens_2", "csr_dens_2", "pool_size", "canopy_cov_2", "max_dep_2", "biofilm")]
head(stoden)
head(multreg)

colnames(stoden) = c("pool", "site", "sppnum", "depth", "substrat", "aream2", "cond", "do", "habcov", "densiom", "maxdepth", "sto_sm_den", "sto_l_den")
colnames(multreg) = c("hab_type", "pool", "qdep", "pred", "cray_dens_2", "csr_dens_2", "pool_size", "canopy_cov_2", "max_dep_2", "biofilm")

stosub = stoden[,c("depth", "substrat", "cond", "habcov", "do", "densiom", "maxdepth", "sto_sm_den", "sto_l_den")]

#correlation matrix
plot(stosub)

#histograms of all variables of interest
par(mfrow=c(3,3))
hist(stosub$depth) # pretty good
hist(stosub$substrat) # pretty good
hist(stosub$cond) #binomial
hist(stosub$habcov) #negative skew
hist(stosub$do) #uniform looking
hist(stosub$densiom) #skewed right
hist(stosub$maxdepth) #skewed left
hist(stosub$sto_sm_den) #skewed left
hist(stosub$sto_l_den) # skewed left
#box plots
par(mfrow=c(3,3))
boxplot(stosub$depth) #has outliers
boxplot(stosub$substrat)
boxplot(stosub$cond)
boxplot(stosub$habcov)
boxplot(stosub$do)
boxplot(stosub$densiom)
boxplot(stosub$maxdepth) # has outliers
boxplot(stosub$sto_sm_den) # has outliers
boxplot(stosub$sto_l_den) #has outliers

#transforming variables
stosub$do = log(stosub$do)
stosub$habcov = stosub$habcov^(1/4)
stosub$maxdepth = stosub$maxdepth^(1/4)
stosub$sto_sm_den = stosub$sto_sm_den^(1/8)
stosub$sto_l_den = stosub$sto_l_den^(1/8)

#pearson's correlation
cor = rcorr(as.matrix(stosub), type="pearson")

#removed depth in favor of max depth
#removed do in favor of densiom because do has missing values
#removed cod in favor of hab_cov

# Models for small fish density
model1sm = lm(sto_sm_den ~ substrat + cond + densiom + maxdepth, data=stosub)
summary(model1sm)
#plots
par(mfrow=c(2,2))
plot(model1sm)
plot(model1sm$model)

#######Crayfish Data

names(multreg)
model1 = lm(biofilm ~ pool + qdep + pred + cray_dens_2 + csr_dens_2 + pool_size + canopy_cov_2 + max_dep_2, data=multreg)
model2 = lm(biofilm ~ qdep + pred + cray_dens_2 + csr_dens_2 + pool_size + canopy_cov_2 + max_dep_2, data=multreg)
model3 = lm(biofilm ~ qdep + pred + cray_dens_2 + pool_size + canopy_cov_2 + max_dep_2, data=multreg)
model4 = lm(biofilm ~ qdep + pred + cray_dens_2 + canopy_cov_2 + max_dep_2, data=multreg)
plot(model2$model)
plot(model4)

#back to fish data
mod1 = lm(sto_sm_den ~ substrat +cond + densiom + maxdepth, data=stosub) #Best Fit 
mod2 = lm(sto_sm_den ~ substrat + cond + densiom, data=stosub)
mod3 = lm(sto_sm_den ~ substrat + cond  + maxdepth, data=stosub)
mod4 = lm(sto_sm_den ~ substrat  + densiom + maxdepth, data=stosub)

AIC = list(AIC(mod1), AIC(mod2), AIC(mod3), AIC(mod4))
AIC 


mod1l = lm(sto_l_den ~ substrat + cond + densiom + maxdepth, data=stosub)
mod2l= lm(sto_l_den ~ substrat + cond + densiom , data=stosub)
mod3l = lm(sto_l_den ~ substrat + cond +  maxdepth, data=stosub) # Best Fit
mod4l= lm(sto_l_den ~ substrat + densiom + maxdepth, data=stosub)

AICl = list(AIC(mod1l), AIC(mod2l), AIC(mod3l), AIC(mod4l))
