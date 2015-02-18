#Biometry Lab 1

library(foreign)
library(Rcmdr)
library(plyr)

setwd("C:/Users/avanderlaar/Dropbox/R/Biometry_Lab1")

#take all of your xls files and covert them into csvs. You can do this in excel
#please, for the love of all good things, don't put spaces in file names or the titles of columns
#just trust me on this one

radish = read.csv("RAD_GROW.csv")
chloro = read.csv("Chlorophyll_e95_for_graph_lab.csv")

#read in systat files
presnork = read.systat("presnork.syd")
toto = read.systat("tottrhab.syd")

#summary statistics
sum = numSummary(radish[,c("RAD_GROW_L", "RAD_GROW_D", "RAD_GROW")], statistics=c("mean", "sd","quantiles"), quantiles=c(0,.25,.5,.75,1)) 
sum
#export summary to a csv
write.csv(sum$table, file="radish_sum_stat.csv")

###################################
#cholor data

#calculate summary stats
sum_c = numSummary(chloro[,c("Pool")], statistics=c("mean", "sd", 'quanties'), quantiles=c(0,.25,.5,.75,1))

#Pool
pool = tapply(chloro$Chl_a_mg_m2, chloro$Pool, summary)
pool
write.csv(pool$table, file="chloro_by_pool")

#Drying
drying = tapply(chloro$Chl_a_mg_m2, chloro$DRYING, summary)
drying
write.csv(drying$table, file="chloro_by_drying")

#Bass
bass = tapply(chloro$Chl_a_mg_m2, chloro$BASS, summary)
bass
write.csv(bass$table, file="chloro_by_bass")

#stem and leaf plots of RAD_GROW_L and RAD_GROW_D
stem(radish$RAD_GROW_L)
stem(radish$RAD_GROW_D)

#histographs of RAD_GROW_L and RAD_GROW_D
par(mfrow = c(1, 2))
hist(radish$RAD_GROW_L)
hist(radish$RAD_GROW_D)

#create empty matrice that is 5x1000
random2 = matrix(,nrow=1000,ncol=5)
#enter in the random values
random2[,1] <- rnorm(10)
random2[,2] <- rnorm(100)
random[,3] <- rnorm(1000)
random[,4] <- rexp(1000)
random[,5] <- runif(1000)
random
head(random)
#plot histograms
par(mfrow = c(2, 3))
hist(random[,1], main="normal(10)" )
hist(random[,2], main="normal(100)")
hist(random[,3], main="normal(1000)")
hist(random[,4], main="exponential")
hist(random[,5], main="uniform")

###################################
#back to radishes
#make boxplots
par(mfrow = c(1,2))
boxplot(radish$RAD_GROW_L, main="Light")
boxplot(radish$RAD_GROW_D, main="Dark")

###################################
#back to random data
#make boxplots
par(mfrow= c(2,2))
boxplot(random[,3], main="N1000")
boxplot(random[,4], main="Exp")
boxplot(random[,5], main="Uniform")

#probability plots
par(mfrow=c(2,2))
qqnorm(random[,3], main="N1000") 
qqnorm(random[,4], main="Exp") 
qqnorm(random[,5], main="Uniform") 

###################################
#onto presnork data

#probability plots
par(mfrow=c(1,2))
qqnorm(presnork$SP1, main="SP1") 
qqnorm(presnork$SP2, main="SP2")

#histograms
par(mfrow=(c(1,2)))
hist(presnork$SP1, main="SP1")
hist(presnork$SP2, main="SP2")

#boxplots
par(mfrow= c(1,2))
boxplot(presnork$SP1, main="SP1")
boxplot(presnork$SP2, main="SP2")

#summary statistics
sum_presnork = numSummary(presnork[,c("SP1", "SP2")], statistics=c("mean", "range", "sd","quantiles"), quantiles=c(0,.25,.5,.75,1)) 
sum_presnork
#export summary to a csv
write.csv(sum_presnork$table, file="presnork_sum_stat.csv")

###################################
#onto toto

#so systat is awful, so you need to export the systat file as a csv
#get it to the point where it can be read outside of systat
#and then import it back into R
#proprietary software is a HORRIBLE format to store your data in
#end rant
write.table(toto, file="toto.csv")

totocsv = read.csv("toto.csv")

brook = totocsv[totocsv$species=="BROOK",]
rainbow = totocsv[totocsv$species=="RAINBOW",]


#histographs
par(mfrow=(c(1,2)))
hist(brook$FOCVEL, main="Brook")
hist(rainbow$FOCVEL, main="Rainbow")

#probability plots
par(mfrow=c(1,2))
qqnorm(brook$FOCVEL, main="Brook")
qqnorm(rainbow$FOCVEL, main="Rainbow")

#boxplots
par(mfrow= c(1,2))
boxplot(brook$FOCVEL, main="Brook")
boxplot(rainbow$FOCVEL, main="Rainbow")

