#Auriel Fournier
#March 6, 2014
#Biometry Homework #2

#set working directory
#When working on my mac
#setwd("~/Dropbox/R/Biometry_HW_2")
#when working on my desktop
setwd("C:/Users/avanderlaar/Dropbox/R/Biometry_HW_2")

################################
#require the following packages
################################
library(lattice)
library(ggplot2)
library(sciplot)
library(jpeg)

#######the input file has been converted into a .csv file
#Import FIle
fish <- read.csv('Biometry_HW_2.csv', header=TRUE)

#examine the first ten rows of the data to ensure it imported correctly
head(fish)

#examine data structure
str(fish)

#generate summary of each variable
summary(fish)

###########################################
#Graphically Assess Distribution of the Data
###########################################

#########
#Boxplots
#########

plot(fish$TOTDENVOL ~ as.factor(fish$SEASNUM))
###There is atleast one outlier in each group, 
#with April and October having some extreme ones
png(filename="seasonbox.png")
plot(fish$TOTDENVOL ~ as.factor(fish$SEASNUM))
dev.off()

plot(fish$TOTDENVOL ~ as.factor(fish$STREAMTYPE))
#both streams have outliers, over a similar range
png(filename="streambox.png")
plot(fish$TOTDENVOL ~ as.factor(fish$STREAMTYPE))
dev.off()


###########
#Histograms
###########

histogram(~TOTDENVOL|SEASNUM, fish) 

png(filename="seasonhist.png")
histogram(~TOTDENVOL|SEASNUM, fish)
dev.off()
#Each season has a variation on a negative binomial distribution
#suggesting that this needs to be transformed

histogram(~TOTDENVOL|STREAMTYPE, fish)

png(filename="streamhist.png")
histogram(~TOTDENVOL|STREAMTYPE, fish)
dev.off()
#each stream has a negative binomial distribution
#suggesting that this needs to be transformed
#lots of zeros so we can't log transform it, lets try square root

###############
#Transformation
###############

fish[,71] = sqrt(fish$TOTDENVOL)

#########
#Boxplots
#########

plot(fish$V71 ~ as.factor(fish$SEASNUM))

png(filename="squareboxseason.png")
plot(fish$V71 ~ as.factor(fish$SEASNUM))
dev.off()

plot(fish$V71 ~ as.factor(fish$STREAMTYPE))

png(filename="squareboxstream.png")
plot(fish$V71 ~ as.factor(fish$STREAMTYPE))
dev.off()

###########
#Histograms
###########
histogram(~V71|SEASNUM, fish) 

png(filename="squareseasonhist.png")
histogram(~V71|SEASNUM, fish) 
dev.off()

histogram(~V71|STREAMTYPE, fish)

png(filename="squarestreamhist.png")
histogram(~V71|STREAMTYPE, fish)
dev.off()
#it looks a little better, but is still skewed 

###############
#Transformation
###############

fish[,72] = fish$V71^(1/4)

#########
#Boxplots
#########

plot(fish$V72 ~ as.factor(fish$SEASNUM))


png(filename="fourboxseason.png")
plot(fish$V72 ~ as.factor(fish$SEASNUM))
dev.off()

plot(fish$V72 ~ as.factor(fish$STREAMTYPE))


png(filename="fourboxstream.png")
plot(fish$V72 ~ as.factor(fish$STREAMTYPE))
dev.off()

###########
#Histograms
###########
histogram(~V72|SEASNUM, fish) 

png(filename="fourhistseason.png")
histogram(~V72|SEASNUM, fish) 
dev.off()

histogram(~V72|STREAMTYPE, fish)

png(filename="fourhiststream.png")
histogram(~V72|STREAMTYPE, fish)
dev.off()
#looks better, not perfect of course

#################################
#Run the TWo-Way Anova, Type III
#################################
options(contrasts = c("contr.sum","contr.poly")) #set the contrasts for a TYPE III ANOVA
model <- lm(V72 ~  STREAMTYPE * SEASNUM, data=fish) #run the ANOVA
fit <- drop1(model, .~., test="F") #Make it a Type III Anova
fit #view the output table


#Residuals plot
################
fish.df <- data.frame(M1_Fit = seq(0,0, length=150), 
                      M1_Resid = seq(0,0, length=150),
                      Season = seq(0,0, length=150),
                      Stream = seq(0,0, length=150))
                      
fish.resid = resid(model)
fish.fitted = fitted(model)

fish.df$M1_Fit = fish.fitted
fish.df$M1_Resid = fish.resid
fish.df$Season = fish$SEASNUM  
fish.df$Stream = fish$STREAMNUM

ggplot(fish.df, aes(M1_Fit, M1_Resid, colour =Stream)) + geom_point() +
  xlab("Fitted Values") + ylab("Residuals") + ggtitle("Residuals of Two-way Anova")

png(filename="TwoANOVAResidual.png")
ggplot(fish.df, aes(M1_Fit, M1_Resid, colour =Stream)) + geom_point() +
  xlab("Fitted Values") + ylab("Residuals") + ggtitle("Residuals of Two-way Anova")
dev.off()

#the residuals look ok, but not great. They are spread over similar values and 
#are not wedge shaped, but they are clumped. 
##################
# Interaction Plot 
##################

lineplot.CI(SEASNUM,V72, group =STREAMTYPE , data =fish, cex = 1.5,
            xlab = "Season", ylab = "means", cex.lab = 1.2, x.leg = 1,
            col = c("blue","red"), pch = c(16,16))

png(filename="TwoANOVAInteraction.png")
lineplot.CI(SEASNUM,V72, group =STREAMTYPE , data =fish, cex = 1.5,
            xlab = "Season", ylab = "means", cex.lab = 1.2, x.leg = 1,
            col = c("blue","red"), pch = c(16,16))
dev.off()

################


#############################################
#one way anovas to look at the main effects
#############################################

############
## STREAM ##
############
options(contrasts = c("contr.sum","contr.poly")) #set the contrasts for a TYPE III ANOVA
stream <- lm(V72 ~  STREAMTYPE, data=fish) #run the ANOVA
streamfit <- drop1(stream, .~., test="F") #Make it a Type III Anova
streamfit
#so stream type is not significant


################
#Residuals plot
################

stream.df <- data.frame(M1_Fit = seq(0,0, length=150), 
                      M1_Resid = seq(0,0, length=150),
                      Stream = seq(0,0, length=150))

stream.resid = resid(stream)
stream.fitted = fitted(stream)

stream.df$M1_Fit = stream.fitted
stream.df$M1_Resid = stream.resid
stream.df$Stream = fish$STREAMNUM

ggplot(stream.df, aes(M1_Fit, M1_Resid, colour =Stream)) + geom_point() +
  xlab("Fitted Values") + ylab("Residuals") + ggtitle("Residuals of One-Way Anova on Stream Type")


png(filename="OneANOVAStreamResidual.png")
ggplot(stream.df, aes(M1_Fit, M1_Resid, colour =Stream)) + geom_point() +
  xlab("Fitted Values") + ylab("Residuals") + ggtitle("Residuals of One-Way Anova on Stream Type")
dev.off()

#the residuals here look fine, well distributed

####################
#plot of the means
####################
lineplot.CI(STREAMTYPE,V72, data =fish, cex = 1.5,
            xlab = "Season", ylab = "means", cex.lab = 1.2, x.leg = 1,
             pch = c(16,16))

png(filename="OneANOVAStreamInteraction.png")
lineplot.CI(STREAMTYPE,V72, data =fish, cex = 1.5,
            xlab = "Season", ylab = "means", cex.lab = 1.2, x.leg = 1,
            pch = c(16,16))
dev.off()

############
## SEASON ##
############
options(contrasts = c("contr.sum","contr.poly")) #set the contrasts for a TYPE III ANOVA
season <- lm(V72 ~  SEASNUM, data=fish) #run the ANOVA
seasonfit <- drop1(season, .~., test="F") #Make it a Type III Anova
seasonfit
#so season is a significant effect on density


################
#Residuals plot
################
season.df <- data.frame(M1_Fit = seq(0,0, length=150), 
                        M1_Resid = seq(0,0, length=150),
                        Season = seq(0,0, length=150))

season.resid = resid(season)
season.fitted = fitted(season)

season.df$M1_Fit = season.fitted
season.df$M1_Resid = season.resid
season.df$Season = fish$SEASNUM

ggplot(season.df, aes(M1_Fit, M1_Resid, colour =Season)) + geom_point() +
  xlab("Fitted Values") + ylab("Residuals") + ggtitle("Residuals of One-Way Anova on Season")

png(filename="OneANOVASeasonResidual.png")
ggplot(season.df, aes(M1_Fit, M1_Resid, colour =Season)) + geom_point() +
  xlab("Fitted Values") + ylab("Residuals") + ggtitle("Residuals of One-Way Anova on Season")

dev.off()

####################
#plot of the means
####################
lineplot.CI(SEASNUM,V72, data =fish, cex = 1.5,
            xlab = "Season", ylab = "means", cex.lab = 1.2, x.leg = 1,
            pch = c(16,16))

png(filename="OneANOVASeasonInteraction.png")
lineplot.CI(SEASNUM,V72, data =fish, cex = 1.5,
            xlab = "Season", ylab = "means", cex.lab = 1.2, x.leg = 1,
            pch = c(16,16))
dev.off()


