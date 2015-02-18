setwd("C:/Users/avanderlaar/Dropbox/R/Distance")

#load required libraries
library(unmarked)
library(AICcmodavg)
library(ggplot2)
library(reshape2)
library(lattice)
library(sjPlot) 
library(grid)

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
