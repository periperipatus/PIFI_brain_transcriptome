library(plyr)
library(lubridate)
library(FSA)
library(ggplot2)
library(ggpubr)
library(readxl)
library(stringr)
setwd("C:/Users/perif/OneDrive - East Carolina University/Manakins/Pipra Brain/Data")

peri_theme <- theme(panel.background = element_rect(fill="white"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 10, 
                                                                                       colour = "black"), axis.title.y = element_text(size = 12), 
                    axis.text.y = element_text(size = 10, colour = "black"), 
                    legend.text = element_text(size = 10), legend.title = element_text(size = 12), 
                    axis.line.y = element_line(colour = "black"), axis.line.x = element_line(colour = "black"),
                    plot.title = element_text(hjust = 0.5, size=12))  + theme(legend.key=element_blank())

harvests<- read.csv("Samples and sequencing/Harvestsamples.csv", fileEncoding="UTF-8-BOM")
harvests$Color_ID<- sub("/", "", harvests$Color_ID)
behav<- read.csv("Samples and sequencing/PIPFIL_T_and_behav_data_for_Peri.csv")
behav<- rename(behav, c("colorID"="Color_ID"))



harvests$Date<- as.Date(harvests$Date, format="%d-%b-%y")
harvests$Class<- revalue(harvests$Class, replace=c("SCB floater"="Predefinitive floater", "DCB floater "="Definitive floater", "DCB territorial"="Territorial"))
harvests$Class<- factor(harvests$Class, levels=c("Predefinitive floater", "Definitive floater", "Territorial"))
harvests$Status<- harvests$Class
harvests$Status<- revalue(harvests$Status, replace=c("Predefinitive floater"="Floater", "Definitive floater"="Floater", "DCB territorial"="Territorial"))



harvests$L_size<- (4/3)*pi*(0.5*harvests$L_length)*(0.5*(harvests$L_width^2))
harvests$R_size<- (4/3)*pi*(0.5*harvests$R_length)*(0.5*(harvests$R_width^2))
harvests$testis_avg<- apply(harvests[,c("L_size","R_size")], 1, mean)

#Testis size  formula from Moore et al (2002) Latitudinal variation in plasma testosterone levels in birds of the genus Zonotrichia


harvests_behav<- merge(behav, harvests, by="Color_ID", all.y = TRUE)


hist(harvests$L_size, breaks=16)
hist(harvests$R_size, breaks=16)
ggqqplot(harvests$L_size)
status_cols<- c("#86AD44","#E54849","#414042")

L<- ggplot(harvests, aes(x=Status, y=L_size)) + geom_boxplot(fill=NA, color="grey40") + geom_point(aes(color=Class), size=3)  + labs(y="Left Testis Volume") + peri_theme + scale_color_manual(values=status_cols)
R<- ggplot(harvests, aes(x=Status, y=R_size)) + geom_boxplot(fill=NA, color="grey40") + geom_point(aes(color=Class), size=3) + labs(y="Right Testis Volume") + peri_theme + scale_color_manual(values=status_cols)

#ggarrange(L, R, common.legend = TRUE)

#is there a difference between the left and right testis between individuals?
t.test(harvests$L_size,harvests$R_size)


summary(aov(testis_avg ~ Status, data=harvests))
summary(aov(L_size ~ Status, data=harvests))
summary(aov(R_size ~ Status, data=harvests))



## Status and mean T effects and testis size

LT<- ggplot(harvests_behav, aes(x=mean_T, y=L_size)) + geom_point(aes(color=Class), size=3) + peri_theme + scale_color_manual(values=status_cols) + labs(y="Left Testis Volume", x="Mean testosterone")
RT<- ggplot(harvests_behav, aes(x=mean_T, y=R_size)) + geom_point(aes(color=Class), size=3) + peri_theme + scale_color_manual(values=status_cols) + labs(y="Right Testis Volume", x="Mean testosterone")

ggarrange(L,R,LT, RT,  labels=c("A","B","C","D"), common.legend = TRUE)

summary(lm(L_size ~ Status*mean_T, data=harvests_behav))
anova(lm(L_size ~ Status*mean_T, data=harvests_behav))
summary(lm(R_size ~ Status*mean_T, data=harvests_behav))
anova(lm(R_size ~ Status*mean_T, data=harvests_behav))

summary(lm(testis_avg ~ status*mean_T, data=harvests_behav))
anova(lm(testis_avg ~ Status*mean_T, data=harvests_behav))
