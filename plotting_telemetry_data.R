library(dplyr)
library(ggplot2)
library(ggpubr)
status_cols<- c("#E54849","#414042")


peri_theme <- theme(panel.background = element_blank(), 
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    plot.background = element_blank(), 
                    axis.title.x = element_text(size = 6), 
                    axis.text.x = element_text(size =5, colour = "black"), 
                    axis.title.y = element_text(size = 6),
                    axis.text.y = element_text(size = 5, colour = "black"),
                    axis.line.y = element_line(colour = "black", size = 0.4), 
                    axis.line.x = element_line(colour = "black", size = 0.4),
                    axis.ticks = element_line(colour = "black", size = 0.3),
                    plot.title = element_text(hjust = 0.5, size=6),
                    plot.subtitle = element_text(hjust = 0.5, size=6),
                    legend.text = element_text(size = 5), legend.title = element_text(size =5),
                    legend.key=element_blank(),
                    plot.margin = margin(t = 0,  # Top margin
                                         r = 0,  # Right margin
                                         b = 0,  # Bottom margin
                                         l = 0))

te<- read.csv("../data_unfiltered/T_phenotype.csv")

te_ind<- te %>% group_by(ID) %>% summarize(meanT=mean(meanT))
hist(te_ind$meanT)


behav<- read.csv("../data_unfiltered/daily_social_phenotypes_4.csv")
ind_behav<- behav %>% filter(pings>1) %>% group_by(ID1, Status) %>% summarize(strength.all_study=mean(num.int, na.rm=TRUE)) %>% rename(ID=ID1)
all<- merge(te_ind, ind_behav, by="ID", all = TRUE)

rna<- read.csv("../data_unfiltered/PIPFIL_T_and_behav_data.csv")
rna<- rna %>% rename(ID=colorID) %>% select(ID, sac_date)

all<- merge(rna, all, by="ID", all=TRUE)
all$rna<- ifelse(is.na(all$sac_date),NA, "yes")
all<- all %>% filter(!is.na(strength.all_study))

p<- ggplot(data=all, aes(x=Status, y=meanT, color=Status, fill=Status)) + geom_flat_violin(position=position_nudge(x=0.1), lwd=0.4) 
p<- p + scale_fill_manual(values=alpha(c(status_cols),0.4)) + theme(aspect.ratio=1.2)  + scale_x_discrete(labels=c("floater", "territorial"))
p<- p + geom_point(data=all[which(all$rna=="yes"),], aes(x=Status, y=meanT, color=Status),size=0.6, position=position_nudge(x=-0.1)) + geom_boxplot(data=all[which(all$rna=="yes"),], aes(x=Status, y=meanT, color=Status), position=position_nudge(x=-0.1), fill=NA, width=0.2, lwd=0.4)
p<- p + scale_color_manual(values=status_cols) + peri_theme + theme(legend.position = "none")
p<- p + labs(y="mean testosterone (corrected)", x="")
p
ggsave("../DE_results/figure_meanT_data.pdf", plot=p, device="pdf", height=1.5, width=1.5, units="in")

q<- ggplot(data=all, aes(x=Status, y=strength.all_study, color=Status, fill=Status)) + geom_flat_violin(position=position_nudge(x=0.1), lwd=0.4) 
q<- q + scale_fill_manual(values=alpha(c(status_cols),0.4)) + theme(aspect.ratio=1.2)  + scale_x_discrete(labels=c("floater", "territorial"))
q<- q + geom_point(data=all[which(all$rna=="yes"),], aes(x=Status, y=strength.all_study, color=Status),size=0.6, position=position_nudge(x=-0.1)) + geom_boxplot(data=all[which(all$rna=="yes"),], aes(x=Status, y=strength.all_study, color=Status), position=position_nudge(x=-0.1), fill=NA, width=0.2, lwd=0.4)
q<- q + scale_color_manual(values=status_cols) + peri_theme + theme(legend.position = "none")
q<- q + labs(y="social network strength\n(mean interaction/day)", x="")
q
ggsave("../DE_results/figure_strength_data.pdf", plot=q, device="pdf", height=1.5, width=1.5, units="in")


terr<- data.frame(str=c(1:10),meanT=c(10:1), status="territorial")
flo<- data.frame(str=c(10:1), meanT=c(10:1), status="floater")
schematic<- rbind(terr,flo)
s<- ggplot(data=schematic, aes(x=meanT, y=str, color=status)) + geom_line() + peri_theme + theme(aspect.ratio = 1) + scale_color_manual(values=status_cols) + theme(legend.position = "none")
ggsave("../../blah.pdf", plot=s, device="pdf", height=1.5, width=1.5, units="in")
#ggarrange(p,q,ncol=2, heights=c(4,4), widths=c(3,3), align="h")