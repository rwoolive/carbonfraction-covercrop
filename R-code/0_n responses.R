library(ggplot2)
library(plyr)


# read in my data
dat <- read.csv("Raw-data/4_metadata - final_withclimdat.csv")
# select only c and organic matter based observations (exclude N or P based observations)
dat <- dat[which(dat$rv %in% c("POC", "TOC", "MAOC", "SOC", "POM", "TOM", "SOM", "MAOM")),]

dat$rv[which(dat$rv %in% c("OM", "SOM", "TOM"))] <- "SOM"
dat$rv[which(dat$rv %in% c("TOC", "SOC"))] <- "SOC"

# set params
xs <- range(dat$logRR)
ys <- c(0,120)
ymeans <- c(110, 100)



# show how cover-crop vs. control comparisons differ from high- vs. low-diversity comparisons
dat$comp <- rep("cc vs. fallow", dim(dat)[1])
dat$comp[which(dat$StudyID %in% c("Conc", "deCa", "Vier", "Zana"))] <- "high- vs. low-diversity cc"

# POC
mu <- ddply(dat[which(dat$rv %in% c("POC", "POM")),], 
            "comp", summarise, grp.mean=mean(logRR), grp.sd=sd(logRR))

pplot_comp <- ggplot(data=dat[which(dat$rv %in% c("POC", "POM")),], 
                aes(x=logRR, fill=comp, color=comp)) + 
  geom_histogram(position="identity", alpha=0.25, bins=25) +
  theme_classic() +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=comp),
             linetype="dashed", size=0.5, show.legend = F)+
  geom_vline(data=mu, aes(xintercept=grp.mean-grp.sd, color=comp),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_vline(data=mu, aes(xintercept=grp.mean+grp.sd, color=comp),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_text(data=mu, aes(x=grp.mean+0.1, color=comp, label=paste("Mean = ", round(grp.mean, 3))), 
            y=ymeans, hjust = 0, show.legend = F) +
  labs(fill="Comparison", y="Frequency", x="Effect size") +
  guides(color=F)  +
  theme(legend.position="bottom")+
  scale_y_continuous(limits=ys, expand = c(0, 0)) +
  scale_x_continuous(limits=xs, expand = c(0, 0)) +
  scale_fill_manual(values=c("purple", "blue")) +
  scale_color_manual(values=c("purple", "blue"))
pplot_comp

# MAOC
mu <- ddply(dat[which(dat$rv %in% c("MAOC", "MAOM")),], 
            "comp", summarise, grp.mean=mean(logRR), grp.sd=sd(logRR))

mplot_comp <- ggplot(data=dat[which(dat$rv %in% c("MAOC", "MAOM")),], 
                     aes(x=logRR, fill=comp, color=comp)) + 
  geom_histogram(position="identity", alpha=0.25, bins=25) +
  theme_classic() +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=comp),
             linetype="dashed", size=0.5, show.legend = F)+
  geom_vline(data=mu, aes(xintercept=grp.mean-grp.sd, color=comp),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_vline(data=mu, aes(xintercept=grp.mean+grp.sd, color=comp),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_text(data=mu, aes(x=grp.mean+0.1, color=comp, label=paste("Mean = ", round(grp.mean, 3))), 
            y=ymeans, hjust = 0, show.legend = F) +
  labs(fill="Comparison", y="Frequency", x="Effect size") +
  guides(color=F)  +
  theme(legend.position="bottom")+
  scale_y_continuous(limits=ys, expand = c(0, 0)) +
  scale_x_continuous(limits=xs, expand = c(0, 0)) +
  scale_fill_manual(values=c("purple", "blue")) +
  scale_color_manual(values=c("purple", "blue"))
mplot_comp

# SOC
mu <- ddply(dat[which(dat$rv %in% c("SOC", "SOM")),], 
            "comp", summarise, grp.mean=mean(logRR), grp.sd=sd(logRR))

splot_comp <- ggplot(data=dat[which(dat$rv %in% c("SOC", "SOM")),], 
                     aes(x=logRR, fill=comp, color=comp)) + 
  geom_histogram(position="identity", alpha=0.25, bins=25) +
  theme_classic() +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=comp),
             linetype="dashed", size=0.5, show.legend = F)+
  geom_vline(data=mu, aes(xintercept=grp.mean-grp.sd, color=comp),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_vline(data=mu, aes(xintercept=grp.mean+grp.sd, color=comp),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_text(data=mu, aes(x=grp.mean+0.1, color=comp, label=paste("Mean = ", round(grp.mean, 3))), 
            y=ymeans, hjust = 0, show.legend = F) +
  labs(fill="Comparison", y="Frequency", x="Effect size") +
  guides(color=F)  +
  theme(legend.position="bottom")+
  scale_y_continuous(limits=ys, expand = c(0, 0)) +
  scale_x_continuous(limits=xs, expand = c(0, 0)) +
  scale_fill_manual(values=c("purple", "blue")) +
  scale_color_manual(values=c("purple", "blue"))
splot_comp


figure_comp <- ggpubr::ggarrange(pplot_comp, mplot_comp, splot_comp,
                                 common.legend = TRUE,legend = "bottom",
                                 label.x = 0.05,labels = c("A) POC", "B) MAOC", "C) SOC"),
                                 ncol = 1, nrow = 3)

jpeg("Figures/A.2_histogram by comp type.jpeg", width=4, height=8, units="in",res=600)
figure_comp
dev.off()


# remove comparisons of high- vs. low-diversity
dat <- dat[-which(dat$comp=="high- vs. low-diversity cc"),]
sort((unique(dat$StudyID)))
length((unique(dat$StudyID)))
dim(dat)


# remove outliers using rosners test
boxplot(dat$logRR,ylab = "hwy")
# install.packages("EnvStats")
library(EnvStats)
test <- rosnerTest(dat$logRR, k = 25)
test
length(which(test$all.stats$Outlier==TRUE)) # 24 outliers
out <- test$all.stats$Obs.Num[which(test$all.stats$Outlier==TRUE)]
length(unique(dat$StudyID[out])) # outliers come from 10 studies
hist(dat$logRR[out]) # histogram of outliers
hist(dat$percent.change[out]) # histogram of outliers
hist(dat$logRR[-out]) # histogram of non-outliers
hist(dat$percent.change[-out]) # histogram of non-outliers
hist(dat$percent.change) # histogram of everything
outlierdat <- dat[out,]; write.csv(outlierdat, "Processed-data/0_outliers.csv") # get outlier dataset
dat <- dat[-out,] # remove outliers from dataset
hist(dat$logRR)
length(unique(dat$StudyID)) # 49 studies represented in remaining data
dim(dat) # and 926 observations



# panels A-C

pplot0 <- ggplot(data=dat[which(dat$rv %in% c("POC", "POM")),], 
                aes(x=logRR)) + 
  geom_histogram(position="identity", alpha=0.25, bins=25, color="black") +
  theme_classic() +
  labs(y="Frequency", x="Effect size") +
  guides(color=F)  +
  theme(legend.position="bottom")+
  scale_y_continuous(limits=ys, expand = c(0, 0)) 
pplot0




mplot0 <- ggplot(data=dat[which(dat$rv %in% c("MAOC", "MAOM")),], 
                aes(x=logRR)) + 
  geom_histogram(position="identity", alpha=0.25, bins = 25, color="black") +
  theme_classic() +
  labs(y="Frequency", x="Effect size") +
  guides(color=F)  +
  theme(legend.position="bottom") +
  scale_y_continuous(limits=ys, expand = c(0, 0)) 
mplot0





splot0 <- ggplot(data=dat[which(dat$rv %in% c("SOC", "SOM")),], 
                aes(x=logRR)) + 
  geom_histogram(position="identity", alpha=0.25, bins = 25, color="black") +
  theme_classic() +
  labs(y="Frequency", x="Effect size") +
  guides(color=F)  + 
  theme(legend.position="bottom")+
  scale_y_continuous(limits=ys, expand = c(0, 0)) 
splot0






### OM vs. OC

# panels D-F
mu <- ddply(dat[which(dat$rv %in% c("POC", "POM")),], 
            "rv", summarise, grp.mean=mean(logRR), grp.sd=sd(logRR))

pplot <- ggplot(data=dat[which(dat$rv %in% c("POC", "POM")),], 
                aes(x=logRR, fill=rv, color=rv)) + 
  geom_histogram(position="identity", alpha=0.25, bins=25) +
  theme_classic() +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=rv),
             linetype="dashed", size=0.5, show.legend = F)+
  geom_vline(data=mu, aes(xintercept=grp.mean-grp.sd, color=rv),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_vline(data=mu, aes(xintercept=grp.mean+grp.sd, color=rv),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_text(data=mu, aes(x=grp.mean+0.1, color=rv, label=paste("Mean = ", round(grp.mean, 3))), 
            y=ymeans, hjust = 0, show.legend = F) +
  labs(fill="Initial pool", y="Frequency", x="Effect size") +
  guides(color=F)  +
  theme(legend.position="bottom")+
  scale_y_continuous(limits=ys, expand = c(0, 0)) 
pplot





mu <- ddply(dat[which(dat$rv %in% c("MAOC", "MAOM")),], 
            "rv", summarise, grp.mean=mean(logRR), grp.sd=sd(logRR))

mplot <- ggplot(data=dat[which(dat$rv %in% c("MAOC", "MAOM")),], 
       aes(x=logRR, fill=rv, color=rv)) + 
  geom_histogram(position="identity", alpha=0.25, bins = 25) +
  theme_classic() +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=rv),
             linetype="dashed", size=0.5, show.legend = F)+
  geom_vline(data=mu, aes(xintercept=grp.mean-grp.sd, color=rv),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_vline(data=mu, aes(xintercept=grp.mean+grp.sd, color=rv),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_text(data=mu, aes(x=grp.mean+0.05, color=rv, label=paste("Mean = ", round(grp.mean, 3))), 
           y=ymeans, hjust = 0, show.legend = F) +
  labs(fill="Initial pool", y="Frequency", x="Effect size", x="Effect size") +
  guides(color=F)  +
  theme(legend.position="bottom") +
  scale_y_continuous(limits=ys, expand = c(0, 0)) 
mplot





mu <- ddply(dat[which(dat$rv %in% c("SOC", "SOM")),], 
            "rv", summarise, grp.mean=mean(logRR), grp.sd=sd(logRR))

splot <- ggplot(data=dat[which(dat$rv %in% c("SOC", "SOM")),], 
                aes(x=logRR, fill=rv, color=rv)) + 
  geom_histogram(position="identity", alpha=0.25, bins = 25) +
  theme_classic() +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=rv),
             linetype="dashed", size=0.5, show.legend = F)+
  geom_vline(data=mu, aes(xintercept=grp.mean-grp.sd, color=rv),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_vline(data=mu, aes(xintercept=grp.mean+grp.sd, color=rv),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_text(data=mu, aes(x=grp.mean-0.1, color=rv, label=paste("Mean = ", round(grp.mean, 3))), 
            y=ymeans, hjust = 1, show.legend = F) +
  labs(fill="Initial pool", y="Frequency", x="Effect size") +
  guides(color=F)  + 
  theme(legend.position="bottom")+
  scale_y_continuous(limits=ys, expand = c(0, 0)) 
splot




### units

# panels G-I
mu <- ddply(dat[which(dat$rv %in% c("POC", "POM")),], 
            "logRR.type", summarise, grp.mean=mean(logRR), grp.sd=sd(logRR))

pplotu <- ggplot(data=dat[which(dat$rv %in% c("POC", "POM")),], 
                aes(x=logRR, fill=logRR.type, color=logRR.type)) + 
  geom_histogram(position="identity", alpha=0.25, bins=25) +
  theme_classic() +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=logRR.type),
             linetype="dashed", size=0.5, show.legend = F)+
  geom_vline(data=mu, aes(xintercept=grp.mean-grp.sd, color=logRR.type),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_vline(data=mu, aes(xintercept=grp.mean+grp.sd, color=logRR.type),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_text(data=mu, aes(x=grp.mean+0.1, color=logRR.type, label=paste("Mean = ", round(grp.mean, 3))), 
            y=ymeans, hjust = 0, show.legend = F) +
  labs(fill="Initial unit", y="Frequency", x="Effect size") +
  guides(color=F)  +
  theme(legend.position="bottom")+
  scale_y_continuous(limits=ys, expand = c(0, 0)) +
  scale_fill_manual(values=c("darkorange2", "darkgreen")) +
  scale_color_manual(values=c("darkorange2", "darkgreen"))
pplotu



mu <- ddply(dat[which(dat$rv %in% c("MAOC", "MAOM")),], 
            "logRR.type", summarise, grp.mean=mean(logRR), grp.sd=sd(logRR))

mplotu <- ggplot(data=dat[which(dat$rv %in% c("MAOC", "MAOM")),], 
                aes(x=logRR, fill=logRR.type, color=logRR.type)) + 
  geom_histogram(position="identity", alpha=0.25, bins = 25) +
  theme_classic() +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=logRR.type),
             linetype="dashed", size=0.5, show.legend = F)+
  geom_vline(data=mu, aes(xintercept=grp.mean-grp.sd, color=logRR.type),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_vline(data=mu, aes(xintercept=grp.mean+grp.sd, color=logRR.type),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_text(data=mu, aes(x=grp.mean+0.1, color=logRR.type, label=paste("Mean = ", round(grp.mean, 3))), 
            y=ymeans, hjust = 0, show.legend = F) +
  labs(fill="Initial unit", y="Frequency", x="Effect size") +
  guides(color=F)  +
  theme(legend.position="bottom") +
  scale_y_continuous(limits=ys, expand = c(0, 0)) +
  scale_fill_manual(values=c("darkorange2", "darkgreen")) +
  scale_color_manual(values=c("darkorange2", "darkgreen"))
mplotu





mu <- ddply(dat[which(dat$rv %in% c("SOC", "SOM")),], 
            "logRR.type", summarise, grp.mean=mean(logRR), grp.sd=sd(logRR))

splotu <- ggplot(data=dat[which(dat$rv %in% c("SOC", "SOM")),], 
                aes(x=logRR, fill=logRR.type, color=logRR.type)) + 
  geom_histogram(position="identity", alpha=0.25, bins = 25) +
  theme_classic() +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=logRR.type),
             linetype="dashed", size=0.5, show.legend = F)+
  geom_vline(data=mu, aes(xintercept=grp.mean-grp.sd, color=logRR.type),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_vline(data=mu, aes(xintercept=grp.mean+grp.sd, color=logRR.type),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_text(data=mu, aes(x=grp.mean-0.1, color=logRR.type, label=paste("Mean = ", round(grp.mean, 3))), 
            y=ymeans, hjust = 1, show.legend = F) +
  labs(fill="Initial unit", y="Frequency", x="Effect size") +
  guides(color=F)  + 
  theme(legend.position="bottom")+
  scale_y_continuous(limits=ys, expand = c(0, 0)) +
  scale_fill_manual(values=c("darkorange2", "darkgreen")) +
  scale_color_manual(values=c("darkorange2", "darkgreen"))
splotu





figure0 <- ggpubr::ggarrange(pplot0, mplot0, splot0,
                             labels = c("A", "B", "C"),
                             ncol = 3, nrow = 1)
figure0

figure1 <- ggpubr::ggarrange(pplot, mplot, splot,
                    labels = c("D", "E", "F"),
                    ncol = 3, nrow = 1)
figure1

figure2 <- ggpubr::ggarrange(pplotu, mplotu, splotu,
                             labels = c("G", "H", "I"),
                             ncol = 3, nrow = 1)
figure2

figure3 <- ggpubr::ggarrange(figure0, figure1, figure2, heights = c(0.8, 1, 1),
                             ncol = 1, nrow = 3)
figure3
#ggsave("Figures/S2_histogram by rv type and unit.jpeg", units="in", height=8, width=10, res=600)
jpeg("Figures/A.2_histogram by rv type and unit.jpeg", width=10, height=8, units="in",res=600)
figure3
dev.off()


write.csv(dat, "Raw-data/4_metadata - final_withclimdat_2.csv")
