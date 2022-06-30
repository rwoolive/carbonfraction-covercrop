library(ggplot2)
library(plyr)


# read in my data
dat <- read.csv("Raw-data/4_metadata - final_withclimdat.csv")
# select only c and organic matter based observations
dat <- dat[which(dat$rv %in% c("POC", "TOC", "MAOC", "SOC", "POM", "TOM", "SOM", "MAOM")),]

dat$rv[which(dat$rv %in% c("OM", "SOM", "TOM"))] <- "SOM"
dat$rv[which(dat$rv %in% c("TOC", "SOC"))] <- "SOC"

# set params
xs <- range(dat$logRR)
ys <- c(0,165)
ymeans <- c(160, 140)



# A
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
  labs(fill="response variable", y="Frequency") +
  guides(color=F) + xlim(xs) +
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
  geom_text(data=mu, aes(x=grp.mean+0.1, color=rv, label=paste("Mean = ", round(grp.mean, 3))), 
           y=ymeans, hjust = 0, show.legend = F) +
  labs(fill="response variable", y="Frequency") +
  guides(color=F) + xlim(xs) +
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
  geom_text(data=mu, aes(x=grp.mean+0.1, color=rv, label=paste("Mean = ", round(grp.mean, 3))), 
            y=ymeans, hjust = 0, show.legend = F) +
  labs(fill="response variable", y="Frequency") +
  guides(color=F) + xlim(xs) + 
  theme(legend.position="bottom")+
  scale_y_continuous(limits=ys, expand = c(0, 0)) 
splot



figure <- ggpubr::ggarrange(pplot, mplot, splot,
                    labels = c("D", "E", "F"),
                    ncol = 3, nrow = 1)
figure

ggsave("Figures/S2_histogram by rv type.png", units="in", height=3, width=11)

