#load ggplot2
library(ggplot2)





###### POC data
POC <- read.csv("Model-output/4_Single-moderator/*effects-by-category_POC.csv")
POC$moderator <- rep(NA, dim(POC)[1])
POC$level <- rep(NA, dim(POC)[1])
POC$mean.mpc <- rep(NA, dim(POC)[1])
POC$lci.mpc <- rep(NA, dim(POC)[1])
POC$uci.mpc <- rep(NA, dim(POC)[1])
# split groups to moderator and level columns, then create columns for percent change
for(i in 1:dim(POC)[1]){
  POC$moderator[i] <- unlist(strsplit(POC$group[i], split ="[.]"))[1]
  POC$level[i] <- unlist(strsplit(POC$group[i], split ="[.]"))[2]
  POC$mean.mpc[i] <- (exp(POC$mean[i])-1)*100
  POC$lci.mpc[i] <- (exp(POC$lci[i])-1)*100
  POC$uci.mpc[i] <- (exp(POC$uci[i])-1)*100
}

POC_maincat1 <- POC[which(POC$moderator %in% c("Overall","depth", "ccseason", "tillage")),]
POC_maincat2 <- POC[which(POC$moderator %in% c( "maincrop", "ccadded")),]
POC_maincon <- POC[which(POC$group %in% c("nfert", "ag.C.inputs",  "sand", "silt", "clay", "toc", "ph", 
                                          "species.added", "duration", "Temp", "Prec")),]
POC_suppcat1 <- POC[which(POC$moderator %in% c("soilorder", "Continent", "SoilCollectionSeason")),]
POC_suppcat2 <- POC[which(POC$moderator %in% c("FractionationMethod", "DispersingAgent", "ResponseCalc")),]



##### MAOC data
MAOC <- read.csv("Model-output/4_Single-moderator/*effects-by-category_MAOC.csv")
MAOC$moderator <- rep(NA, dim(MAOC)[1])
MAOC$level <- rep(NA, dim(MAOC)[1])
MAOC$mean.mpc <- rep(NA, dim(MAOC)[1])
MAOC$lci.mpc <- rep(NA, dim(MAOC)[1])
MAOC$uci.mpc <- rep(NA, dim(MAOC)[1])
# split groups to moderator and level columns, then create columns for percent change
for(i in 1:dim(MAOC)[1]){
  MAOC$moderator[i] <- unlist(strsplit(MAOC$group[i], split ="[.]"))[1]
  MAOC$level[i] <- unlist(strsplit(MAOC$group[i], split ="[.]"))[2]
  MAOC$mean.mpc[i] <- (exp(MAOC$mean[i])-1)*100
  MAOC$lci.mpc[i] <- (exp(MAOC$lci[i])-1)*100
  MAOC$uci.mpc[i] <- (exp(MAOC$uci[i])-1)*100
}

MAOC_maincat1 <- MAOC[which(MAOC$moderator %in% c("Overall","depth", "ccseason", "tillage")),]
MAOC_maincat2 <- MAOC[which(MAOC$moderator %in% c( "maincrop", "ccadded")),]
MAOC_maincon <- MAOC[which(MAOC$group %in% c("nfert", "ag.C.inputs",  "sand", "silt", "clay", "toc", "ph", 
                                          "species.added", "duration", "Temp", "Prec")),]
MAOC_suppcat1 <- MAOC[which(MAOC$moderator %in% c("soilorder", "Continent", "SoilCollectionSeason")),]
MAOC_suppcat2 <- MAOC[which(MAOC$moderator %in% c("FractionationMethod", "DispersingAgent", "ResponseCalc")),]







##### SOC data
SOC <- read.csv("Model-output/4_Single-moderator/*effects-by-category_SOC.csv")
SOC$moderator <- rep(NA, dim(SOC)[1])
SOC$level <- rep(NA, dim(SOC)[1])
SOC$mean.mpc <- rep(NA, dim(SOC)[1])
SOC$lci.mpc <- rep(NA, dim(SOC)[1])
SOC$uci.mpc <- rep(NA, dim(SOC)[1])
# split groups to moderator and level columns, then create columns for percent change
for(i in 1:dim(SOC)[1]){
  SOC$moderator[i] <- unlist(strsplit(SOC$group[i], split ="[.]"))[1]
  SOC$level[i] <- unlist(strsplit(SOC$group[i], split ="[.]"))[2]
  SOC$mean.mpc[i] <- (exp(SOC$mean[i])-1)*100
  SOC$lci.mpc[i] <- (exp(SOC$lci[i])-1)*100
  SOC$uci.mpc[i] <- (exp(SOC$uci[i])-1)*100
}

SOC_maincat1 <- SOC[which(SOC$moderator %in% c("Overall","depth", "ccseason", "tillage")),]
SOC_maincat2 <- SOC[which(SOC$moderator %in% c( "maincrop", "ccadded")),]
SOC_maincon <- SOC[which(SOC$group %in% c("nfert", "ag.C.inputs",  "sand", "silt", "clay", "toc", "ph", 
                                          "species.added", "duration", "Temp", "Prec")),]
SOC_suppcat1 <- SOC[which(SOC$moderator %in% c("soilorder", "Continent", "SoilCollectionSeason")),]
SOC_suppcat2 <- SOC[which(SOC$moderator %in% c("FractionationMethod", "DispersingAgent", "ResponseCalc")),]




dim(POC_maincat1); dim(MAOC_maincat1); dim(SOC_maincat1)
dim(POC_maincat2); dim(MAOC_maincat2); dim(SOC_maincat2)
dim(POC_suppcat1); dim(MAOC_suppcat1); dim(SOC_suppcat1)
dim(POC_suppcat2); dim(MAOC_suppcat2); dim(SOC_suppcat2)

### index

POC_maincat1$index <- as.numeric(c(dim(POC_maincat1)[1]:1))
POC_maincat2$index <- as.numeric(c(dim(POC_maincat2)[1]:1))
POC_suppcat1$index <- as.numeric(c(dim(POC_suppcat1)[1]:1))
POC_suppcat2$index <- as.numeric(c(dim(POC_suppcat2)[1]:1))

MAOC_maincat1$index <- as.numeric(c(9:6,4:1)) # MAOC_maincat1 is missing spring (index 5)
MAOC_maincat2$index <- as.numeric(c(10:8,5:3,1)) # brassica (index 7), grasslegumebrassica (index 6), otherrowcrop (index 2)
MAOC_suppcat1$index <- as.numeric(c(13,11,7,5,3:1)) # Oxisols (12), Ultisols (10), Inceptisol (9), Asia (8), NorthAmerica (6), winter (4)
MAOC_suppcat2$index <- as.numeric(c(dim(MAOC_suppcat2)[1]:1))

SOC_maincat1$index <- as.numeric(c(dim(SOC_maincat1)[1]:1))
SOC_maincat2$index <- as.numeric(c(dim(SOC_maincat2)[1]:1))
SOC_suppcat1$index <- as.numeric(c(13:9,7:5,3:1)) # Asia (8), winter (4)
SOC_suppcat2$index <- as.numeric(c(6:2)) # Indirect










# Fig. 2 in manuscript
dist <- 0.2
pcol <- "olivedrab4"
mcol <- "orange4"
scol <- "black"
xmaxim <- 85
texcol <- "black"
modsize<-3

POC_maincat1$signif <- rep("*", dim(POC_maincat1)[1]); POC_maincat1$signif[which(POC_maincat1$lci<0)] <- ""
MAOC_maincat1$signif <- rep("*", dim(MAOC_maincat1)[1]); MAOC_maincat1$signif[which(MAOC_maincat1$lci<0)] <- ""
SOC_maincat1$signif <- rep("*", dim(SOC_maincat1)[1]); SOC_maincat1$signif[which(SOC_maincat1$lci<0)] <- ""


plot2 <- ggplot(data=POC_maincat1, aes(y=index+dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  # add dividers
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=1, size=0.25) +
  geom_hline(yintercept=c(8.5, 6.5, 2.5), color='black', linetype='solid', alpha=1, size=0.25) +
  # add POC responses
  geom_errorbarh(height=.1, color="black", aes(y=index+dist), size=0.25) +
  geom_point(fill=pcol, shape=21, size=3) +
  # add MAOC responses
  geom_errorbarh(data = MAOC_maincat1, height=.1, color="black", aes(y=index), size=0.25) +
  geom_point(data=MAOC_maincat1, fill=mcol, aes(y=index, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc), shape=21, size=3) +
  # add SOC responses
  geom_errorbarh(data = SOC_maincat1, height=.1, color="black", aes(y=index-dist), size=0.25) +
  geom_point(data=SOC_maincat1, fill=scol, aes(y=index-dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc), shape=21, size=3) +
  # specify axes
  scale_y_continuous(name = "", breaks=c(9.3, 9, 8.7, 8:1), expand = c(0, 0),
                     labels=c("POC", "MAOC", "SOC", 
                              "0-10 cm", 
                              "10-30 cm", 
                              "Winter", "Spring", "Summer", "Year-round", 
                              "Conventional", "Reduced or no-till"), # labels based on POC_maincat1$level
                     limits=c(0.5,9.5)) + 
  scale_x_continuous(limits=c(-20,xmaxim), breaks= seq(-20,100,20),
                     sec.axis = sec_axis(~ log((. / 100)+1), name = "Log response ratio")) +
  labs(title='', x='Percent change', y = '') +
  # set theme
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.1,0.1,0, "cm"),
        axis.ticks.x = element_line(size = 0.4), axis.text=element_text(colour="black"), axis.title=element_text(size=10)) +
  # add labels for moderators
  annotate(geom= "text", x=rep(xmaxim,4), y=c(9, 7.5, 4.5, 1.5), hjust = 1,vjust=0.5,fontface =3,
           label=c("Overall", "Depth", "Cover crop\nseason", "Tillage"), size=modsize) +
  # add sample sizes
  annotate(geom= "text", x=POC_maincat1$uci.mpc+4, y=c(POC_maincat1$index+dist), 
           hjust = 0, size=2.25, label=paste0(POC_maincat1$nobs, " / ", POC_maincat1$nstudy, " ", POC_maincat1$signif)) +
  annotate(geom= "text", x=MAOC_maincat1$uci.mpc+4, y=c(MAOC_maincat1$index), 
           hjust = 0, size=2.25, label=paste0(MAOC_maincat1$nobs, " / ", MAOC_maincat1$nstudy, " ", MAOC_maincat1$signif)) +
  annotate(geom= "text", x=SOC_maincat1$uci.mpc+4, y=c(SOC_maincat1$index-dist), 
           hjust = 0, size=2.25, label=paste0(SOC_maincat1$nobs, " / ", SOC_maincat1$nstudy, " ", SOC_maincat1$signif))

jpeg("Figures/2_overall_depth_season_tillage.jpeg", width=5, height=6.2, units="in",res=600)
plot2
dev.off()



# Fig. 6 in manuscript
dist <- 0.2
pcol <- "olivedrab4"
mcol <- "orange4"
scol <- "black"
xmaxim <- 85

POC_maincat2$signif <- rep("*", dim(POC_maincat2)[1]); POC_maincat2$signif[which(POC_maincat2$lci<0)] <- ""
MAOC_maincat2$signif <- rep("*", dim(MAOC_maincat2)[1]); MAOC_maincat2$signif[which(MAOC_maincat2$lci<0)] <- ""
SOC_maincat2$signif <- rep("*", dim(SOC_maincat2)[1]); SOC_maincat2$signif[which(SOC_maincat2$lci<0)] <- ""

plot3 <- ggplot(data=POC_maincat2, aes(y=index+dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  # add dividers
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=1, size=0.25) +
  geom_hline(yintercept=c(5.5), color='black', linetype='solid', alpha=1, size=0.25) +
  # add POC responses
  geom_errorbarh(height=.1, color="black", aes(y=index+dist), size=0.25) +
  geom_point(fill=pcol, shape=21, size=3) +
  # add MAOC responses
  geom_errorbarh(data = MAOC_maincat2, height=.1, color="black", aes(y=index), size=0.25) +
  geom_point(data=MAOC_maincat2, fill=mcol, aes(y=index, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc), shape=21, size=3) +
  # add SOC responses
  geom_errorbarh(data = SOC_maincat2, height=.1, color="black", aes(y=index-0.25), size=0.25) +
  geom_point(data=SOC_maincat2, fill=scol, aes(y=index-0.25, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc), shape=21, size=3) +
  # specify axes
  scale_y_continuous(name = "", breaks=c(10:1), expand = c(0, 0),
                     labels=c("Grass", "Legume", "Grass + legume",
                              "Brassica",
                              "Grass + legume + brassica", 
                              "Cereal", "Legume", "Cereal + legume", "Other row crop", "Perennial"), # labels based on POC_maincat2$level
                     limits=c(0.5,10.5)) + 
  scale_x_continuous(limits=c(-33,xmaxim), breaks= seq(-20,80,20),
                     sec.axis = sec_axis(~ log((. / 100)+1), name = "Log response ratio")) +
  labs(title='', x='Percent change', y = '') +
  # set theme
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.1,0.1,0, "cm"),
        axis.ticks.x = element_line(size = 0.4), axis.text=element_text(colour="black"), axis.title=element_text(size=10)) +
  # add labels for moderators
  annotate(geom= "text", x=rep(xmaxim,2), y=c(8,3), hjust = 1,vjust=0.5, fontface =3,
           label=c("Cover crop\ntype", "Cropping\nsystem"), size=modsize) +
  # add sample sizes
  annotate(geom= "text", x=POC_maincat2$uci.mpc+4, y=c(POC_maincat2$index+dist), 
           hjust = 0, size=2.25, label=paste0(POC_maincat2$nobs, " / ", POC_maincat2$nstudy, " ", POC_maincat2$signif)) +
  annotate(geom= "text", x=MAOC_maincat2$uci.mpc+4, y=c(MAOC_maincat2$index), 
           hjust = 0, size=2.25, label=paste0(MAOC_maincat2$nobs, " / ", MAOC_maincat2$nstudy, " ", MAOC_maincat2$signif)) +
  annotate(geom= "text", x=SOC_maincat2$uci.mpc+4, y=c(SOC_maincat2$index-dist), 
           hjust = 0, size=2.25, label=paste0(SOC_maincat2$nobs, " / ", SOC_maincat2$nstudy, " ", SOC_maincat2$signif))

jpeg("Figures/6_maincrop covercrop.jpeg", width=5, height=6.4, units="in",res=600)
plot3
dev.off()









# supplementary figs
dist <- 0.225
pcol <- "olivedrab4"
mcol <- "orange4"
scol <- "black"
xmaxim <- 85


POC_suppcat2$signif <- rep("*", dim(POC_suppcat2)[1]); POC_suppcat2$signif[which(POC_suppcat2$lci<0)] <- ""
MAOC_suppcat2$signif <- rep("*", dim(MAOC_suppcat2)[1]); MAOC_suppcat2$signif[which(MAOC_suppcat2$lci<0)] <- ""
SOC_suppcat2$signif <- rep("*", dim(SOC_suppcat2)[1]); SOC_suppcat2$signif[which(SOC_suppcat2$lci<0)] <- ""

plots2 <- ggplot(data=POC_suppcat2, aes(y=index+dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  # add dividers
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=1, size=0.25) +
  geom_hline(yintercept=c(2.5, 5.5), color='black', linetype='solid', alpha=1, size=0.25) +
  # add POC responses
  geom_errorbarh(height=.1, color='black', aes(y=index+dist), size=0.25) +
  geom_point(fill=pcol, shape=21, size=3) +
  # add MAOC responses
  geom_errorbarh(data = MAOC_suppcat2, height=.1, color='black', aes(y=index), size=0.25) +
  geom_point(data=MAOC_suppcat2, fill=mcol, aes(y=index, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc), shape=21, size=3) +
  # add SOC responses
  #geom_errorbarh(data = SOC_suppcat2, height=.1, color='black', aes(y=index-dist), size=0.25) +
  #geom_point(data=SOC_suppcat2, fill=scol, aes(y=index-dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc), shape=21, size=3) +
  # specify axes
  scale_y_continuous(name = "", breaks=c(6:1), expand = c(0, 0), 
                     labels=c("Size", 
                              "Sodium hexametaphosphate", "Other solution", "Glass beads", 
                              "Direct", "Indirect"), 
                     limits=c(0.5,6.5)) + 
  scale_x_continuous(limits=c(-33,xmaxim), breaks= seq(-20,100,20),
                     sec.axis = sec_axis(~ log((. / 100)+1), name = "Log response ratio")) +
  labs(title='', x='Percent change', y = '') +
  # set theme
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.1,0.1,0, "cm"),
        axis.ticks.x = element_line(size = 0.4), axis.text=element_text(colour="black"), axis.title=element_text(size=10)) +
  # add labels for moderators     2.5, 7.5, 9.5, 13.5,17.5
  annotate(geom= "text", x=rep(xmaxim,3), y=c(6, 4, 1.5), hjust = 1,vjust=0.5, fontface =3,
           label=c("Fractionation\nmethod", "Dispersing\nagent", "Measurement"), size=modsize) +
  # add sample sizes
  annotate(geom= "text", x=POC_suppcat2$uci.mpc+4, y=c(POC_suppcat2$index+dist), 
           hjust = 0, size=2, label=paste0(POC_suppcat2$nobs, " / ", POC_suppcat2$nstudy, " ", POC_suppcat2$signif)) +
  annotate(geom= "text", x=MAOC_suppcat2$uci.mpc+4, y=c(MAOC_suppcat2$index), 
           hjust = 0, size=2, label=paste0(MAOC_suppcat2$nobs, " / ", MAOC_suppcat2$nstudy, " ", MAOC_suppcat2$signif)) #+
  #annotate(geom= "text", x=SOC_suppcat2$uci.mpc+4, y=c(SOC_suppcat2$index-dist), 
  #         hjust = 0, size=2, label=paste0(SOC_suppcat2$nobs, " / ", SOC_suppcat2$nstudy))

jpeg("Figures/A.3_method-dispersing agent-measurement.jpeg", width=5, height=5, units="in",res=600)
plots2
dev.off()







# Supplementary figs
dist <- 0.275
pcol <- "olivedrab4"
mcol <- "orange4"
scol <- "black"
xmaxim <- 97


POC_suppcat1$signif <- rep("*", dim(POC_suppcat1)[1]); POC_suppcat1$signif[which(POC_suppcat1$lci<0)] <- ""
MAOC_suppcat1$signif <- rep("*", dim(MAOC_suppcat1)[1]); MAOC_suppcat1$signif[which(MAOC_suppcat1$lci<0)] <- ""
SOC_suppcat1$signif <- rep("*", dim(SOC_suppcat1)[1]); SOC_suppcat1$signif[which(SOC_suppcat1$lci<0)] <- ""

#MAOC_suppcat1[which(MAOC_suppcat1$group %in% c("soilorder.Oxisols", "soilorder.Ultisols","Continent.NorthAmerica")),c("mean.mpc", "lci.mpc", "uci.mpc")] <- NA

plots1 <- ggplot(data=POC_suppcat1, aes(y=index+dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  # add dividers
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=1, size=0.25) +
  geom_hline(yintercept=c(4.5, 8.5), color='black', linetype='solid', alpha=1, size=0.25) +
  # add POC responses
  geom_errorbarh(height=.15, color='black', aes(y=index+dist), size=0.25) +
  geom_point(fill=pcol, shape=21, size=3) +
  # add MAOC responses
  geom_errorbarh(data = MAOC_suppcat1, height=.15, color='black', aes(y=index), size=0.25) +
  geom_point(data=MAOC_suppcat1, fill=mcol, aes(y=index, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc), shape=21, size=3) +
  # add SOC responses
  geom_errorbarh(data = SOC_suppcat1, height=.15, color='black', aes(y=index-dist), size=0.25) +
  geom_point(data=SOC_suppcat1, fill=scol, aes(y=index-dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc), shape=21, size=3) +
  # specify axes
  scale_y_continuous(name = "", breaks=c(13:1), expand = c(0, 0), 
                     labels=c("Alfisols", "Oxisols", "Mollisols", "Ultisols", "Inceptisols",
                              "Asia", "Europe", "North America", "South America", 
                              "Winter", "Spring", "Summer", "Fall"), 
                     limits=c(0.5,13.5)) + 
  scale_x_continuous(limits=c(-33,xmaxim), breaks= seq(-20,100,20),
                     sec.axis = sec_axis(~ log((. / 100)+1), name = "Log response ratio")) +
  labs(title='', x='Percent change', y = '') +
  # set theme
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,0.1,0.1,0, "cm"),
        axis.ticks.x = element_line(size = 0.4), axis.text=element_text(colour="black"), axis.title=element_text(size=10)) +
  # add labels for moderators     2.5, 7.5, 9.5, 13.5,17.5
  annotate(geom= "text", x=rep(xmaxim,3), y=c(11, 6.5, 2.5), hjust = 1,vjust=0.5, fontface =3,
           label=c("Soil type", "Continent", "Soil collection\nseason"), size=modsize) +
  # add sample sizes
  annotate(geom= "text", x=POC_suppcat1$uci.mpc+4, y=c(POC_suppcat1$index+dist), 
           hjust = 0, size=2, label=paste0(POC_suppcat1$nobs, " / ", POC_suppcat1$nstudy, " ", POC_suppcat1$signif)) +
  annotate(geom= "text", x=MAOC_suppcat1$uci.mpc+4, y=c(MAOC_suppcat1$index), 
           hjust = 0, size=2, label=paste0(MAOC_suppcat1$nobs, " / ", MAOC_suppcat1$nstudy, " ", MAOC_suppcat1$signif)) +
  annotate(geom= "text", x=SOC_suppcat1$uci.mpc+4, y=c(SOC_suppcat1$index-dist), 
           hjust = 0, size=2, label=paste0(SOC_suppcat1$nobs, " / ", SOC_suppcat1$nstudy, " ", SOC_suppcat1$signif))

jpeg("Figures/A.7_soil order-continent-sampling season.jpeg", width=4.5, height=7, units="in",res=600)
plots1
dev.off()



