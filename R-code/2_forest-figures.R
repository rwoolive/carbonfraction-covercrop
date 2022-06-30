#load ggplot2
library(ggplot2)





###### POC data
POC <- read.csv("Model-output/4_Single-moderator/effects-by-category_POC_no-nitrogen.csv")
POC <- POC[-which(POC$group=="ccseason.fall"),]
#POC[which(POC$nstudy<3), 4:8] <- NA # remove effects represented by less than three studies
POC <- POC[c(1:63),]
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
POC_maincat1 <- POC_maincat1[-which(POC_maincat1$group %in% c("depth.deepsoil")),]
POC_maincat2 <- POC[which(POC$moderator %in% c( "maincrop", "ccadded")),]
POC_maincat2 <- POC_maincat2[-which(POC_maincat2$group %in% c("ccadded.grassbrassica", "ccadded.legumebrassica")),]
POC_maincon <- POC[which(POC$group %in% c("nfert", "ag.C.inputs",  "sand", "silt", "clay", "toc", "ph", 
                                          "species.added", "duration", "Temp", "Prec")),]
POC_suppcat1 <- POC[which(POC$moderator %in% c("soilorder", "Continent", "SoilCollectionSeason")),]
POC_suppcat1 <- POC_suppcat1[-which(POC_suppcat1$group %in% c("soilorder.Spodosols", "Continent.Africa", "Continent.Australia")),]
POC_suppcat2 <- POC[which(POC$moderator %in% c("FractionationMethod", "DispersingAgent", "ResponseCalc")),]
POC_suppcat2 <- POC_suppcat2[-which(POC_suppcat2$group %in% c("DispersingAgent.Combo", "FractionationMethod.SizeAndDensity")),]

POC_maincat1$index <- as.numeric(c(dim(POC_maincat1)[1]:1))
POC_maincat2$index <- as.numeric(c(dim(POC_maincat2)[1]:1))
POC_suppcat1$index <- as.numeric(c(dim(POC_suppcat1)[1]:1))
POC_suppcat2$index <- as.numeric(c(dim(POC_suppcat2)[1]:1))


##### MAOC data
MAOC <- read.csv("Model-output/4_Single-moderator/effects-by-category_MAOC_no-nitrogen.csv")
MAOC <- MAOC[-which(MAOC$group=="ccseason.fall"),]
#MAOC[which(MAOC$nstudy<3), 4:8] <- NA # remove effects represented by less than three studies
MAOC <- MAOC[c(1:63),]
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
MAOC_maincat1 <- MAOC_maincat1[-which(MAOC_maincat1$group %in% c("depth.deepsoil")),]
MAOC_maincat2 <- MAOC[which(MAOC$moderator %in% c( "maincrop", "ccadded")),]
MAOC_maincat2 <- MAOC_maincat2[-which(MAOC_maincat2$group %in% c("ccadded.grassbrassica", "ccadded.legumebrassica")),]
MAOC_maincon <- MAOC[which(MAOC$group %in% c("nfert", "ag.C.inputs",  "sand", "silt", "clay", "toc", "ph", 
                                          "species.added", "duration", "Temp", "Prec")),]
MAOC_suppcat1 <- MAOC[which(MAOC$moderator %in% c("soilorder", "Continent", "SoilCollectionSeason")),]
MAOC_suppcat1 <- MAOC_suppcat1[-which(MAOC_suppcat1$group %in% c("soilorder.Spodosols", "Continent.Africa", "Continent.Australia")),]
MAOC_suppcat2 <- MAOC[which(MAOC$moderator %in% c("FractionationMethod", "DispersingAgent", "ResponseCalc")),]
MAOC_suppcat2 <- MAOC_suppcat2[-which(MAOC_suppcat2$group %in% c("DispersingAgent.Combo", "FractionationMethod.SizeAndDensity")),]

MAOC_maincat1$index <- as.numeric(c(dim(MAOC_maincat1)[1]:1))
MAOC_maincat2$index <- as.numeric(c(dim(MAOC_maincat2)[1]:1))
MAOC_suppcat1$index <- as.numeric(c(dim(MAOC_suppcat1)[1]:1))
MAOC_suppcat2$index <- as.numeric(c(dim(MAOC_suppcat2)[1]:1))






##### SOC data
SOC <- read.csv("Model-output/4_Single-moderator/effects-by-category_SOC_no-nitrogen.csv")
SOC <- SOC[-which(SOC$group=="ccseason.fall"),]
#SOC[which(SOC$nstudy<3), 4:8] <- NA # remove effects represented by less than three studies
SOC <- SOC[c(1:63),]
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
SOC_maincat1 <- SOC_maincat1[-which(SOC_maincat1$group %in% c("depth.deepsoil")),]
SOC_maincat2 <- SOC[which(SOC$moderator %in% c( "maincrop", "ccadded")),]
SOC_maincat2 <- SOC_maincat2[-which(SOC_maincat2$group %in% c("ccadded.grassbrassica", "ccadded.legumebrassica")),]
SOC_maincon <- SOC[which(SOC$group %in% c("nfert", "ag.C.inputs",  "sand", "silt", "clay", "toc", "ph", 
                                          "species.added", "duration", "Temp", "Prec")),]
SOC_suppcat1 <- SOC[which(SOC$moderator %in% c("soilorder", "Continent", "SoilCollectionSeason")),]
SOC_suppcat1 <- SOC_suppcat1[-which(SOC_suppcat1$group %in% c("soilorder.Spodosols", "Continent.Africa", "Continent.Australia")),]
SOC_suppcat2 <- SOC[which(SOC$moderator %in% c("FractionationMethod", "DispersingAgent", "ResponseCalc")),]
SOC_suppcat2 <- SOC_suppcat2[-which(SOC_suppcat2$group %in% c("DispersingAgent.Combo", "FractionationMethod.SizeAndDensity")),]

SOC_maincat1$index <- as.numeric(c(dim(SOC_maincat1)[1]:1))
SOC_maincat2$index <- as.numeric(c(dim(SOC_maincat2)[1]:1))
SOC_suppcat1$index <- as.numeric(c(dim(SOC_suppcat1)[1]:1))
SOC_suppcat2$index <- as.numeric(c(dim(SOC_suppcat2)[1]:1))










# Fig. 2 in manuscript
dist <- 0.2
pcol <- "olivedrab4"
mcol <- "orange4"
scol <- "black"
xmaxim <- 85

plot2 <- ggplot(data=POC_maincat1, aes(y=index+dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  # add dividers
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  geom_hline(yintercept=c(8.5, 6.5, 2.5), color='black', linetype='solid', alpha=.5) +
  # add POC responses
  geom_point(color=pcol) +
  geom_errorbarh(height=.1, color=pcol, aes(y=index+dist)) +
  # add MAOC responses
  geom_point(data=MAOC_maincat1, color=mcol, aes(y=index, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  geom_errorbarh(data = MAOC_maincat1, height=.1, color=mcol, aes(y=index)) +
  # add SOC responses
  geom_point(data=SOC_maincat1, color=scol, aes(y=index-dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  geom_errorbarh(data = SOC_maincat1, height=.1, color=scol, aes(y=index-dist)) +
  # specify axes
  scale_y_continuous(name = "", breaks=c(9.3, 9, 8.7, 8:1), expand = c(0, 0),
                     labels=c("POC", "MAOC", "SOC", 
                              "Surface (0-10 cm)", 
                              "Subsoil (10-30 cm)", 
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
        axis.ticks.x = element_line(size = 0.4)) +
  # add labels for moderators
  annotate(geom= "text", x=rep(xmaxim,4), y=c(9, 7.5, 4.5, 1.5), hjust = 1,vjust=0.5,fontface =3,
           label=c("Overall", "Depth", "Cover\ncrop\nseason", "Tillage")) +
  # add sample sizes
  annotate(geom= "text", x=POC_maincat1$uci.mpc+4, y=c(9:1+dist), 
           hjust = 0, size=2.25, label=paste0(POC_maincat1$nobs, " / ", POC_maincat1$nstudy)) +
  annotate(geom= "text", x=MAOC_maincat1$uci.mpc+4, y=c(9:1), 
           hjust = 0, size=2.25, label=paste0(MAOC_maincat1$nobs, " / ", MAOC_maincat1$nstudy)) +
  annotate(geom= "text", x=SOC_maincat1$uci.mpc+4, y=c(9:1-dist), 
           hjust = 0, size=2.25, label=paste0(SOC_maincat1$nobs, " / ", SOC_maincat1$nstudy))

png("Figures/2_overall_depth_season_tillage.png", width=5, height=6, units="in",res=300)
plot2
dev.off()



# Fig. 3 in manuscript
dist <- 0.2
pcol <- "olivedrab4"
mcol <- "orange4"
scol <- "black"
xmaxim <- 85

plot3 <- ggplot(data=POC_maincat2, aes(y=index+dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  # add dividers
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  geom_hline(yintercept=c(5.5), color='black', linetype='solid', alpha=.5) +
  # add POC responses
  geom_point(color=pcol) +
  geom_errorbarh(height=.1, color=pcol, aes(y=index+dist)) +
  # add MAOC responses
  geom_point(data=MAOC_maincat2, color=mcol, aes(y=index, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  geom_errorbarh(data = MAOC_maincat2, height=.1, color=mcol, aes(y=index)) +
  # add SOC responses
  geom_point(data=SOC_maincat2, color=scol, aes(y=index-dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  geom_errorbarh(data = SOC_maincat2, height=.1, color=scol, aes(y=index-dist)) +
  # specify axes
  scale_y_continuous(name = "", breaks=c(10:1), expand = c(0, 0),
                     labels=c("Grass", "Legume", "Grass + legume",
                              expression(paste(italic("Brassica"))),
                              expression(paste("Grass + Legume + ",italic("Brassica"))), 
                              "Cereal", "Legume", "Cereal+legume", "Other row crop", "Perennial"), # labels based on POC_maincat2$level
                     limits=c(0.5,10.5)) + 
  scale_x_continuous(limits=c(-33,xmaxim), breaks= seq(-20,80,20),
                     sec.axis = sec_axis(~ log((. / 100)+1), name = "Log response ratio")) +
  labs(title='', x='Percent change', y = '') +
  # set theme
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(size = 0.4)) +
  # add labels for moderators
  annotate(geom= "text", x=rep(xmaxim,2), y=c(8,3), hjust = 1,vjust=0.5, fontface =3,
           label=c("Cover\ncrop\ntype", "Cropping\nsystem")) +
  # add sample sizes
  annotate(geom= "text", x=POC_maincat2$uci.mpc+4, y=c(POC_maincat2$index+dist), 
           hjust = 0, size=2.25, label=paste0(POC_maincat2$nobs, " / ", POC_maincat2$nstudy)) +
  annotate(geom= "text", x=MAOC_maincat2$uci.mpc+4, y=c(MAOC_maincat2$index), 
           hjust = 0, size=2.25, label=paste0(MAOC_maincat2$nobs, " / ", MAOC_maincat2$nstudy)) +
  annotate(geom= "text", x=SOC_maincat2$uci.mpc+4, y=c(SOC_maincat2$index-dist), 
           hjust = 0, size=2.25, label=paste0(SOC_maincat2$nobs, " / ", SOC_maincat2$nstudy))

png("Figures/3_maincrop covercrop.png", width=5, height=6, units="in",res=300)
plot3
dev.off()






# Supplementary figures
dist <- 0.2
pcol <- "olivedrab4"
mcol <- "orange4"
scol <- "black"
xmaxim <- 95



plots1 <- ggplot(data=POC_suppcat1, aes(y=index+dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  # add dividers
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  geom_hline(yintercept=c(4.5, 8.5), color='black', linetype='solid', alpha=.5) +
  # add POC responses
  geom_point(color=pcol) +
  geom_errorbarh(height=.1, color=pcol, aes(y=index+dist)) +
  # add MAOC responses
  geom_point(data=MAOC_suppcat1, color=mcol, aes(y=index, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  geom_errorbarh(data = MAOC_suppcat1, height=.1, color=mcol, aes(y=index)) +
  # add SOC responses
  geom_point(data=SOC_suppcat1, color=scol, aes(y=index-dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  geom_errorbarh(data = SOC_suppcat1, height=.1, color=scol, aes(y=index-dist)) +
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
        axis.ticks.x = element_line(size = 0.4)) +
  # add labels for moderators     2.5, 7.5, 9.5, 13.5,17.5
  annotate(geom= "text", x=rep(xmaxim,3), y=c(11, 6.5, 2.5), hjust = 1,vjust=0.5, fontface =3,
           label=c("Soil\ntype", "Continent", "Soil\ncollection\nseason")) +
  # add sample sizes
  annotate(geom= "text", x=POC_suppcat1$uci.mpc+4, y=c(POC_suppcat1$index+dist), 
           hjust = 0, size=2, label=paste0(POC_suppcat1$nobs, " / ", POC_suppcat1$nstudy)) +
  annotate(geom= "text", x=MAOC_suppcat1$uci.mpc+4, y=c(MAOC_suppcat1$index), 
           hjust = 0, size=2, label=paste0(MAOC_suppcat1$nobs, " / ", MAOC_suppcat1$nstudy)) +
  annotate(geom= "text", x=SOC_suppcat1$uci.mpc+4, y=c(SOC_suppcat1$index-dist), 
           hjust = 0, size=2, label=paste0(SOC_suppcat1$nobs, " / ", SOC_suppcat1$nstudy))

png("Figures/S1_.png", width=4.5, height=6, units="in",res=300)
plots1
dev.off()





# supplementary fig
dist <- 0.2
pcol <- "olivedrab4"
mcol <- "orange4"
scol <- "black"
xmaxim <- 85



plots2 <- ggplot(data=POC_suppcat2, aes(y=index+dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  # add dividers
  geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
  geom_hline(yintercept=c(2.5, 6.5), color='black', linetype='solid', alpha=.5) +
  # add POC responses
  geom_point(color=pcol) +
  geom_errorbarh(height=.1, color=pcol, aes(y=index+dist)) +
  # add MAOC responses
  geom_point(data=MAOC_suppcat2, color=mcol, aes(y=index, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  geom_errorbarh(data = MAOC_suppcat2, height=.1, color=mcol, aes(y=index)) +
  # add SOC responses
  geom_point(data=SOC_suppcat2, color=scol, aes(y=index-dist, x=mean.mpc, xmin=lci.mpc, xmax=uci.mpc)) +
  geom_errorbarh(data = SOC_suppcat2, height=.1, color=scol, aes(y=index-dist)) +
  # specify axes
  scale_y_continuous(name = "", breaks=c(8:1), expand = c(0, 0), 
                     labels=c("Size", "Density",  
                              "Sodium hexametaphosphate", "Other solution", "Glass beads", "Sonication", 
                              "Direct", "Indirect"), 
                     limits=c(0.5,8.5)) + 
  scale_x_continuous(limits=c(-33,xmaxim), breaks= seq(-20,100,20),
                     sec.axis = sec_axis(~ log((. / 100)+1), name = "Log response ratio")) +
  labs(title='', x='Percent change', y = '') +
  # set theme
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(size = 0.4)) +
  # add labels for moderators     2.5, 7.5, 9.5, 13.5,17.5
  annotate(geom= "text", x=rep(xmaxim,3), y=c(7.5, 6, 1.75), hjust = 1,vjust=0.5, fontface =3,
           label=c("Fractionation\nmethod", "Dispersing\nagent", "Measurement")) +
  # add sample sizes
  annotate(geom= "text", x=POC_suppcat2$uci.mpc+4, y=c(POC_suppcat2$index+dist), 
           hjust = 0, size=2, label=paste0(POC_suppcat2$nobs, " / ", POC_suppcat2$nstudy)) +
  annotate(geom= "text", x=MAOC_suppcat2$uci.mpc+4, y=c(MAOC_suppcat2$index), 
           hjust = 0, size=2, label=paste0(MAOC_suppcat2$nobs, " / ", MAOC_suppcat2$nstudy)) +
  annotate(geom= "text", x=SOC_suppcat2$uci.mpc+4, y=c(SOC_suppcat2$index-dist), 
           hjust = 0, size=2, label=paste0(SOC_suppcat2$nobs, " / ", SOC_suppcat2$nstudy))

png("Figures/S2_.png", width=5, height=5.5, units="in",res=300)
plots2
dev.off()





