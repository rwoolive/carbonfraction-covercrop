




# Use metafor function rma.mv to model effects of cover cropping 
# on soil carbon pools while allowing for variation at three levels:  
# 1. within observations, 
# 2. among observations of a single study, and 
# 3. among studies
# For tutorial, see Assink & Wibbelink (2016) TQMP

library(metafor) # version 3.4-0
library(ggplot2)
library(plyr)


############################### 
### read in data
dat0 <- read.csv("Raw-data/4_metadata - final_withclimdat_2.csv")

hist(dat0$logRR) # check histogram of effect sizes
unique(dat0$Continent) # 6 continents


############################### 
### types of responses
dat0$rv.type <- as.factor(dat0$rv.type)
respvars <- table(dat0$StudyID, dat0$rv.type)
write.csv(respvars, "Processed-data/1_response variables across studies.csv")



############################### 
### moderators

# categorical variables to include in main analysis

dat0$depth <- as.factor(dat0$depth.type)
dat0$depth <- factor(dat0$depth, levels(dat0$depth)[c(3,2,1)])
dat0$depth # check

dat0$ccseason.spring <- as.factor(dat0$ccseason.spring)
dat0$ccseason.summer <- as.factor(dat0$ccseason.summer)
dat0$ccseason.winter <- as.factor(dat0$ccseason.winter)
dat0$ccseason.yearround <- as.factor(dat0$ccseason.yearround)

dat0$StudyID[which(dat0$ccseason.spring==1 & dat0$rv %in% c("MAOC", "MAOM"))]


hist(dat0$lat.decimal[which(dat0$ccseason.spring==1)])
hist(dat0$lat.decimal[which(dat0$ccseason.summer==1)])
range(abs(dat0$lat.decimal[which(dat0$ccseason.winter==1)]))
range(abs(dat0$lat.decimal[which(dat0$ccseason.yearround==1)]))


dat0$tillage[which(dat0$tillage=="not stated")] <- NA
dat0$tillage[which(dat0$tillage %in% c("no-till", "reduced"))] <- "reduced or no-till"
dat0$tillage <- as.factor(dat0$tillage)


unique(dat0$maincrop[which(dat0$maincrop.cereal==1)])
unique(dat0$maincrop[which(dat0$maincrop.legume==1)])
unique(dat0$maincrop[which(dat0$maincrop.cereallegume==1)])
unique(dat0$maincrop[which(dat0$maincrop.otherrowcrop==1)])
unique(dat0$maincrop[which(dat0$maincrop.perennial==1)])
dat0$maincrop.category <- rep(NA, dim(dat0)[1])
dat0$maincrop.category[which(dat0$maincrop.cereal==1)] <- "cereal"
dat0$maincrop.category[which(dat0$maincrop.legume==1)] <- "legume"
dat0$maincrop.category[which(dat0$maincrop.cereallegume==1)] <- "cereal+legume"
dat0$maincrop.category[which(dat0$maincrop.otherrowcrop==1)] <- "other row crop"
dat0$maincrop.category[which(dat0$maincrop.perennial==1)] <- "perennial"
dat0$cropping.system <- dat0$maincrop.category
dat0$cropping.system <- as.factor(dat0$cropping.system)
dat0$cropping.system <- factor(dat0$cropping.system, levels(dat0$cropping.system)[c(1,3,2,4,5)])



dat0$cover.crop.category <- rep(NA, dim(dat0)[1])
dat0$cover.crop.category[which(dat0$ccadded.grass==1)] <- "grass"
dat0$cover.crop.category[which(dat0$ccadded.legume==1)] <- "legume"
dat0$cover.crop.category[which(dat0$ccadded.grasslegume==1)] <- "grass+legume"
dat0$cover.crop.category[which(dat0$ccadded.brassica==1)] <- "brassica"
dat0$cover.crop.category[which(dat0$ccadded.grassbrassica==1)] <- "grass+brassica"
dat0$cover.crop.category[which(dat0$ccadded.legumebrassica==1)] <- "legume+brassica"
dat0$cover.crop.category[which(dat0$ccadded.grasslegumebrassica==1)] <- "grass+legume+brassica"
dat0$cover.crop <- as.factor(dat0$cover.crop.category)
dat0$cover.crop <- factor(dat0$cover.crop, levels(dat0$cover.crop)[c(2,6,1,4,3,7,5)])

sort(table(dat0$cover.crop.added[which(dat0$cover.crop.category=="grass")] ))
sort(table(dat0$cover.crop.added[which(dat0$cover.crop.category=="legume")] ))
sort(table(dat0$cover.crop.added[which(dat0$cover.crop.category=="grass+legume")] ))
sort(table(dat0$cover.crop.added[which(dat0$cover.crop.category=="brassica")] ))
sort(table(dat0$cover.crop.added[which(dat0$cover.crop.category=="grass+brassica")] ))
sort(table(dat0$cover.crop.added[which(dat0$cover.crop.category=="legume+brassica")] ))
sort(table(dat0$cover.crop.added[which(dat0$cover.crop.category=="grass+legume+brassica")] ))


# continuous variables to include in main analysis

hist(dat0$Baseline...sand); dat0$sand <- dat0$Baseline...sand
hist(dat0$Baseline...silt); dat0$silt <- dat0$Baseline...silt
hist(dat0$Baseline...clay); dat0$clay <- dat0$Baseline...clay
hist(dat0$Baseline.TOC..); dat0$toc <- dat0$Baseline.TOC..
hist(dat0$Baseline.pH); dat0$ph <- dat0$Baseline.pH
#hist(dat0$number.of.crop.species.in.control); dat0$species.control <- dat0$number.of.crop.species.in.control
hist(dat0$number.crop.species.added); dat0$species.added <- dat0$number.crop.species.added
hist(dat0$ag.C.inputs.cover.crops..Mg.ha.1.yr.1.); dat0$ag.C.inputs <- dat0$ag.C.inputs.cover.crops..Mg.ha.1.yr.1.
hist(dat0$bg.C.inputs.cover.crops..Mg.ha.1.yr.1.); dat0$bg.C.inputs.cover.crops..Mg.ha.1.yr.1.[which(dat0$bg.C.inputs.cover.crops..Mg.ha.1.yr.1.>10)] <- NA
#hist(dat0$bg.C.inputs.cover.crops..Mg.ha.1.yr.1.); dat0$bg.C.inputs <- dat0$bg.C.inputs.cover.crops..Mg.ha.1.yr.1.
hist(dat0$duration..yr.); dat0$duration <- dat0$duration..yr.
dat0$duration[which(dat0$StudyID=="Sala")] <- 8 # fix data entry error
hist(dat0$N.fertilization.rate..kg.ha.1.yr.1.); dat0$nfert <- dat0$N.fertilization.rate..kg.ha.1.yr.1.
#hist(dat0$residue.removal....); dat0$residue.removal <- dat0$residue.removal....



# variables to include in supplementary analysis

dat0$soilorder <- as.factor(dat0$soilorder)
dat0$soilorder # check

dat0$Continent <- as.factor(dat0$Continent)

hist(dat0$Temp)

hist(dat0$Prec)

dat0$SoilCollectionSeason <- as.factor(dat0$SoilCollectionSeason)
dat0$SoilCollectionSeason <- factor(dat0$SoilCollectionSeason, levels(dat0$SoilCollectionSeason)[c(5,2,4,1,3)])

dat0$FractionationMethod <- as.factor(dat0$FractionationMethod)

dat0$DispersingAgent <- as.factor(dat0$DispersingAgent)
dat0$DispersingAgent <- factor(dat0$DispersingAgent, levels(dat0$DispersingAgent)[c(4,3,2,5,1)])

dat0$ResponseCalc <- rep("indirect", dim(dat0)[1])
dat0$ResponseCalc[which(dat0$ResponseCalc.Direct==1)] <- "direct"
dat0$ResponseCalc <- as.factor(dat0$ResponseCalc)




### give each study a unique identifier
StudyIDs <- data.frame(StudyID = unique(dat0$StudyID),
                       StudyNum = seq(1:length(unique(dat0$StudyID))))
dat0$StudyNum <- rep(NA, dim(dat0)[1])
for(i in 1:dim(dat0)[1]){
  dat0$StudyNum[i] <- StudyIDs$StudyNum[which(StudyIDs$StudyID==dat0$StudyID[i])]
}
write.csv(StudyIDs, "Processed-data/0_StudyIDs.csv")


# subset data to pom, maom, and tom pools
dat0$rv.type <- as.factor(dat0$rv.type)
dat_POC <- dat0[which(dat0$rv.type=="pom fraction" & is.na(dat0$logRR)==FALSE & is.na(dat0$var)==FALSE),]
dat_MAOC <- dat0[which(dat0$rv.type=="maom fraction" & is.na(dat0$logRR)==FALSE & is.na(dat0$var)==FALSE),]
dat_SOC <- dat0[which(dat0$rv.type=="total organic matter" & is.na(dat0$logRR)==FALSE & is.na(dat0$var)==FALSE),]

unique(dat_POC$rv)
unique(dat_MAOC$rv)
unique(dat_SOC$rv)

sampsizes <- data.frame(datatype = c("All", "POC", "MAOC", "SOC"),
                        numstud = c(dim(table(dat0$StudyID)), dim(table(dat_POC$StudyID)), dim(table(dat_MAOC$StudyID)), dim(table(dat_SOC$StudyID))),
                        numobs = c(dim(table(dat0$Obs.ID)), dim(table(dat_POC$Obs.ID)), dim(table(dat_MAOC$Obs.ID)),  dim(table(dat_SOC$Obs.ID))),
                        averageduration = c(mean(na.omit(dat0$duration)), mean(na.omit(dat_POC$duration)), mean(na.omit(dat_MAOC$duration)), mean(na.omit(dat_SOC$duration))),
                        averagedepthmidpoint = c(mean(dat0$depth.lower.limit..cm.-dat0$depth.upper.limit..cm.), mean(dat_POC$depth.lower.limit..cm.-dat_POC$depth.upper.limit..cm.), mean(dat_MAOC$depth.lower.limit..cm.-dat_MAOC$depth.upper.limit..cm.), mean(dat_SOC$depth.lower.limit..cm.-dat_SOC$depth.upper.limit..cm.) ),
                        averagenfert = c(mean(na.omit(dat0$nfert)), mean(na.omit(dat_POC$nfert)), mean(na.omit(dat_MAOC$nfert)), mean(na.omit(dat_SOC$nfert))),
                        averagecinputs = c(mean(na.omit(dat0$ag.C.inputs)), mean(na.omit(dat_POC$ag.C.inputs)), mean(na.omit(dat_MAOC$ag.C.inputs)), mean(na.omit(dat_SOC$ag.C.inputs))))





write.csv(sampsizes, "Processed-data/0_summary-data.csv")
write.csv(dat_POC, "Processed-data/POC.csv")
write.csv(dat_MAOC, "Processed-data/MAOC.csv")
write.csv(dat_SOC, "Processed-data/SOC.csv")

dat_POC <- read.csv("Processed-data/POC.csv")
dat_MAOC <- read.csv("Processed-data/MAOC.csv")
dat_SOC <- read.csv("Processed-data/SOC.csv")


mean(na.omit(dat_POC$duration))
mean(na.omit(dat_MAOC$duration))
mean(na.omit(dat_SOC$duration))


### data exploration


datcm <- data.frame(type=c("carbon", "matter"),
                    n_POC=c(length(dat_POC$logRR[which(dat_POC$rv == "POC")]), length(dat_POC$logRR[which(dat_POC$rv == "POM")])),
                    n_MAOC=c(length(dat_MAOC$logRR[which(dat_MAOC$rv == "MAOC")]), length(dat_MAOC$logRR[which(dat_MAOC$rv == "MAOM")])),
                    n_SOC=c(length(dat_SOC$logRR[which(dat_SOC$rv == "SOC")]), length(dat_SOC$logRR[which(dat_SOC$rv == "SOM")])),
                    n_tot=rep(NA,2),
                    nstud_tot= c(length(unique(c(dat_POC$StudyID[which(dat_POC$rv == "POC")], dat_MAOC$StudyID[which(dat_MAOC$rv == "MAOC")], dat_SOC$StudyID[which(dat_SOC$rv == "SOC")]))), 
                                 length(unique(c(dat_POC$StudyID[which(dat_POC$rv == "POM")], dat_MAOC$StudyID[which(dat_MAOC$rv == "MAOM")], dat_SOC$StudyID[which(dat_SOC$rv == "SOM")])))))
datcm$n_tot <- datcm$n_POC + datcm$n_MAOC + datcm$n_SOC

write.csv(datcm, "Processed-data/0_sample size for OC vs. OM.csv")


# depth of each fraction for year-round cover crop season
table(dat_POC$ccseason.yearround, dat_POC$depth)
33/(33+16) # 0.6734694 surface
table(dat_MAOC$ccseason.yearround, dat_MAOC$depth)
18/(18+5) # 0.7826087 surface
table(dat_SOC$ccseason.yearround, dat_SOC$depth)
22/(22+9) # 0.7096774 surface


# unit overall
table(dat_POC$logRR.type)[1]/(table(dat_POC$logRR.type)[1]+table(dat_POC$logRR.type)[2]) # 94% g kg-1 
table(dat_MAOC$logRR.type)[1]/(table(dat_MAOC$logRR.type)[1]+table(dat_MAOC$logRR.type)[2]) # 93% g kg-1 
table(dat_SOC$logRR.type)[1]/(table(dat_SOC$logRR.type)[1]+table(dat_SOC$logRR.type)[2]) # 90% g kg-1 

# see if responses differ by unit reported
muP <- ddply(dat_POC, "logRR.type", summarise, grp.mean=mean(logRR), 
             grp.sd=sd(logRR), grp.n=length(logRR))
muP
(exp(muP$grp.mean)-1)*100 # effect size larger for mg/ha than g/kg

muM <- ddply(dat_MAOC, "logRR.type", summarise, grp.mean=mean(logRR), 
             grp.sd=sd(logRR), grp.n=length(logRR))
muM
(exp(muM$grp.mean)-1)*100 # effect size larger for g/kg than mg/ha  
(exp(muM$grp.sd)-1)*100 # effect size larger for g/kg than mg/ha  

muS <- ddply(dat_SOC, "logRR.type", summarise, grp.mean=mean(logRR), 
             grp.sd=sd(logRR), grp.n=length(logRR))
muS
(exp(muS$grp.mean)-1)*100 # effect size larger for mg/ha than g/kg

# explore each fraction for year-round cover crop season
poc_yr <- dat_POC[which(dat_POC$ccseason.yearround==1),]
poc_yr$logRR.type # all reports of g kg-1
maoc_yr <- dat_MAOC[which(dat_MAOC$ccseason.yearround==1),]
maoc_yr$logRR.type # all reports of g kg-1
soc_yr <- dat_SOC[which(dat_SOC$ccseason.yearround==1),]
soc_yr$logRR.type # mmost reports of g kg-1, 4 mg ha-1

# see if year-round measurements of SOC differ by unit reported
mu <- ddply(soc_yr, "logRR.type", summarise, grp.mean=mean(logRR), 
            grp.sd=sd(logRR), grp.n=length(logRR))
mu
(exp(mu$grp.mean)-1)*100
(exp(mu$grp.sd)-1)*100

ggplot(data=soc_yr, 
       aes(x=logRR, fill=logRR.type, color=logRR.type)) + 
  geom_histogram(position="identity", alpha=0.25, bins=25) +
  theme_classic() +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=logRR.type),
             linetype="dashed", size=0.5, show.legend = F)+
  geom_vline(data=mu, aes(xintercept=grp.mean-grp.sd, color=logRR.type),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_vline(data=mu, aes(xintercept=grp.mean+grp.sd, color=logRR.type),
             linetype="dotted", size=0.5, show.legend = F)+ 
  geom_text(data=mu, aes(x=grp.mean+0.01, color=logRR.type, label=paste("Mean = ", round(grp.mean, 3))), 
            y=c(9,8), hjust = 0, show.legend = F) +
  labs(fill="Initial pool", y="Frequency", x="Effect size") +
  guides(color=F) + #xlim(xs) +
  theme(legend.position="bottom")+
  scale_y_continuous(limits=c(0,10), expand = c(0, 0)) 


# explore each fraction for cereal cropping systems
poc_yr <- dat_POC[which(dat_POC$maincrop.cereal==1),]
poc_yr$logRR.type # all reports of g kg-1
maoc_yr <- dat_MAOC[which(dat_MAOC$maincrop.cereal==1),]
maoc_yr$logRR.type # all reports of g kg-1
soc_yr <- dat_SOC[which(dat_SOC$maincrop.cereal==1),]
soc_yr$logRR.type # most reports of g kg-1, 12 mg ha-1

# see if cereal cropping systems differ by unit reported
mu <- ddply(soc_yr, "logRR.type", summarise, grp.mean=mean(logRR), 
            grp.sd=sd(logRR), grp.n=length(logRR))
mu
(exp(mu$grp.mean)-1)*100
(exp(mu$grp.sd)-1)*100



# representation of SOC in studies that report strong POC and MAOC in cereal systems
poc_stud <-  plyr::ddply(poc_yr,"StudyID", dplyr::summarise, poc.mean=mean(logRR), poc.sd=sd(logRR))
maoc_stud <-  plyr::ddply(maoc_yr,"StudyID", dplyr::summarise, maoc.mean=mean(logRR), maoc.sd=sd(logRR))
soc_stud <-  plyr::ddply(soc_yr,"StudyID", dplyr::summarise, soc.mean=mean(logRR), soc.sd=sd(logRR))
pm_stud <- merge(poc_stud, maoc_stud, by = "StudyID", all=T)
pms_stud <- merge(pm_stud, soc_stud, by = "StudyID", all=T)
View(pms_stud)

# representation of SOC in studies that report strong POC and MAOC
poc_stud <-  plyr::ddply(dat_POC,"StudyID", dplyr::summarise, poc.mean=mean(logRR), poc.sd=sd(logRR))
maoc_stud <-  plyr::ddply(dat_MAOC,"StudyID", dplyr::summarise, maoc.mean=mean(logRR), maoc.sd=sd(logRR))
soc_stud <-  plyr::ddply(dat_SOC,"StudyID", dplyr::summarise, soc.mean=mean(logRR), soc.sd=sd(logRR))
pm_stud <- merge(poc_stud, maoc_stud, by = "StudyID", all=T)
pms_stud <- merge(pm_stud, soc_stud, by = "StudyID", all=T)
View(pms_stud)


# which studies used brassicas?
poc_bras <- dat_POC[which(dat_POC$ccadded.brassica==1),]; unique(poc_bras$StudyID)
unique(poc_bras$maincrop) #  "corn-common bean-soybean" "rice" "winter wheat,sorghum"    
unique(poc_bras$cover.crop.added) # "oilseed radish" "oilseed rape"   "canola" 
mean(poc_bras$ag.C.inputs) # 1.393063
soc_bras <- dat_SOC[which(dat_SOC$ccadded.brassica==1),]; unique(soc_bras$StudyID)
unique(soc_bras$maincrop) # "corn-common bean-soybean" "rice" "winter wheat,sorghum" "wheat,barley,spring oat,rapeseed,maize"
unique(soc_bras$cover.crop.added) # "oilseed radish" "oilseed rape" "canola" "mustard"
mean(soc_bras$ag.C.inputs) # 1.233246

# which studies used grass+legume+brassica?
poc_bras <- dat_POC[which(dat_POC$ccadded.grasslegumebrassica==1),]; unique(poc_bras$StudyID)
unique(poc_bras$maincrop) # "corn" "corn, soybean" "winter wheat,sorghum" "winter wheat,pea,potato,barley,red clover"
mean(poc_bras$ag.C.inputs) # 0.599049
soc_bras <- dat_SOC[which(dat_SOC$ccadded.grasslegumebrassica==1),]; unique(soc_bras$StudyID)
unique(soc_bras$maincrop) # "corn" "corn, soybean" "winter wheat,sorghum" "winter wheat,pea,potato,barley,red clover"
mean(soc_bras$ag.C.inputs) # 0.6142273

table(dat_POC$Continent, dat_POC$soilorder)
table(dat_POC$Continent, dat_POC$depth)
104/(104+45) # 71% surface soils for south america, 
table(dat_MAOC$Continent, dat_MAOC$depth)
72/(72+34) # 68% surface soils
table(dat_SOC$Continent, dat_SOC$depth)
105/(105+41) # 72% surface soils

poc_agc <- dat_POC %>%
  group_by(Continent) %>%
  dplyr::summarize(Mean = mean(ag.C.inputs, na.rm=TRUE))
2.38/mean(c(0.68, 0.611)) # ag.C.inputs 3.687064 times higher in south america than europe or north america
maoc_agc <- dat_MAOC %>%
  group_by(Continent) %>%
  dplyr::summarize(Mean = mean(ag.C.inputs, na.rm=TRUE))
2.58/mean(c(0.597, 1.03)) # ag.C.inputs 2.667486 times higher in south america than europe or north america
soc_agc <- dat_SOC %>%
  group_by(Continent) %>%
  dplyr::summarize(Mean = mean(ag.C.inputs, na.rm=TRUE))
2.29/mean(c(0.625, 0.517)) # ag.C.inputs 4.010508 times higher in south america than europe or north america

mean(c(2.38, 2.58)) # south american mean ag.C.input for POC/MAOC effect sizes
mean(c(0.68, 0.597)) # european mean ag.C.input for POC/MAOC effect sizes
mean(c(0.611, 1.03)) # north american mean ag.C.input for POC/MAOC effect sizes




### supplementary fig

# depth distribution

dat_POC <- dat_POC[order(dat_POC$depth.upper.limit..cm., dat_POC$depth.lower.limit..cm.),]
dat_POC$number <- seq(1,dim(dat_POC)[1])
POC_depth <- ggplot()+
  geom_segment(size=0.2,data=dat_POC, aes(x = number, y = depth.upper.limit..cm., xend = number, yend = depth.lower.limit..cm.)) +
  scale_y_reverse(limits = c(100, 0), breaks=seq(0,100,by=20)) + xlim(0,430) +
  theme_bw() + labs(y="Soil depth (cm)", x="Observation", title="A) POC") 

dat_MAOC <- dat_MAOC[order(dat_MAOC$depth.upper.limit..cm., dat_MAOC$depth.lower.limit..cm.),]
dat_MAOC$number <- seq(1,dim(dat_MAOC)[1])
MAOC_depth <- ggplot()+
  geom_segment(size=0.2,data=dat_MAOC, aes(x = number, y = depth.upper.limit..cm., xend = number, yend = depth.lower.limit..cm.)) +
  scale_y_reverse(limits = c(100, 0), breaks=seq(0,100,by=20)) + xlim(0,430) +
  theme_bw() + labs(y="Soil depth (cm)", x="Observation", title="B) MAOC") 

dat_SOC <- dat_SOC[order(dat_SOC$depth.upper.limit..cm., dat_SOC$depth.lower.limit..cm.),]
dat_SOC$number <- seq(1,dim(dat_SOC)[1])
SOC_depth <- ggplot()+
  geom_segment(size=0.2,data=dat_SOC, aes(x = number, y = depth.upper.limit..cm., xend = number, yend = depth.lower.limit..cm.)) +
  scale_y_reverse(limits = c(100, 0), breaks=seq(0,100,by=20)) + xlim(0,430) +
  theme_bw() + labs(y="Soil depth (cm)", x="Observation", title="C) SOC") 

depthfig <- ggpubr::ggarrange(POC_depth, MAOC_depth, SOC_depth, ncol = 1, nrow = 3)
depthfig
#ggsave("Figures/S5_depth distribution by obsevrvation.png", units="in", height=8, width=8)
jpeg("Figures/A.5_depth distribution by obsevrvation.jpeg", width=8, height=8, units="in",res=600)
depthfig
dev.off()

# depth distribution for year-round cover crop season
poc_yr <- poc_yr[order(poc_yr$depth.upper.limit..cm., poc_yr$depth.lower.limit..cm.),]
poc_yr$number <- seq(1,dim(poc_yr)[1])
POC_depth <- ggplot()+
  geom_segment(size=0.2,data=poc_yr, aes(x = number, y = depth.upper.limit..cm., xend = number, yend = depth.lower.limit..cm.)) +
  scale_y_reverse(limits = c(100, 0), breaks=seq(0,100,by=20)) + xlim(0,75) +
  theme_bw() + labs(y="Soil depth (cm)", x="Observation", title="A) POC") 

maoc_yr <- maoc_yr[order(maoc_yr$depth.upper.limit..cm., maoc_yr$depth.lower.limit..cm.),]
maoc_yr$number <- seq(1,dim(maoc_yr)[1])
MAOC_depth <- ggplot()+
  geom_segment(size=0.2,data=maoc_yr, aes(x = number, y = depth.upper.limit..cm., xend = number, yend = depth.lower.limit..cm.)) +
  scale_y_reverse(limits = c(100, 0), breaks=seq(0,100,by=20)) + xlim(0,75) +
  theme_bw() + labs(y="Soil depth (cm)", x="Observation", title="B) MAOC") 

soc_yr <- soc_yr[order(soc_yr$depth.upper.limit..cm., soc_yr$depth.lower.limit..cm.),]
soc_yr$number <- seq(1,dim(soc_yr)[1])
SOC_depth <- ggplot()+
  geom_segment(size=0.2,data=soc_yr, aes(x = number, y = depth.upper.limit..cm., xend = number, yend = depth.lower.limit..cm.)) +
  scale_y_reverse(limits = c(100, 0), breaks=seq(0,100,by=20)) + xlim(0,75) +
  theme_bw() + labs(y="Soil depth (cm)", x="Observation", title="C) SOC") 

depthfig <- ggpubr::ggarrange(POC_depth, MAOC_depth, SOC_depth, ncol = 1, nrow = 3)
depthfig
#ggsave("Figures/S5_depth distribution by obsevrvation_year-round.png", units="in", height=6, width=4)



# IS MAOC A FUNCTION OF SOIL TEXTURE?
anova(lm(data=dat_MAOC, formula=mean.control.in.g.kg.1~Baseline...sand))
MAOC_sand_control <- ggplot(data=dat_MAOC, aes(x = Baseline...sand, y = mean.control.in.g.kg.1))+
  geom_point(size=1, alpha=0.5) + lims(y=c(0,40)) +
  theme_bw() + labs(y="MAOC (g/kg soil)", x="%Sand", title="Control") + geom_smooth(method = "lm", se = T)
anova(lm(data=dat_MAOC, formula=mean.treatment.in.g.kg.1~Baseline...sand))
MAOC_sand_trt <- ggplot(data=dat_MAOC, aes(x = Baseline...sand, y = mean.treatment.in.g.kg.1))+
  geom_point(size=1, alpha=0.5) + lims(y=c(0,40)) +
  theme_bw() + labs(y="MAOC (g/kg soil)", x="%Sand", title="Treatment") + geom_smooth(method = "lm", se = T)
MAOC_sand <- ggpubr::ggarrange(MAOC_sand_control, MAOC_sand_trt, ncol = 2, nrow = 1, labels = c("A", "B"))

anova(lm(data=dat_MAOC, formula=mean.control.in.g.kg.1~Baseline...silt))
MAOC_silt_control <- ggplot(data=dat_MAOC, aes(x = Baseline...silt, y = mean.control.in.g.kg.1))+
  geom_point(size=1, alpha=0.5) + lims(y=c(0,40)) +
  theme_bw() + labs(y="MAOC (g/kg soil)", x="%silt", title="") + geom_smooth(method = "lm", se = T)
anova(lm(data=dat_MAOC, formula=mean.treatment.in.g.kg.1~Baseline...silt))
MAOC_silt_trt <- ggplot(data=dat_MAOC, aes(x = Baseline...silt, y = mean.treatment.in.g.kg.1))+
  geom_point(size=1, alpha=0.5) + lims(y=c(0,40)) +
  theme_bw() + labs(y="MAOC (g/kg soil)", x="%silt", title="") + geom_smooth(method = "lm", se = T)
MAOC_silt <- ggpubr::ggarrange(MAOC_silt_control, MAOC_silt_trt, ncol = 2, nrow = 1, labels = c("C", "D"))

anova(lm(data=dat_MAOC, formula=mean.control.in.g.kg.1~Baseline...clay))
MAOC_clay_control <- ggplot(data=dat_MAOC, aes(x = Baseline...clay, y = mean.control.in.g.kg.1))+
  geom_point(size=1, alpha=0.5) + lims(y=c(0,40)) +
  theme_bw() + labs(y="MAOC (g/kg soil)", x="%clay", title="") + geom_smooth(method = "lm", se = T, linetype="dotted")
anova(lm(data=dat_MAOC, formula=mean.treatment.in.g.kg.1~Baseline...clay))
MAOC_clay_trt <- ggplot(data=dat_MAOC, aes(x = Baseline...clay, y = mean.treatment.in.g.kg.1))+
  geom_point(size=1, alpha=0.5) + lims(y=c(0,40)) +
  theme_bw() + labs(y="MAOC (g/kg soil)", x="%clay", title="") + geom_smooth(method = "lm", se = T, linetype="dotted")
MAOC_clay <- ggpubr::ggarrange(MAOC_clay_control, MAOC_clay_trt, ncol = 2, nrow = 1, labels = c("E", "F"))

MAOC_texture <- ggpubr::ggarrange(MAOC_sand, MAOC_silt, MAOC_clay, ncol = 1, nrow = 3)
MAOC_texture
#ggsave("Figures/S6_maoc by texture.png", units="in", height=8, width=8)
jpeg("Figures/A.6_maoc by texture.jpeg", width=8, height=8, units="in",res=600)
MAOC_texture
dev.off()








#### overall models
#### testing whether within-study variation is significant
#### estimating distribution of variance across the three levels
# note that sigma^2.1 is the variance between effect sizes within studies
# and sigma^2.2 is the variance between studies

r <- 70
effectsdat_POC <- data.frame(pool=rep(NA,r),
                             group=rep(NA,r),
                             mean=rep(NA,r),
                             lci=rep(NA,r),
                             uci=rep(NA,r),
                             nobs=rep(NA,r),
                             nstudy=rep(NA,r))
effectsdat_MAOC <- data.frame(pool=rep(NA,r),
                              group=rep(NA,r),
                              mean=rep(NA,r),
                              lci=rep(NA,r),
                              uci=rep(NA,r),
                              nobs=rep(NA,r),
                              nstudy=rep(NA,r))
effectsdat_SOC <- data.frame(pool=rep(NA,r),
                             group=rep(NA,r),
                             mean=rep(NA,r),
                             lci=rep(NA,r),
                             uci=rep(NA,r),
                             nobs=rep(NA,r),
                             nstudy=rep(NA,r))
r<-23
stats_POC <- data.frame(moderator=rep(NA,r),
                        Qm=rep(NA,r), # Qm = test of moderators
                        Qmdf=rep(NA,r),
                        Qmp=rep(NA,r),
                        Qe=rep(NA,r), # Qe = test of residual heterogeneity
                        Qedf=rep(NA,r),
                        Qep=rep(NA,r))
stats_MAOC <- data.frame(moderator=rep(NA,r),
                         Qm=rep(NA,r), # Qm = test of moderators
                         Qmdf=rep(NA,r),
                         Qmp=rep(NA,r),
                         Qe=rep(NA,r), # Qe = test of residual heterogeneity
                         Qedf=rep(NA,r),
                         Qep=rep(NA,r))
stats_SOC <- data.frame(moderator=rep(NA,r),
                        Qm=rep(NA,r), # Qm = test of moderators
                        Qmdf=rep(NA,r),
                        Qmp=rep(NA,r),
                        Qe=rep(NA,r), # Qe = test of residual heterogeneity
                        Qedf=rep(NA,r),
                        Qep=rep(NA,r))
# overall POC model
dat <- dat_POC
mod <- rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
effectsdat_POC[1,] <- c("POC","Overall", mod$b, mod$ci.lb, mod$ci.ub, mod$k,length(unique(dat$StudyID)))
stats_POC[1,] <- c("Overall", NA, NA, NA, mod$QE, mod$QMdf[2], mod$QEp)
# test for significance of variation within- and between-studies
mod_2 <- rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, sigma2=c(0,NA))
mod_3 <- rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, sigma2=c(NA,0))
# variance partitioning
sum.inverse.variances <- sum(1 / (dat$var))
list.inverse.variances.square <- 1 / (dat$var^2)
numerator <- (length(dat$var) - 1) * sum.inverse.variances
denominator <- (sum.inverse.variances) ^ 2 - sum(list.inverse.variances.square)
estimated.sampling.variance <- numerator / denominator
I2_1 <- (estimated.sampling.variance) / (mod$sigma2[1] + mod$sigma2[2] + estimated.sampling.variance)
I2_2 <- (mod$sigma2[1]) / (mod$sigma2[1] + mod$sigma2[2] + estimated.sampling.variance)
I2_3 <- (mod$sigma2[2]) / (mod$sigma2[1]  + mod$sigma2[2] + estimated.sampling.variance)
# write files
sink(file = "Model-output/1_Overall/POC.txt"); summary(mod); sink(file = NULL)
sink(file = "Model-output/2_Significance-of-variation/POC_within-study.txt"); anova(mod, mod_2); sink(file = NULL) # if p<0.05, within-study variance is significant
sink(file = "Model-output/2_Significance-of-variation/POC_between-study.txt"); anova(mod, mod_3); sink(file = NULL) # if p<0.05, between-study variance is significant
sink(file = "Model-output/3_Partitioning-of-variation/POC.txt"); data.frame(samplingvar = I2_1 * 100, withinstudyvar = I2_2 * 100, betweenstudyvar = I2_3 * 100); sink(file = NULL)


# overall POC model: excluding summed observations from subfractions
dat <- dat_POC[which(dat$summed==0),]
mod <- rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
effectsdat_POC[70,] <- c("POC_nosummed","Overall", mod$b, mod$ci.lb, mod$ci.ub, mod$k,length(unique(dat$StudyID)))
(exp(c(mod$b, mod$ci.lb, mod$ci.ub))-1)*100


# overall POC model: standardizing by study duration
dat <- dat_POC
mod <- rma.mv(logRR/duration, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
effectsdat_POC[69,] <- c("POC_annual","Overall", mod$b, mod$ci.lb, mod$ci.ub, mod$k,length(unique(dat$StudyID)))
(exp(0.0316)-1)*100


# overall MAOC model
dat <- dat_MAOC
mod <- rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
effectsdat_MAOC[1,] <- c("MAOC","Overall", mod$b, mod$ci.lb, mod$ci.ub, mod$k,length(unique(dat$StudyID)))
stats_MAOC[1,] <- c("Overall", NA, NA, NA, mod$QE, mod$QMdf[2], mod$QEp)
# test for significance of variation within- and between-studies
mod_2 <- rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, sigma2=c(0,NA))
mod_3 <- rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, sigma2=c(NA,0))
# variance partitioning
sum.inverse.variances <- sum(1 / (dat$var))
list.inverse.variances.square <- 1 / (dat$var^2)
numerator <- (length(dat$var) - 1) * sum.inverse.variances
denominator <- (sum.inverse.variances) ^ 2 - sum(list.inverse.variances.square)
estimated.sampling.variance <- numerator / denominator
I2_1 <- (estimated.sampling.variance) / (mod$sigma2[1] + mod$sigma2[2] + estimated.sampling.variance)
I2_2 <- (mod$sigma2[1]) / (mod$sigma2[1] + mod$sigma2[2] + estimated.sampling.variance)
I2_3 <- (mod$sigma2[2]) / (mod$sigma2[1]  + mod$sigma2[2] + estimated.sampling.variance)
# write files
sink(file = "Model-output/1_Overall/MAOC.txt"); summary(mod); sink(file = NULL)
sink(file = "Model-output/2_Significance-of-variation/MAOC_within-study.txt"); anova(mod, mod_2); sink(file = NULL) # if p<0.05, within-study variance is significant
sink(file = "Model-output/2_Significance-of-variation/MAOC_between-study.txt"); anova(mod, mod_3); sink(file = NULL) # if p<0.05, between-study variance is significant
sink(file = "Model-output/3_Partitioning-of-variation/MAOC.txt"); data.frame(samplingvar = I2_1 * 100, withinstudyvar = I2_2 * 100, betweenstudyvar = I2_3 * 100); sink(file = NULL)

# overall MAOC model without subfractions
dat <- dat_MAOC[which(dat$summed==0),]
mod <- rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
effectsdat_MAOC[70,] <- c("MAOC_nosummed","Overall", mod$b, mod$ci.lb, mod$ci.ub, mod$k,length(unique(dat$StudyID)))
(exp(c(mod$b, mod$ci.lb, mod$ci.ub))-1)*100

# overall MAOC model: standardizing by study duration
dat <- dat_MAOC
mod <- rma.mv(logRR/duration, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
effectsdat_MAOC[69,] <- c("MAOC_annual","Overall", mod$b, mod$ci.lb, mod$ci.ub, mod$k,length(unique(dat$StudyID)))
(exp(0.017265310082919)-1)*100


# overall SOC model
dat <- dat_SOC
mod <- rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
effectsdat_SOC[1,] <- c("SOC","Overall", mod$b, mod$ci.lb, mod$ci.ub, mod$k,length(unique(dat$StudyID)))
stats_SOC[1,] <- c("Overall", NA, NA, NA, mod$QE, mod$QMdf[2], mod$QEp)
# test for significance of variation within- and between-studies
mod_2 <- rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, sigma2=c(0,NA))
mod_3 <- rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, sigma2=c(NA,0))
# variance partitioning
sum.inverse.variances <- sum(1 / (dat$var))
list.inverse.variances.square <- 1 / (dat$var^2)
numerator <- (length(dat$var) - 1) * sum.inverse.variances
denominator <- (sum.inverse.variances) ^ 2 - sum(list.inverse.variances.square)
estimated.sampling.variance <- numerator / denominator
I2_1 <- (estimated.sampling.variance) / (mod$sigma2[1] + mod$sigma2[2] + estimated.sampling.variance)
I2_2 <- (mod$sigma2[1]) / (mod$sigma2[1] + mod$sigma2[2] + estimated.sampling.variance)
I2_3 <- (mod$sigma2[2]) / (mod$sigma2[1]  + mod$sigma2[2] + estimated.sampling.variance)
# write files
sink(file = "Model-output/1_Overall/SOC.txt"); summary(mod); sink(file = NULL)
sink(file = "Model-output/2_Significance-of-variation/SOC_within-study.txt"); anova(mod, mod_2); sink(file = NULL) # if p<0.05, within-study variance is significant
sink(file = "Model-output/2_Significance-of-variation/SOC_between-study.txt"); anova(mod, mod_3); sink(file = NULL) # if p<0.05, between-study variance is significant
sink(file = "Model-output/3_Partitioning-of-variation/SOC.txt"); data.frame(samplingvar = I2_1 * 100, withinstudyvar = I2_2 * 100, betweenstudyvar = I2_3 * 100); sink(file = NULL)

# overall SOC model: standardizing by study duration
dat <- dat_SOC
mod <- rma.mv(logRR/duration, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
effectsdat_SOC[69,] <- c("SOC_annual","Overall", mod$b, mod$ci.lb, mod$ci.ub, mod$k,length(unique(dat$StudyID)))
(exp(0.017265310082919)-1)*100



















#### Testing for moderator effects
# Rocci et al. 2021: Began by testing each moderator individually with each global
# change-fraction pairing and performed pairwise comparisons of each
# level of the moderator. 
# Note that you need "mods= ~0 + moderators" for categorical variables
# Note also that order does not matter when adding dummy-coded variables to the model




### start by analyzing POC
dat <- dat_POC

## depth
table(dat$StudyID, dat$depth.surface)
table(dat$StudyID, dat$depth.subsoil)
table(dat$StudyID, dat$depth.deepsoil) # remove, only one study represented
mod <- rma.mv(logRR, var, mods= ~0+depth.surface + depth.subsoil , 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_depth.type.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[2,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                        length(which(dat$depth.surface==1)), length(unique(dat$StudyID[which(dat$depth.surface==1)])))
effectsdat_POC[3,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                        length(which(dat$depth.subsoil==1)), length(unique(dat$StudyID[which(dat$depth.subsoil==1)])))

ref <- 2
stats_POC[ref,] <- c("depth", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)



# ccseason
table(dat$StudyID, dat$ccseason.winter)
table(dat$StudyID, dat$ccseason.spring)
table(dat$StudyID, dat$ccseason.summer)
table(dat$StudyID, dat$ccseason.yearround)
mod <- rma.mv(logRR, var, mods= ~0+ccseason.winter +  ccseason.spring + ccseason.summer + ccseason.yearround, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_ccseason.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[8,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                        length(which(dat$ccseason.winter==1)), length(unique(dat$StudyID[which(dat$ccseason.winter==1)])))
effectsdat_POC[9,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                        length(which(dat$ccseason.spring==1)), length(unique(dat$StudyID[which(dat$ccseason.spring==1)])))
effectsdat_POC[10,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$ccseason.summer==1)), length(unique(dat$StudyID[which(dat$ccseason.summer==1)])))
effectsdat_POC[11,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], 
                         length(which(dat$ccseason.yearround==1)), length(unique(dat$StudyID[which(dat$ccseason.yearround==1)])))

(exp(mod$b)-1)*100

ref <- 3
stats_POC[ref,] <- c("ccseason", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ccadded
table(dat$StudyID, dat$ccadded.grass)
table(dat$StudyID, dat$ccadded.legume)
table(dat$StudyID, dat$ccadded.grasslegume)
table(dat$StudyID, dat$ccadded.brassica)
table(dat$StudyID, dat$ccadded.grassbrassica) # remove
table(dat$StudyID, dat$ccadded.legumebrassica) # remove
table(dat$StudyID, dat$ccadded.grasslegumebrassica)
mod <- rma.mv(logRR, var, mods= ~0+ccadded.grass + ccadded.legume + ccadded.grasslegume + ccadded.brassica + ccadded.grasslegumebrassica, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_ccadded.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[13,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$ccadded.grass==1)), length(unique(dat$StudyID[which(dat$ccadded.grass==1)])))
effectsdat_POC[14,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$ccadded.legume ==1)), length(unique(dat$StudyID[which(dat$ccadded.legume ==1)])))
effectsdat_POC[15,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$ccadded.grasslegume ==1)), length(unique(dat$StudyID[which(dat$ccadded.grasslegume ==1)])))
effectsdat_POC[16,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], 
                         length(which(dat$ccadded.brassica==1)), length(unique(dat$StudyID[which(dat$ccadded.brassica==1)])))
effectsdat_POC[17,] <- c("POC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], 
                         length(which(dat$ccadded.grasslegumebrassica==1)), length(unique(dat$StudyID[which(dat$ccadded.grasslegumebrassica==1)])))

ref <- 4
stats_POC[ref,] <- c("ccadded", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## maincrop
table(dat$StudyID, dat$maincrop.cereal)
table(dat$StudyID, dat$maincrop.legume)
table(dat$StudyID, dat$maincrop.cereallegume)
table(dat$StudyID, dat$maincrop.otherrowcrop)
table(dat$StudyID, dat$maincrop.perennial)
mod <- rma.mv(logRR, var, mods= ~0+maincrop.cereal + maincrop.legume + maincrop.cereallegume + maincrop.otherrowcrop + maincrop.perennial, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_maincrop.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[20,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$maincrop.cereal==1)), length(unique(dat$StudyID[which(dat$maincrop.cereal==1)])))
effectsdat_POC[21,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$maincrop.legume ==1)), length(unique(dat$StudyID[which(dat$maincrop.legume ==1)])))
effectsdat_POC[22,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$maincrop.cereallegume ==1)), length(unique(dat$StudyID[which(dat$maincrop.cereallegume ==1)])))
effectsdat_POC[23,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], 
                         length(which(dat$maincrop.otherrowcrop==1)), length(unique(dat$StudyID[which(dat$maincrop.otherrowcrop==1)])))
effectsdat_POC[24,] <- c("POC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], 
                         length(which(dat$maincrop.perennial ==1)), length(unique(dat$StudyID[which(dat$maincrop.perennial ==1)])))

ref <- 5
stats_POC[ref,] <- c("maincrop", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## tillage
table(dat$StudyID, dat$tillage.conventional)
table(dat$StudyID, dat$tillage.reducednotill)
mod <- rma.mv(logRR, var, mods= ~0+tillage.conventional + tillage.reducednotill, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_tillage.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[25,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$tillage.conventional==1)), 
                         length(unique(dat$StudyID[which(dat$tillage.conventional==1)])))
effectsdat_POC[26,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$tillage.reducednotill==1)), length(unique(dat$StudyID[which(dat$tillage.reducednotill==1)])))

ref <- 6
stats_POC[ref,] <- c("tillage", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## nfert
mod <- rma.mv(logRR, var, mods= ~nfert, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
mod2 <- rma.mv(logRR, var, mods= ~nfert + I(nfert^2), random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
fitstats(mod, mod2)
sink(file = "Model-output/4_Single-moderator/POC_nfert.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[27,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$nfert)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$nfert)==FALSE)])))

ref <- 7
stats_POC[ref,] <- c("nfert", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ag.C.inputs
mod <- rma.mv(logRR, var, mods= ~ag.C.inputs, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_ag.C.inputs.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[28,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$ag.C.inputs)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ag.C.inputs)==FALSE)])))

ref <- 8
stats_POC[ref,] <- c("ag.C.inputs", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## species.added
mod <- rma.mv(logRR, var, mods= ~species.added, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_species.added.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[29,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$species.added)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$species.added)==FALSE)])))

ref <- 9
stats_POC[ref,] <- c("species.added", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## duration
mod <- rma.mv(logRR, var, mods= ~duration, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_duration.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[30,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$duration)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$duration)==FALSE)])))

ref <- 10
stats_POC[ref,] <- c("duration", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## sand
mod <- rma.mv(logRR, var, mods= ~sand, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_sand.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[31,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$sand)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$sand)==FALSE)])))

ref <- 11
stats_POC[ref,] <- c("sand", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## silt
mod <- rma.mv(logRR, var, mods= ~silt, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_silt.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[32,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$silt)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$silt)==FALSE)])))

ref <- 12
stats_POC[ref,] <- c("silt", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## clay
mod <- rma.mv(logRR, var, mods= ~clay, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_clay.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[33,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$clay)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$clay)==FALSE)])))

ref <- 13
stats_POC[ref,] <- c("clay", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## toc 
mod <- rma.mv(logRR, var, mods= ~toc, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_toc.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[34,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$toc)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$toc)==FALSE)])))

ref <- 14
stats_POC[ref,] <- c("toc", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ph
mod <- rma.mv(logRR, var, mods= ~ph, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
mod2 <- rma.mv(logRR, var, mods= ~ph + I(ph^2), random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
fitstats(mod, mod2)
sink(file = "Model-output/4_Single-moderator/POC_ph.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[35,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$ph)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ph)==FALSE)])))

ref <- 15
stats_POC[ref,] <- c("ph", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## Temp
mod <- rma.mv(logRR, var, mods= ~Temp, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
mod2 <- rma.mv(logRR, var, mods= ~Temp + I(Temp^2), random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
fitstats(mod, mod2)
sink(file = "Model-output/4_Single-moderator/POC_Temp.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[36,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$Temp)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Temp)==FALSE)])))

ref <- 16
stats_POC[ref,] <- c("Temp", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## Prec
mod <- rma.mv(logRR, var, mods= ~Prec, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
mod2 <- rma.mv(logRR, var, mods= ~Prec + I(Prec^2), random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
fitstats(mod, mod2)
sink(file = "Model-output/4_Single-moderator/POC_Prec.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[37,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$Prec)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Prec)==FALSE)])))

ref <- 17
stats_POC[ref,] <- c("Prec", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)




## soilorder 
table(dat$StudyID, dat$soilorder.Alfisols) 
table(dat$StudyID, dat$soilorder.Oxisols) 
table(dat$StudyID, dat$soilorder.Mollisols) 
table(dat$StudyID, dat$soilorder.Ultisols) 
table(dat$StudyID, dat$soilorder.Spodosols) # remove
table(dat$StudyID, dat$soilorder.Inceptisols) 
mod <- rma.mv(logRR, var, mods= ~0+soilorder.Alfisols + soilorder.Oxisols + soilorder.Mollisols + soilorder.Ultisols + soilorder.Inceptisols, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_soilorder.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[38,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$soilorder.Alfisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Alfisols==1)])))
effectsdat_POC[39,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$soilorder.Oxisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Oxisols==1)])))
effectsdat_POC[40,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$soilorder.Mollisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Mollisols==1)])))
effectsdat_POC[41,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], 
                         length(which(dat$soilorder.Ultisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Ultisols==1)])))
effectsdat_POC[42,] <- c("POC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], 
                         length(which(dat$soilorder.Inceptisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Inceptisols==1)])))

ref <- 18
stats_POC[ref,] <- c("soilorder", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## Continent
table(dat$StudyID, dat$Continent.Africa) # remove
table(dat$StudyID, dat$Continent.Asia) 
table(dat$StudyID, dat$Continent.Australia) # remove
table(dat$StudyID, dat$Continent.Europe) 
table(dat$StudyID, dat$Continent.NorthAmerica) 
table(dat$StudyID, dat$Continent.SouthAmerica) 
mod <- rma.mv(logRR, var, mods= ~0+ Continent.Asia + Continent.Europe + Continent.NorthAmerica + Continent.SouthAmerica, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_Continent.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[45,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$Continent.Asia==1)), length(unique(dat$StudyID[which(dat$Continent.Asia==1)])))
effectsdat_POC[46,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$Continent.Europe==1)), length(unique(dat$StudyID[which(dat$Continent.Europe==1)])))
effectsdat_POC[47,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$Continent.NorthAmerica==1)), length(unique(dat$StudyID[which(dat$Continent.NorthAmerica==1)])))
effectsdat_POC[48,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], 
                         length(which(dat$Continent.SouthAmerica==1)), length(unique(dat$StudyID[which(dat$Continent.SouthAmerica==1)])))

ref <- 19
stats_POC[ref,] <- c("Continent", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## SoilCollectionSeason
table(dat$StudyID, dat$SoilCollectionSeason.winter) 
table(dat$StudyID, dat$SoilCollectionSeason.spring) 
table(dat$StudyID, dat$SoilCollectionSeason.summer) 
table(dat$StudyID, dat$SoilCollectionSeason.fall) 
mod <- rma.mv(logRR, var, mods= ~0+SoilCollectionSeason.winter + SoilCollectionSeason.spring + SoilCollectionSeason.summer + SoilCollectionSeason.fall, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_SoilCollectionSeason.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[50,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$SoilCollectionSeason.winter==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.winter==1)])))
effectsdat_POC[51,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$SoilCollectionSeason.spring==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.spring==1)])))
effectsdat_POC[52,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$SoilCollectionSeason.summer==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.summer==1)])))
effectsdat_POC[53,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], 
                         length(which(dat$SoilCollectionSeason.fall==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.fall==1)])))

ref <- 20
stats_POC[ref,] <- c("SoilCollectionSeason", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## FractionationMethod
table(dat$StudyID, dat$FractionationMethod.Size) 
table(dat$StudyID, dat$FractionationMethod.Density) # remove
table(dat$StudyID, dat$FractionationMethod.SizeAndDensity) # remove
mod <- rma.mv(logRR, var, mods= ~0+FractionationMethod.Size  , 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_FractionationMethod.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[54,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$FractionationMethod.Size==1)), length(unique(dat$StudyID[which(dat$FractionationMethod.Size==1)])))

ref <- 21
stats_POC[ref,] <- c("FractionationMethod", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

# DispersingAgent
table(dat$StudyID, dat$DispersingAgent.SodiumHex) 
table(dat$StudyID, dat$DispersingAgent.OtherSolution) 
table(dat$StudyID, dat$DispersingAgent.GlassBeads) 
table(dat$StudyID, dat$DispersingAgent.Sonication) # remove, represents only one study
table(dat$StudyID, dat$DispersingAgent.Combo) # remove, represents only one study
mod <- rma.mv(logRR, var, mods= ~0+DispersingAgent.SodiumHex + DispersingAgent.OtherSolution + DispersingAgent.GlassBeads, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_DispersingAgent.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[57,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$DispersingAgent.SodiumHex==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.SodiumHex==1)])))
effectsdat_POC[58,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$DispersingAgent.OtherSolution==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.OtherSolution==1)])))
effectsdat_POC[59,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$DispersingAgent.GlassBeads==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.GlassBeads==1)])))

ref <- 22
stats_POC[ref,] <- c("DispersingAgent", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ResponseCalc
table(dat$StudyID, dat$ResponseCalc.Direct) 
table(dat$StudyID, dat$ResponseCalc.Indirect) 
mod <- rma.mv(logRR, var, mods= ~0+ResponseCalc.Direct + ResponseCalc.Indirect, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_ResponseCalc.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[62,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$ResponseCalc.Direct==1)), length(unique(dat$StudyID[which(dat$ResponseCalc.Direct==1)])))
effectsdat_POC[63,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$ResponseCalc.Indirect==1)), length(unique(dat$StudyID[which(dat$ResponseCalc.Indirect==1)])))

ref <- 23
stats_POC[ref,] <- c("ResponseCalc", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

write.csv(effectsdat_POC, "Model-output/4_Single-moderator/*effects-by-category_POC.csv")
write.csv(stats_POC, "Model-output/4_Single-moderator/*stats-by-category_POC.csv")







### analyze MAOC
dat <- dat_MAOC


## depth
table(dat$StudyID, dat$depth.surface)
table(dat$StudyID, dat$depth.subsoil)
table(dat$StudyID, dat$depth.deepsoil) # remove, no studies represented
mod <- rma.mv(logRR, var, mods= ~0+depth.surface + depth.subsoil , 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_depth.type.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[2,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$depth.surface==1)), length(unique(dat$StudyID[which(dat$depth.surface==1)])))
effectsdat_MAOC[3,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$depth.subsoil==1)), length(unique(dat$StudyID[which(dat$depth.subsoil==1)])))


ref <- 2
stats_MAOC[ref,] <- c("depth", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


# ccseason
table(dat$StudyID, dat$ccseason.winter)
table(dat$StudyID, dat$ccseason.spring) # remove
table(dat$StudyID, dat$ccseason.summer)
table(dat$StudyID, dat$ccseason.yearround)
mod <- rma.mv(logRR, var, mods= ~0+ccseason.winter +ccseason.summer + ccseason.yearround, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_ccseason.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[8,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$ccseason.winter==1)), length(unique(dat$StudyID[which(dat$ccseason.winter==1)])))
effectsdat_MAOC[9,] <- c("MAOC", rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                          length(which(dat$ccseason.summer==1)), length(unique(dat$StudyID[which(dat$ccseason.summer==1)])))
effectsdat_MAOC[10,] <- c("MAOC", rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                          length(which(dat$ccseason.yearround==1)), length(unique(dat$StudyID[which(dat$ccseason.yearround==1)])))

(exp(mod$b)-1)*100


ref <- 3
stats_MAOC[ref,] <- c("ccseason", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ccadded
table(dat$StudyID, dat$ccadded.grass)
table(dat$StudyID, dat$ccadded.legume)
table(dat$StudyID, dat$ccadded.grasslegume)
table(dat$StudyID, dat$ccadded.brassica) # remove
table(dat$StudyID, dat$ccadded.grassbrassica) # remove
table(dat$StudyID, dat$ccadded.legumebrassica) # remove
table(dat$StudyID, dat$ccadded.grasslegumebrassica) # remove
mod <- rma.mv(logRR, var, mods= ~0+ccadded.grass + ccadded.legume + ccadded.grasslegume, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_ccadded.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[13,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                          length(which(dat$ccadded.grass==1)), length(unique(dat$StudyID[which(dat$ccadded.grass==1)])))
effectsdat_MAOC[14,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                          length(which(dat$ccadded.legume ==1)), length(unique(dat$StudyID[which(dat$ccadded.legume ==1)])))
effectsdat_MAOC[15,] <- c("MAOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                          length(which(dat$ccadded.grasslegume==1)), length(unique(dat$StudyID[which(dat$ccadded.grasslegume==1)])))

ref <- 4
stats_MAOC[ref,] <- c("ccadded", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## maincrop
table(dat$StudyID, dat$maincrop.cereal)
table(dat$StudyID, dat$maincrop.legume)
table(dat$StudyID, dat$maincrop.cereallegume)
table(dat$StudyID, dat$maincrop.otherrowcrop) # remove
table(dat$StudyID, dat$maincrop.perennial)
mod <- rma.mv(logRR, var, mods= ~0+maincrop.cereal + maincrop.legume + maincrop.cereallegume + maincrop.perennial, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_maincrop.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[20,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                          length(which(dat$maincrop.cereal==1)), length(unique(dat$StudyID[which(dat$maincrop.cereal==1)])))
effectsdat_MAOC[21,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                          length(which(dat$maincrop.legume ==1)), length(unique(dat$StudyID[which(dat$maincrop.legume ==1)])))
effectsdat_MAOC[22,] <- c("MAOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                          length(which(dat$maincrop.cereallegume ==1)), length(unique(dat$StudyID[which(dat$maincrop.cereallegume ==1)])))
effectsdat_MAOC[23,] <- c("MAOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], 
                          length(which(dat$maincrop.perennial ==1)), length(unique(dat$StudyID[which(dat$maincrop.perennial ==1)])))

ref <- 5
stats_MAOC[ref,] <- c("maincrop", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## tillage
table(dat$StudyID, dat$tillage.conventional)
table(dat$StudyID, dat$tillage.reducednotill)
mod <- rma.mv(logRR, var, mods= ~0+tillage.conventional + tillage.reducednotill, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_tillage.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[25,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                          length(which(dat$tillage.conventional==1)), length(unique(dat$StudyID[which(dat$tillage.conventional==1)])))
effectsdat_MAOC[26,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                          length(which(dat$tillage.reducednotill==1)), length(unique(dat$StudyID[which(dat$tillage.reducednotill==1)])))

ref <- 6
stats_MAOC[ref,] <- c("tillage", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## nfert
mod <- rma.mv(logRR, var, mods= ~nfert, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
mod2 <- rma.mv(logRR, var, mods= ~nfert + I(nfert^2), random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
fitstats(mod, mod2)
sink(file = "Model-output/4_Single-moderator/MAOC_nfert.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[27,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$nfert)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$nfert)==FALSE)])))

ref <- 7
stats_MAOC[ref,] <- c("nfert", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ag.C.inputs
mod <- rma.mv(logRR, var, mods= ~ag.C.inputs, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_ag.C.inputs.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[28,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$ag.C.inputs)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ag.C.inputs)==FALSE)])))

ref <- 8
stats_MAOC[ref,] <- c("ag.C.inputs", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## species.added
mod <- rma.mv(logRR, var, mods= ~species.added, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_species.added.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[29,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$species.added)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$species.added)==FALSE)])))

ref <- 9
stats_MAOC[ref,] <- c("species.added", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## duration
mod <- rma.mv(logRR, var, mods= ~duration, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_duration.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[30,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$duration)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$duration)==FALSE)])))

ref <- 10
stats_MAOC[ref,] <- c("duration", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## sand
mod <- rma.mv(logRR, var, mods= ~sand, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_sand.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[31,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$sand)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$sand)==FALSE)])))

ref <- 11
stats_MAOC[ref,] <- c("sand", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## silt
mod <- rma.mv(logRR, var, mods= ~silt, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_silt.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[32,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$silt)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$silt)==FALSE)])))

ref <- 12
stats_MAOC[ref,] <- c("silt", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## clay
mod <- rma.mv(logRR, var, mods= ~clay, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_clay.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[33,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$clay)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$clay)==FALSE)])))

ref <- 13
stats_MAOC[ref,] <- c("clay", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## toc 
mod <- rma.mv(logRR, var, mods= ~toc, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_toc.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[34,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$toc)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$toc)==FALSE)])))

ref <- 14
stats_MAOC[ref,] <- c("toc", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ph
mod <- rma.mv(logRR, var, mods= ~ph, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
mod2 <- rma.mv(logRR, var, mods= ~ph + I(ph^2), random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
fitstats(mod, mod2)
sink(file = "Model-output/4_Single-moderator/MAOC_ph.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[35,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$ph)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ph)==FALSE)])))

ref <- 15
stats_MAOC[ref,] <- c("ph", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## Temp
mod <- rma.mv(logRR, var, mods= ~Temp, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
mod2 <- rma.mv(logRR, var, mods= ~Temp + I(Temp^2), random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
fitstats(mod, mod2)
sink(file = "Model-output/4_Single-moderator/MAOC_Temp.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[36,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$Temp)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Temp)==FALSE)])))

ref <- 16
stats_MAOC[ref,] <- c("Temp", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## Prec
mod <- rma.mv(logRR, var, mods= ~Prec, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
mod2 <- rma.mv(logRR, var, mods= ~Prec + I(Prec^2), random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
fitstats(mod, mod2)
sink(file = "Model-output/4_Single-moderator/MAOC_Prec.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[37,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$Prec)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Prec)==FALSE)])))

ref <- 17
stats_MAOC[ref,] <- c("Prec", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)



## soilorder 
table(dat$StudyID, dat$soilorder.Alfisols) 
table(dat$StudyID, dat$soilorder.Oxisols) # remove
table(dat$StudyID, dat$soilorder.Mollisols) 
table(dat$StudyID, dat$soilorder.Ultisols) # remove
table(dat$StudyID, dat$soilorder.Spodosols) # remove
table(dat$StudyID, dat$soilorder.Inceptisols) # remove
mod <- rma.mv(logRR, var, mods= ~0+soilorder.Alfisols + soilorder.Mollisols, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_soilorder.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[38,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                          length(which(dat$soilorder.Alfisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Alfisols==1)])))
effectsdat_MAOC[39,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                          length(which(dat$soilorder.Mollisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Mollisols==1)])))

ref <- 18
stats_MAOC[ref,] <- c("soilorder", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## Continent
table(dat$StudyID, dat$Continent.Africa) # remove
table(dat$StudyID, dat$Continent.Asia) # remove
table(dat$StudyID, dat$Continent.Australia) # remove
table(dat$StudyID, dat$Continent.Europe) 
table(dat$StudyID, dat$Continent.NorthAmerica) # remove
table(dat$StudyID, dat$Continent.SouthAmerica)
mod <- rma.mv(logRR, var, mods= ~0+Continent.Europe + Continent.SouthAmerica, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_Continent.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[47,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                          length(which(dat$Continent.Europe==1)), length(unique(dat$StudyID[which(dat$Continent.Europe==1)])))
effectsdat_MAOC[48,] <-  c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                           length(which(dat$Continent.SouthAmerica==1)), length(unique(dat$StudyID[which(dat$Continent.SouthAmerica==1)])))

ref <- 19
stats_MAOC[ref,] <- c("Continent", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## SoilCollectionSeason
table(dat$StudyID, dat$SoilCollectionSeason.winter) # remove
table(dat$StudyID, dat$SoilCollectionSeason.spring) 
table(dat$StudyID, dat$SoilCollectionSeason.summer) 
table(dat$StudyID, dat$SoilCollectionSeason.fall)
mod <- rma.mv(logRR, var, mods= ~0+ SoilCollectionSeason.spring + SoilCollectionSeason.summer + SoilCollectionSeason.fall, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_SoilCollectionSeason.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[51,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                          length(which(dat$SoilCollectionSeason.spring==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.spring==1)])))
effectsdat_MAOC[52,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                          length(which(dat$SoilCollectionSeason.summer==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.summer==1)])))
effectsdat_MAOC[53,] <- c("MAOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                          length(which(dat$SoilCollectionSeason.fall==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.fall==1)])))

ref <- 20
stats_MAOC[ref,] <- c("SoilCollectionSeason", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## FractionationMethod
table(dat$StudyID, dat$FractionationMethod.Size) 
table(dat$StudyID, dat$FractionationMethod.Density) # remove
table(dat$StudyID, dat$FractionationMethod.SizeAndDensity) # remove
mod <- rma.mv(logRR, var, mods= ~0+FractionationMethod.Size, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_FractionationMethod.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[54,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                          length(which(dat$FractionationMethod.Size==1)), length(unique(dat$StudyID[which(dat$FractionationMethod.Size==1)])))

ref <- 21
stats_MAOC[ref,] <- c("FractionationMethod", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

# DispersingAgent
table(dat$StudyID, dat$DispersingAgent.SodiumHex) 
table(dat$StudyID, dat$DispersingAgent.OtherSolution) 
table(dat$StudyID, dat$DispersingAgent.GlassBeads) 
table(dat$StudyID, dat$DispersingAgent.Sonication) # remove, represents only one study
table(dat$StudyID, dat$DispersingAgent.Combo) # remove, represents only one study
mod <- rma.mv(logRR, var, mods= ~0+DispersingAgent.SodiumHex + DispersingAgent.OtherSolution + DispersingAgent.GlassBeads, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_DispersingAgent.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[57,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                          length(which(dat$DispersingAgent.SodiumHex==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.SodiumHex==1)])))
effectsdat_MAOC[58,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                          length(which(dat$DispersingAgent.OtherSolution==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.OtherSolution==1)])))
effectsdat_MAOC[59,] <- c("MAOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                          length(which(dat$DispersingAgent.GlassBeads==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.GlassBeads==1)])))

ref <- 22
stats_MAOC[ref,] <- c("DispersingAgent", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ResponseCalc
table(dat$StudyID, dat$ResponseCalc.Direct) 
table(dat$StudyID, dat$ResponseCalc.Indirect)
mod <- rma.mv(logRR, var, mods= ~0+ResponseCalc.Direct + ResponseCalc.Indirect, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_ResponseCalc.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[62,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$ResponseCalc.Direct==1)), length(unique(dat$StudyID[which(dat$ResponseCalc.Direct==1)])))
effectsdat_MAOC[63,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$ResponseCalc.Indirect==1)), length(unique(dat$StudyID[which(dat$ResponseCalc.Indirect==1)])))

ref <- 23
stats_MAOC[ref,] <- c("ResponseCalc", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

write.csv(effectsdat_MAOC, "Model-output/4_Single-moderator/*effects-by-category_MAOC.csv")
write.csv(stats_MAOC, "Model-output/4_Single-moderator/*stats-by-category_MAOC.csv")









### analyze SOC
dat <- dat_SOC

## depth
table(dat$StudyID, dat$depth.surface)
table(dat$StudyID, dat$depth.subsoil)
table(dat$StudyID, dat$depth.deepsoil) # remove, only two studies represented
mod <- rma.mv(logRR, var, mods= ~0+depth.surface + depth.subsoil , 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_depth.type.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[2,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                        length(which(dat$depth.surface==1)), length(unique(dat$StudyID[which(dat$depth.surface==1)])))
effectsdat_SOC[3,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                        length(which(dat$depth.subsoil==1)), length(unique(dat$StudyID[which(dat$depth.subsoil==1)])))

ref <- 2
stats_SOC[ref,] <- c("depth", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


# ccseason
table(dat$StudyID, dat$ccseason.winter)
table(dat$StudyID, dat$ccseason.spring)
table(dat$StudyID, dat$ccseason.summer) 
table(dat$StudyID, dat$ccseason.yearround) 
mod <- rma.mv(logRR, var, mods= ~0+ccseason.winter +  ccseason.spring + ccseason.summer + ccseason.yearround, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_ccseason.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[8,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$ccseason.winter==1)), length(unique(dat$StudyID[which(dat$ccseason.winter==1)])))
effectsdat_SOC[9,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$ccseason.spring==1)), length(unique(dat$StudyID[which(dat$ccseason.spring==1)])))
effectsdat_SOC[10,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$ccseason.summer==1)), length(unique(dat$StudyID[which(dat$ccseason.summer==1)])))
effectsdat_SOC[11,] <- c("SOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$ccseason.yearround==1)), length(unique(dat$StudyID[which(dat$ccseason.yearround==1)])))

(exp(mod$b)-1)*100

ref <- 3
stats_SOC[ref,] <- c("ccseason", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


# ccseason [no mg ha derived values]
table(dat$StudyID, dat$ccseason.winter)
table(dat$StudyID, dat$ccseason.spring)
table(dat$StudyID, dat$ccseason.summer)
table(dat$StudyID, dat$ccseason.yearround)
mod <- rma.mv(logRR, var, mods= ~0+ccseason.winter +  ccseason.spring + ccseason.summer + ccseason.yearround, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat[which(dat$logRR.type=="gkg"),])
sink(file = "Model-output/4_Single-moderator/SOC_ccseason_no-mgha.txt"); summary(mod); sink(file = NULL)
mod
(exp(mod$b)-1)*100
(exp(mod$ci.lb)-1)*100

## ccadded
table(dat$StudyID, dat$ccadded.grass)
table(dat$StudyID, dat$ccadded.legume)
table(dat$StudyID, dat$ccadded.grasslegume)
table(dat$StudyID, dat$ccadded.brassica)
table(dat$StudyID, dat$ccadded.grassbrassica) # remove
table(dat$StudyID, dat$ccadded.legumebrassica) # remove
table(dat$StudyID, dat$ccadded.grasslegumebrassica)
mod <- rma.mv(logRR, var, mods= ~0+ccadded.grass + ccadded.legume + ccadded.grasslegume + ccadded.brassica + ccadded.grasslegumebrassica, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_ccadded.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[13,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$ccadded.grass==1)), length(unique(dat$StudyID[which(dat$ccadded.grass==1)])))
effectsdat_SOC[14,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$ccadded.legume ==1)), length(unique(dat$StudyID[which(dat$ccadded.legume ==1)])))
effectsdat_SOC[15,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$ccadded.grasslegume ==1)), length(unique(dat$StudyID[which(dat$ccadded.grasslegume ==1)])))
effectsdat_SOC[16,] <- c("SOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], 
                         length(which(dat$ccadded.brassica==1)), length(unique(dat$StudyID[which(dat$ccadded.brassica==1)])))
effectsdat_SOC[17,] <- c("SOC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], 
                         length(which(dat$ccadded.grasslegumebrassica==1)), length(unique(dat$StudyID[which(dat$ccadded.grasslegumebrassica==1)])))

ref <- 4
stats_SOC[ref,] <- c("ccadded", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## maincrop
table(dat$StudyID, dat$maincrop.cereal)
table(dat$StudyID, dat$maincrop.legume)
table(dat$StudyID, dat$maincrop.cereallegume)
table(dat$StudyID, dat$maincrop.otherrowcrop)
table(dat$StudyID, dat$maincrop.perennial)
mod <- rma.mv(logRR, var, mods= ~0+maincrop.cereal + maincrop.legume + maincrop.cereallegume + maincrop.otherrowcrop + maincrop.perennial, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_maincrop.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[20,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$maincrop.cereal==1)), length(unique(dat$StudyID[which(dat$maincrop.cereal==1)])))
effectsdat_SOC[21,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$maincrop.legume ==1)), length(unique(dat$StudyID[which(dat$maincrop.legume ==1)])))
effectsdat_SOC[22,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$maincrop.cereallegume ==1)), length(unique(dat$StudyID[which(dat$maincrop.cereallegume ==1)])))
effectsdat_SOC[23,] <- c("SOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], 
                         length(which(dat$maincrop.otherrowcrop==1)), length(unique(dat$StudyID[which(dat$maincrop.otherrowcrop==1)])))
effectsdat_SOC[24,] <- c("SOC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], 
                         length(which(dat$maincrop.perennial ==1)), length(unique(dat$StudyID[which(dat$maincrop.perennial ==1)])))

ref <- 5
stats_SOC[ref,] <- c("maincrop", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## tillage
table(dat$StudyID, dat$tillage.conventional)
table(dat$StudyID, dat$tillage.reducednotill)
mod <- rma.mv(logRR, var, mods= ~0+tillage.conventional + tillage.reducednotill, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_tillage.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[25,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$tillage.conventional==1)), length(unique(dat$StudyID[which(dat$tillage.conventional==1)])))
effectsdat_SOC[26,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$tillage.reducednotill==1)), length(unique(dat$StudyID[which(dat$tillage.reducednotill==1)])))

ref <- 6
stats_SOC[ref,] <- c("tillage", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)



## nfert
mod <- rma.mv(logRR, var, mods= ~nfert, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
mod2 <- rma.mv(logRR, var, mods= ~nfert + I(nfert^2), random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
fitstats(mod, mod2)
sink(file = "Model-output/4_Single-moderator/SOC_nfert.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[27,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$nfert)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$nfert)==FALSE)])))

ref <- 7
stats_SOC[ref,] <- c("nfert", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## ag.C.inputs
mod <- rma.mv(logRR, var, mods= ~ag.C.inputs, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_ag.C.inputs.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[28,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$ag.C.inputs)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ag.C.inputs)==FALSE)])))

ref <- 8
stats_SOC[ref,] <- c("ag.C.inputs", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## species.added
mod <- rma.mv(logRR, var, mods= ~species.added, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_species.added.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[29,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$species.added)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$species.added)==FALSE)])))

ref <- 9
stats_SOC[ref,] <- c("species.added", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## duration
mod <- rma.mv(logRR, var, mods= ~duration, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_duration.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[30,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$duration)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$duration)==FALSE)])))

ref <- 10
stats_SOC[ref,] <- c("duration", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## sand
mod <- rma.mv(logRR, var, mods= ~sand, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_sand.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[31,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$sand)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$sand)==FALSE)])))

ref <- 11
stats_SOC[ref,] <- c("sand", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## silt
mod <- rma.mv(logRR, var, mods= ~silt, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_silt.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[32,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$silt)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$silt)==FALSE)])))

ref <- 12
stats_SOC[ref,] <- c("silt", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## clay
mod <- rma.mv(logRR, var, mods= ~clay, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_clay.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[33,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$clay)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$clay)==FALSE)])))

ref <- 13
stats_SOC[ref,] <- c("clay", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## toc 
mod <- rma.mv(logRR, var, mods= ~toc, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_toc.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[34,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$toc)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$toc)==FALSE)])))

ref <- 14
stats_SOC[ref,] <- c("toc", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## ph
mod <- rma.mv(logRR, var, mods= ~ph, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
mod2 <- rma.mv(logRR, var, mods= ~ph + I(ph^2), random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
fitstats(mod, mod2)
sink(file = "Model-output/4_Single-moderator/SOC_ph.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[35,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$ph)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ph)==FALSE)])))

ref <- 15
stats_SOC[ref,] <- c("ph", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## Temp
mod <- rma.mv(logRR, var, mods= ~Temp, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
mod2 <- rma.mv(logRR, var, mods= ~Temp + I(Temp^2), random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
fitstats(mod, mod2)
sink(file = "Model-output/4_Single-moderator/SOC_Temp.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[36,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$Temp)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Temp)==FALSE)])))

ref <- 16
stats_SOC[ref,] <- c("Temp", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## Prec
mod <- rma.mv(logRR, var, mods= ~Prec, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
mod2 <- rma.mv(logRR, var, mods= ~Prec + I(Prec^2), random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
fitstats(mod, mod2)
sink(file = "Model-output/4_Single-moderator/SOC_Prec.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[37,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$Prec)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Prec)==FALSE)])))

ref <- 17
stats_SOC[ref,] <- c("Prec", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)





## soilorder 
table(dat$StudyID, dat$soilorder.Alfisols) 
table(dat$StudyID, dat$soilorder.Oxisols) 
table(dat$StudyID, dat$soilorder.Mollisols) 
table(dat$StudyID, dat$soilorder.Ultisols) 
table(dat$StudyID, dat$soilorder.Spodosols) # remove
table(dat$StudyID, dat$soilorder.Inceptisols)
mod <- rma.mv(logRR, var, mods= ~0+soilorder.Alfisols + soilorder.Oxisols + soilorder.Mollisols + soilorder.Ultisols + soilorder.Inceptisols, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_soilorder.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[38,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$soilorder.Alfisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Alfisols==1)])))
effectsdat_SOC[39,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$soilorder.Oxisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Oxisols==1)])))
effectsdat_SOC[40,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$soilorder.Mollisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Mollisols==1)])))
effectsdat_SOC[41,] <- c("SOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], 
                         length(which(dat$soilorder.Ultisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Ultisols==1)])))
effectsdat_SOC[43,] <- c("SOC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], 
                         length(which(dat$soilorder.Inceptisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Inceptisols==1)])))

ref <- 18
stats_SOC[ref,] <- c("soilorder", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)



## Continent
table(dat$StudyID, dat$Continent.Africa) # remove
table(dat$StudyID, dat$Continent.Asia) # remove
table(dat$StudyID, dat$Continent.Australia) # remove
table(dat$StudyID, dat$Continent.Europe) 
table(dat$StudyID, dat$Continent.NorthAmerica) 
table(dat$StudyID, dat$Continent.SouthAmerica)
mod <- rma.mv(logRR, var, mods= ~0+Continent.Europe + Continent.NorthAmerica + Continent.SouthAmerica, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_Continent.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[47,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$Continent.Europe==1)), length(unique(dat$StudyID[which(dat$Continent.Europe==1)])))
effectsdat_SOC[48,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$Continent.NorthAmerica==1)), length(unique(dat$StudyID[which(dat$Continent.NorthAmerica==1)])))
effectsdat_SOC[49,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$Continent.SouthAmerica==1)), length(unique(dat$StudyID[which(dat$Continent.SouthAmerica==1)])))

ref <- 19
stats_SOC[ref,] <- c("Continent", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## SoilCollectionSeason
table(dat$StudyID, dat$SoilCollectionSeason.winter) 
table(dat$StudyID, dat$SoilCollectionSeason.spring) 
table(dat$StudyID, dat$SoilCollectionSeason.summer) 
table(dat$StudyID, dat$SoilCollectionSeason.fall)
mod <- rma.mv(logRR, var, mods= ~0+ SoilCollectionSeason.spring + SoilCollectionSeason.summer + SoilCollectionSeason.fall, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_SoilCollectionSeason.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[51,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$SoilCollectionSeason.spring==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.spring==1)])))
effectsdat_SOC[52,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$SoilCollectionSeason.summer==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.summer==1)])))
effectsdat_SOC[53,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$SoilCollectionSeason.fall==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.fall==1)])))

ref <- 20
stats_SOC[ref,] <- c("SoilCollectionSeason", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## FractionationMethod
table(dat$StudyID, dat$FractionationMethod.Size) 
table(dat$StudyID, dat$FractionationMethod.Density) # remove
table(dat$StudyID, dat$FractionationMethod.SizeAndDensity) # remove
mod <- rma.mv(logRR, var, mods= ~0+FractionationMethod.Size , 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_FractionationMethod.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[54,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$FractionationMethod.Size==1)), length(unique(dat$StudyID[which(dat$FractionationMethod.Size==1)])))

ref <- 21
stats_SOC[ref,] <- c("FractionationMethod", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)





# DispersingAgent
table(dat$StudyID, dat$DispersingAgent.SodiumHex) 
table(dat$StudyID, dat$DispersingAgent.OtherSolution) 
table(dat$StudyID, dat$DispersingAgent.GlassBeads) 
table(dat$StudyID, dat$DispersingAgent.Sonication) # remove
table(dat$StudyID, dat$DispersingAgent.Combo) # remove
mod <- rma.mv(logRR, var, mods= ~0+DispersingAgent.SodiumHex + DispersingAgent.OtherSolution + DispersingAgent.GlassBeads , 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_DispersingAgent.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[57,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], 
                         length(which(dat$DispersingAgent.SodiumHex==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.SodiumHex==1)])))
effectsdat_SOC[58,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], 
                         length(which(dat$DispersingAgent.OtherSolution==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.OtherSolution==1)])))
effectsdat_SOC[59,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], 
                         length(which(dat$DispersingAgent.GlassBeads==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.GlassBeads==1)])))

ref <- 22
stats_SOC[ref,] <- c("DispersingAgent", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## ResponseCalc
table(dat$StudyID, dat$ResponseCalc.Direct) 
table(dat$StudyID, dat$ResponseCalc.Indirect) # remove
mod <- rma.mv(logRR, var, mods= ~0+ResponseCalc.Direct, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_ResponseCalc.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[62,] <- c("SOC","ResponseCalc.Direct", mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$ResponseCalc.Direct==1)), length(unique(dat$StudyID[which(dat$ResponseCalc.Direct==1)])))

ref <- 23
stats_SOC[ref,] <- c("ResponseCalc", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)




write.csv(effectsdat_SOC, "Model-output/4_Single-moderator/*effects-by-category_SOC.csv")
write.csv(stats_SOC, "Model-output/4_Single-moderator/*stats-by-category_SOC.csv")








