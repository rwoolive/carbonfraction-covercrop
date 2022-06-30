




# Use metafor function rma.mv to model effects of cover cropping 
# on soil carbon pools while allowing for variation at three levels:  
# 1. within observations, 
# 2. among observations of a single study, and 
# 3. among studies
# For tutorial, see Assink & Wibbelink (2016) TQMP

library(metafor) # version 3.4-0


############################### 
### read in data
dat0 <- read.csv("Raw-data/4_metadata - final_withclimdat.csv")

############################### 
###  n-based data
dat0n <- dat0[which(dat0$rv %in% c("PON", "TN", "MAON") & is.na(dat0$logRR)==FALSE & is.na(dat0$var)==FALSE),]
unique(dat0n$StudyID) # nitrogen-based effect sizes: 285 observations from 21 studies
###  matter-based data
dat0m <- dat0[which(dat0$rv %in% c("POM", "TOM", "SOM", "MAOM") & is.na(dat0$logRR)==FALSE & is.na(dat0$var)==FALSE),]
unique(dat0m$StudyID) # matter-based effect sizes: 90 observations from 9 studies
###  carbon-based data
dat0c <- dat0[which(dat0$rv %in% c("POC", "TOC", "MAOC", "SOC") & is.na(dat0$logRR)==FALSE & is.na(dat0$var)==FALSE),]
unique(dat0c$StudyID) # carbon-based effect sizes: 958 observations from 50 studies
###  phosphorus-based data
dat0p <- dat0[which(dat0$rv %in% c("POP") & is.na(dat0$logRR)==FALSE & is.na(dat0$var)==FALSE),]
unique(dat0p$StudyID) # phosphorus-based effect sizes: 4 observations from 1 study
###  remove nitrogen- and phosphorus-based data
dat0 <- dat0[which(dat0$rv %in% c("POC", "TOC", "MAOC", "SOC", "POM", "TOM", "SOM", "MAOM")& is.na(dat0$logRR)==FALSE & is.na(dat0$var)==FALSE),]
unique(dat0$StudyID) # carbon- and matter-based effect sizes: 1048 observations from 53 studies


unique(dat0$Continent)
table(dat0$ccseason)

############################### 
### types of responses
dat0$rv.type <- as.factor(dat0$rv.type)
respvars <- table(dat0$StudyID, dat0$rv.type)
write.csv(respvars, "Processed-data/1_response variables across studies_no-nitrogen.csv")



############################### 
### moderators

# categorical variables to include in main analysis

dat0$depth <- as.factor(dat0$depth.type)
dat0$depth <- factor(dat0$depth, levels(dat0$depth)[c(3,2,1)])

dat0$soilorder <- as.factor(dat0$soilorder)

dat0$ccseason <- as.factor(dat0$ccseason)

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
hist(dat0$N.fertilization.rate..kg.ha.1.yr.1.); dat0$nfert <- dat0$N.fertilization.rate..kg.ha.1.yr.1.
#hist(dat0$residue.removal....); dat0$residue.removal <- dat0$residue.removal....



# variables to include in supplementary analysis

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
write.csv(StudyIDs, "Processed-data/0_StudyIDs_no-nitrogen.csv")


# subset data to pom, maom, and tom pools
dat0$rv.type <- as.factor(dat0$rv.type)
dat_POC <- dat0[which(dat0$rv.type=="pom fraction" & is.na(dat0$logRR)==FALSE & is.na(dat0$var)==FALSE),]
dat_MAOC <- dat0[which(dat0$rv.type=="maom fraction" & is.na(dat0$logRR)==FALSE & is.na(dat0$var)==FALSE),]
dat_SOC <- dat0[which(dat0$rv.type=="total organic matter" & is.na(dat0$logRR)==FALSE & is.na(dat0$var)==FALSE),]



dim(table(dat_POC$StudyID)); dim(table(dat_POC$Obs.ID))
dim(table(dat_MAOC$StudyID)); dim(table(dat_MAOC$Obs.ID))
dim(table(dat_SOC$StudyID)); dim(table(dat_SOC$Obs.ID))


write.csv(dat_POC, "Processed-data/POC_no-nitrogen.csv")
write.csv(dat_MAOC, "Processed-data/MAOC_no-nitrogen.csv")
write.csv(dat_SOC, "Processed-data/SOC_no-nitrogen.csv")


#### overall models
#### testing whether within-study variation is significant
#### estimating distribution of variance across the three levels
# note that sigma^2.1 is the variance between effect sizes within studies
# and sigma^2.2 is the variance between studies

r <- 100
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
sink(file = "Model-output/1_Overall/POC_no-nitrogen.txt"); summary(mod); sink(file = NULL)
sink(file = "Model-output/2_Significance-of-variation/POC_within-study_no-nitrogen.txt"); anova(mod, mod_2); sink(file = NULL) # if p<0.05, within-study variance is significant
sink(file = "Model-output/2_Significance-of-variation/POC_between-study_no-nitrogen.txt"); anova(mod, mod_3); sink(file = NULL) # if p<0.05, between-study variance is significant
sink(file = "Model-output/3_Partitioning-of-variation/POC_no-nitrogen.txt"); data.frame(samplingvar = I2_1 * 100, withinstudyvar = I2_2 * 100, betweenstudyvar = I2_3 * 100); sink(file = NULL)


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
sink(file = "Model-output/1_Overall/MAOC_no-nitrogen.txt"); summary(mod); sink(file = NULL)
sink(file = "Model-output/2_Significance-of-variation/MAOC_within-study_no-nitrogen.txt"); anova(mod, mod_2); sink(file = NULL) # if p<0.05, within-study variance is significant
sink(file = "Model-output/2_Significance-of-variation/MAOC_between-study_no-nitrogen.txt"); anova(mod, mod_3); sink(file = NULL) # if p<0.05, between-study variance is significant
sink(file = "Model-output/3_Partitioning-of-variation/MAOC_no-nitrogen.txt"); data.frame(samplingvar = I2_1 * 100, withinstudyvar = I2_2 * 100, betweenstudyvar = I2_3 * 100); sink(file = NULL)


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
sink(file = "Model-output/1_Overall/SOC_no-nitrogen.txt"); summary(mod); sink(file = NULL)
sink(file = "Model-output/2_Significance-of-variation/SOC_within-study_no-nitrogen.txt"); anova(mod, mod_2); sink(file = NULL) # if p<0.05, within-study variance is significant
sink(file = "Model-output/2_Significance-of-variation/SOC_between-study_no-nitrogen.txt"); anova(mod, mod_3); sink(file = NULL) # if p<0.05, between-study variance is significant
sink(file = "Model-output/3_Partitioning-of-variation/SOC_no-nitrogen.txt"); data.frame(samplingvar = I2_1 * 100, withinstudyvar = I2_2 * 100, betweenstudyvar = I2_3 * 100); sink(file = NULL)



















#### Testing for moderator effects
# Rocci et al. 2021: Began by testing each moderator individually with each global
# change-fraction pairing and performed pairwise comparisons of each
# level of the moderator. 
# Note that you need "mods= ~0 + moderators" for categorical variables
# Note also that order does not matter when adding dummy-coded variables to the model




### start by analyzing POC
dat <- dat_POC

## depth, 3 levels: depth.surface + depth.subsoil + depth.deepsoil 
mod <- rma.mv(logRR, var, mods= ~0+depth.surface + depth.subsoil + depth.deepsoil , 
               random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_depth.type_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[2,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$depth.surface==1)), length(unique(dat$StudyID[which(dat$depth.surface==1)])))
effectsdat_POC[3,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$depth.subsoil==1)), length(unique(dat$StudyID[which(dat$depth.subsoil==1)])))
effectsdat_POC[4,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$depth.deepsoil==1)), length(unique(dat$StudyID[which(dat$depth.deepsoil==1)])))

ref <- 2
stats_POC[ref,] <- c("depth", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

  
# ccseason, 5 levels: ccseason.winter +  ccseason.spring + ccseason.summer + ccseason.yearround
mod <- rma.mv(logRR, var, mods= ~0+ccseason.winter +  ccseason.spring + ccseason.summer + ccseason.yearround, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, )
sink(file = "Model-output/4_Single-moderator/POC_ccseason_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[8,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$ccseason.winter==1)), length(unique(dat$StudyID[which(dat$ccseason.winter==1)])))
effectsdat_POC[9,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$ccseason.spring==1)), length(unique(dat$StudyID[which(dat$ccseason.spring==1)])))
effectsdat_POC[10,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$ccseason.summer==1)), length(unique(dat$StudyID[which(dat$ccseason.summer==1)])))
effectsdat_POC[11,] <- c("POC", "ccseason.fall", rep(NA, 5))
effectsdat_POC[12,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$ccseason.yearround==1)), length(unique(dat$StudyID[which(dat$ccseason.yearround==1)])))

ref <- 3
stats_POC[ref,] <- c("ccseason", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ccadded, 7 levels: ccadded.grass + ccadded.legume + ccadded.grasslegume + ccadded.brassica + ccadded.grassbrassica + ccadded.legumebrassica + ccadded.grasslegumebrassica
mod <- rma.mv(logRR, var, mods= ~0+ccadded.grass + ccadded.legume + ccadded.grasslegume + ccadded.brassica + ccadded.grasslegumebrassica, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_ccadded_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[13,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$ccadded.grass==1)), length(unique(dat$StudyID[which(dat$ccadded.grass==1)])))
effectsdat_POC[14,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$ccadded.legume ==1)), length(unique(dat$StudyID[which(dat$ccadded.legume ==1)])))
effectsdat_POC[15,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$ccadded.grasslegume ==1)), length(unique(dat$StudyID[which(dat$ccadded.grasslegume ==1)])))
effectsdat_POC[16,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$ccadded.brassica==1)), length(unique(dat$StudyID[which(dat$ccadded.brassica==1)])))
effectsdat_POC[17,] <- c("POC", "ccadded.grassbrassica", rep(NA, 5))
effectsdat_POC[18,] <- c("POC","ccadded.legumebrassica", rep(NA, 5))
effectsdat_POC[19,] <- c("POC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], length(which(dat$ccadded.grasslegumebrassica==1)), length(unique(dat$StudyID[which(dat$ccadded.grasslegumebrassica==1)])))

ref <- 4
stats_POC[ref,] <- c("ccadded", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## maincrop, 6 levels: maincrop.cereal + maincrop.legume + maincrop.cereallegume + maincrop.otherrowcrop + maincrop.perennial
mod <- rma.mv(logRR, var, mods= ~0+maincrop.cereal + maincrop.legume + maincrop.cereallegume + maincrop.otherrowcrop + maincrop.perennial, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_maincrop_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[20,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$maincrop.cereal==1)), length(unique(dat$StudyID[which(dat$maincrop.cereal==1)])))
effectsdat_POC[21,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$maincrop.legume ==1)), length(unique(dat$StudyID[which(dat$maincrop.legume ==1)])))
effectsdat_POC[22,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$maincrop.cereallegume ==1)), length(unique(dat$StudyID[which(dat$maincrop.cereallegume ==1)])))
effectsdat_POC[23,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$maincrop.otherrowcrop==1)), length(unique(dat$StudyID[which(dat$maincrop.otherrowcrop==1)])))
effectsdat_POC[24,] <- c("POC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], length(which(dat$maincrop.perennial ==1)), length(unique(dat$StudyID[which(dat$maincrop.perennial ==1)])))

ref <- 5
stats_POC[ref,] <- c("maincrop", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## tillage, 2 levels: tillage.conventional + tillage.reducednotill
mod <- rma.mv(logRR, var, mods= ~0+tillage.conventional + tillage.reducednotill, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_tillage_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[25,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$tillage.conventional==1)), length(unique(dat$StudyID[which(dat$tillage.conventional==1)])))
effectsdat_POC[26,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$tillage.reducednotill==1)), length(unique(dat$StudyID[which(dat$tillage.reducednotill==1)])))

ref <- 6
stats_POC[ref,] <- c("tillage", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## nfert
mod <- rma.mv(logRR, var, mods= ~nfert, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_nfert_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[27,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$nfert)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$nfert)==FALSE)])))

ref <- 7
stats_POC[ref,] <- c("nfert", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ag.C.inputs
mod <- rma.mv(logRR, var, mods= ~ag.C.inputs, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_ag.C.inputs_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[28,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$ag.C.inputs)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ag.C.inputs)==FALSE)])))

ref <- 8
stats_POC[ref,] <- c("ag.C.inputs", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## species.added
mod <- rma.mv(logRR, var, mods= ~species.added, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_species.added_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[29,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$species.added)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$species.added)==FALSE)])))

ref <- 9
stats_POC[ref,] <- c("species.added", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## duration
mod <- rma.mv(logRR, var, mods= ~duration, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_duration_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[30,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$duration)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$duration)==FALSE)])))

ref <- 10
stats_POC[ref,] <- c("duration", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## sand
mod <- rma.mv(logRR, var, mods= ~sand, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_sand_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[31,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$sand)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$sand)==FALSE)])))

ref <- 11
stats_POC[ref,] <- c("sand", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## silt
mod <- rma.mv(logRR, var, mods= ~silt, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_silt_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[32,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$silt)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$silt)==FALSE)])))

ref <- 12
stats_POC[ref,] <- c("silt", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## clay
mod <- rma.mv(logRR, var, mods= ~clay, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_clay_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[33,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$clay)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$clay)==FALSE)])))

ref <- 13
stats_POC[ref,] <- c("clay", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## toc 
mod <- rma.mv(logRR, var, mods= ~toc, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_toc_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[34,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$toc)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$toc)==FALSE)])))

ref <- 14
stats_POC[ref,] <- c("toc", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ph
mod <- rma.mv(logRR, var, mods= ~ph, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_ph_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[35,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$ph)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ph)==FALSE)])))

ref <- 15
stats_POC[ref,] <- c("ph", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## Temp
mod <- rma.mv(logRR, var, mods= ~Temp, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_Temp_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[36,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$Temp)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Temp)==FALSE)])))

ref <- 16
stats_POC[ref,] <- c("Temp", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## Prec
mod <- rma.mv(logRR, var, mods= ~Prec, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_Prec_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[37,] <- c("POC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$Prec)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Prec)==FALSE)])))

ref <- 17
stats_POC[ref,] <- c("Prec", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)




## soilorder, 6 levels: soilorder.Alfisols + soilorder.Oxisols + soilorder.Mollisols + soilorder.Ultisols + soilorder.Spodosols + soilorder.Inceptisols 
mod <- rma.mv(logRR, var, mods= ~0+soilorder.Alfisols + soilorder.Oxisols + soilorder.Mollisols + soilorder.Ultisols + soilorder.Inceptisols, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_soilorder_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[38,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$soilorder.Alfisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Alfisols==1)])))
effectsdat_POC[39,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$soilorder.Oxisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Oxisols==1)])))
effectsdat_POC[40,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$soilorder.Mollisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Mollisols==1)])))
effectsdat_POC[41,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$soilorder.Ultisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Ultisols==1)])))
effectsdat_POC[42,] <- c("POC", "soilorder.Spodosols", rep(NA, 5))
effectsdat_POC[43,] <- c("POC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], length(which(dat$soilorder.Inceptisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Inceptisols==1)])))

ref <- 18
stats_POC[ref,] <- c("soilorder", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## Continent, 6 levels: Continent.Africa + Continent.Asia + Continent.Australia + Continent.Europe + Continent.NorthAmerica + Continent.SouthAmerica
mod <- rma.mv(logRR, var, mods= ~0+ Continent.Asia + Continent.Europe + Continent.NorthAmerica + Continent.SouthAmerica, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_Continent_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[44,] <- c("POC", "Continent.Africa", rep(NA, 5))
effectsdat_POC[45,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$Continent.Asia==1)), length(unique(dat$StudyID[which(dat$Continent.Asia==1)])))
effectsdat_POC[46,] <- c("POC", "Continent.Australia",  rep(NA, 5))
effectsdat_POC[47,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$Continent.Europe==1)), length(unique(dat$StudyID[which(dat$Continent.Europe==1)])))
effectsdat_POC[48,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$Continent.NorthAmerica==1)), length(unique(dat$StudyID[which(dat$Continent.NorthAmerica==1)])))
effectsdat_POC[49,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$Continent.SouthAmerica==1)), length(unique(dat$StudyID[which(dat$Continent.SouthAmerica==1)])))

ref <- 19
stats_POC[ref,] <- c("Continent", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## SoilCollectionSeason, 4 levels: SoilCollectionSeason.winter + SoilCollectionSeason.spring + SoilCollectionSeason.summer + SoilCollectionSeason.fall
mod <- rma.mv(logRR, var, mods= ~0+SoilCollectionSeason.winter + SoilCollectionSeason.spring + SoilCollectionSeason.summer + SoilCollectionSeason.fall, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_SoilCollectionSeason_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[50,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$SoilCollectionSeason.winter==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.winter==1)])))
effectsdat_POC[51,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$SoilCollectionSeason.spring==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.spring==1)])))
effectsdat_POC[52,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$SoilCollectionSeason.summer==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.summer==1)])))
effectsdat_POC[53,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$SoilCollectionSeason.fall==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.fall==1)])))

ref <- 20
stats_POC[ref,] <- c("SoilCollectionSeason", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## FractionationMethod, 3 levels: FractionationMethod.Size + FractionationMethod.Density + FractionationMethod.SizeAndDensity
mod <- rma.mv(logRR, var, mods= ~0+FractionationMethod.Size + FractionationMethod.Density , 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_FractionationMethod_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[54,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$FractionationMethod.Size==1)), length(unique(dat$StudyID[which(dat$FractionationMethod.Size==1)])))
effectsdat_POC[55,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$FractionationMethod.Density==1)), length(unique(dat$StudyID[which(dat$FractionationMethod.Density==1)])))
effectsdat_POC[56,] <- c("POC","FractionationMethod.SizeAndDensity", rep(NA, 5))

ref <- 21
stats_POC[ref,] <- c("FractionationMethod", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

# DispersingAgent, 5 levels: DispersingAgent.SodiumHex + DispersingAgent.OtherSolution + DispersingAgent.GlassBeads + DispersingAgent.Sonication + DispersingAgent.Combo
mod <- rma.mv(logRR, var, mods= ~0+DispersingAgent.SodiumHex + DispersingAgent.OtherSolution + DispersingAgent.GlassBeads + DispersingAgent.Sonication + DispersingAgent.Combo, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_DispersingAgent_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[57,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$DispersingAgent.SodiumHex==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.SodiumHex==1)])))
effectsdat_POC[58,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$DispersingAgent.OtherSolution==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.OtherSolution==1)])))
effectsdat_POC[59,] <- c("POC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$DispersingAgent.GlassBeads==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.GlassBeads==1)])))
effectsdat_POC[60,] <- c("POC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$DispersingAgent.Sonication==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.Sonication==1)])))
effectsdat_POC[61,] <- c("POC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], length(which(dat$DispersingAgent.Combo==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.Combo==1)])))

ref <- 22
stats_POC[ref,] <- c("DispersingAgent", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ResponseCalc, 2 levels: ResponseCalc.Direct + ResponseCalc.Indirect
mod <- rma.mv(logRR, var, mods= ~0+ResponseCalc.Direct + ResponseCalc.Indirect, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/POC_ResponseCalc_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_POC[62,] <- c("POC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$ResponseCalc.Direct==1)), length(unique(dat$StudyID[which(dat$ResponseCalc.Direct==1)])))
effectsdat_POC[63,] <- c("POC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$ResponseCalc.Indirect==1)), length(unique(dat$StudyID[which(dat$ResponseCalc.Indirect==1)])))

ref <- 23
stats_POC[ref,] <- c("ResponseCalc", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

write.csv(effectsdat_POC, "Model-output/4_Single-moderator/effects-by-category_POC_no-nitrogen.csv")
write.csv(stats_POC, "Model-output/4_Single-moderator/stats-by-category_POC_no-nitrogen.csv")







### analyze MAOC
dat <- dat_MAOC


## depth, 3 levels: depth.surface + depth.subsoil + depth.deepsoil
mod <- rma.mv(logRR, var, mods= ~0+depth.surface + depth.subsoil , 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_depth.type_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[2,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$depth.surface==1)), length(unique(dat$StudyID[which(dat$depth.surface==1)])))
effectsdat_MAOC[3,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$depth.subsoil==1)), length(unique(dat$StudyID[which(dat$depth.subsoil==1)])))
effectsdat_MAOC[4,] <- c("MAOC","depth.deepsoil", rep(NA, 5))

ref <- 2
stats_MAOC[ref,] <- c("depth", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

# ccseason, 5 levels: ccseason.winter +  ccseason.spring + ccseason.summer + ccseason.yearround
mod <- rma.mv(logRR, var, mods= ~0+ccseason.winter + ccseason.summer + ccseason.yearround, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, )
sink(file = "Model-output/4_Single-moderator/MAOC_ccseason_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[8,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$ccseason.winter==1)), length(unique(dat$StudyID[which(dat$ccseason.winter==1)])))
effectsdat_MAOC[9,] <- c("MAOC", "ccseason.spring", rep(NA, 5))
effectsdat_MAOC[10,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$ccseason.summer==1)), length(unique(dat$StudyID[which(dat$ccseason.summer==1)])))
effectsdat_MAOC[11,] <- c("MAOC", "ccseason.fall", rep(NA, 5))
effectsdat_MAOC[12,] <- c("MAOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$ccseason.yearround==1)), length(unique(dat$StudyID[which(dat$ccseason.yearround==1)])))

ref <- 3
stats_MAOC[ref,] <- c("ccseason", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ccadded, 7 levels: ccadded.grass + ccadded.legume + ccadded.grasslegume + ccadded.brassica + ccadded.grassbrassica + ccadded.legumebrassica + ccadded.grasslegumebrassica
mod <- rma.mv(logRR, var, mods= ~0+ccadded.grass + ccadded.legume + ccadded.grasslegume, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_ccadded_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[13,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$ccadded.grass==1)), length(unique(dat$StudyID[which(dat$ccadded.grass==1)])))
effectsdat_MAOC[14,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$ccadded.legume ==1)), length(unique(dat$StudyID[which(dat$ccadded.legume ==1)])))
effectsdat_MAOC[15,] <- c("MAOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$ccadded.grasslegume==1)), length(unique(dat$StudyID[which(dat$ccadded.grasslegume==1)])))
effectsdat_MAOC[16,] <- c("MAOC", "ccadded.brassica", rep(NA, 5))
effectsdat_MAOC[17,] <- c("MAOC","ccadded.grassbrassica", rep(NA,5))
effectsdat_MAOC[18,] <- c("MAOC","ccadded.legumebrassica", rep(NA,5))
effectsdat_MAOC[19,] <- c("MAOC","ccadded.grasslegumebrassica", rep(NA,5))

ref <- 4
stats_MAOC[ref,] <- c("ccadded", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## maincrop, 6 levels: maincrop.cereal + maincrop.legume + maincrop.cereallegume + maincrop.otherrowcrop + maincrop.perennial
mod <- rma.mv(logRR, var, mods= ~0+maincrop.cereal + maincrop.legume + maincrop.cereallegume + maincrop.perennial, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_maincrop_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[20,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$maincrop.cereal==1)), length(unique(dat$StudyID[which(dat$maincrop.cereal==1)])))
effectsdat_MAOC[21,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$maincrop.legume ==1)), length(unique(dat$StudyID[which(dat$maincrop.legume ==1)])))
effectsdat_MAOC[22,] <- c("MAOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$maincrop.cereallegume ==1)), length(unique(dat$StudyID[which(dat$maincrop.cereallegume ==1)])))
effectsdat_MAOC[23,] <- c("MAOC", "maincrop.otherrowcrop", rep(NA, 5))
effectsdat_MAOC[24,] <- c("MAOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$maincrop.perennial ==1)), length(unique(dat$StudyID[which(dat$maincrop.perennial ==1)])))

ref <- 5
stats_MAOC[ref,] <- c("maincrop", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## tillage, 2 levels: tillage.conventional + tillage.reducednotill
mod <- rma.mv(logRR, var, mods= ~0+tillage.conventional + tillage.reducednotill, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_tillage_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[25,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$tillage.conventional==1)), length(unique(dat$StudyID[which(dat$tillage.conventional==1)])))
effectsdat_MAOC[26,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$tillage.reducednotill==1)), length(unique(dat$StudyID[which(dat$tillage.reducednotill==1)])))

ref <- 6
stats_MAOC[ref,] <- c("tillage", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## nfert
mod <- rma.mv(logRR, var, mods= ~nfert, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_nfert_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[27,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$nfert)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$nfert)==FALSE)])))

ref <- 7
stats_MAOC[ref,] <- c("nfert", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ag.C.inputs
mod <- rma.mv(logRR, var, mods= ~ag.C.inputs, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_ag.C.inputs_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[28,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$ag.C.inputs)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ag.C.inputs)==FALSE)])))

ref <- 8
stats_MAOC[ref,] <- c("ag.C.inputs", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## species.added
mod <- rma.mv(logRR, var, mods= ~species.added, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_species.added_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[29,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$species.added)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$species.added)==FALSE)])))

ref <- 9
stats_MAOC[ref,] <- c("species.added", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## duration
mod <- rma.mv(logRR, var, mods= ~duration, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_duration_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[30,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$duration)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$duration)==FALSE)])))

ref <- 10
stats_MAOC[ref,] <- c("duration", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## sand
mod <- rma.mv(logRR, var, mods= ~sand, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_sand_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[31,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$sand)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$sand)==FALSE)])))

ref <- 11
stats_MAOC[ref,] <- c("sand", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## silt
mod <- rma.mv(logRR, var, mods= ~silt, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_silt_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[32,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$silt)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$silt)==FALSE)])))

ref <- 12
stats_MAOC[ref,] <- c("silt", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## clay
mod <- rma.mv(logRR, var, mods= ~clay, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_clay_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[33,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$clay)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$clay)==FALSE)])))

ref <- 13
stats_MAOC[ref,] <- c("clay", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## toc 
mod <- rma.mv(logRR, var, mods= ~toc, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_toc_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[34,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$toc)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$toc)==FALSE)])))

ref <- 14
stats_MAOC[ref,] <- c("toc", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ph
mod <- rma.mv(logRR, var, mods= ~ph, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_ph_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[35,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$ph)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ph)==FALSE)])))

ref <- 15
stats_MAOC[ref,] <- c("ph", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## Temp
mod <- rma.mv(logRR, var, mods= ~Temp, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_Temp_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[36,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$Temp)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Temp)==FALSE)])))

ref <- 16
stats_MAOC[ref,] <- c("Temp", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## Prec
mod <- rma.mv(logRR, var, mods= ~Prec, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_Prec_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[37,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$Prec)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Prec)==FALSE)])))

ref <- 17
stats_MAOC[ref,] <- c("Prec", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)



## soilorder, 6 levels: soilorder.Alfisols + soilorder.Oxisols + soilorder.Mollisols + soilorder.Ultisols + soilorder.Spodosols + soilorder.Inceptisols 
mod <- rma.mv(logRR, var, mods= ~0+soilorder.Alfisols + soilorder.Oxisols + soilorder.Mollisols + soilorder.Ultisols, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_soilorder_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[38,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$soilorder.Alfisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Alfisols==1)])))
effectsdat_MAOC[39,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$soilorder.Oxisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Oxisols==1)])))
effectsdat_MAOC[40,] <- c("MAOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$soilorder.Mollisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Mollisols==1)])))
effectsdat_MAOC[41,] <- c("MAOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$soilorder.Ultisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Ultisols==1)])))
effectsdat_MAOC[42,] <- c("MAOC","soilorder.Spodosols", rep(NA,5))
effectsdat_MAOC[43,] <- c("MAOC","soilorder.Inceptisols", rep(NA, 5))

ref <- 18
stats_MAOC[ref,] <- c("soilorder", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## Continent, 6 levels: Continent.Africa + Continent.Asia + Continent.Australia + Continent.Europe + Continent.NorthAmerica + Continent.SouthAmerica
mod <- rma.mv(logRR, var, mods= ~0+Continent.Europe + Continent.NorthAmerica + Continent.SouthAmerica, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_Continent_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[44,] <- c("MAOC","Continent.Africa", rep(NA,5))
effectsdat_MAOC[45,] <- c("MAOC","Continent.Asia", rep(NA,5))
effectsdat_MAOC[46,] <- c("MAOC","Continent.Australia", rep(NA,5))
effectsdat_MAOC[47,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$Continent.Europe==1)), length(unique(dat$StudyID[which(dat$Continent.Europe==1)])))
effectsdat_MAOC[48,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$Continent.NorthAmerica==1)), length(unique(dat$StudyID[which(dat$Continent.NorthAmerica==1)])))
effectsdat_MAOC[49,] <- c("MAOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$Continent.SouthAmerica==1)), length(unique(dat$StudyID[which(dat$Continent.SouthAmerica==1)])))

ref <- 19
stats_MAOC[ref,] <- c("Continent", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## SoilCollectionSeason, 4 levels: SoilCollectionSeason.winter + SoilCollectionSeason.spring + SoilCollectionSeason.summer + SoilCollectionSeason.fall
mod <- rma.mv(logRR, var, mods= ~0+ SoilCollectionSeason.spring + SoilCollectionSeason.summer + SoilCollectionSeason.fall, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_SoilCollectionSeason_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[50,] <- c("MAOC","SoilCollectionSeason.winter", rep(NA, 5))
effectsdat_MAOC[51,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$SoilCollectionSeason.spring==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.spring==1)])))
effectsdat_MAOC[52,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$SoilCollectionSeason.summer==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.summer==1)])))
effectsdat_MAOC[53,] <- c("MAOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$SoilCollectionSeason.fall==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.fall==1)])))

ref <- 20
stats_MAOC[ref,] <- c("SoilCollectionSeason", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## FractionationMethod, 3 levels: FractionationMethod.Size + FractionationMethod.Density + FractionationMethod.SizeAndDensity
mod <- rma.mv(logRR, var, mods= ~0+FractionationMethod.Size + FractionationMethod.Density, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_FractionationMethod_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[54,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$FractionationMethod.Size==1)), length(unique(dat$StudyID[which(dat$FractionationMethod.Size==1)])))
effectsdat_MAOC[55,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$FractionationMethod.Density==1)), length(unique(dat$StudyID[which(dat$FractionationMethod.Density==1)])))
effectsdat_MAOC[56,] <- c("MAOC","FractionationMethod.SizeAndDensity", rep(NA, 5))

ref <- 21
stats_MAOC[ref,] <- c("FractionationMethod", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

# DispersingAgent, 5 levels: DispersingAgent.SodiumHex + DispersingAgent.OtherSolution + DispersingAgent.GlassBeads + DispersingAgent.Sonication + DispersingAgent.Combo
mod <- rma.mv(logRR, var, mods= ~0+DispersingAgent.SodiumHex + DispersingAgent.OtherSolution + DispersingAgent.GlassBeads + DispersingAgent.Combo, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_DispersingAgent_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[57,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$DispersingAgent.SodiumHex==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.SodiumHex==1)])))
effectsdat_MAOC[58,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$DispersingAgent.OtherSolution==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.OtherSolution==1)])))
effectsdat_MAOC[59,] <- c("MAOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$DispersingAgent.GlassBeads==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.GlassBeads==1)])))
effectsdat_MAOC[60,] <- c("MAOC","DispersingAgent.Sonication", rep(NA, 5))
effectsdat_MAOC[61,] <- c("MAOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$DispersingAgent.Combo==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.Combo==1)])))

ref <- 22
stats_MAOC[ref,] <- c("DispersingAgent", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

## ResponseCalc, 2 levels: ResponseCalc.Direct + ResponseCalc.Indirect
mod <- rma.mv(logRR, var, mods= ~0+ResponseCalc.Direct + ResponseCalc.Indirect, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/MAOC_ResponseCalc_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_MAOC[62,] <- c("MAOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$ResponseCalc.Direct==1)), length(unique(dat$StudyID[which(dat$ResponseCalc.Direct==1)])))
effectsdat_MAOC[63,] <- c("MAOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$ResponseCalc.Indirect==1)), length(unique(dat$StudyID[which(dat$ResponseCalc.Indirect==1)])))

ref <- 23
stats_MAOC[ref,] <- c("ResponseCalc", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)

write.csv(effectsdat_MAOC, "Model-output/4_Single-moderator/effects-by-category_MAOC_no-nitrogen.csv")
write.csv(stats_MAOC, "Model-output/4_Single-moderator/stats-by-category_MAOC_no-nitrogen.csv")









### analyze SOC
dat <- dat_SOC

## depth, 6 levels: depth.surface + depth.subsoil + depth.deepsoil 
mod <- rma.mv(logRR, var, mods= ~0+depth.surface + depth.subsoil  + depth.deepsoil , 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_depth.type_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[2,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$depth.surface==1)), length(unique(dat$StudyID[which(dat$depth.surface==1)])))
effectsdat_SOC[3,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$depth.subsoil==1)), length(unique(dat$StudyID[which(dat$depth.subsoil==1)])))
effectsdat_SOC[4,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$depth.deepsoil==1)), length(unique(dat$StudyID[which(dat$depth.deepsoil==1)])))

ref <- 2
stats_SOC[ref,] <- c("depth", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


# ccseason, 5 levels: ccseason.winter +  ccseason.spring + ccseason.summer + ccseason.yearround
mod <- rma.mv(logRR, var, mods= ~0+ccseason.winter +  ccseason.spring + ccseason.summer + ccseason.yearround, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, )
sink(file = "Model-output/4_Single-moderator/SOC_ccseason_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[8,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$ccseason.winter==1)), length(unique(dat$StudyID[which(dat$ccseason.winter==1)])))
effectsdat_SOC[9,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$ccseason.spring==1)), length(unique(dat$StudyID[which(dat$ccseason.spring==1)])))
effectsdat_SOC[10,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$ccseason.summer==1)), length(unique(dat$StudyID[which(dat$ccseason.summer==1)])))
effectsdat_SOC[11,] <- c("SOC", "ccseason.fall", rep(NA, 5))
effectsdat_SOC[12,] <- c("SOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$ccseason.yearround==1)), length(unique(dat$StudyID[which(dat$ccseason.yearround==1)])))

ref <- 3
stats_SOC[ref,] <- c("ccseason", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## ccadded, 7 levels: ccadded.grass + ccadded.legume + ccadded.grasslegume + ccadded.brassica + ccadded.grassbrassica + ccadded.legumebrassica + ccadded.grasslegumebrassica
mod <- rma.mv(logRR, var, mods= ~0+ccadded.grass + ccadded.legume + ccadded.grasslegume + ccadded.brassica + ccadded.grasslegumebrassica, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_ccadded_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[13,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$ccadded.grass==1)), length(unique(dat$StudyID[which(dat$ccadded.grass==1)])))
effectsdat_SOC[14,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$ccadded.legume ==1)), length(unique(dat$StudyID[which(dat$ccadded.legume ==1)])))
effectsdat_SOC[15,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$ccadded.grasslegume ==1)), length(unique(dat$StudyID[which(dat$ccadded.grasslegume ==1)])))
effectsdat_SOC[16,] <- c("SOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$ccadded.brassica==1)), length(unique(dat$StudyID[which(dat$ccadded.brassica==1)])))
effectsdat_SOC[17,] <- c("SOC","ccadded.grassbrassica", rep(NA, 5))
effectsdat_SOC[18,] <- c("SOC","ccadded.legumebrassica", rep(NA, 5))
effectsdat_SOC[19,] <- c("SOC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], length(which(dat$ccadded.grasslegumebrassica==1)), length(unique(dat$StudyID[which(dat$ccadded.grasslegumebrassica==1)])))

ref <- 4
stats_SOC[ref,] <- c("ccadded", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## maincrop, 6 levels: maincrop.cereal + maincrop.legume + maincrop.cereallegume + maincrop.otherrowcrop + maincrop.perennial
mod <- rma.mv(logRR, var, mods= ~0+maincrop.cereal + maincrop.legume + maincrop.cereallegume + maincrop.otherrowcrop + maincrop.perennial, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_maincrop_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[20,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$maincrop.cereal==1)), length(unique(dat$StudyID[which(dat$maincrop.cereal==1)])))
effectsdat_SOC[21,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$maincrop.legume ==1)), length(unique(dat$StudyID[which(dat$maincrop.legume ==1)])))
effectsdat_SOC[22,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$maincrop.cereallegume ==1)), length(unique(dat$StudyID[which(dat$maincrop.cereallegume ==1)])))
effectsdat_SOC[23,] <- c("SOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$maincrop.otherrowcrop==1)), length(unique(dat$StudyID[which(dat$maincrop.otherrowcrop==1)])))
effectsdat_SOC[24,] <- c("SOC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], length(which(dat$maincrop.perennial ==1)), length(unique(dat$StudyID[which(dat$maincrop.perennial ==1)])))

ref <- 5
stats_SOC[ref,] <- c("maincrop", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## tillage, 2 levels: tillage.conventional + tillage.reducednotill
mod <- rma.mv(logRR, var, mods= ~0+tillage.conventional + tillage.reducednotill, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_tillage_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[25,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$tillage.conventional==1)), length(unique(dat$StudyID[which(dat$tillage.conventional==1)])))
effectsdat_SOC[26,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$tillage.reducednotill==1)), length(unique(dat$StudyID[which(dat$tillage.reducednotill==1)])))

ref <- 6
stats_SOC[ref,] <- c("tillage", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)



## nfert
mod <- rma.mv(logRR, var, mods= ~nfert, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_nfert_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[27,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$nfert)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$nfert)==FALSE)])))

ref <- 7
stats_SOC[ref,] <- c("nfert", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## ag.C.inputs
mod <- rma.mv(logRR, var, mods= ~ag.C.inputs, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_ag.C.inputs_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[28,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$ag.C.inputs)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ag.C.inputs)==FALSE)])))

ref <- 8
stats_SOC[ref,] <- c("ag.C.inputs", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## species.added
mod <- rma.mv(logRR, var, mods= ~species.added, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_species.added_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[29,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$species.added)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$species.added)==FALSE)])))

ref <- 9
stats_SOC[ref,] <- c("species.added", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## duration
mod <- rma.mv(logRR, var, mods= ~duration, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_duration_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[30,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$duration)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$duration)==FALSE)])))

ref <- 10
stats_SOC[ref,] <- c("duration", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## sand
mod <- rma.mv(logRR, var, mods= ~sand, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_sand_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[31,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$sand)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$sand)==FALSE)])))

ref <- 11
stats_SOC[ref,] <- c("sand", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## silt
mod <- rma.mv(logRR, var, mods= ~silt, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_silt_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[32,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$silt)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$silt)==FALSE)])))

ref <- 12
stats_SOC[ref,] <- c("silt", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## clay
mod <- rma.mv(logRR, var, mods= ~clay, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_clay_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[33,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$clay)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$clay)==FALSE)])))

ref <- 13
stats_SOC[ref,] <- c("clay", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## toc 
mod <- rma.mv(logRR, var, mods= ~toc, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_toc_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[34,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$toc)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$toc)==FALSE)])))

ref <- 14
stats_SOC[ref,] <- c("toc", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## ph
mod <- rma.mv(logRR, var, mods= ~ph, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_ph_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[35,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$ph)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ph)==FALSE)])))

ref <- 15
stats_SOC[ref,] <- c("ph", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## Temp
mod <- rma.mv(logRR, var, mods= ~Temp, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_Temp_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[36,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$Temp)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Temp)==FALSE)])))

ref <- 16
stats_SOC[ref,] <- c("Temp", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## Prec
mod <- rma.mv(logRR, var, mods= ~Prec, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_Prec_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[37,] <- c("SOC",rownames(mod$b)[2], mod$b[2,], mod$ci.lb[2], mod$ci.ub[2], length(which(is.na(dat$Prec)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Prec)==FALSE)])))

ref <- 17
stats_SOC[ref,] <- c("Prec", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)





## soilorder, 6 levels: soilorder.Alfisols + soilorder.Oxisols + soilorder.Mollisols + soilorder.Ultisols + soilorder.Spodosols + soilorder.Inceptisols 
mod <- rma.mv(logRR, var, mods= ~0+soilorder.Alfisols + soilorder.Oxisols + soilorder.Mollisols + soilorder.Ultisols + soilorder.Inceptisols, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_soilorder_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[38,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$soilorder.Alfisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Alfisols==1)])))
effectsdat_SOC[39,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$soilorder.Oxisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Oxisols==1)])))
effectsdat_SOC[40,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$soilorder.Mollisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Mollisols==1)])))
effectsdat_SOC[41,] <- c("SOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$soilorder.Ultisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Ultisols==1)])))
effectsdat_SOC[42,] <- c("SOC","soilorder.Spodosols", rep(NA, 5))
effectsdat_SOC[43,] <- c("SOC",rownames(mod$b)[5], mod$b[5,1], mod$ci.lb[5], mod$ci.ub[5], length(which(dat$soilorder.Inceptisols==1)), length(unique(dat$StudyID[which(dat$soilorder.Inceptisols==1)])))

ref <- 18
stats_SOC[ref,] <- c("soilorder", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)



## Continent, 6 levels: Continent.Africa + Continent.Asia + Continent.Australia + Continent.Europe + Continent.NorthAmerica + Continent.SouthAmerica
mod <- rma.mv(logRR, var, mods= ~0+Continent.Europe + Continent.NorthAmerica + Continent.SouthAmerica, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_Continent_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[44,] <- c("SOC","Continent.Africa", rep(NA, 5))
effectsdat_SOC[45,] <- c("SOC","Continent.Asia", rep(NA, 5))
effectsdat_SOC[46,] <- c("SOC","Continent.Australia", rep(NA, 5))
effectsdat_SOC[47,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$Continent.Europe==1)), length(unique(dat$StudyID[which(dat$Continent.Europe==1)])))
effectsdat_SOC[48,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$Continent.NorthAmerica==1)), length(unique(dat$StudyID[which(dat$Continent.NorthAmerica==1)])))
effectsdat_SOC[49,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$Continent.SouthAmerica==1)), length(unique(dat$StudyID[which(dat$Continent.SouthAmerica==1)])))

ref <- 19
stats_SOC[ref,] <- c("Continent", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## SoilCollectionSeason, 4 levels: SoilCollectionSeason.winter + SoilCollectionSeason.spring + SoilCollectionSeason.summer + SoilCollectionSeason.fall
mod <- rma.mv(logRR, var, mods= ~0+ SoilCollectionSeason.spring + SoilCollectionSeason.summer + SoilCollectionSeason.fall, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_SoilCollectionSeason_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[50,] <- c("SOC","SoilCollectionSeason.winter", rep(NA, 5))
effectsdat_SOC[51,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$SoilCollectionSeason.spring==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.spring==1)])))
effectsdat_SOC[52,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$SoilCollectionSeason.summer==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.summer==1)])))
effectsdat_SOC[53,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$SoilCollectionSeason.fall==1)), length(unique(dat$StudyID[which(dat$SoilCollectionSeason.fall==1)])))

ref <- 20
stats_SOC[ref,] <- c("SoilCollectionSeason", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## FractionationMethod, 3 levels: FractionationMethod.Size + FractionationMethod.Density + FractionationMethod.SizeAndDensity
mod <- rma.mv(logRR, var, mods= ~0+FractionationMethod.Size + FractionationMethod.Density, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_FractionationMethod_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[54,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$FractionationMethod.Size==1)), length(unique(dat$StudyID[which(dat$FractionationMethod.Size==1)])))
effectsdat_SOC[55,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$FractionationMethod.Density==1)), length(unique(dat$StudyID[which(dat$FractionationMethod.Density==1)])))
effectsdat_SOC[56,] <- c("SOC","FractionationMethod.SizeAndDensity", rep(NA, 5))

ref <- 21
stats_SOC[ref,] <- c("FractionationMethod", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


  
  

# DispersingAgent, 5 levels: DispersingAgent.SodiumHex + DispersingAgent.OtherSolution + DispersingAgent.GlassBeads + DispersingAgent.Sonication + DispersingAgent.Combo
mod <- rma.mv(logRR, var, mods= ~0+DispersingAgent.SodiumHex + DispersingAgent.OtherSolution + DispersingAgent.GlassBeads + DispersingAgent.Sonication, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_DispersingAgent_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[57,] <- c("SOC",rownames(mod$b)[1], mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$DispersingAgent.SodiumHex==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.SodiumHex==1)])))
effectsdat_SOC[58,] <- c("SOC",rownames(mod$b)[2], mod$b[2,1], mod$ci.lb[2], mod$ci.ub[2], length(which(dat$DispersingAgent.OtherSolution==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.OtherSolution==1)])))
effectsdat_SOC[59,] <- c("SOC",rownames(mod$b)[3], mod$b[3,1], mod$ci.lb[3], mod$ci.ub[3], length(which(dat$DispersingAgent.GlassBeads==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.GlassBeads==1)])))
effectsdat_SOC[60,] <- c("SOC",rownames(mod$b)[4], mod$b[4,1], mod$ci.lb[4], mod$ci.ub[4], length(which(dat$DispersingAgent.Sonication==1)), length(unique(dat$StudyID[which(dat$DispersingAgent.Sonication==1)])))
effectsdat_SOC[61,] <- c("SOC","DispersingAgent.Combo", rep(NA, 5))

ref <- 22
stats_SOC[ref,] <- c("DispersingAgent", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


## ResponseCalc, 2 levels: ResponseCalc.Direct + ResponseCalc.Indirect
mod <- rma.mv(logRR, var, mods= ~0+ResponseCalc.Direct, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/4_Single-moderator/SOC_ResponseCalc_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod
effectsdat_SOC[62,] <- c("SOC","ResponseCalc.Direct", mod$b[1,1], mod$ci.lb[1], mod$ci.ub[1], length(which(dat$ResponseCalc.Direct==1)), length(unique(dat$StudyID[which(dat$ResponseCalc.Direct==1)])))
effectsdat_SOC[63,] <- c("SOC","ResponseCalc.Indirect", rep(NA, 5))

ref <- 23
stats_SOC[ref,] <- c("ResponseCalc", mod$QM,  mod$QMdf[1], mod$QMp, mod$QE, mod$QMdf[2], mod$QEp)


write.csv(effectsdat_SOC, "Model-output/4_Single-moderator/effects-by-category_SOC_no-nitrogen.csv")
write.csv(stats_SOC, "Model-output/4_Single-moderator/stats-by-category_SOC_no-nitrogen.csv")








# Then, to determine how robust these relationships
# were, we added all significant moderators for a given global
# change-fraction pair into one model as suggested in Assink and
# Wibbelink (2016). If the moderators were still significant following
# the multiple moderator test, we considered those relationships robust
# (Assink andWibbelink, 2016).
# to test for effects of moderators with multiple levels, use this code for an omnibus test:
# anova(mod, btt=3:4)
# where 'mod' is the model object, and 3:4 are the effects of levels within the moderator


### POC
dat <- dat_POC
# significant single moderators for POC: depth + ccseason + tillage + ccadded  + maincrop + ag.C.inputs
# significant single moderators for POC (supplementary): Continent + DispersingAgent + FractionationMethod + ResponseCalc + SoilCollectionSeason+ soilorder

mod <- rma.mv(logRR, var, mods= ~ 0 + 
                depth.surface + depth.subsoil + depth.deepsoil + 
                ccseason.winter+ ccseason.spring+ ccseason.summer+ ccseason.yearround  + 
                tillage.conventional + tillage.reducednotill+ 
                ccadded.grass + ccadded.legume + ccadded.grasslegume + ccadded.brassica + ccadded.grasslegumebrassica+ 
                maincrop.cereal + maincrop.legume + maincrop.cereallegume + maincrop.otherrowcrop + maincrop.perennial +
                ag.C.inputs + Temp, # +
                # soilorder.Alfisols + soilorder.Oxisols + soilorder.Mollisols + soilorder.Ultisols + soilorder.Inceptisols +
                # Continent.Africa + Continent.Asia + Continent.Australia + Continent.Europe + Continent.NorthAmerica + Continent.SouthAmerica + 
                # SoilCollectionSeason.winter + SoilCollectionSeason.spring + SoilCollectionSeason.summer + SoilCollectionSeason.fall +
                # FractionationMethod.Size + FractionationMethod.Density + 
                # DispersingAgent.SodiumHex + DispersingAgent.OtherSolution + DispersingAgent.GlassBeads + DispersingAgent.Sonication + DispersingAgent.Combo +
                # ResponseCalc.Direct + ResponseCalc.Indirect, 
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/5_Full-models/1_POC-full_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod

anova_table <- data.frame(moderators = c("depth" ,"ccseason" , "tillage", "ccadded" ,  "maincrop", "agCinputs", "Temp"),# , 
                                         #"soilorder", "Continent", "soilcollectionseason", "fractionationmethod", "DispersingAgent", "responseCalc"),
                          result = rep(NA,7))
# depth
res <- anova(mod, btt=1:2)
anova_table$result[1] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# ccseason
res <- anova(mod, btt=3:6)
anova_table$result[2] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# tillage
res <- anova(mod, btt=7:8)
anova_table$result[3] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# ccadded
res <- anova(mod, btt=9:13)
anova_table$result[4] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# maincrop
res <- anova(mod, btt=14:16)
anova_table$result[5] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# ag.C.inputs
res <- anova(mod, btt=17)
anova_table$result[6] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# Temp
res <- anova(mod, btt=18)
anova_table$result[7] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# # soilorder
# res <- anova(mod, btt=19:23)
# anova_table$result[8] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))
# 
# # Continent
# res <- anova(mod, btt=24:25)
# anova_table$result[9] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))
# 
# # SoilCollectionSeason
# res <- anova(mod, btt=26:28)
# anova_table$result[10] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))
# 
# # Fractionationmethod
# # dropped from model
# 
# # DispersingAgent
# res <- anova(mod, btt=29:30)
# anova_table$result[12] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))
# 
# # ResponseCalc
# res <- anova(mod, btt=31)
# anova_table$result[13] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))


write.csv(anova_table, "Model-output/5_Full-models/2_POC-anova-by-mod_no-nitrogen.csv")






### MAOC
dat <- dat_MAOC
# significant single moderators for MAOC: depth + ccseason + ccadded  + maincrop + ag.C.inputs
# significant single moderators for MAOC (supplementary): FractionationMethod + ResponseCalc + SoilCollectionSeason+ soilorder

mod2 <- rma.mv(logRR, var, mods= ~ 0 + 
                 ccseason.winter+ ccseason.summer+ ccseason.yearround  +
                ccadded.grass + ccadded.legume + ccadded.grasslegume + 
                maincrop.cereal + maincrop.legume + maincrop.cereallegume + maincrop.perennial +
                 ag.C.inputs,
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/5_Full-models/1_MAOC-full_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod2

anova_table <- data.frame(moderators = c( "ccseason" , "ccadded" ,  "maincrop", "agCinputs" ),
                          result = rep(NA,4))
# ccseason
res <- anova(mod2, btt=1:3)
anova_table$result[1] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# ccadded
res <- anova(mod2, btt=4:6)
anova_table$result[2] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# maincrop
res <- anova(mod2, btt=7:9)
anova_table$result[3] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# ag.C.inputs
res <- anova(mod2, btt=10)
anova_table$result[4] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))


write.csv(anova_table, "Model-output/5_Full-models/2_MAOC-anova-by-mod_no-nitrogen.csv")






### SOC
dat <- dat_SOC
# significant single moderators for SOC: depth + ccseason + tillage + ccadded  + maincrop + ag.C.inputs
# significant single moderators for SOC (supplementary): Continent + DispersingAgent + FractionationMethod + ResponseCalc + soilorder

mod <- rma.mv(logRR, var, mods= ~ 0 + 
                depth.surface + depth.subsoil + depth.deepsoil + 
                ccseason.winter+ ccseason.spring+ ccseason.summer+ ccseason.yearround  + 
                tillage.conventional + tillage.reducednotill+ 
                ccadded.grass + ccadded.legume + ccadded.grasslegume + ccadded.brassica + ccadded.grasslegumebrassica+ 
                maincrop.cereal + maincrop.legume + maincrop.cereallegume + maincrop.otherrowcrop + maincrop.perennial +
                ag.C.inputs ,
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/5_Full-models/1_SOC-full_no-nitrogen.txt"); summary(mod); sink(file = NULL)
mod

anova_table <- data.frame(moderators = c("depth" ,"ccseason" , "tillage", "ccadded" ,  "maincrop", "agCinputs" ),
                          result = rep(NA,6))
# depth
res <- anova(mod, btt=1:3)
anova_table$result[1] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# ccseason
res <- anova(mod, btt=4:7)
anova_table$result[2] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# tillage
res <- anova(mod, btt=8:9)
anova_table$result[3] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# ccadded
res <- anova(mod, btt=10:14)
anova_table$result[4] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# maincrop
res <- anova(mod, btt=15:17)
anova_table$result[5] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))

# ag.C.inputs
res <- anova(mod, btt=18)
anova_table$result[6] <- paste0("F(df1 = ", res$QMdf[1], ", df2 = ", res$QMdf[2], ") = ", round(res$QM, 3), ", p-val = ", round(res$QMp,3))


write.csv(anova_table, "Model-output/5_Full-models/2_SOC-anova-by-mod_no-nitrogen.csv")








### Now, do one last model for each response variable coontinaing
### only those moderators that remained significant in the multiple-
### moderator models in the previous section. 





# final POC model
dat <- dat_POC
mod <- rma.mv(logRR, var, 
              mods= ~ 0 + 
                depth.surface + depth.subsoil + depth.deepsoil + 
                ag.C.inputs,
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
# test for significance of variation within- and between-studies
mod_2 <- rma.mv(logRR, var, 
                mods= ~ 0 + 
                  depth.surface + depth.subsoil + depth.deepsoil + 
                  ag.C.inputs, 
                random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, 
                sigma2=c(0,NA))
mod_3 <- rma.mv(logRR, var, 
                mods= ~ 0 + 
                  depth.surface + depth.subsoil + depth.deepsoil + 
                  ag.C.inputs, 
                random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, 
                sigma2=c(NA,0))
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
sink(file = "Model-output/1_Overall/POC_final_no-nitrogen.txt"); summary(mod); sink(file = NULL)
sink(file = "Model-output/2_Significance-of-variation/POC_within-study_final_no-nitrogen.txt"); anova(mod, mod_2); sink(file = NULL) # if p<0.05, within-study variance is significant
sink(file = "Model-output/2_Significance-of-variation/POC_between-study_final_no-nitrogen.txt"); anova(mod, mod_3); sink(file = NULL) # if p<0.05, between-study variance is significant
sink(file = "Model-output/3_Partitioning-of-variation/POC_final_no-nitrogen.txt"); data.frame(samplingvar = I2_1 * 100, withinstudyvar = I2_2 * 100, betweenstudyvar = I2_3 * 100); sink(file = NULL)



# final MAOC model
dat <- dat_MAOC
mod <- rma.mv(logRR, var, 
              mods= ~ 1,
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
# test for significance of variation within- and between-studies
mod_2 <- rma.mv(logRR, var, 
                mods= ~ 1, 
                random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, 
                sigma2=c(0,NA))
mod_3 <- rma.mv(logRR, var, 
                mods= ~ 1, 
                random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, 
                sigma2=c(NA,0))
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
sink(file = "Model-output/1_Overall/MAOC_final_no-nitrogen.txt"); summary(mod); sink(file = NULL)
sink(file = "Model-output/2_Significance-of-variation/MAOC_within-study_final_no-nitrogen.txt"); anova(mod, mod_2); sink(file = NULL) # if p<0.05, within-study variance is significant
sink(file = "Model-output/2_Significance-of-variation/MAOC_between-study_final_no-nitrogen.txt"); anova(mod, mod_3); sink(file = NULL) # if p<0.05, between-study variance is significant
sink(file = "Model-output/3_Partitioning-of-variation/MAOC_final_no-nitrogen.txt"); data.frame(samplingvar = I2_1 * 100, withinstudyvar = I2_2 * 100, betweenstudyvar = I2_3 * 100); sink(file = NULL)




# overall SOC model
dat <- dat_SOC
mod <- rma.mv(logRR, var, 
              mods= ~ 0 + 
                depth.surface + depth.subsoil + depth.deepsoil + 
                ag.C.inputs ,
              random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
# test for significance of variation within- and between-studies
mod_2 <- rma.mv(logRR, var, 
                mods= ~ 0 + 
                  depth.surface + depth.subsoil + depth.deepsoil + 
                  ag.C.inputs , 
                random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, 
                sigma2=c(0,NA))
mod_3 <- rma.mv(logRR, var, 
                mods= ~ 0 + 
                  depth.surface + depth.subsoil + depth.deepsoil + 
                  ag.C.inputs , 
                random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat, 
                sigma2=c(NA,0))
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
sink(file = "Model-output/1_Overall/SOC_final_no-nitrogen.txt"); summary(mod); sink(file = NULL)
sink(file = "Model-output/2_Significance-of-variation/SOC_within-study_final_no-nitrogen.txt"); anova(mod, mod_2); sink(file = NULL) # if p<0.05, within-study variance is significant
sink(file = "Model-output/2_Significance-of-variation/SOC_between-study_final_no-nitrogen.txt"); anova(mod, mod_3); sink(file = NULL) # if p<0.05, between-study variance is significant
sink(file = "Model-output/3_Partitioning-of-variation/SOC_final_no-nitrogen.txt"); data.frame(samplingvar = I2_1 * 100, withinstudyvar = I2_2 * 100, betweenstudyvar = I2_3 * 100); sink(file = NULL)
