library(metafor)
library(ggplot2)
library(fmsb)
library(ggpubr)





###### read in data
dat_POC <- read.csv("Processed-data/POC_no-nitrogen.csv")
dat_MAOC <- read.csv("Processed-data/MAOC_no-nitrogen.csv")
dat_SOC <- read.csv("Processed-data/SOC_no-nitrogen.csv")


# center and scale all predictor and response variables
dat_POC$logRR <- scale(dat_POC$logRR)
dat_POC$sand <- scale(dat_POC$sand)
dat_POC$silt <- scale(dat_POC$silt)
dat_POC$clay <- scale(dat_POC$clay)
dat_POC$toc <- scale(dat_POC$toc)
dat_POC$ph <- scale(dat_POC$ph)
dat_POC$species.added <- scale(dat_POC$species.added)
dat_POC$ag.C.inputs <- scale(dat_POC$ag.C.inputs)
dat_POC$duration <- scale(dat_POC$duration)
dat_POC$nfert <- scale(dat_POC$nfert)
dat_POC$Temp <- scale(dat_POC$Temp)
dat_POC$Prec <- scale(dat_POC$Prec)

dat_MAOC$logRR <- scale(dat_MAOC$logRR)
dat_MAOC$sand <- scale(dat_MAOC$sand)
dat_MAOC$silt <- scale(dat_MAOC$silt)
dat_MAOC$clay <- scale(dat_MAOC$clay)
dat_MAOC$toc <- scale(dat_MAOC$toc)
dat_MAOC$ph <- scale(dat_MAOC$ph)
dat_MAOC$species.added <- scale(dat_MAOC$species.added)
dat_MAOC$ag.C.inputs <- scale(dat_MAOC$ag.C.inputs)
dat_MAOC$duration <- scale(dat_MAOC$duration)
dat_MAOC$nfert <- scale(dat_MAOC$nfert)
dat_MAOC$Temp <- scale(dat_MAOC$Temp)
dat_MAOC$Prec <- scale(dat_MAOC$Prec)

dat_SOC$logRR <- scale(dat_SOC$logRR)
dat_SOC$sand <- scale(dat_SOC$sand)
dat_SOC$silt <- scale(dat_SOC$silt)
dat_SOC$clay <- scale(dat_SOC$clay)
dat_SOC$toc <- scale(dat_SOC$toc)
dat_SOC$ph <- scale(dat_SOC$ph)
dat_SOC$species.added <- scale(dat_SOC$species.added)
dat_SOC$ag.C.inputs <- scale(dat_SOC$ag.C.inputs)
dat_SOC$duration <- scale(dat_SOC$duration)
dat_SOC$nfert <- scale(dat_SOC$nfert)
dat_SOC$Temp <- scale(dat_SOC$Temp)
dat_SOC$Prec <- scale(dat_SOC$Prec)

# We will calculate standardized betas (slopes of regressions where both y and x are centered and scaled), 
# and then pseudo rsquares as described at 
# https://github.com/nmolanog/issue_r2_metafor
# which means we are calculating the amount (in percent) of the heterogeneity 
# in the reduced model that is accounted for in the full model (NA for fixed-
# effects models or for "rma.mv" objects). This can beregarded as a pseudo R2 
# statistic (Raudenbush, 2009)

r <- 20
effectsdat_POC <- data.frame(resp=rep(NA,r),
                             mod=rep(NA,r),
                             r2=rep(NA,r),
                             b=rep(NA,r),
                             m=rep(NA,r),
                             p=rep(NA,r),
                             nstudy=rep(NA,r),
                             nobs=rep(NA,r))
effectsdat_MAOC <- data.frame(resp=rep(NA,r),
                             mod=rep(NA,r),
                             r2=rep(NA,r),
                             b=rep(NA,r),
                             m=rep(NA,r),
                             p=rep(NA,r),
                             nstudy=rep(NA,r),
                             nobs=rep(NA,r))
effectsdat_SOC <- data.frame(resp=rep(NA,r),
                             mod=rep(NA,r),
                             r2=rep(NA,r),
                             b=rep(NA,r),
                             m=rep(NA,r),
                             p=rep(NA,r),
                             nstudy=rep(NA,r),
                             nobs=rep(NA,r))






# POC regressions
dat <- dat_POC

## sand
num <- 1
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~sand, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)
effectsdat_POC[1,] <- c("POC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                        length(which(is.na(dat$sand)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$sand)==FALSE)])))


## silt
num <- 2
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~silt, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)
effectsdat_POC[num,] <- c("POC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                        length(which(is.na(dat$silt)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$silt)==FALSE)])))


## clay
num <- 3
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~clay, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_POC[num,] <- c("POC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$clay)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$clay)==FALSE)])))

## toc
num <- 4
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~toc, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_POC[num,] <- c("POC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$toc)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$toc)==FALSE)])))

## ph
num <- 5
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~ph, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_POC[num,] <- c("POC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$ph)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ph)==FALSE)])))


## species.added
num <- 7
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~species.added, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_POC[num,] <- c("POC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$species.added)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$species.added)==FALSE)])))

## ag.C.inputs
num <- 8
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~ag.C.inputs, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_POC[num,] <- c("POC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$ag.C.inputs)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ag.C.inputs)==FALSE)])))


## duration
num <- 10
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~duration, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_POC[num,] <- c("POC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$duration)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$duration)==FALSE)])))

## nfert
num <- 11
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~nfert, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_POC[num,] <- c("POC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$nfert)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$nfert)==FALSE)])))


## Temp
num <- 13
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~Temp, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_POC[num,] <- c("POC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$Temp)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Temp)==FALSE)])))

## Prec
num <- 14
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~Prec, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_POC[num,] <- c("POC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$Prec)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Prec)==FALSE)])))








# MAOC regressions
dat <- dat_MAOC

## sand
num <- 1
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~sand, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)
effectsdat_MAOC[1,] <- c("MAOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                        length(which(is.na(dat$sand)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$sand)==FALSE)])))


## silt
num <- 2
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~silt, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)
effectsdat_MAOC[num,] <- c("MAOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$silt)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$silt)==FALSE)])))


## clay
num <- 3
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~clay, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_MAOC[num,] <- c("MAOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$clay)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$clay)==FALSE)])))

## toc
num <- 4
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~toc, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_MAOC[num,] <- c("MAOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$toc)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$toc)==FALSE)])))

## ph
num <- 5
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~ph, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_MAOC[num,] <- c("MAOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$ph)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ph)==FALSE)])))


## species.added
num <- 7
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~species.added, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_MAOC[num,] <- c("MAOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$species.added)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$species.added)==FALSE)])))

## ag.C.inputs
num <- 8
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~ag.C.inputs, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_MAOC[num,] <- c("MAOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$ag.C.inputs)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ag.C.inputs)==FALSE)])))


## duration
num <- 10
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~duration, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_MAOC[num,] <- c("MAOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$duration)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$duration)==FALSE)])))

## nfert
num <- 11
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~nfert, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_MAOC[num,] <- c("MAOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$nfert)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$nfert)==FALSE)])))



## Temp
num <- 13
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~Temp, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_MAOC[num,] <- c("MAOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$Temp)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Temp)==FALSE)])))

## Prec
num <- 14
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~Prec, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_MAOC[num,] <- c("MAOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$Prec)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Prec)==FALSE)])))






# SOC regressions
dat <- dat_SOC

## sand
num <- 1
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~sand, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)
effectsdat_SOC[1,] <- c("SOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                        length(which(is.na(dat$sand)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$sand)==FALSE)])))


## silt
num <- 2
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~silt, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)
effectsdat_SOC[num,] <- c("SOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$silt)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$silt)==FALSE)])))


## clay
num <- 3
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~clay, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_SOC[num,] <- c("SOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$clay)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$clay)==FALSE)])))

## toc
num <- 4
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~toc, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_SOC[num,] <- c("SOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$toc)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$toc)==FALSE)])))

## ph
num <- 5
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~ph, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
            data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_SOC[num,] <- c("SOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$ph)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ph)==FALSE)])))


## species.added
num <- 7
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~species.added, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_SOC[num,] <- c("SOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$species.added)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$species.added)==FALSE)])))

## ag.C.inputs
num <- 8
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~ag.C.inputs, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_SOC[num,] <- c("SOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$ag.C.inputs)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$ag.C.inputs)==FALSE)])))

## duration
num <- 10
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~duration, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_SOC[num,] <- c("SOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$duration)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$duration)==FALSE)])))

## nfert
num <- 11
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~nfert, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_SOC[num,] <- c("SOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$nfert)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$nfert)==FALSE)])))

## Temp
num <- 13
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~Temp, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_SOC[num,] <- c("SOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$Temp)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Temp)==FALSE)])))

## Prec
num <- 14
m1<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), 
           data = dat, mods= ~Prec, method = "REML")
m10<-rma.mv(logRR, var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), data = dat, method = "REML")
sig <- (sum(m10$sigma2) - sum(m1$sigma2)) / sum(m10$sigma2)

effectsdat_SOC[num,] <- c("SOC",rownames(m1$b)[2], sig, m1$b[1,], m1$b[2,], m1$pval[2],
                          length(which(is.na(dat$Prec)==FALSE)), length(unique(dat$StudyID[which(is.na(dat$Prec)==FALSE)])))




# save correlation tables
write.csv(effectsdat_POC, "Model-output/4_Single-moderator/effects-correlations_POC_standardized-beta.csv")
write.csv(effectsdat_MAOC, "Model-output/4_Single-moderator/effects-correlations_MAOC_standardized-beta.csv")
write.csv(effectsdat_SOC, "Model-output/4_Single-moderator/effects-correlations_SOC_standardized-beta.csv")



# import correlation tables
effectsdat_POC2 <- read.csv("Model-output/4_Single-moderator/effects-correlations_POC_standardized-beta.csv")
effectsdat_MAOC2 <- read.csv("Model-output/4_Single-moderator/effects-correlations_MAOC_standardized-beta.csv")
effectsdat_SOC2 <- read.csv("Model-output/4_Single-moderator/effects-correlations_SOC_standardized-beta.csv")



# get rid of some variables
effectsdat_POC2 <- effectsdat_POC2[-which(effectsdat_POC2$mod %in% c(NA)),]
effectsdat_MAOC2 <- effectsdat_MAOC2[-which(effectsdat_MAOC2$mod %in% c(NA)),]
effectsdat_SOC2 <- effectsdat_SOC2[-which(effectsdat_SOC2$mod %in% c(NA)),]

# create new index
effectsdat_POC2$index <- seq(1:dim(effectsdat_POC2)[1])
effectsdat_MAOC2$index <- seq(1:dim(effectsdat_MAOC2)[1])
effectsdat_SOC2$index <- seq(1:dim(effectsdat_SOC2)[1])





################# Fig. 4 in manuscript: 
# standardized betas between carbon pool response to cover cropping and continuous moderators
pcol <- "olivedrab4"
mcol <- "orange4"
scol <- "black"
nmods <- 11


# Create dataframe: 
options(scipen = 100)
dat <- data.frame(r2 = c(as.numeric(effectsdat_SOC2$r2[1:nmods]), as.numeric(effectsdat_MAOC2$r2[1:nmods]), as.numeric(effectsdat_POC2$r2[1:nmods])),
                  # p-value
                  p = c(as.numeric(effectsdat_SOC2$p[1:nmods]), as.numeric(effectsdat_MAOC2$p[1:nmods]), as.numeric(effectsdat_POC2$p[1:nmods])),
                  # slope (standardized)
                  m = c(as.numeric(effectsdat_SOC2$m[1:nmods]), as.numeric(effectsdat_MAOC2$m[1:nmods]), as.numeric(effectsdat_POC2$m[1:nmods])),
                  # intercept (standardized)
                  b = c(as.numeric(effectsdat_SOC2$b[1:nmods]), as.numeric(effectsdat_MAOC2$b[1:nmods]), as.numeric(effectsdat_POC2$b[1:nmods])),
                  # predictor
                  mod = rep(effectsdat_SOC2$mod[1:nmods],3),
                  # response
                  resp = rep(c("SOC", "MAOC", "POC"), each=nmods),
                  index = rep(1:nmods, 3))
dat$r2[dat$r2<0] <- 0 # replace negative r-square values with zero
dat <- dat[order(dat$index),]

dat$mod <- as.factor(dat$mod)
dat$mod2 <- rep(c("Baseline\nsand\n(%)", "Baseline\nsilt\n(%)", "Baseline\nclay\n(%)", "Baseline\nSOC\n(%)", "Baseline\npH", 
                  "Number of\nspecies added", "Aboveground\nC inputs\n(Mg ha-1 yr-1)",
                  "Duration\n(yrs)", "Nitrogen\nfertilization\n(kg ha-1 yr-1)", 
                  "Mean annual\ntemperature\n(°C)", "Annual\nprecipitation\n(mm)"), each=3)
dat$resp <- as.factor(dat$resp)
dat$id <- as.factor(1:dim(dat)[1])
# create column indicating significance
dat$signif <- rep("", dim(dat)[1])
dat$signif[which(dat$p<0.05)] <- "*"
# create column indicating direction of the regression
dat$color <- rep("red", dim(dat)[1])
dat$color[which(dat$m>0)] <- "blue"

write.csv(dat, "Model-output/4_Single-moderator/*regression-data_standardized.csv")



# df for lines separating each mod
df <- data.frame(x = seq(0.5,dim(dat)[1], by=3),
                 y= rep(-0.25, 11),
                 yend=rep(0.75,11))

# Make the plot
p <- ggplot(dat, aes(x=id, y=m, fill=resp)) + # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(stat="identity" ) +
  scale_fill_manual(values=c(mcol, pcol, scol))+
  ylim(-0.3,0.9) +
  # vertical lines
  geom_segment(inherit.aes = F, data=df, aes(x=x, xend=x, y=y, yend=yend),
               color='black', alpha=.25) +
  # horizontal lines
  geom_hline(yintercept=seq(-0.4, 0.7, by=0.1), color='black', alpha=.25) +
  geom_hline(yintercept=0, color='black', alpha=.75) +
  annotate(geom= "label", x=17, y=c(-0.4, -0.2, 0.2, 0.4, 0.6), fill="white", alpha=0.5, label.size = NA,
           size=1.75, label=c(-0.4, -0.2, 0.2, 0.4, 0.6)) +
  # predictor variable labels
  annotate(geom= "text", x=seq(2,dim(dat)[1], by=3), y=0.9, 
           size=2.25, label=unique(dat$mod2)) +
  # Custom the theme: 
  theme_minimal() +
  theme(axis.text = element_blank(),axis.title = element_blank(),
        panel.grid = element_blank(), legend.position="bottom", 
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.7, 'cm'), #change legend key width
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=6),
        legend.margin=margin(t = -0.7, unit='cm'),
        plot.margin = margin(0,0,0,0, "cm"))+
  # mark significant associations
  #annotate(geom= "text", x=1:dim(dat)[1], y=dat$m+0.05, hjust=0.5,
           #size=5, label=dat$signif, col=dat$color)+   
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start=6) +
  labs(fill = "Cover crop response")  +
  geom_bar(stat="identity" ) 
  
  
p


# jpeg("Figures/4_standardized-betas.jpeg", width=4.2, height=4.2, units="in",res=600)
# p
# dev.off()














###################### appendix figure: regression plots for POC
mods <- unique(dat$mod2)
tsize <- 1.5
hlinesize <- 1.5
hlinealpha <- 0.5
dat$linetype <- rep(2,dim(dat)[1]); dat$linetype[which(dat$signif=="*")] <- 1
psize <- 1.5
palpha <- 0.5
roundto <- 4


##### POC
rawdat <- dat_POC
ymax <- max(na.omit(rawdat$logRR))



#  ~ sand
ref_sand <- which(dat$resp=="POC" & dat$mod=="sand")
m_sand <- round(dat$m[ref_sand], roundto)
b_sand <- round(dat$b[ref_sand], roundto)
r_sand <- as.character(round(dat$r2[ref_sand], roundto))
lty_sand <- dat$linetype[ref_sand]
xmin_sand <- min(na.omit(rawdat$sand))
xmax_sand <- max(na.omit(rawdat$sand))
p_sand <- ggplot(rawdat, aes(x = sand, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_sand], linetype=lty_sand, aes(x = xmin_sand, xend = xmax_sand, y = m_sand*xmin_sand + b_sand , yend = m_sand*xmax_sand + b_sand))  +
  annotate(geom="text", x=xmin_sand, y=ymax ,hjust=0, label=paste("y = ", m_sand, "*x", "+", b_sand)) +  labs(x="Standardized Baseline sand (%)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_sand, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_sand)) + theme_bw()
p_sand 

#  ~ silt
ref_silt <- which(dat$resp=="POC" & dat$mod=="silt")
m_silt <- round(dat$m[ref_silt], roundto)
b_silt <- round(dat$b[ref_silt], roundto)
r_silt <- as.character(round(dat$r2[ref_silt], roundto))
lty_silt <- dat$linetype[ref_silt]
xmin_silt <- min(na.omit(rawdat$silt))
xmax_silt <- max(na.omit(rawdat$silt))
p_silt <- ggplot(rawdat, aes(x = silt, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_silt], linetype=lty_silt, aes(x = xmin_silt, xend = xmax_silt, y = m_silt*xmin_silt + b_silt , yend = m_silt*xmax_silt + b_silt))  +
  annotate(geom="text", x=xmin_silt, y=ymax ,hjust=0, label=paste("y = ", m_silt, "*x", "+", b_silt)) +  labs(x="Standardized Baseline silt (%)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_silt, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_silt)) + theme_bw()
p_silt 

#  ~ clay
ref_clay <- which(dat$resp=="POC" & dat$mod=="clay")
m_clay <- round(dat$m[ref_clay], roundto)
b_clay <- round(dat$b[ref_clay], roundto)
r_clay <- as.character(round(dat$r2[ref_clay], roundto))
lty_clay <- dat$linetype[ref_clay]
xmin_clay <- min(na.omit(rawdat$clay))
xmax_clay <- max(na.omit(rawdat$clay))
p_clay <- ggplot(rawdat, aes(x = clay, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_clay], linetype=lty_clay, aes(x = xmin_clay, xend = xmax_clay, y = m_clay*xmin_clay + b_clay , yend = m_clay*xmax_clay + b_clay))  +
  annotate(geom="text", x=xmin_clay, y=ymax ,hjust=0, label=paste("y = ", m_clay, "*x", "+", b_clay)) +  labs(x="Standardized Baseline clay (%)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_clay, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_clay)) + theme_bw()
p_clay 

#  ~ toc
ref_toc <- which(dat$resp=="POC" & dat$mod=="toc")
m_toc <- round(dat$m[ref_toc], roundto)
b_toc <- round(dat$b[ref_toc], roundto)
r_toc <- as.character(round(dat$r2[ref_toc], roundto))
lty_toc <- dat$linetype[ref_toc]
xmin_toc <- min(na.omit(rawdat$toc))
xmax_toc <- max(na.omit(rawdat$toc))
p_toc <- ggplot(rawdat, aes(x = toc, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_toc], linetype=lty_toc, aes(x = xmin_toc, xend = xmax_toc, y = m_toc*xmin_toc + b_toc , yend = m_toc*xmax_toc + b_toc))  +
  annotate(geom="text", x=xmin_toc, y=ymax ,hjust=0, label=paste("y = ", m_toc, "*x", "+", b_toc)) +  labs(x="Standardized Baseline SOC (%)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_toc, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_toc)) + theme_bw()
p_toc 


#  ~ ph
ref_ph <- which(dat$resp=="POC" & dat$mod=="ph")
m_ph <- round(dat$m[ref_ph], roundto)
b_ph <- round(dat$b[ref_ph], roundto)
r_ph <- as.character(round(dat$r2[ref_ph], roundto))
lty_ph <- dat$linetype[ref_ph]
xmin_ph <- min(na.omit(rawdat$ph))
xmax_ph <- max(na.omit(rawdat$ph))
p_ph <- ggplot(rawdat, aes(x = ph, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_ph], linetype=lty_ph, aes(x = xmin_ph, xend = xmax_ph, y = m_ph*xmin_ph + b_ph , yend = m_ph*xmax_ph + b_ph))  +
  annotate(geom="text", x=xmin_ph, y=ymax ,hjust=0, label=paste("y = ", m_ph, "*x", "+", b_ph)) +  labs(x="Standardized Baseline pH", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_ph, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_ph)) + theme_bw()
p_ph 

#  ~ species.added
ref_species.added <- which(dat$resp=="POC" & dat$mod=="species.added")
m_species.added <- round(dat$m[ref_species.added], roundto)
b_species.added <- round(dat$b[ref_species.added], roundto)
r_species.added <- as.character(round(dat$r2[ref_species.added], roundto))
lty_species.added <- dat$linetype[ref_species.added]
xmin_species.added <- min(na.omit(rawdat$species.added))
xmax_species.added <- max(na.omit(rawdat$species.added))
p_species.added <- ggplot(rawdat, aes(x = species.added, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_species.added], linetype=lty_species.added, aes(x = xmin_species.added, xend = xmax_species.added, y = m_species.added*xmin_species.added + b_species.added , yend = m_species.added*xmax_species.added + b_species.added))  +
  annotate(geom="text", x=xmin_species.added, y=ymax ,hjust=0, label=paste("y = ", m_species.added, "*x", "+", b_species.added)) +  labs(x="Standardized Number of species added", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_species.added, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_species.added)) + theme_bw()
p_species.added

#  ~ ag.C.inputs
ref_ag.C.inputs <- which(dat$resp=="POC" & dat$mod=="ag.C.inputs")
m_ag.C.inputs <- round(dat$m[ref_ag.C.inputs], roundto)
b_ag.C.inputs <- round(dat$b[ref_ag.C.inputs], roundto)
r_ag.C.inputs <- as.character(round(dat$r2[ref_ag.C.inputs], roundto))
lty_ag.C.inputs <- dat$linetype[ref_ag.C.inputs]
xmin_ag.C.inputs <- min(na.omit(rawdat$ag.C.inputs))
xmax_ag.C.inputs <- max(na.omit(rawdat$ag.C.inputs))
p_ag.C.inputs <- ggplot(rawdat, aes(x = ag.C.inputs, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_ag.C.inputs], linetype=lty_ag.C.inputs, aes(x = xmin_ag.C.inputs, xend = xmax_ag.C.inputs, y = m_ag.C.inputs*xmin_ag.C.inputs + b_ag.C.inputs , yend = m_ag.C.inputs*xmax_ag.C.inputs + b_ag.C.inputs))  +
  annotate(geom="text", x=xmin_ag.C.inputs, y=ymax ,hjust=0, label=paste("y = ", m_ag.C.inputs, "*x", "+", b_ag.C.inputs)) +  labs(x="Standardized Aboveground C inputs (Mg ha-1 yr-1)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_ag.C.inputs, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_ag.C.inputs)) + theme_bw()
p_ag.C.inputs

#  ~ duration
ref_duration <- which(dat$resp=="POC" & dat$mod=="duration")
m_duration <- round(dat$m[ref_duration], roundto)
b_duration <- round(dat$b[ref_duration], roundto)
r_duration <- as.character(round(dat$r2[ref_duration], roundto))
lty_duration <- dat$linetype[ref_duration]
xmin_duration <- min(na.omit(rawdat$duration))
xmax_duration <- max(na.omit(rawdat$duration))
p_duration <- ggplot(rawdat, aes(x = duration, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_duration], linetype=lty_duration, aes(x = xmin_duration, xend = xmax_duration, y = m_duration*xmin_duration + b_duration , yend = m_duration*xmax_duration + b_duration))  +
  annotate(geom="text", x=xmin_duration, y=ymax ,hjust=0, label=paste("y = ", m_duration, "*x", "+", b_duration)) +  labs(x="Standardized Duration (yrs)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_duration, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_duration)) + theme_bw()
p_duration

#  ~ nfert
ref_nfert <- which(dat$resp=="POC" & dat$mod=="nfert")
m_nfert <- round(dat$m[ref_nfert], roundto)
b_nfert <- round(dat$b[ref_nfert], roundto)
r_nfert <- as.character(round(dat$r2[ref_nfert], roundto))
lty_nfert <- dat$linetype[ref_nfert]
xmin_nfert <- min(na.omit(rawdat$nfert))
xmax_nfert <- max(na.omit(rawdat$nfert))
p_nfert <- ggplot(rawdat, aes(x = nfert, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_nfert], linetype=lty_nfert, aes(x = xmin_nfert, xend = xmax_nfert, y = m_nfert*xmin_nfert + b_nfert , yend = m_nfert*xmax_nfert + b_nfert))  +
  annotate(geom="text", x=xmin_nfert, y=ymax ,hjust=0, label=paste("y = ", m_nfert, "*x", "+", b_nfert)) +  labs(x="Standardized N fertilization (kg ha-1 yr-1)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_nfert, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_nfert)) + theme_bw()
p_nfert

#  ~ Temp
ref_Temp <- which(dat$resp=="POC" & dat$mod=="Temp")
m_Temp <- round(dat$m[ref_Temp], roundto)
b_Temp <- round(dat$b[ref_Temp], roundto)
r_Temp <- as.character(round(dat$r2[ref_Temp], roundto))
lty_Temp <- dat$linetype[ref_Temp]
xmin_Temp <- min(na.omit(rawdat$Temp))
xmax_Temp <- max(na.omit(rawdat$Temp))
p_Temp <- ggplot(rawdat, aes(x = Temp, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_Temp], linetype=lty_Temp, aes(x = xmin_Temp, xend = xmax_Temp, y = m_Temp*xmin_Temp + b_Temp , yend = m_Temp*xmax_Temp + b_Temp))  +
  annotate(geom="text", x=xmin_Temp, y=ymax ,hjust=0, label=paste("y = ", m_Temp, "*x", "+", b_Temp)) +  labs(x="Standardized Mean annual temperature (°C)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_Temp, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_Temp)) + theme_bw()
p_Temp

#  ~ Prec
ref_Prec <- which(dat$resp=="POC" & dat$mod=="Prec")
m_Prec <- round(dat$m[ref_Prec], roundto)
b_Prec <- round(dat$b[ref_Prec], roundto)
r_Prec <- as.character(round(dat$r2[ref_Prec], roundto))
lty_Prec <- dat$linetype[ref_Prec]
xmin_Prec <- min(na.omit(rawdat$Prec))
xmax_Prec <- max(na.omit(rawdat$Prec))
p_Prec <- ggplot(rawdat, aes(x = Prec, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_Prec], linetype=lty_Prec, aes(x = xmin_Prec, xend = xmax_Prec, y = m_Prec*xmin_Prec + b_Prec , yend = m_Prec*xmax_Prec + b_Prec))  +
  annotate(geom="text", x=xmin_Prec, y=ymax ,hjust=0, label=paste("y = ", m_Prec, "*x", "+", b_Prec)) +  labs(x="Standardized annual precipitation (mm)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_Prec, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_Prec)) + theme_bw()
p_Prec


jpeg("Figures/A.4_POC regressions_standardized.jpeg", width=12, height=14, units="in",res=600)
ggarrange(p_sand, p_silt, p_clay, p_toc, p_ph, 
          p_species.added, p_ag.C.inputs, p_duration, p_nfert, 
          p_Temp, p_Prec,
          labels = LETTERS[1:11],
          ncol = 3, nrow = 4)
dev.off()










###################### appendix figure: regression plots for MAOC
mods <- unique(dat$mod2)
tsize <- 1.5
hlinesize <- 1.5
hlinealpha <- 0.5
dat$linetype <- rep(2,dim(dat)[1]); dat$linetype[which(dat$signif=="*")] <- 1
psize <- 1.5
palpha <- 0.5
roundto <- 4


##### MAOC
rawdat <- dat_MAOC
ymax <- max(na.omit(rawdat$logRR))



#  ~ sand
ref_sand <- which(dat$resp=="MAOC" & dat$mod=="sand")
m_sand <- round(dat$m[ref_sand], roundto)
b_sand <- round(dat$b[ref_sand], roundto)
r_sand <- as.character(round(dat$r2[ref_sand], roundto))
lty_sand <- dat$linetype[ref_sand]
xmin_sand <- min(na.omit(rawdat$sand))
xmax_sand <- max(na.omit(rawdat$sand))
p_sand <- ggplot(rawdat, aes(x = sand, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_sand], linetype=lty_sand, aes(x = xmin_sand, xend = xmax_sand, y = m_sand*xmin_sand + b_sand , yend = m_sand*xmax_sand + b_sand))  +
  annotate(geom="text", x=xmin_sand, y=ymax ,hjust=0, label=paste("y = ", m_sand, "*x", "+", b_sand)) +  labs(x="Standardized Baseline sand (%)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_sand, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_sand)) + theme_bw()
p_sand 

#  ~ silt
ref_silt <- which(dat$resp=="MAOC" & dat$mod=="silt")
m_silt <- round(dat$m[ref_silt], roundto)
b_silt <- round(dat$b[ref_silt], roundto)
r_silt <- as.character(round(dat$r2[ref_silt], roundto))
lty_silt <- dat$linetype[ref_silt]
xmin_silt <- min(na.omit(rawdat$silt))
xmax_silt <- max(na.omit(rawdat$silt))
p_silt <- ggplot(rawdat, aes(x = silt, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_silt], linetype=lty_silt, aes(x = xmin_silt, xend = xmax_silt, y = m_silt*xmin_silt + b_silt , yend = m_silt*xmax_silt + b_silt))  +
  annotate(geom="text", x=xmin_silt, y=ymax ,hjust=0, label=paste("y = ", m_silt, "*x", "+", b_silt)) +  labs(x="Standardized Baseline silt (%)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_silt, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_silt)) + theme_bw()
p_silt 

#  ~ clay
ref_clay <- which(dat$resp=="MAOC" & dat$mod=="clay")
m_clay <- round(dat$m[ref_clay], roundto)
b_clay <- round(dat$b[ref_clay], roundto)
r_clay <- as.character(round(dat$r2[ref_clay], roundto))
lty_clay <- dat$linetype[ref_clay]
xmin_clay <- min(na.omit(rawdat$clay))
xmax_clay <- max(na.omit(rawdat$clay))
p_clay <- ggplot(rawdat, aes(x = clay, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_clay], linetype=lty_clay, aes(x = xmin_clay, xend = xmax_clay, y = m_clay*xmin_clay + b_clay , yend = m_clay*xmax_clay + b_clay))  +
  annotate(geom="text", x=xmin_clay, y=ymax ,hjust=0, label=paste("y = ", m_clay, "*x", "+", b_clay)) +  labs(x="Standardized Baseline clay (%)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_clay, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_clay)) + theme_bw()
p_clay 

#  ~ toc
ref_toc <- which(dat$resp=="MAOC" & dat$mod=="toc")
m_toc <- round(dat$m[ref_toc], roundto)
b_toc <- round(dat$b[ref_toc], roundto)
r_toc <- as.character(round(dat$r2[ref_toc], roundto))
lty_toc <- dat$linetype[ref_toc]
xmin_toc <- min(na.omit(rawdat$toc))
xmax_toc <- max(na.omit(rawdat$toc))
p_toc <- ggplot(rawdat, aes(x = toc, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_toc], linetype=lty_toc, aes(x = xmin_toc, xend = xmax_toc, y = m_toc*xmin_toc + b_toc , yend = m_toc*xmax_toc + b_toc))  +
  annotate(geom="text", x=xmin_toc, y=ymax ,hjust=0, label=paste("y = ", m_toc, "*x", "+", b_toc)) +  labs(x="Standardized Baseline SOC (%)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_toc, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_toc)) + theme_bw()
p_toc 


#  ~ ph
ref_ph <- which(dat$resp=="MAOC" & dat$mod=="ph")
m_ph <- round(dat$m[ref_ph], roundto)
b_ph <- round(dat$b[ref_ph], roundto)
r_ph <- as.character(round(dat$r2[ref_ph], roundto))
lty_ph <- dat$linetype[ref_ph]
xmin_ph <- min(na.omit(rawdat$ph))
xmax_ph <- max(na.omit(rawdat$ph))
p_ph <- ggplot(rawdat, aes(x = ph, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_ph], linetype=lty_ph, aes(x = xmin_ph, xend = xmax_ph, y = m_ph*xmin_ph + b_ph , yend = m_ph*xmax_ph + b_ph))  +
  annotate(geom="text", x=xmin_ph, y=ymax ,hjust=0, label=paste("y = ", m_ph, "*x", "+", b_ph)) +  labs(x="Standardized Baseline pH", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_ph, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_ph)) + theme_bw()
p_ph 

#  ~ species.added
ref_species.added <- which(dat$resp=="MAOC" & dat$mod=="species.added")
m_species.added <- round(dat$m[ref_species.added], roundto)
b_species.added <- round(dat$b[ref_species.added], roundto)
r_species.added <- as.character(round(dat$r2[ref_species.added], roundto))
lty_species.added <- dat$linetype[ref_species.added]
xmin_species.added <- min(na.omit(rawdat$species.added))
xmax_species.added <- max(na.omit(rawdat$species.added))
p_species.added <- ggplot(rawdat, aes(x = species.added, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_species.added], linetype=lty_species.added, aes(x = xmin_species.added, xend = xmax_species.added, y = m_species.added*xmin_species.added + b_species.added , yend = m_species.added*xmax_species.added + b_species.added))  +
  annotate(geom="text", x=xmin_species.added, y=ymax ,hjust=0, label=paste("y = ", m_species.added, "*x", "+", b_species.added)) +  labs(x="Standardized Number of species added", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_species.added, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_species.added)) + theme_bw()
p_species.added

#  ~ ag.C.inputs
ref_ag.C.inputs <- which(dat$resp=="MAOC" & dat$mod=="ag.C.inputs")
m_ag.C.inputs <- round(dat$m[ref_ag.C.inputs], roundto)
b_ag.C.inputs <- round(dat$b[ref_ag.C.inputs], roundto)
r_ag.C.inputs <- as.character(round(dat$r2[ref_ag.C.inputs], roundto))
lty_ag.C.inputs <- dat$linetype[ref_ag.C.inputs]
xmin_ag.C.inputs <- min(na.omit(rawdat$ag.C.inputs))
xmax_ag.C.inputs <- max(na.omit(rawdat$ag.C.inputs))
p_ag.C.inputs <- ggplot(rawdat, aes(x = ag.C.inputs, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_ag.C.inputs], linetype=lty_ag.C.inputs, aes(x = xmin_ag.C.inputs, xend = xmax_ag.C.inputs, y = m_ag.C.inputs*xmin_ag.C.inputs + b_ag.C.inputs , yend = m_ag.C.inputs*xmax_ag.C.inputs + b_ag.C.inputs))  +
  annotate(geom="text", x=xmin_ag.C.inputs, y=ymax ,hjust=0, label=paste("y = ", m_ag.C.inputs, "*x", "+", b_ag.C.inputs)) +  labs(x="Standardized Aboveground C inputs (Mg ha-1 yr-1)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_ag.C.inputs, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_ag.C.inputs)) + theme_bw()
p_ag.C.inputs

#  ~ duration
ref_duration <- which(dat$resp=="MAOC" & dat$mod=="duration")
m_duration <- round(dat$m[ref_duration], roundto)
b_duration <- round(dat$b[ref_duration], roundto)
r_duration <- as.character(round(dat$r2[ref_duration], roundto))
lty_duration <- dat$linetype[ref_duration]
xmin_duration <- min(na.omit(rawdat$duration))
xmax_duration <- max(na.omit(rawdat$duration))
p_duration <- ggplot(rawdat, aes(x = duration, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_duration], linetype=lty_duration, aes(x = xmin_duration, xend = xmax_duration, y = m_duration*xmin_duration + b_duration , yend = m_duration*xmax_duration + b_duration))  +
  annotate(geom="text", x=xmin_duration, y=ymax ,hjust=0, label=paste("y = ", m_duration, "*x", "+", b_duration)) +  labs(x="Standardized Duration (yrs)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_duration, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_duration)) + theme_bw()
p_duration

#  ~ nfert
ref_nfert <- which(dat$resp=="MAOC" & dat$mod=="nfert")
m_nfert <- round(dat$m[ref_nfert], roundto)
b_nfert <- round(dat$b[ref_nfert], roundto)
r_nfert <- as.character(round(dat$r2[ref_nfert], roundto))
lty_nfert <- dat$linetype[ref_nfert]
xmin_nfert <- min(na.omit(rawdat$nfert))
xmax_nfert <- max(na.omit(rawdat$nfert))
p_nfert <- ggplot(rawdat, aes(x = nfert, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_nfert], linetype=lty_nfert, aes(x = xmin_nfert, xend = xmax_nfert, y = m_nfert*xmin_nfert + b_nfert , yend = m_nfert*xmax_nfert + b_nfert))  +
  annotate(geom="text", x=xmin_nfert, y=ymax ,hjust=0, label=paste("y = ", m_nfert, "*x", "+", b_nfert)) +  labs(x="Standardized N fertilization (kg ha-1 yr-1)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_nfert, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_nfert)) + theme_bw()
p_nfert

#  ~ Temp
ref_Temp <- which(dat$resp=="MAOC" & dat$mod=="Temp")
m_Temp <- round(dat$m[ref_Temp], roundto)
b_Temp <- round(dat$b[ref_Temp], roundto)
r_Temp <- as.character(round(dat$r2[ref_Temp], roundto))
lty_Temp <- dat$linetype[ref_Temp]
xmin_Temp <- min(na.omit(rawdat$Temp))
xmax_Temp <- max(na.omit(rawdat$Temp))
p_Temp <- ggplot(rawdat, aes(x = Temp, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_Temp], linetype=lty_Temp, aes(x = xmin_Temp, xend = xmax_Temp, y = m_Temp*xmin_Temp + b_Temp , yend = m_Temp*xmax_Temp + b_Temp))  +
  annotate(geom="text", x=xmin_Temp, y=ymax ,hjust=0, label=paste("y = ", m_Temp, "*x", "+", b_Temp)) +  labs(x="Standardized Mean annual temperature (°C)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_Temp, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_Temp)) + theme_bw()
p_Temp

#  ~ Prec
ref_Prec <- which(dat$resp=="MAOC" & dat$mod=="Prec")
m_Prec <- round(dat$m[ref_Prec], roundto)
b_Prec <- round(dat$b[ref_Prec], roundto)
r_Prec <- as.character(round(dat$r2[ref_Prec], roundto))
lty_Prec <- dat$linetype[ref_Prec]
xmin_Prec <- min(na.omit(rawdat$Prec))
xmax_Prec <- max(na.omit(rawdat$Prec))
p_Prec <- ggplot(rawdat, aes(x = Prec, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_Prec], linetype=lty_Prec, aes(x = xmin_Prec, xend = xmax_Prec, y = m_Prec*xmin_Prec + b_Prec , yend = m_Prec*xmax_Prec + b_Prec))  +
  annotate(geom="text", x=xmin_Prec, y=ymax ,hjust=0, label=paste("y = ", m_Prec, "*x", "+", b_Prec)) +  labs(x="Standardized annual precipitation (mm)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_Prec, y=ymax-0.1*ymax ,hjust=0, label=paste("R2 =", r_Prec)) + theme_bw()
p_Prec


jpeg("Figures/A.5_MAOC regressions_standardized.jpeg", width=12, height=14, units="in",res=600)
ggarrange(p_sand, p_silt, p_clay, p_toc, p_ph, 
          p_species.added, p_ag.C.inputs, p_duration, p_nfert, 
          p_Temp, p_Prec,
          labels = LETTERS[1:11],
          ncol = 3, nrow = 4)
dev.off()






###################### appendix figure: regression plots for SOC
mods <- unique(dat$mod2)
tsize <- 1.5
hlinesize <- 1.5
hlinealpha <- 0.5
dat$linetype <- rep(2,dim(dat)[1]); dat$linetype[which(dat$signif=="*")] <- 1
psize <- 1.5
palpha <- 0.5
roundto <- 4


##### SOC
rawdat <- dat_SOC
ymax <- max(na.omit(rawdat$logRR))



#  ~ sand
ref_sand <- which(dat$resp=="SOC" & dat$mod=="sand")
m_sand <- round(dat$m[ref_sand], roundto)
b_sand <- round(dat$b[ref_sand], roundto)
r_sand <- as.character(round(dat$r2[ref_sand], roundto))
lty_sand <- dat$linetype[ref_sand]
xmin_sand <- min(na.omit(rawdat$sand))
xmax_sand <- max(na.omit(rawdat$sand))
p_sand <- ggplot(rawdat, aes(x = sand, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_sand], linetype=lty_sand, aes(x = xmin_sand, xend = xmax_sand, y = m_sand*xmin_sand + b_sand , yend = m_sand*xmax_sand + b_sand))  +
  annotate(geom="text", x=xmin_sand, y=ymax ,hjust=0, label=paste("y = ", m_sand, "*x", "+", b_sand)) +  labs(x="Standardized Baseline sand (%)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_sand, y=ymax-0.2*ymax ,hjust=0, label=paste("R2 =", r_sand)) + theme_bw()
p_sand 

#  ~ silt
ref_silt <- which(dat$resp=="SOC" & dat$mod=="silt")
m_silt <- round(dat$m[ref_silt], roundto)
b_silt <- round(dat$b[ref_silt], roundto)
r_silt <- as.character(round(dat$r2[ref_silt], roundto))
lty_silt <- dat$linetype[ref_silt]
xmin_silt <- min(na.omit(rawdat$silt))
xmax_silt <- max(na.omit(rawdat$silt))
p_silt <- ggplot(rawdat, aes(x = silt, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_silt], linetype=lty_silt, aes(x = xmin_silt, xend = xmax_silt, y = m_silt*xmin_silt + b_silt , yend = m_silt*xmax_silt + b_silt))  +
  annotate(geom="text", x=xmin_silt, y=ymax ,hjust=0, label=paste("y = ", m_silt, "*x", "+", b_silt)) +  labs(x="Standardized Baseline silt (%)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_silt, y=ymax-0.2*ymax ,hjust=0, label=paste("R2 =", r_silt)) + theme_bw()
p_silt 

#  ~ clay
ref_clay <- which(dat$resp=="SOC" & dat$mod=="clay")
m_clay <- round(dat$m[ref_clay], roundto)
b_clay <- round(dat$b[ref_clay], roundto)
r_clay <- as.character(round(dat$r2[ref_clay], roundto))
lty_clay <- dat$linetype[ref_clay]
xmin_clay <- min(na.omit(rawdat$clay))
xmax_clay <- max(na.omit(rawdat$clay))
p_clay <- ggplot(rawdat, aes(x = clay, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_clay], linetype=lty_clay, aes(x = xmin_clay, xend = xmax_clay, y = m_clay*xmin_clay + b_clay , yend = m_clay*xmax_clay + b_clay))  +
  annotate(geom="text", x=xmin_clay, y=ymax ,hjust=0, label=paste("y = ", m_clay, "*x", "+", b_clay)) +  labs(x="Standardized Baseline clay (%)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_clay, y=ymax-0.2*ymax ,hjust=0, label=paste("R2 =", r_clay)) + theme_bw()
p_clay 

#  ~ toc
ref_toc <- which(dat$resp=="SOC" & dat$mod=="toc")
m_toc <- round(dat$m[ref_toc], roundto)
b_toc <- round(dat$b[ref_toc], roundto)
r_toc <- as.character(round(dat$r2[ref_toc], roundto))
lty_toc <- dat$linetype[ref_toc]
xmin_toc <- min(na.omit(rawdat$toc))
xmax_toc <- max(na.omit(rawdat$toc))
p_toc <- ggplot(rawdat, aes(x = toc, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_toc], linetype=lty_toc, aes(x = xmin_toc, xend = xmax_toc, y = m_toc*xmin_toc + b_toc , yend = m_toc*xmax_toc + b_toc))  +
  annotate(geom="text", x=xmin_toc, y=ymax ,hjust=0, label=paste("y = ", m_toc, "*x", "+", b_toc)) +  labs(x="Standardized Baseline SOC (%)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_toc, y=ymax-0.2*ymax ,hjust=0, label=paste("R2 =", r_toc)) + theme_bw()
p_toc 


#  ~ ph
ref_ph <- which(dat$resp=="SOC" & dat$mod=="ph")
m_ph <- round(dat$m[ref_ph], roundto)
b_ph <- round(dat$b[ref_ph], roundto)
r_ph <- as.character(round(dat$r2[ref_ph], roundto))
lty_ph <- dat$linetype[ref_ph]
xmin_ph <- min(na.omit(rawdat$ph))
xmax_ph <- max(na.omit(rawdat$ph))
p_ph <- ggplot(rawdat, aes(x = ph, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_ph], linetype=lty_ph, aes(x = xmin_ph, xend = xmax_ph, y = m_ph*xmin_ph + b_ph , yend = m_ph*xmax_ph + b_ph))  +
  annotate(geom="text", x=xmin_ph, y=ymax ,hjust=0, label=paste("y = ", m_ph, "*x", "+", b_ph)) +  labs(x="Standardized Baseline pH", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_ph, y=ymax-0.2*ymax ,hjust=0, label=paste("R2 =", r_ph)) + theme_bw()
p_ph 

#  ~ species.added
ref_species.added <- which(dat$resp=="SOC" & dat$mod=="species.added")
m_species.added <- round(dat$m[ref_species.added], roundto)
b_species.added <- round(dat$b[ref_species.added], roundto)
r_species.added <- as.character(round(dat$r2[ref_species.added], roundto))
lty_species.added <- dat$linetype[ref_species.added]
xmin_species.added <- min(na.omit(rawdat$species.added))
xmax_species.added <- max(na.omit(rawdat$species.added))
p_species.added <- ggplot(rawdat, aes(x = species.added, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_species.added], linetype=lty_species.added, aes(x = xmin_species.added, xend = xmax_species.added, y = m_species.added*xmin_species.added + b_species.added , yend = m_species.added*xmax_species.added + b_species.added))  +
  annotate(geom="text", x=xmin_species.added, y=ymax ,hjust=0, label=paste("y = ", m_species.added, "*x", "+", b_species.added)) +  labs(x="Standardized Number of species added", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_species.added, y=ymax-0.2*ymax ,hjust=0, label=paste("R2 =", r_species.added)) + theme_bw()
p_species.added

#  ~ ag.C.inputs
ref_ag.C.inputs <- which(dat$resp=="SOC" & dat$mod=="ag.C.inputs")
m_ag.C.inputs <- round(dat$m[ref_ag.C.inputs], roundto)
b_ag.C.inputs <- round(dat$b[ref_ag.C.inputs], roundto)
r_ag.C.inputs <- as.character(round(dat$r2[ref_ag.C.inputs], roundto))
lty_ag.C.inputs <- dat$linetype[ref_ag.C.inputs]
xmin_ag.C.inputs <- min(na.omit(rawdat$ag.C.inputs))
xmax_ag.C.inputs <- max(na.omit(rawdat$ag.C.inputs))
p_ag.C.inputs <- ggplot(rawdat, aes(x = ag.C.inputs, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_ag.C.inputs], linetype=lty_ag.C.inputs, aes(x = xmin_ag.C.inputs, xend = xmax_ag.C.inputs, y = m_ag.C.inputs*xmin_ag.C.inputs + b_ag.C.inputs , yend = m_ag.C.inputs*xmax_ag.C.inputs + b_ag.C.inputs))  +
  annotate(geom="text", x=xmin_ag.C.inputs, y=ymax ,hjust=0, label=paste("y = ", m_ag.C.inputs, "*x", "+", b_ag.C.inputs)) +  labs(x="Standardized Aboveground C inputs (Mg ha-1 yr-1)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_ag.C.inputs, y=ymax-0.2*ymax ,hjust=0, label=paste("R2 =", r_ag.C.inputs)) + theme_bw()
p_ag.C.inputs

#  ~ duration
ref_duration <- which(dat$resp=="SOC" & dat$mod=="duration")
m_duration <- round(dat$m[ref_duration], roundto)
b_duration <- round(dat$b[ref_duration], roundto)
r_duration <- as.character(round(dat$r2[ref_duration], roundto))
lty_duration <- dat$linetype[ref_duration]
xmin_duration <- min(na.omit(rawdat$duration))
xmax_duration <- max(na.omit(rawdat$duration))
p_duration <- ggplot(rawdat, aes(x = duration, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_duration], linetype=lty_duration, aes(x = xmin_duration, xend = xmax_duration, y = m_duration*xmin_duration + b_duration , yend = m_duration*xmax_duration + b_duration))  +
  annotate(geom="text", x=xmin_duration, y=ymax ,hjust=0, label=paste("y = ", m_duration, "*x", "+", b_duration)) +  labs(x="Standardized Duration (yrs)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_duration, y=ymax-0.2*ymax ,hjust=0, label=paste("R2 =", r_duration)) + theme_bw()
p_duration

#  ~ nfert
ref_nfert <- which(dat$resp=="SOC" & dat$mod=="nfert")
m_nfert <- round(dat$m[ref_nfert], roundto)
b_nfert <- round(dat$b[ref_nfert], roundto)
r_nfert <- as.character(round(dat$r2[ref_nfert], roundto))
lty_nfert <- dat$linetype[ref_nfert]
xmin_nfert <- min(na.omit(rawdat$nfert))
xmax_nfert <- max(na.omit(rawdat$nfert))
p_nfert <- ggplot(rawdat, aes(x = nfert, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_nfert], linetype=lty_nfert, aes(x = xmin_nfert, xend = xmax_nfert, y = m_nfert*xmin_nfert + b_nfert , yend = m_nfert*xmax_nfert + b_nfert))  +
  annotate(geom="text", x=xmin_nfert, y=ymax ,hjust=0, label=paste("y = ", m_nfert, "*x", "+", b_nfert)) +  labs(x="Standardized N fertilization (kg ha-1 yr-1)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_nfert, y=ymax-0.2*ymax ,hjust=0, label=paste("R2 =", r_nfert)) + theme_bw()
p_nfert

#  ~ Temp
ref_Temp <- which(dat$resp=="SOC" & dat$mod=="Temp")
m_Temp <- round(dat$m[ref_Temp], roundto)
b_Temp <- round(dat$b[ref_Temp], roundto)
r_Temp <- as.character(round(dat$r2[ref_Temp], roundto))
lty_Temp <- dat$linetype[ref_Temp]
xmin_Temp <- min(na.omit(rawdat$Temp))
xmax_Temp <- max(na.omit(rawdat$Temp))
p_Temp <- ggplot(rawdat, aes(x = Temp, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_Temp], linetype=lty_Temp, aes(x = xmin_Temp, xend = xmax_Temp, y = m_Temp*xmin_Temp + b_Temp , yend = m_Temp*xmax_Temp + b_Temp))  +
  annotate(geom="text", x=xmin_Temp, y=ymax ,hjust=0, label=paste("y = ", m_Temp, "*x", "+", b_Temp)) +  labs(x="Standardized Mean annual temperature (°C)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_Temp, y=ymax-0.2*ymax ,hjust=0, label=paste("R2 =", r_Temp)) + theme_bw()
p_Temp

#  ~ Prec
ref_Prec <- which(dat$resp=="SOC" & dat$mod=="Prec")
m_Prec <- round(dat$m[ref_Prec], roundto)
b_Prec <- round(dat$b[ref_Prec], roundto)
r_Prec <- as.character(round(dat$r2[ref_Prec], roundto))
lty_Prec <- dat$linetype[ref_Prec]
xmin_Prec <- min(na.omit(rawdat$Prec))
xmax_Prec <- max(na.omit(rawdat$Prec))
p_Prec <- ggplot(rawdat, aes(x = Prec, y = logRR))+ geom_hline(yintercept=0, size=hlinesize, alpha=hlinealpha) +geom_point(size=psize, alpha=palpha) +
  geom_segment(inherit.aes=F, lwd=1.5, color=dat$color[ref_Prec], linetype=lty_Prec, aes(x = xmin_Prec, xend = xmax_Prec, y = m_Prec*xmin_Prec + b_Prec , yend = m_Prec*xmax_Prec + b_Prec))  +
  annotate(geom="text", x=xmin_Prec, y=ymax ,hjust=0, label=paste("y = ", m_Prec, "*x", "+", b_Prec)) +  labs(x="Standardized annual precipitation (mm)", y="Standardized log reponse ratio") +
  annotate(geom="text", x=xmin_Prec, y=ymax-0.2*ymax ,hjust=0, label=paste("R2 =", r_Prec)) + theme_bw()
p_Prec


jpeg("Figures/A.6_SOC regressions_standardized.jpeg", width=12, height=14, units="in",res=600)
ggarrange(p_sand, p_silt, p_clay, p_toc, p_ph, 
          p_species.added, p_ag.C.inputs, p_duration, p_nfert, 
          p_Temp, p_Prec,
          labels = LETTERS[1:11],
          ncol = 3, nrow = 4)
dev.off()
