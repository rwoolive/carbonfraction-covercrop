

library(metafor) # version 3.0-2




dat_POC <- read.csv("Processed-data/POC_no-nitrogen.csv")
dat_MAOC <- read.csv("Processed-data/MAOC_no-nitrogen.csv")
dat_SOC <- read.csv("Processed-data/SOC_no-nitrogen.csv")

#  POC model
dat <- dat_POC
mod <- rma.mv(logRR, var, mods = ~ var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/6_Publication-bias/eggers_POC.txt"); summary(mod); sink(file = NULL)

#  MAOC model
dat <- dat_MAOC
mod <- rma.mv(logRR, var, mods = ~ var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/6_Publication-bias/eggers_MAOC.txt"); summary(mod); sink(file = NULL)

#  SOC model
dat <- dat_SOC
mod <- rma.mv(logRR, var, mods = ~ var, random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat)
sink(file = "Model-output/6_Publication-bias/eggers_SOC.txt"); summary(mod); sink(file = NULL)

# png(height=300, width=300, "Figures/S2_histogram POC.png")
# hist(dat_POC$logRR, xlab=expression(paste("POC"[CC])), main=""); title("A", adj=0, line=0.5); dev.off()
# png(height=300, width=300, "Figures/S2_histogram MAOC.png")
# hist(dat_MAOC$logRR, xlab=expression(paste("MAOC"[CC])), main=""); title("B", adj=0, line=0.5); dev.off()
# png(height=300, width=300, "Figures/S2_histogram SOC.png")
# hist(dat_SOC$logRR, xlab=expression(paste("SOC"[CC])), main=""); title("C", adj=0, line=0.5); dev.off()






### Sensitivity analysis by jacknife procedure, 
### where each study is removed and overall mean recalculated



dat <- dat_POC
r <- length(unique(dat$StudyID))
ids <- unique(dat$StudyID)
effectsdat_POC <- data.frame(study=rep(NA,r),
                             mean=rep(NA,r),
                             lci=rep(NA,r),
                             uci=rep(NA,r),
                             nobs=rep(NA,r))
for(i in 1:r){
  study <- ids[i]
  effectsdat_POC$study[i] <- study
  mod <- rma.mv(logRR, var,random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat[-which(dat$StudyID==study),])
  effectsdat_POC$mean[i] <- mod$b
  effectsdat_POC$lci[i] <- mod$ci.lb
  effectsdat_POC$uci[i] <- mod$ci.ub
  effectsdat_POC$nobs[i] <- mod$k
}
effectsdat_POC$mean_bt <- 100*(exp(effectsdat_POC$mean)-1)
effectsdat_POC$lci_bt <- 100*(exp(effectsdat_POC$lci)-1)
effectsdat_POC$uci_bt <- 100*(exp(effectsdat_POC$uci)-1)

write.csv(effectsdat_POC, "Model-output/6_Publication-bias/sensitivity-jacknife_POC.csv")





dat <- dat_MAOC
r <- length(unique(dat$StudyID))
ids <- unique(dat$StudyID)
effectsdat_MAOC <- data.frame(study=rep(NA,r),
                             mean=rep(NA,r),
                             lci=rep(NA,r),
                             uci=rep(NA,r),
                             nobs=rep(NA,r))
for(i in 1:r){
  study <- ids[i]
  effectsdat_MAOC$study[i] <- study
  mod <- rma.mv(logRR, var,random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat[-which(dat$StudyID==study),])
  effectsdat_MAOC$mean[i] <- mod$b
  effectsdat_MAOC$lci[i] <- mod$ci.lb
  effectsdat_MAOC$uci[i] <- mod$ci.ub
  effectsdat_MAOC$nobs[i] <- mod$k
}
effectsdat_MAOC$mean_bt <- 100*(exp(effectsdat_MAOC$mean)-1)
effectsdat_MAOC$lci_bt <- 100*(exp(effectsdat_MAOC$lci)-1)
effectsdat_MAOC$uci_bt <- 100*(exp(effectsdat_MAOC$uci)-1)

write.csv(effectsdat_MAOC, "Model-output/6_Publication-bias/sensitivity-jacknife_MAOC.csv")






dat <- dat_SOC
r <- length(unique(dat$StudyID))
ids <- unique(dat$StudyID)
effectsdat_SOC <- data.frame(study=rep(NA,r),
                             mean=rep(NA,r),
                             lci=rep(NA,r),
                             uci=rep(NA,r),
                             nobs=rep(NA,r))
for(i in 1:r){
  study <- ids[i]
  effectsdat_SOC$study[i] <- study
  mod <- rma.mv(logRR, var,random = list(~ 1 | Obs.ID, ~ 1 | StudyNum), tdist=TRUE, data=dat[-which(dat$StudyID==study),])
  effectsdat_SOC$mean[i] <- mod$b
  effectsdat_SOC$lci[i] <- mod$ci.lb
  effectsdat_SOC$uci[i] <- mod$ci.ub
  effectsdat_SOC$nobs[i] <- mod$k
}
effectsdat_SOC$mean_bt <- 100*(exp(effectsdat_SOC$mean)-1)
effectsdat_SOC$lci_bt <- 100*(exp(effectsdat_SOC$lci)-1)
effectsdat_SOC$uci_bt <- 100*(exp(effectsdat_SOC$uci)-1)

write.csv(effectsdat_SOC, "Model-output/6_Publication-bias/sensitivity-jacknife_SOC.csv")






mdat <- merge(effectsdat_POC, effectsdat_MAOC, by="study", all=TRUE, sort = TRUE)
mdat2 <- merge(mdat, effectsdat_SOC, by="study", all=TRUE, sort = TRUE)

write.csv(mdat2, "Model-output/6_Publication-bias/sensitivity-jacknife_merged.csv")




