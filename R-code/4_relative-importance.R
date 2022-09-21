
library(metafor)
library(glmulti)

dat_POC <- read.csv("Processed-data/POC_no-nitrogen.csv")
dat_MAOC <- read.csv("Processed-data/MAOC_no-nitrogen.csv")
dat_SOC <- read.csv("Processed-data/SOC_no-nitrogen.csv")

## for each dataset, 
# response variable: logRR
# variance: var
# predictor variables: depth.type + ccseason + tillage + cover.crop.category + maincrop + ag.C.inputs + Temp


dat_POC1 <- dat_POC[!apply(dat_POC[,c("logRR", "var", "depth.type" , "ccseason" , "tillage" , "cover.crop.category" , "maincrop" , "ag.C.inputs" ,  "Temp")], 1, anyNA),]
dat_MAOC1 <- dat_MAOC[!apply(dat_MAOC[,c("logRR", "var", "ccseason" , "cover.crop.category" , "maincrop" , "ag.C.inputs" )], 1, anyNA),]
dat_SOC1 <- dat_SOC[!apply(dat_SOC[,c("logRR", "var", "depth.type" , "ccseason" , "tillage" , "cover.crop.category" , "maincrop" , "ag.C.inputs" )], 1, anyNA),]



### POC

# model selection
rma.glmulti <- function(formula, data, ...)
  rma(formula, var, data=data, method="ML", ...)

res_POC <- glmulti(logRR ~ depth.type + ccseason + tillage + cover.crop.category + maincrop + ag.C.inputs + Temp, data=dat_POC1,
               level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=128)


# plot variable (i.e. terms) importances
ww = exp(-(res_POC@crits - res_POC@crits[1])/2)
ww=ww/sum(ww)
# handle synonymies for interactions
# this translates to unique notations (e.g. x:y and y:x are the same)
clartou=function(res_POC) {
  sort(strsplit(res_POC, ":")[[1]])-> pieces
  if (length(pieces)>1) paste(pieces[1],":",pieces[2],sep="")
  else res_POC
}
# list terms in models
tet = lapply(res_POC@formulas, function(res_POC) sapply(attr(delete.response(terms(res_POC)),"term.labels"), clartou))
# all unique terms
unique(unlist(tet))-> allt_POC
# importances
sapply(allt_POC, function(res_POC) sum(ww[sapply(tet, function(t) res_POC%in%t)]))-> imp_POC
allt_POC2 <- c("Soil depth", "Cover crop season", "Aboveground C inputs", 
           "Tillage", "Mean annual temperature", "Cover crop type", "Cropping system")




### MAOC

# model selection
rma.glmulti <- function(formula, data, ...)
  rma(formula, var, data=data, method="ML", ...)

res_MAOC <- glmulti(logRR ~ ccseason + cover.crop.category + maincrop + ag.C.inputs, data=dat_MAOC1,
                   level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=128)


# plot variable (i.e. terms) importances
ww = exp(-(res_MAOC@crits - res_MAOC@crits[1])/2)
ww=ww/sum(ww)
# handle synonymies for interactions
# this translates to unique notations (e.g. x:y and y:x are the same)
clartou=function(res_MAOC) {
  sort(strsplit(res_MAOC, ":")[[1]])-> pieces
  if (length(pieces)>1) paste(pieces[1],":",pieces[2],sep="")
  else res_MAOC
}
# list terms in models
tet = lapply(res_MAOC@formulas, function(res_MAOC) sapply(attr(delete.response(terms(res_MAOC)),"term.labels"), clartou))
# all unique terms
unique(unlist(tet))-> allt_MAOC
# importances
sapply(allt_MAOC, function(res_MAOC) sum(ww[sapply(tet, function(t) res_MAOC%in%t)]))-> imp_MAOC
allt_MAOC2 <- c("Cover crop season", "Cropping system", "Aboveground C inputs", 
                "Cover crop type")



### SOC

# model selection
rma.glmulti <- function(formula, data, ...)
  rma(formula, var, data=data, method="ML", ...)

res_SOC <- glmulti(logRR ~ depth.type + ccseason + tillage + cover.crop.category + maincrop + ag.C.inputs, data=dat_SOC1,
                   level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=128)


# plot variable (i.e. terms) importances
ww = exp(-(res_SOC@crits - res_SOC@crits[1])/2)
ww=ww/sum(ww)
# handle synonymies for interactions
# this translates to unique notations (e.g. x:y and y:x are the same)
clartou=function(res_SOC) {
  sort(strsplit(res_SOC, ":")[[1]])-> pieces
  if (length(pieces)>1) paste(pieces[1],":",pieces[2],sep="")
  else res_SOC
}
# list terms in models
tet = lapply(res_SOC@formulas, function(res_SOC) sapply(attr(delete.response(terms(res_SOC)),"term.labels"), clartou))
# all unique terms
unique(unlist(tet))-> allt_SOC
# importances
sapply(allt_SOC, function(res_SOC) sum(ww[sapply(tet, function(t) res_SOC%in%t)]))-> imp_SOC
allt_SOC2 <- c("Soil depth", "Cover crop season", "Aboveground C inputs", 
               "Tillage","Cover crop type", "Cropping system")





# Plot

jpeg("Figures/4_Variable-importance.jpeg", width = 2000, height = 1200, res=300)

par(oma=c(0,12,0,1), mfrow=c(1,3), mar=c(3,1,3,1))

barplot(sort(imp_POC),
        xlab="",xlim=c(0,1), 
        ylab="",horiz=T,las=2, 
        names.arg=allt_POC2[order(imp_POC)],
        main="A) POC", font.main=1, las=1, mgp=c(1.75,0.5,0), tck=-0.05, xaxp=c(0,1,5),
        col="olivedrab4")
box(lty = 'solid', col = 'black')
abline(v=0.8, col="black", lty=2, cex=2)

barplot(rev(c(NA, imp_MAOC[3], imp_MAOC[1], NA, NA, imp_MAOC[4], imp_MAOC[2])),
        xlab="Relative importance",xlim=c(0,1), 
        ylab="",horiz=T,las=2, 
        names.arg=NA,
        main="B) MAOC", font.main=1, las=1, mgp=c(1.75,0.5,0), tck=-0.05, xaxp=c(0,1,5),
        col="orange4")
box(lty = 'solid', col = 'black')
abline(v=0.8, col="black", lty=2, cex=2)

barplot(rev(c(imp_SOC[1], imp_SOC[3], imp_SOC[2], imp_SOC[4], NA, imp_SOC[5], imp_SOC[6])),
        xlab="",xlim=c(0,1), 
        ylab="",horiz=T,las=2, 
        names.arg=NA,
        main="C) SOC", font.main=1, las=1, mgp=c(1.75,0.5,0), tck=-0.05, xaxp=c(0,1,5),
        col=scales::alpha("black", 0.75))
box(lty = 'solid', col = 'black')
abline(v=0.8, col="black", lty=2, cex=2)

dev.off()

# compare to fig to double check
imp_POC
imp_MAOC
imp_SOC

# The importance value for a particular predictor is equal to the 
# sum of the weights/probabilities for the models in which the variable 
# appears. So, a variable that shows up in lots of models with large 
# weights will receive a high importance value. In that sense, these 
# values can be regarded as the overall support for each variable across 
# all models in the candidate set. The vertical red line is drawn at 0.8, 
# which is sometimes used as a cutoff to differentiate between important 
# and not so important variables, but this is again a more or less 
# arbitrary division (and a cutoff of 0.5 has also been at times used/suggested).
