library(raster)
library(sp)

# read in my data
dat <- read.csv("Raw-data/4_metadata - final.csv")


############################### 
###  remove n/p-based data
datn <- dat[which(dat$rv %in% c("PON", "TN", "MAON") & is.na(dat$logRR)==FALSE & is.na(dat$var)==FALSE),]
unique(datn$StudyID) # nitrogen-based effect sizes: 285 observations from 21 studies
###  matter-based data
datm <- dat[which(dat$rv %in% c("POM", "TOM", "SOM", "MAOM") & is.na(dat$logRR)==FALSE & is.na(dat$var)==FALSE),]
unique(datm$StudyID) # matter-based effect sizes: 90 observations from 9 studies
###  carbon-based data
datc <- dat[which(dat$rv %in% c("POC", "TOC", "MAOC", "SOC") & is.na(dat$logRR)==FALSE & is.na(dat$var)==FALSE),]
unique(datc$StudyID) # carbon-based effect sizes: 958 observations from 50 studies
###  phosphorus-based data
datp <- dat[which(dat$rv %in% c("POP") & is.na(dat$logRR)==FALSE & is.na(dat$var)==FALSE),]
unique(datp$StudyID) # phosphorus-based effect sizes: 4 observations from 1 study
###  remove nitrogen- and phosphorus-based data
dat <- dat[which(dat$rv %in% c("POC", "TOC", "MAOC", "SOC", "POM", "TOM", "SOM", "MAOM")& is.na(dat$logRR)==FALSE & is.na(dat$var)==FALSE),]
length(unique(dat$StudyID)) # carbon- and matter-based effect sizes: 934 observations from 48 studies



# make a dataframe with only lat and long
xy <- dat[,c("long.decimal", "lat.decimal")]
# make spatial points dataframe
spdf <- SpatialPointsDataFrame(coords = xy, data = dat,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))




# load worldclim data
r <- raster::getData("worldclim",var="bio",res=10)

# Bio 1 and Bio12 are mean annual temperature and annual precipitation
r <- r[[c(1,12)]]
names(r) <- c("Temp","Prec")

# extract temperature and precip values at my coordinates
values <- raster::extract(r,spdf)

# add temp and precip values to original dataframe
df <- cbind.data.frame(dat,values)

# fix temp
df$Temp <- df$Temp/10


# export dataframe
write.csv(df, "Raw-data/4_metadata - final_withclimdat.csv")






