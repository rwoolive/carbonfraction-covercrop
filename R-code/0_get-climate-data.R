library(raster)
library(sp)

# read in my data
dat <- read.csv("Raw-data/4_metadata - final.csv")

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






