


##############################
# load in necessary libraries
##############################
library(tidyverse)
library(gridExtra)
library(rnaturalearth)
library(tmap)
library(sf) 
library(shinyjs)
library(sjmisc)
library(ggplot2)
library(ggmap)




##############################
# read in data
##############################
dat <- read.csv("Raw-data/4_metadata - final_withclimdat_2.csv")

dat$country[which(dat$country=="USA")] <- "United States"


# get unique locations
dat$lat.long <- paste(dat$lat.decimal, dat$long.decimal)

# make dataframe for locations and add info like number of observations
locdat <- data.frame(lat.long = unique(dat$lat.long),
                     lat = rep(NA, length(unique(dat$lat.long))),
                     long = rep(NA, length(unique(dat$lat.long))),
                     nobs = rep(NA, length(unique(dat$lat.long))),
                     nstudy = rep(NA, length(unique(dat$lat.long))),
                     StudyID = rep(NA, length(unique(dat$lat.long))),
                     country = rep(NA, length(unique(dat$lat.long))),
                     Continent = rep(NA, length(unique(dat$lat.long))))
for(i in 1:dim(locdat)[1]){
  locdat$lat[i] <- strsplit(locdat$lat.long[i], " ")[[1]][1]
  locdat$long[i] <- strsplit(locdat$lat.long[i], " ")[[1]][2]
  locdat$nobs[i] <- length(which(dat$long.decimal==locdat$long[i] & dat$lat.decimal==locdat$lat[i]))
  study <- unique(dat$StudyID[which(dat$long.decimal==locdat$long[i] & dat$lat.decimal==locdat$lat[i])])
  locdat$nstudy[i] <- length(study)
  if(length(study==1)){locdat$StudyID[i] <- study[1]} 
  if(length(study==2)){locdat$StudyID[i] <- paste(study[1], study[2], sep=",")} 
  if(length(study==3)){locdat$StudyID[i] <- paste(study[1], study[2], study[3],sep=",")} 
  locdat$country[i] <- unique(dat$country[which(dat$long.decimal==locdat$long[i] & dat$lat.decimal==locdat$lat[i])])
  locdat$Continent[i] <- unique(dat$Continent[which(dat$long.decimal==locdat$long[i] & dat$lat.decimal==locdat$lat[i])])
}

locdat$lat <- as.numeric(locdat$lat)
locdat$long <- as.numeric(locdat$long)



##############################
### map
##############################

# get world map
world = ne_countries(returnclass = "sf") 
countries <- unique(world$name_long)
countries <- which(countries %in% locdat$country)
states <- unique(map_data("state")$region)




# create plot for number of observations
sf_use_s2(FALSE)
my_map <- ggplot() +
  geom_sf(data = world, inherit.aes = F) +
  geom_sf(data = world, fill="white", alpha=0.2, colour = "gray75") +
  geom_point(data = locdat, aes(x = long, y = lat, size=nobs),  shape = 21, fill = "darkturquoise", alpha=0.5) +
  theme(legend.position = c(0.8, 0.2)) +
  labs(x="Longitude", y="Latitude") +
  theme_bw() +
  coord_sf(expand = F) +
  scale_x_continuous(breaks = seq(-180, 180, by=60)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  scale_size_continuous(name = "Observations", range = c(0.3,6))
my_map

# png("Figures/1_WorldMap_ggmap_nobs.png", height=2000, width=4000,  units="px", res=600, bg = "transparent")
# my_map
# dev.off()










# create plot for number of studies
# colors uses: sprirenggreen4, red, springgreen, turquoise1
sf_use_s2(FALSE)
my_map <- ggplot() +
  geom_sf(data = world, inherit.aes = F) +
  geom_sf(data = world, fill="white", alpha=0.2, colour = "gray65", size=0.55) +
  geom_point(data = locdat[which(locdat$nstudy>1),], aes(x = long, y = lat, size=nstudy),  shape = 21, fill = "red", alpha=0.75) +
  geom_point(data = locdat[which(locdat$nstudy<2),], aes(x = long, y = lat, size=nstudy),  shape = 21, fill = "red", alpha=0.75) +
  theme(legend.position = c(0.8, 0.2)) +
  labs(x="Longitude", y="Latitude") +
  theme_bw() +
  theme(axis.text.x=element_text(colour="black")) +
  coord_sf(expand = F) +
  scale_x_continuous(breaks = seq(-180, 180, by=60)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  scale_size_continuous(name = "Studies", breaks = c(1,2,3), range = c(1,3))
my_map

jpeg("Figures/1_WorldMap_ggmap_nstudy.jpeg", height=2000, width=4000,  units="px", res=600, bg = "transparent")
my_map
dev.off()
dev.off()


