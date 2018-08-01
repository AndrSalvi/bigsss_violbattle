library(cshapes)
library(raster)
library(maptools)
library(cshapes)
library(spatstat)
library(rgdal)
library(dplyr)
library(sf)
library(rgeos)

set.seed(666)

# download cshapes file
worldmap <- cshp(as.Date("1997-1-1"), useGW = FALSE)
worldmap@proj4string <- CRS("+proj=longlat +ellps=WGS84 ")
drc_map <- worldmap[worldmap$CNTRY_NAME == "Congo, DRC",]

# load simulated events data
sim_m1 <- read.csv("sim_rast_1.csv")
sim_m1 <- sim_m1[,2:3]
colnames(sim_m1) <- c("LONG","LAT") 
proj <- CRS("+proj=longlat +datum=WGS84") 
sim_events <- SpatialPointsDataFrame(coords=sim_m1, data=sim_m1, proj4string=proj) 

# load observed battle events and VAC
setwd("/Users/markwilliamson/Documents/BIGSSS CSS")
acled_drc <- read.csv("acled_drc.csv")
sp_point <- cbind(acled_drc$LONGITUDE, acled_drc$LATITUDE) 
colnames(sp_point) <- c("LONG","LAT") 
drc_events <- SpatialPointsDataFrame(coords = sp_point, data = acled_drc, 
                                     proj4string = CRS("+proj=longlat +ellps=WGS84 "))
drc_battles <- subset(drc_events, EVENT_TYPE %in% c("Battle-Government regains territory",
                                                    "Battle-No change of territory",
                                                    "Battle-Non-state actor overtakes territory"))
drc_vac <- subset(drc_events, EVENT_TYPE == "Violence against civilians")


plot(drc_map)
points(sim_events, col = alpha("green", 0.4), pch = 20)
points(drc_battles, col = alpha("blue", 0.4), pch = 20)
points(drc_vac, col = alpha("red", 0.4), pch = 17)


# compute distances








