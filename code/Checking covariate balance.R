library(cshapes)
library(raster)
library(maptools)
library(cshapes)
library(spatstat)
library(rgdal)
library(dplyr)
library(rgeos)
library(geosphere)

# download cshapes file
worldmap <- cshp(as.Date("1997-1-1"), useGW = FALSE)
worldmap@proj4string <- CRS("+proj=longlat +ellps=WGS84 ")
drc_map <- worldmap[worldmap$CNTRY_NAME == "Congo, DRC",]

# read in DRC events data and subset by type 
acled_drc <- read.csv("acled_drc.csv")
sp_point <- cbind(acled_drc$LONGITUDE, acled_drc$LATITUDE) 
colnames(sp_point) <- c("LONG","LAT") 
drc_events <- SpatialPointsDataFrame(coords = sp_point, data = acled_drc, 
                                     proj4string = CRS("+proj=longlat +ellps=WGS84 "))
drc_battles <- subset(drc_events, EVENT_TYPE %in% c("Battle-Government regains territory",
                                                    "Battle-No change of territory",
                                                    "Battle-Non-state actor overtakes territory"))


# load simulated events data
sim_m1 <- read.csv("sim_points1.csv")
sim_m1 <- sim_m1[,2:3]
colnames(sim_m1) <- c("LONG","LAT") 
proj <- CRS("+proj=longlat +ellps=WGS84 ")
sim_events <- SpatialPointsDataFrame(coords=sim_m1, data=sim_m1, proj4string=proj) 


# Covariate 1: Distance to capital 
kinshasa <- as.data.frame(rbind(c(15.307045, -4.322447)))
colnames(kinshasa) <- c("LONG","LAT") 
kinshasa <- SpatialPointsDataFrame(coords = kinshasa, data = kinshasa,
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 "))

obs_cap_dist <- distm(road_battles, kinshasa)
sim_cap_dist <- distm(sim_events, kinshasa)


# Covariate 2: Distance to border
border <- as(drc_map, "SpatialLines")
obs_bord_dist = gDistance(border, road_battles, byid=TRUE)
sim_bord_dist = gDistance(border, sim_events, byid=TRUE)

# Covariate 3: Distance to nearest settlement and number of settlements w/i 5km
setwd("/Users/markwilliamson/Documents/BIGSSS CSS/settlements")
settlements <- readOGR("Localite.shp")
settlements@coords <- settlements@coords / 100000
settlements@proj4string <- CRS("+proj=longlat +ellps=WGS84 ")

obs_settle_dist <- distm(road_battles, settlements)

obs_settle_nearest <- vector()
obs_isolation <- vector()

for (i in 1:nrow(obs_settle_dist)) {
  
  # distance to nearest settlement
  distances <- sort(c(obs_settle_dist[i,]))
  obs_settle_nearest[i] <- distances[1]
  
  # number of settlements within 5km of event
  obs_isolation[i] <- length(distances[distances < 5000])
  
}

sim_settle_dist <- distm(sim_events, settlements)

sim_settle_nearest <- vector()
sim_isolation <- vector()

for (i in 1:nrow(sim_settle_dist)) {
  
  # distance to nearest settlement
  distances <- sort(c(sim_settle_dist[i,]))
  sim_settle_nearest[i] <- distances[1]

  # number of settlements within 5km of event
  sim_isolation[i] <- length(distances[distances < 5000])
  
}


# Covariate 4: Terrain
# Source: https://topotools.cr.usgs.gov/GMTED_viewer/viewer.htm
#     Mean 30 arc sec
setwd("/Users/markwilliamson/Documents/BIGSSS CSS/Elevation raster")
terrain1 <- raster("10S000E_20101117_gmted_mea300.tif")
terrain1@crs <- CRS("+proj=longlat +ellps=WGS84 ")
terrain2 <- raster("10S030E_20101117_gmted_mea300.tif")
terrain2@crs <- CRS("+proj=longlat +ellps=WGS84 ")

plot(drc_map)
plot(terrain1, add = TRUE)
plot(terrain2, add = TRUE)
plot(drc_map, add = TRUE)











