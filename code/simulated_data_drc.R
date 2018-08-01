getwd()

# ppp
to_ppp <- function(events,polygon){
  events  <- as(events,"SpatialPoints")
  events  <- as(events,"ppp")
  events  <- unique(events)
  polygon <- as(polygon,"SpatialPolygons")
  polygon <- as(polygon,"owin")
  ppp  <- ppp(events$x, events$y,window=polygon)
  return(ppp)
}

# libraries and stuff

library(raster)
library(maptools)
library(spatstat)
library(tidyverse)

# Set working directory
#setwd('')
getwd()

#Load data
cshapes_drc <- readShapeSpatial("gadm36_COD_1.shp",  proj4string=CRS("+proj=longlat +ellps=WGS84"))


# load events data
acled_drc <- read.csv("acled_drc.csv")
as.factor(levels(acled_drc$EVENT_TYPE))
acled_drc_bat <- acled_drc %>% filter(EVENT_TYPE %in% c("Battle-Government regains territory", "Battle-No change of territory", "Battle-Non-state actor overtakes territory"))

# convert events data to spatial dataframe
sp_point <- cbind(acled_drc_bat$LONGITUDE, acled_drc_bat$LATITUDE) 
colnames(sp_point) <- c("LONG","LAT") 
proj <- CRS("+proj=longlat +datum=WGS84") 
drc_events <- SpatialPointsDataFrame(coords=sp_point, data=acled_drc_bat, proj4string=proj) 
plot(drc_events, pch=16, cex=.5, axes=T)


# project data to Eckert IV - why?? 
drc_events <- (spTransform(drc_events, CRS("+proj=eck4")))
drc_map <- (spTransform(cshapes_drc, CRS("+proj=eck4")))

# Generate a ppp object using helper function shown above
drc_ppp <- to_ppp(drc_events, drc_map)

# new ppp, in roads polygon
road_ppp <- to_ppp(drc_events, roads_buff)

# preliminary plot of events

plot(drc_ppp)
plot(density(drc_ppp))
plot(road_ppp, col = "red", add = T)

# randomising the existing point pattern:
a <- rpoispp(intensity(road_ppp), win=Window(road_ppp), ex = road_ppp)

# creating loop to get closer number of simulated events
sim.road.df <- data.frame()
for (i in 1:3) {
  sims.road <- rpoispp(intensity(road_ppp), win=Window(road_ppp), ex = road_ppp)
  sims.road <- data.frame(sims.road) 
  sim.road.df <- rbind(sim.road.df, sims.road)
}

# again for simulation in whole country

sim.drc.df <- data.frame()
for (i in 1:3) {
  sims.drc <- rpoispp(intensity(drc_ppp), win=Window(drc_ppp), ex = drc_ppp)
  sims.drc <- data.frame(sims.drc) 
  sim.drc.df <- rbind(sim.drc.df, sims.drc)
}

names(sim.road.df) <- c("LONG", "LAT")
names(sim.drc.df) <- c("LONG", "LAT")

#Simulated events
par(mfrow = c(1, 2))
plot(density(road_ppp), main = "Observed events") # actual 
plot(density(sim.road.sp), main = "Simulated") # simulated location; [not working atm]


write.csv(sim_points, file = "sim_points.csv")

