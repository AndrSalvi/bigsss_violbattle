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

# read in roads data
setwd("C:/Users/Andrea/Documents/GitProjects/bigsss_violbattle/data")
roads <- readOGR("cod_trs_roads_osm.shp")
roads@proj4string <- CRS("+proj=longlat +ellps=WGS84 ")

# take only highways, primary and secondary roads, clip and buffer
summary(roads@data$ntlclass)
roads_buff <- subset(roads, ntlclass %in% c("motorway", "primary", "primary_link",
                                            "secondary", "secondary_link")) %>%
  gSimplify(tol = 0.01) %>%
  gIntersection(drc_map, byid = TRUE) %>% 
  spTransform(CRS("+proj=utm +zone=32")) %>% # need this for accurate buffer width
  gBuffer(width = 5000) %>%
  spTransform(CRS("+proj=longlat +ellps=WGS84 ")) %>%
  gIntersection(drc_map, byid = TRUE)

plot(drc_map)
plot(roads_buff, add = TRUE, col = "lightgrey")


# read in DRC events data and subset by type 
setwd("C:/Users/Andrea/Documents/GitProjects/bigsss_violbattle/data")
acled_drc <- read.csv("acled_drc.csv")
sp_point <- cbind(acled_drc$LONGITUDE, acled_drc$LATITUDE) 
colnames(sp_point) <- c("LONG","LAT") 
drc_events <- SpatialPointsDataFrame(coords = sp_point, data = acled_drc, 
                                     proj4string = CRS("+proj=longlat +ellps=WGS84 "))
drc_battles <- subset(drc_events, EVENT_TYPE %in% c("Battle-Government regains territory",
                                                    "Battle-No change of territory",
                                                    "Battle-Non-state actor overtakes territory"))
drc_vac <- subset(drc_events, EVENT_TYPE == "Violence against civilians")

# find intersection with buffer area
road_battles <- gIntersection(roads_buff, drc_battles, byid = TRUE)
road_vac <- gIntersection(roads_buff, drc_vac, byid = TRUE)

# percentage of events falling within road buffer
# battles: 
nrow(road_battles@coords) / nrow(drc_battles@coords) # 69% of battles w/i 5km roads


# VAC: 
nrow(road_vac@coords) / nrow(drc_vac@coords) # 68% of VAC events fall w/i 5km of roads

# percentage of country territory covered by 5km road buffer
area(roads_buff) / area(drc_map) # 13.7%


# graphic
library(scales)
plot(drc_map)
plot(roads_buff, add = TRUE, col = "lightgrey")
points(drc_battles, col = alpha("grey", 0.4), pch = 2)
points(drc_vac, col = alpha("grey", 0.4), pch = 1)
points(road_battles, col = alpha("blue", 0.5), pch = 17)
points(road_vac, col = alpha("red", 0.5), pch = 19)
legend("bottomleft", legend=c("Violence against civilians (in buffer)", "Battle event (in buffer)", 
                              "Violence against civilians (outside buffer)",
                              "Battle event (outside buffer)"),
       col=c("red", "blue", "grey", "grey"), pch = c(19, 17, 1, 2),
       cex = 0.9, bty = "n")

# simulated points

to_ppp <- function(events,polygon){
  events  <- as(events,"SpatialPoints")
  events  <- as(events,"ppp")
  events  <- unique(events)
  polygon <- as(polygon,"SpatialPolygons")
  polygon <- as(polygon,"owin")
  ppp  <- ppp(events$x, events$y,window=polygon)
  return(ppp)
}


drc_ppp <- to_ppp(road_battles, roads_buff)

sim_p1 <- rpoispp(intensity(drc_ppp), win=Window(drc_ppp))
sim_p1 <- rpoispp(ex=drc_ppp)
sim_p1

sim_p2 <- rpoispp(intensity(drc_ppp), win=Window(drc_ppp))
sim_p2 <- rpoispp(ex=drc_ppp)
sim_p2

sim_p3 <- rpoispp(intensity(drc_ppp), win=Window(drc_ppp))
sim_p3 <- rpoispp(ex=drc_ppp)
sim_p3

plot(sim_p1)
plot(sim_p2)
plot(sim_p3)


# Back to points


sim_points1 <- as.data.frame(sim_p1)
sim_points2 <- as.data.frame(sim_p2)
sim_points3 <- as.data.frame(sim_p3)

sim_points1 <- sim_points1 %>% rename(LONG = x , LAT = y)
sim_points2 <- sim_points2 %>% rename(LONG = x , LAT = y)
sim_points3 <- sim_points3 %>% rename(LONG = x , LAT = y)

simul1 <- SpatialPointsDataFrame(coords = sim_points1, data = sim_points1, 
                                     proj4string = CRS("+proj=longlat +ellps=WGS84 "))
simul2 <- SpatialPointsDataFrame(coords = sim_points2, data = sim_points2, 
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 "))
simul3 <- SpatialPointsDataFrame(coords = sim_points3, data = sim_points3, 
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 "))


# graphics with simulated

library(scales)
plot(drc_map)
plot(roads_buff, add = TRUE, col = "lightgrey")
points(drc_battles, col = alpha("grey", 0.4), pch = 2)
points(drc_vac, col = alpha("grey", 0.4), pch = 1)
points(road_battles, col = alpha("blue", 0.5), pch = 17)
points(road_vac, col = alpha("red", 0.5), pch = 19)
points(simul1, col = alpha("yellow", 0.5), pch = 19)
legend("bottomleft", legend=c("Violence against civilians (in buffer)", "Battle event (in buffer)", 
                              "Violence against civilians (outside buffer)",
                              "Battle event (outside buffer)",
                              "Simulated Points"),
       col=c("red", "blue", "grey", "grey"), pch = c(19, 17, 1, 2),
       cex = 0.9, bty = "n")


