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
setwd("/Users/markwilliamson/Documents/BIGSSS CSS/Exploratory mapping/DRC road network")
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

