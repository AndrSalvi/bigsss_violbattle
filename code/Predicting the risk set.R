
# Stage 1: Predicting the Risk Set
library(dplyr)
library(raster)
library(maptools)
library(cshapes)
library(spatstat)

to_ppp <- function(events, polygon){
  events <- as(events, "SpatialPoints")
  events <- as(events, "ppp")
  events <- unique(events)
  polygon <- as(polygon, "SpatialPolygons") 
  polygon <- as(polygon, "owin")
  ppp <- ppp(events$x, events$y, window = polygon)
  return (ppp)
}

setwd('/Users/markwilliamson/Documents/BIGSSS CSS/')

# load events data
acled_drc <- read.csv("acled_drc.csv")

# convert events data to spatial dataframe
sp_point <- cbind(acled_drc$LONGITUDE, acled_drc$LATITUDE) 
colnames(sp_point) <- c("LONG","LAT") 
proj <- CRS("+proj=longlat +datum=WGS84") 
drc_events <- SpatialPointsDataFrame(coords=sp_point, data=acled_drc, proj4string=proj) 
plot(drc_events, pch=16, cex=.5, axes=T)

# download cshapes file
worldmap <- cshp(as.Date("1997-1-1"), useGW = FALSE)
worldmap@proj4string <- CRS("+proj=longlat +ellps=WGS84 ")
drc_map <- worldmap[worldmap$CNTRY_NAME == "Congo, DRC",]

# load in population
population <- raster("population.asc", proj4string = CRS("+proj=longlat +ellps=WGS84"))
# population <- log(population)

# crop down population data
population_drc <- crop(population, drc_map)

# project data to Eckert IV - why?? 
crs(population_drc) <- "+proj=longlat +datum=WGS84 "
population_drc <- projectRaster(population_drc, crs ="+proj=eck4 +over")
drc_events <- (spTransform(drc_events, CRS("+proj=eck4")))
drc_map <- (spTransform(drc_map, CRS("+proj=eck4")))

# Generate a ppp object using helper function shown above
drc_ppp <- to_ppp(drc_events, drc_map)

# preliminary plot of events
plot(drc_ppp)
plot(density(drc_ppp))

#Turn raster population data into a spatial covariate (im) onject
im.pop <- as.im(as(population_drc,'SpatialGridDataFrame'))

#Fit a bivariate spatial inhomogenous poisson process model
m1 <- ppm(drc_ppp ~ im.pop)
summary(m1)

#Look at the in-sample predictions
plot(predict(m1)) # predictions
plot(drc_events, col="red", pch=2) # actual

#Simulate events
par(mfrow = c(1, 2))
plot(density(drc_ppp), main = "Observed events") # actual 
plot(density(rmh(m1)), main = "Simulated") # simulated location







