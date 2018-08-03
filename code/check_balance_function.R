# ----- Packages -----
p_needed <- c("ggplot2", "reshape2", "tidyverse", "readr", "ggmap", "dplyr", "raster", "maptools", "spatstat", "sp", "cshapes", "rgdal", "geosphere", "sf", "rgeos", "scales", "mwa", "gridExtra")
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_needed, require, character.only = TRUE)
# --------------------

# ====================================
# Covariate Balance Check
# ====================================

# ---------- Covariate 1: Distance to capital ----------

kinshasa <- as.data.frame(rbind(c(15.307045, -4.322447)))
colnames(kinshasa) <- c("LONG","LAT") 
kinshasa <- SpatialPointsDataFrame(coords = kinshasa, data = kinshasa,
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 "))

# ---------- Covariate 2: Distance to border ----------

border <- as(drc_map, "SpatialLines")

# ---------- Covariate 3: Distance to nearest settlement and number of settlements w/i 5km ----------

#setwd("/Users/Jess/Desktop/BIGSSS Summer School on Conflict 2018/settlements")
settlements <- readOGR("Localite.shp")
settlements@coords <- settlements@coords / 100000
settlements@proj4string <- CRS("+proj=longlat +ellps=WGS84 ")

# ---------- Covariate 4: Terrain ----------

# setwd("/Users/Jess/Desktop/BIGSSS Summer School on Conflict 2018/Elevation raster")
terrain1 <- raster("10S000E_20101117_gmted_mea300.tif")
terrain1@crs <- CRS("+proj=longlat +ellps=WGS84 ")
terrain2 <- raster("10S030E_20101117_gmted_mea300.tif")
terrain2@crs <- CRS("+proj=longlat +ellps=WGS84 ")
terrain3 <- raster("30S000E_20101117_gmted_mea300.tif")
terrain3@crs <- CRS("+proj=longlat +ellps=WGS84 ")
terrain4 <- raster("30S030E_20101117_gmted_mea300.tif")
terrain4@crs <- CRS("+proj=longlat +ellps=WGS84 ")

terrain <- raster::merge(terrain1, terrain2, terrain3, terrain4, extent = drc_map)
terrain_drc <- crop(terrain, drc_map)
plot(terrain_drc)
plot(drc_map, add = T)
head(road_battles@coords
     as.data.frame(road_battles@coords)$LAT

# ---------- Function ----------

check.balance.fun <- function(pointarea, label) {
  
  dist_vac <- distm(pointarea, drc_vac)
  dist_capital <- distm(pointarea, kinshasa)
  dist_border <- as.vector(gDistance(border, pointarea, byid=TRUE))
  dist_settlements <- distm(pointarea, settlements)
  terrain <- extract(terrain_drc, pointarea)
  
  # Pull location data
  longitude <- as.data.frame(pointarea@coords)$LONG
  latitude <- as.data.frame(pointarea@coords)$LAT
  
  # Pull timestamp
  timestamp <- as.data.frame(pointarea$TIMESTAMP)
  
  # VAC loop
  mean_dist_vac <- vector()
  for (i in 1:nrow(pointarea@coords)) {
    distances <- sort(c(dist_vac[i,]))
    nearest_neigh <- distances[1:5]
    mean_dist_vac[i] <- mean(nearest_neigh)
  }
  
  # Settlement loop
  settle_nearest <- vector()
  settle_isolation <- vector()
  for (i in 1:nrow(dist_settlements)) {
    # distance to nearest settlement
    distances <- sort(c(dist_settlements[i,]))
    settle_nearest[i] <- distances[1]
    # number of settlements within 5km of event
    settle_isolation[i] <- length(distances[distances < 5000])
  }
  
  # Combine covariate data
  combine <- data.frame(area_type = label, mean_dist_vac, dist_capital, dist_border, settle_nearest, settle_isolation, terrain, latitude, longitude, timestamp = timestamp)
  
  return(combine)
}

test <- check.balance.fun(sim.road.sp, "test")

road_sim <- check.balance.fun(sim.road.sp, "road_sim_battles")
road_obs <- check.balance.fun(road_battles, "road_obs_battles")
all_sim <- check.balance.fun(sim.drc.sp, "all_sim")
all_obs <- check.balance.fun(drc_battles, "all_obs")

covariate.balance.df <- as_tibble(rbind(road_obs, road_sim, all_obs, all_sim))
covariate.df <- as_tibble(rbind(road_obs, road_sim))

# creating new VAC dataframe
new.drc_vac <- data.frame(area_type = "vac", mean_dist_vac = NA, dist_capital = NA, dist_border = NA, settle_nearest = NA, settle_isolation = NA, terrain = NA, latitude = drc_vac$LATITUDE, longitude = drc_vac$LONGITUDE, timestamp = drc_vac$TIMESTAMP)

# combining the covariate data with VAC data
new.combined <- rbind(covariate.balance.df, new.drc_vac)
new.combined <- rbind(covariate.df, new.drc_vac)
names(covariate.df)[10] <- "timestamp"
names(new.drc_vac)

table(new.combined$area_type)

getwd()
write.csv(new.combined, file = "combined_data_file.csv")

# ---------- ggplots ----------

# Base plot
p <- ggplot() + scale_x_discrete(labels=c("road_obs" = "Observed", "road_sim" = "Simulated", "all_obs" = "Observed (DRC)", "all_sim" = "Simulated (DRC)"))

p.vac <- p + geom_boxplot(data = subset(covariate.balance.df, area_type %in% c("road_obs", "road_sim")), aes(x=area_type, y=log(mean_dist_vac/1000))) + 
  labs(x = "", y = "Average distance (in km, logged)", title = "Distance of battles to five nearest VAC events in road buffer", subtitle = "Comparison between observed vs. simulated battle events")

p.capital <- p + geom_boxplot(data = covariate.balance.df, aes(x=area_type, y=log(dist_capital/1000), fill = area_type, color=area_type)) + 
  labs(x = "", y = "Average distance (in km)", title = "Average distance of battles from Kinshasa (capital)")

p.border <- p + geom_boxplot(data = covariate.balance.df, aes(x=area_type, y=log(dist_border/1000), fill = area_type, color=area_type)) + 
  labs(x = "", y = "Average distance (in km)", title = "Average distance of battles from DRC border")

p.settle.near <- p + geom_boxplot(data = covariate.balance.df, aes(x=area_type, y=log(settle_nearest/1000), fill = area_type, color=area_type)) + 
  labs(x = "", y = "Average distance (in km)", title = "Average distance of battles from nearest settlements")

p.settle.isolation <- p + geom_boxplot(data = covariate.balance.df, aes(x=area_type, y=log(settle_isolation), fill = area_type, color=area_type)) + 
  labs(x = "", y = "Number of settlements", title = "Number of settles within 5km range from battle")

p.terrain <- p + geom_boxplot(data = covariate.balance.df, aes(x=area_type, y=terrain, fill = area_type, color=area_type)) + 
  labs(x = "", y = "Terrain", title = "Terrain")

grid.arrange(p.vac, p.capital, p.border, p.settle.near, p.settle.isolation, p.terrain, ncol = 3)

p.terrain$plot_env
ggplot_build(p.terrain$data)

tapply(log(covariate.balance.df$mean_dist_vac + 0.0001), INDEX = covariate.balance.df$area_type, 
       FUN = function(x) summary(x))
