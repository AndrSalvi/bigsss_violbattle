# ====================================
#
# BIGSSS CSS Conflict Project Code
#
# Includes:
# - Importing/setting up data
# - Creating road buffer zone
# - Simulating events
# - Calculating and visualizing descriptive stats
#
# ====================================

# ----- Packages -----
p_needed <- c("ggplot2", "reshape2", "tidyverse", "readr", "ggmap", "dplyr", "raster", "maptools", "spatstat", "sp", "cshapes", "rgdal", "geosphere", "sf", "rgeos", "scales", "mwa")
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_needed, require, character.only = TRUE)
# --------------------

#dev.off()

# ========================================
# Setting Up Observed and Simulated Data
# ========================================

setwd("/Users/Jess/Desktop/BIGSSS Summer School on Conflict 2018/BIGSSS Conflict Project Git/data")
# setwd("/Users/markwilliamson/Documents/BIGSSS CSS/Exploratory mapping/DRC road network")
#setwd("C:/Users/Andrea/Documents/GitProjects/bigsss_violbattle/data")

# ---------- DRC shape area ----------

worldmap <- cshp(as.Date("1997-1-1"), useGW = FALSE)
worldmap@proj4string <- CRS("+proj=longlat +ellps=WGS84 ")
drc_map <- worldmap[worldmap$CNTRY_NAME == "Congo, DRC",]

# Check map
plot(drc_map)

# ---------- DRC roads buffer ----------

roads <- readOGR("/Users/Jess/Desktop/BIGSSS Summer School on Conflict 2018/Conflict Project/drc_roads/cod_trs_roads_osm.shp") # use your custom directory
#roads <- readOGR("C:/Users/Andrea/Documents/GitProjects/bigsss_violbattle/data/cod_trs_roads_osm.shp") # use your custom directory
roads@proj4string <- CRS("+proj=longlat +ellps=WGS84 ")

# take only highways, primary and secondary roads, clip and buffer
roads_buff <- subset(roads, ntlclass %in% c("motorway", "primary", "primary_link",
                                            "secondary", "secondary_link")) %>%
  gSimplify(tol = 0.01) %>%
  gIntersection(drc_map, byid = TRUE) %>% 
  spTransform(CRS("+proj=utm +zone=32")) %>% # need this for accurate buffer width
  gBuffer(width = 5000) %>%
  spTransform(CRS("+proj=longlat +ellps=WGS84 ")) %>%
  gIntersection(drc_map, byid = TRUE)

# Visualize
plot(drc_map)
plot(roads_buff, add = TRUE, col = "lightgrey")


# ---------- Load observed events ----------

acled_drc <- read.csv("acled_drc.csv")
sp_point <- cbind(acled_drc$LONGITUDE, acled_drc$LATITUDE) 
colnames(sp_point) <- c("LONG","LAT") 

drc_events <- SpatialPointsDataFrame(coords = sp_point, data = acled_drc, 
                                     proj4string = CRS("+proj=longlat +ellps=WGS84 "))

drc_battles <- subset(drc_events, EVENT_TYPE %in% c("Battle-Government regains territory",
                                                    "Battle-No change of territory",
                                                    "Battle-Non-state actor overtakes territory"))

drc_vac <- subset(drc_events, EVENT_TYPE == "Violence against civilians")

# Visualize observed events
plot(drc_map)
points(drc_battles, col = alpha("blue", 0.4), pch = 20) # Battles
points(drc_vac, col = alpha("red", 0.4), pch = 17) # VAC

# ---------- Take events within buffer ---------- #

road_battles <- raster::intersect(drc_battles, roads_buff)
road_vac <- raster::intersect(drc_vac, roads_buff)

# Percentage of events falling within road buffer
nrow(road_battles@coords) / nrow(drc_battles@coords) # 69% of BATTLES w/i 5km roads
nrow(road_vac@coords) / nrow(drc_vac@coords) # 68% of VAC events fall w/i 5km of roads

# Percentage of country territory covered by 5km road buffer
area(roads_buff) / area(drc_map) # 13.8%

# Visualize
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

# ---------- Simulate events ----------

# PPP function
to_ppp <- function(events,polygon){
  events  <- as(events,"SpatialPoints")
  events  <- as(events,"ppp")
  events  <- unique(events)
  polygon <- as(polygon,"SpatialPolygons")
  polygon <- as(polygon,"owin")
  ppp  <- ppp(events$x, events$y,window=polygon)
  return(ppp)
}

# Generate PPP from data
drc_ppp <- to_ppp(drc_battles, drc_map) # all drc
road_ppp <- to_ppp(drc_events, roads_buff) # near roads

# Looping a randomized version of point pattern (Road Buffer)
sim.road.df <- data.frame()
for (i in 1:5) {
  sims.road <- rpoispp(intensity(road_ppp), win=Window(road_ppp), ex = road_ppp)
  sims.road <- data.frame(sims.road) 
  sim.road.df <- rbind(sim.road.df, sims.road)
}

sim.road.df$date <- sample(seq(as.Date('1997/01/01'), as.Date('2018/07/20'), by="day"), nrow(sim.road.df))
sim.road.df <- sim.road.df %>% arrange(date)

# Comparing obs to sim
nrow(road_battles@coords)
nrow(sim.road.df)

# Looping a randomized version of point pattern (All DRC)
sim.drc.df <- data.frame()
for (i in 1:5) {
  sims.drc <- rpoispp(intensity(drc_ppp), win=Window(drc_ppp), ex = drc_ppp)
  sims.drc <- data.frame(sims.drc) 
  sim.drc.df <- rbind(sim.drc.df, sims.drc)
}

sim.drc.df$date <- sample(seq(as.Date('1997/01/01'), as.Date('2018/07/20'), by="day"), nrow(sim.drc.df))
sim.drc.df <- sim.drc.df %>% arrange(date)

# Comparing obs to sim
nrow(drc_battles@coords)
nrow(sim.drc.df)

# Naming columns
names(sim.road.df) <- c("LONG", "LAT", "TIMESTAMP")
names(sim.drc.df) <- c("LONG", "LAT", "TIMESTAMP")

# Transforming to sp
proj <- CRS("+proj=longlat +datum=WGS84") 

#sim.road.sp.coo <- cbind(sim.road.df$LONG, sim.road.df$LAT, sim.road.df$TIMESTAMP) 
sim.road.sp <- SpatialPointsDataFrame(coords=sim.road.df[,1:2], data=sim.road.df, proj4string=proj) 

sim.drc.sp <- SpatialPointsDataFrame(coords=sim.drc.df, data=sim.drc.df, proj4string=proj) 
sim.road.sp@data$TIMESTAMP <- sim.road.df$TIMESTAMP
plot(sim.drc.sp)

# Visualize observed AND simulated battles
plot(drc_map)
points(sim.drc.sp, col = alpha("green", 0.4), pch = 20) # Sim DRC battles
points(sim.road.sp, col = alpha("blue", 0.4), pch = 20) # Sim road Battles
points(drc_battles, col = alpha("black", 0.4), pch = 20) # Obs DRC battles
points(road_battles, col = alpha("red", 0.4), pch = 20) # Obs road battles

# ===========================================================================
# Descriptive: Computing Avg Distances (between observed/sim - battles/VAC)
# ===========================================================================

# ----- Coverage sensitivity check ------- # 

# iteratively change buffer width
widths <- seq(1000, 10000, by = 1000)
battle_capture <- vector()
area_capture <- vector()

for (i in 1:length(widths)) {
  
  # create buffer based on iterative width spec
  roads_buff <- roads_sub %>% 
    gBuffer(width = widths[i]) %>%
    spTransform(CRS("+proj=longlat +ellps=WGS84 ")) %>%
    gIntersection(drc_map, byid = TRUE)
  
  # find intersection of events with buffer area
  road_battles <- gIntersection(roads_buff, drc_battles, byid = TRUE)

  # percentage of battles falling within road buffer:
  battle_capture[i] <- nrow(road_battles@coords) / nrow(drc_battles@coords) 
  # percentage of country territory covered by buffer
  area_capture[i] <- area(roads_buff) / area(drc_map) 
  
}

buffer_percentages <- data.frame(battle_capture, area_capture, width = 1:10)

# or:
# buffer_percentages <- read.csv("buffer_width_sensitivity.csv")

ggplot() + 
  geom_line(data = buffer_percentages, aes(width, battle_capture*100), col = "darkgrey") + 
  geom_line(data = buffer_percentages, aes(width, area_capture*100), col = "black") + 
  geom_text(aes(c(5.5, 5.5), c(80,25), 
                label = c("Percentage of battles \n in road buffer", 
                          "Percentage of country \n area in road buffer"))) + 
  labs(x = "Width of buffer (km)", y = "%", 
       title = "Coverage sensitivity of road buffer") + 
  coord_cartesian(ylim = c(0, 100)) + 
  scale_x_continuous(breaks = 1:10)


# ---------- Observed events ----------

# Calculating distance
d.road <- distm(road_battles, drc_vac)
d.drc  <- distm(drc_battles, drc_vac)

# Checking dimensions
dim(d.road)
dim(d.drc)

# Empty vectors to hold avg. distances
mean_dist_obs_road <- vector()
mean_dist_obs_drc <- vector()

# Observed Battles to VAC (5 nearest in roads buffer)
for (i in 1:nrow(d.road)) {
  distances <- sort(c(d.road[i,]))
  nearest_neigh <- distances[1:5]
  mean_dist_obs_road[i] <- mean(nearest_neigh)
}

# Observed Battles to VAC (5 nearest all DRC)
for (i in 1:nrow(d.drc)) {
  distances <- sort(c(d.drc[i,]))
  nearest_neigh <- distances[1:5]
  mean_dist_obs_drc[i] <- mean(nearest_neigh)
}

summary(mean_dist_obs_road)
summary(mean_dist_obs_drc)

# ---------- Simulated events ----------

# Calculating distance
d.sim.road <- distm(sim.road.sp, drc_vac)
d.sim.drc <- distm(sim.drc.sp, drc_vac)

# Checking dimensions
dim(d.sim.road)
dim(d.sim.drc)

# Empty vectors to hold avg. distances
mean_dist_sim_road <- vector()
mean_dist_sim_drc <- vector()

# Simulated Battles to VAC (5 nearest in roads buffer)
for (i in 1:nrow(d.sim.road)) {
  distances <- sort(c(d.sim.road[i,]))
  nearest_neigh <- distances[1:5]
  mean_dist_sim_road[i] <- mean(nearest_neigh)
}

# Simulated Battles to VAC (5 nearest all DRC)
for (i in 1:nrow(d.sim.drc)) {
  distances <- sort(c(d.sim.drc[i,]))
  nearest_neigh <- distances[1:5]
  mean_dist_sim_drc[i] <- mean(nearest_neigh)
}

par(mfrow=c(1,2))
summary(mean_dist_sim_drc[,1])
summary(mean_dist_sim_road[,1])

# ---------- T-tests / Creating Dataframe ----------

# T-test between road buffer and all DRC (obs vs. simulated)
t.test(as.vector(mean_dist_obs_road), as.vector(mean_dist_sim_road))
t.test(as.vector(mean_dist_obs_drc), as.vector(mean_dist_sim_drc))

# Create dataframes, add columns
mean_dist_obs_road <- as.data.frame(mean_dist_obs_road)
mean_dist_sim_road <- as.data.frame(mean_dist_sim_road)
mean_dist_obs_drc <- as.data.frame(mean_dist_obs_drc)
mean_dist_sim_drc <- as.data.frame(mean_dist_sim_drc)

mean_dist_obs_road$cat <- "Observed battles (road)"
mean_dist_sim_road$cat <- "Simulated battles (road)"
mean_dist_obs_drc$cat <- "Observed battles (DRC)"
mean_dist_sim_drc$cat <- "Simulated battles (DRC)"

mean_dist_obs_road$area <- "Road Buffer"
mean_dist_sim_road$area <- "Road Buffer"
mean_dist_obs_drc$area <- "DRC"
mean_dist_sim_drc$area <- "DRC"

colnames(mean_dist_obs_road) <- c("avg_dist", "cat", "area")
colnames(mean_dist_sim_road) <- c("avg_dist", "cat", "area")
colnames(mean_dist_obs_drc) <- c("avg_dist", "cat", "area")
colnames(mean_dist_sim_drc) <- c("avg_dist", "cat", "area")

# Combine into one (for ggplot)
combined.df <-rbind(mean_dist_obs_road,mean_dist_sim_road,mean_dist_obs_drc,mean_dist_sim_drc)
head(combined.df)

# ---------- Boxplot ----------

# Theme settings
main.theme <- theme_bw() + 
  theme(legend.key = element_rect(fill = NA, color = NA), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Roboto", size = 14))
theme_set(main.theme)

# Changing order of cateogires
combined.df$cat <- ordered(combined.df$cat, levels = c("Observed battles (road)", "Simulated battles (road)","Observed battles (DRC)","Simulated battles (DRC)"))

# ggplot: distance of battles to five nearest VAC events, comparison between observed vs. simulated battles
ggplot() + 
  geom_boxplot(data = combined.df, aes(x=cat, avg_dist/1000)) + 
  labs(x = "", y = "Avg. Distance (in km, logged)", title = "Distance of battles to five nearest VAC events", subtitle = "Comparison between observed vs. simulated battles") +
  geom_text(aes(1.5,200, label = "T-test, p < 0.001")) + 
  geom_text(aes(3.5,200, label = "T-test, p < 0.001"))

# --- still working on plot styles ---
ggplot() + 
  geom_boxplot(data = combined.df, aes(x=cat, avg_dist/1000, fill = area, color = area)) + 
  labs(x = "", y = "Avg. Distance (in km, logged)", title = "Distance of battles to five nearest VAC events", subtitle = "Comparison between observed vs. simulated battles") +
  geom_text(aes(1.5,200, label = "T-test, p < 0.001")) + 
  geom_text(aes(3.5,200, label = "T-test, p < 0.001")) + stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){c(y=median(x), ymin=median(x), ymax=median(x))}) + guides(color=FALSE, fill = FALSE)


#----- DO NOT RUN

# MWA

#Load population data
population <- raster("gpw_drcv4.tif",proj4string = CRS("+proj=longlat +ellps=WGS84"))

#Build subsets of events and the country polygon
population_drc <-  crop(population, drc_map)

#Combine all entries


con <- as.data.frame(sim.road.sp)
con
treat <- as.data.frame(road_battles)
treat
dv <- as.data.frame(drc_vac)
dv



all_relevant_events <- rbind(con, treat, dv)

#Build MWA dataset
dataset <- as.data.frame(cbind(rep(as.character("NA"),length(all_relevant_events))))
names(dataset) <- "type"
dataset$lon <- 0.0
dataset$lat <- 0.0
dataset$timestamp <- as.Date("1900-01-01")
dataset$population <- 0.0

#Copy the data
dataset$type <- c(rep("control",length(riots[,1])),rep("treatment",length(protests[,1])),rep("dependent",length(military_actions[,1])))
dataset$lat  <- c(riots$LATITUDE,protests$LATITUDE,military_actions$LATITUDE)
dataset$lon  <- c(riots$LONGITUDE,protests$LONGITUDE,military_actions$LONGITUDE)
dataset$timestamp  <- c(riots$EVENT_DATE,protests$EVENT_DATE,military_actions$EVENT_DATE)

#Now we need to go spatial for the population values
dataset$population <- c(population_liberia[riots,],population_liberia[protests,],population_liberia[military_actions,])

#MWA Analysis
# Specify required parameters:
t_window <- c(5,50,5)
spat_window <- c(5,50,5)
t_unit <- "days" 
TCM <- TRUE
weighted <- FALSE

treatment <- c("type","treatment")
# - column and entries that indicate control events 
control  <- c("type","control")
# - column and entries that indicate dependent events 
dependent <- c("type","dependent")
# - columns for matching
matchColumns <- c("population")

# Execute method:
results <- matchedwake(dataset, t_window, spat_window, treatment, control, dependent, matchColumns, weighted = weighted, t_unit = t_unit, TCM = TCM)

plot(results)
summary(results)


