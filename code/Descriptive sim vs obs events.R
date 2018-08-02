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
#sim_m1 <- read.csv("data/sim_points1.csv")

head(sim_m1)
sim_m1 <- sim_m1[,2:3]
colnames(sim_m1) <- c("LONG","LAT") 
proj <- CRS("+proj=longlat +datum=WGS84") 
sim_events <- SpatialPointsDataFrame(coords=sim_m1, data=sim_m1, proj4string=proj) 
sim.road.sp <- SpatialPointsDataFrame(coords=sim.road.df, data=sim.road.df, proj4string=proj) 
sim.drc.sp <- SpatialPointsDataFrame(coords=sim.drc.df, data=sim.drc.df, proj4string=proj) 

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


# compute distances between observed and VAC

d.road <- distm(road_battles, drc_vac)
d.drc  <- distm(drc_battles, drc_vac)

dim(d.road)
dim(d.drc)

mean_dist_obs_road <- vector()
mean_dist_obs_drc <- vector()

# Obs Battles - VAC (in roads)
for (i in 1:nrow(d.road)) {
  distances <- sort(c(d.road[i,]))
  nearest_neigh <- distances[1:5]
  mean_dist_obs_road[i] <- mean(nearest_neigh)
}

# Obs Battles - VAC (all DRC)
for (i in 1:nrow(d.drc)) {
  distances <- sort(c(d.drc[i,]))
  nearest_neigh <- distances[1:5]
  mean_dist_obs_drc[i] <- mean(nearest_neigh)
}

summary(mean_dist_obs_road)
summary(mean_dist_obs_drc)

# compute distances between simulations and VAC

d.sim.road <- distm(sim.road.df, drc_vac)
d.sim.drc <- distm(sim.drc.df, drc_vac)

dim(d.sim.road) # battle events
dim(d.sim.drc) # VAC events

mean_dist_sim_road <- vector()
mean_dist_sim_drc <- vector()

# Sim Battles - VAC (in roads)
for (i in 1:nrow(d.sim.road)) {
  distances <- sort(c(d.sim.road[i,]))
  nearest_neigh <- distances[1:5]
  mean_dist_sim_road[i] <- mean(nearest_neigh)
}

# Sim Battles - VAC (all DRC)
for (i in 1:nrow(d.sim.drc)) {
  distances <- sort(c(d.sim.drc[i,]))
  nearest_neigh <- distances[1:5]
  mean_dist_sim_drc[i] <- mean(nearest_neigh)
}

hist(mean_dist_obs_road)
hist(mean_dist_obs_drc)
hist(mean_dist_sim_road)
hist(mean_dist_sim_drc)

t.test(as.vector(mean_dist_obs_drc[,1]), as.vector(mean_dist_sim_drc[,1]))

mean_dist_obs_road <- as.data.frame(mean_dist_obs_road)
mean_dist_sim_road <- as.data.frame(mean_dist_sim_road)
mean_dist_obs_drc <- as.data.frame(mean_dist_obs_drc)
mean_dist_sim_drc <- as.data.frame(mean_dist_sim_drc)

colnames(mean_dist_obs_road) <- c("avg_dist", "cat")
colnames(mean_dist_sim_road) <- c("avg_dist", "cat")
colnames(mean_dist_obs_drc) <- c("avg_dist", "cat")
colnames(mean_dist_sim_drc) <- c("avg_dist", "cat")

mean_dist_obs_road$cat <- "Observed battles (road)"
mean_dist_sim_road$cat <- "Simulated battles (road)"
mean_dist_obs_drc$cat <- "Observed battles (DRC)"
mean_dist_sim_drc$cat <- "Simulated battles (DRC)"

combined.df <-rbind(mean_dist_obs_road,mean_dist_sim_road,mean_dist_obs_drc,mean_dist_sim_drc )

# theme settings

main.theme <- theme_bw() + 
  theme(legend.key = element_rect(fill = NA, color = NA), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Roboto", size = 14))
        theme_set(main.theme)
        
# Comparison boxplot
        
combined.df$cat <- ordered(combined.df$cat, levels = c("Observed battles (road)", "Simulated battles (road)","Observed battles (DRC)","Simulated battles (DRC)"))

ggplot() + 
  geom_boxplot(data = combined.df, aes(x=cat, avg_dist/1000)) + 
  labs(x = "", y = "Avg. Distance (in km)", title = "Distance of battles to five nearest VAC events", subtitle = "Comparison between observed vs. simulated battles") +
  geom_text(aes(1.5,200, label = "T-test, p < 0.001")) + 
  geom_text(aes(3.5,200, label = "T-test, p < 0.001"))




