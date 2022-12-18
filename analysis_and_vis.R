# Project: A systematic review of outdoor airborne fungal spore seasonality 
# across Europe and its relevance for (public) health
# Key:
# Input data referred to as INPUT (i.e. INPUT.csv)
# Output data referred to as OUTPUT (i.e. OUTPUT.tiff)

# PACKAGES USED ACROSS ENTIRE SCRIPT ============
library(tidyverse) # including ggplot2 for figs. 3-6.
library(lubridate)
library(readxl)


# FIGURES #####################################################

# FIG. 2. Sampling location map ===============================
# Packages used
library(sp)
library(sf)
library(rgdal)
library(ggmap)
library(tmap)
library(raster)
library(RColorBrewer)
library(viridis)
library(classInt)
library(rnaturalearth)
library(rnaturalearthhires)
library(maps)
library(grid)

# Import extracted data from studies
INPUT <- read_excel("extracted_data.xlsx", sheet = "extracted_data")

# Import map polygon files
map <- ne_countries(type = "map_units", scale = "large")
# Extract dataframe from map object
map_dat <- map@data
# Define a function to format coordinates from "xx-xx-xx X" to a decimal value
coord_formatter <- function(x){
  x %>%
    sub('-', 'd', .) %>%
    sub('-', '\'', .) %>%
    sub(' ', '" ', .) %>%
    sp::char2dms %>%
    as.numeric
}
# Use coord_formatter() to convert latitude and longitude character columns to
# numerical (decimal)
coords_df <- map_df(INPUT[,c("latitude","longitude")], coord_formatter)

# Convert these values (decimal coordinates) to sf format
main <- st_crs(map) # retrieves coordinate reference system from sf or sfc object
coords <- coords_df %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = main) # creates an sf object

# join coordinates back with INPUT data. This results in an sf object (geometry column)
# WITH the extracted data
coords <- cbind(coords, INPUT)

# Import biogeographical region raster file for overlay
# Source (cited in manuscript): Cervellini M, Zannini P, Di Musciano M,
# Fattorini S, Jimenez-Alfaro B, Rocchini D, et al.
# A grid-based map for the Biogeographical Regions of Europe.
# Biodivers Data J 2020; 8: e53720.
rast <- raster('rast.tif')

# Set colour palette for biogeographical regions
pal_h <- c("red",     # 
           "#FF00FF", # Alpine - EEA colour = Mauve
           "#FF007F", # Anatolian - EEA colour = Ruby pink
           "#72B0D4",   # Arctic = Light blue
           "#3399FF", # Atlantic - EEA colour = Blue
           "#FFCCFF", # Black Sea - EEA colour = Pink
           "#0000FF", # Boreal - EEA colour = Dark blue
           "#00FF00", # Continental - EEA colour = Toad Green
           "#DC2020",    # Macaronesian - EEA colour = Red/Orange
           "#FF9933", # Mediterranean - EEA colour = Yellow sand
           "white",   # NON-EUROPE - EEA colour = White
           "#994C00",  # Pannonian - EEA colour = Orange
           "#FFE5CC" # Steppic - EEA colour = Light sand 
)

# Set map boundaries (main map and inlet maps)
boundbox1 <- st_bbox(c(xmin = -10, xmax = 39.8, ymax =69, ymin = 36),
                     crs = st_crs(4326))
boundbox2 <- st_bbox(c(xmin = -18.1, xmax = -13.44, ymax = 29.5, ymin = 27.5),
                     crs = st_crs(4326))
boundbox3 <- st_bbox(c(xmin = -17.32, xmax = -16.65, ymax = 32.9, ymin = 32.6),
                     crs = st_crs(4326))

# Calculate years of sampling
coords <- coords %>% 
  mutate(daysSampling = sampling_end - sampling_start)
coords$daysSampling <- as.numeric(coords$daysSampling)/365

# Create main EU map
map_loc_main <- tm_shape(rast, bbox = boundbox1) +
  tm_raster(style = "fixed", breaks = seq(0, 13, by=1), palette = pal_h, alpha = 0.5) +
  tm_shape(map) +
  tm_layout(bg.color = "white", legend.outside = TRUE, legend.bg.color = "grey") +
  tm_borders(col = "black", lwd = 1.5) +
  tm_shape(coords) +
  tm_bubbles(size = 0.7, col = "daysSampling",  midpoint = 17, alpha = 1, style = "fixed",
             breaks = c(0.2,1,2,3,10,20,35), border.col = "black",
             palette = "RdYlGn", border.lwd = 1.7) +
  tm_layout(legend.show = TRUE) +
  tm_scale_bar(position = c("left","top")) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.color = "white",
            legend.bg.alpha = 1)
# Create Canary Islands inlet map
map_loc_canary <- tm_shape(rast, bbox = boundbox2) +
  tm_raster(style = "fixed", breaks = seq(0, 13, by=1), palette = pal_h, alpha = 0.5) +
  tm_shape(map) +
  tm_layout(bg.color = "white", legend.outside = TRUE, legend.bg.color = "grey") +
  tm_borders(col = "black", lwd = 1.5) +
  tm_shape(coords) +
  tm_bubbles(size = 0.7, col = "daysSampling",  midpoint = 17, alpha = 1, style = "fixed",
             breaks = c(0.2,1,2,3,10,20,35), border.col = "black",
             palette = "RdYlGn", border.lwd = 1.7) +
  tm_layout(legend.show = FALSE) +
  tm_scale_bar(position = c("left","top")) +
  tm_layout(legend.bg.color = "white",
            legend.bg.alpha = 1)
# Create Madeira island inlet map
map_loc_mad <- tm_shape(rast, bbox = boundbox3) +
  tm_raster(style = "fixed", breaks = seq(0, 13, by=1), palette = pal_h, alpha = 0.5) +
  tm_layout(bg.color = "white", legend.outside = TRUE, legend.bg.color = "grey") +
  tm_borders(col = "black", lwd = 1.5) +
  tm_shape(coords) +
  tm_bubbles(size = 0.7, col = "daysSampling",  midpoint = 17, alpha = 1, style = "fixed",
             breaks = c(0.2,1,2,3,10,20,35), border.col = "black",
             palette = "RdYlGn", border.lwd = 1.7) +
  tm_layout(legend.show = FALSE) +
  tm_scale_bar(position = c("left","top")) +
  tm_layout(legend.bg.color = "white",
            legend.bg.alpha = 1)
# Print the main map, and inlay the two inlet maps
map_loc_main
print(map_loc_canary, vp = grid::viewport(0.85, 0.87, width = 0.25, height = 0.34))
print(map_loc_mad, vp = grid::viewport(0.89, 0.71, width = 0.17, height = 0.22)) 
# Export complete map
viewport_canary <- grid::viewport(0.85, 0.87, width = 0.25, height = 0.34)
viewport_mad <- grid::viewport(0.89, 0.71, width = 0.17, height = 0.22)
list_inlets <- list(viewport_canary, viewport_mad)
list_maps <- list(map_loc_canary, map_loc_mad)

tmap_save(map_loc_main, filename = "OUTPUT.tiff", dpi = 300, insets_vp = list_inlets,
          insets_tm = list_maps)


# FIG. 3. Seasonal dates ==========================================

# Import extracted data from studies
INPUT <- read_excel("extracted_data.xlsx", sheet = "extracted_data")

# Bind coords dataframe (from Fig. 2) together with INPUT data for 
# access to geographical data
study_lats <- cbind(coords, INPUT) %>% 
  dplyr::select(study, sampler_location, country, latitude, longitude)
# Create new col with location + country (for appending text to Y axis)
test_lat <- study_lats %>% 
  mutate(sampler_location2 = paste0(sampler_location," (",country, ")"))
# Join with data
test_lat <- full_join(INPUT, test_lat, by = c("study", "sampler_location"))
# Order y-axis (sampler_locations) by latitude
test_lat$sampler_location2 <- fct_reorder(test_lat$sampler_location2, test_lat$latitude, min)

# Data wrangling
# Filter for relevant factors
test_x <- test_lat %>% 
  dplyr::select(study, country, sampler_location, sampler_location2, spore_taxa,
                Year, season_dates_start, season_dates_end, peak_dates, latitude) %>% 
  filter(spore_taxa %in% c("Alternaria", "Cladosporium"))
# This line converts an excel-style date value to a functional date format.
test_x$season_dates_start <- as.Date(test_x$season_dates_start, origin = "1899-12-30")
# Calculate season dates (with a dummy year of 2021 to ensure all points will
# fit onto the same X-axis)
# Season start date
test_x <- test_x %>% 
  mutate(day_x = day(season_dates_start)) %>% 
  mutate(month_x = month(season_dates_start)) %>% 
  mutate(season_dates_start_dummy = paste("2021-",month_x,"-",day_x, sep = ""))
test_x$season_dates_start_dummy <- ymd(test_x$season_dates_start_dummy)
# Season end date
test_x$season_dates_end <- as.Date(test_x$season_dates_end, origin = "1899-12-30")
test_x <- test_x %>% 
  mutate(day_x = day(season_dates_end)) %>% 
  mutate(month_x = month(season_dates_end)) %>% 
  mutate(season_dates_end_dummy = paste("2021-",month_x,"-",day_x, sep = ""))
test_x$season_dates_end_dummy <- ymd(test_x$season_dates_end_dummy)
# peak date
test_x$peak_dates <- as.Date(test_x$peak_dates, origin = "1899-12-30")
test_x <- test_x %>% 
  mutate(day_x = day(peak_dates)) %>% 
  mutate(month_x = month(peak_dates)) %>% 
  mutate(peak_dates_dummy = paste("2021-",month_x,"-",day_x, sep = ""))
test_x$peak_dates_dummy <- ymd(test_x$peak_dates_dummy)
# Pivot into long format for ggplot2 functionality
test_x <- pivot_longer(test_x, cols = c(season_dates_start_dummy, season_dates_end_dummy,
                                        peak_dates_dummy), names_to = "seasonal_date_type",
                       values_to = "seasonal_date") %>% 
  filter(!is.na("seasonal_date"))
# Calculate number of studies per point
alt_studies <- test_x %>% 
  filter(spore_taxa == "Alternaria") %>% 
  group_by(sampler_location) %>% 
  summarise(n_distinct(study))
clad_studies <-test_x %>% 
  filter(spore_taxa == "Cladosporium") %>% 
  group_by(sampler_location) %>% 
  summarise(n_distinct(study))
alt_studies <- rename(alt_studies, alt_studies = "n_distinct(study)")
clad_studies <- rename(clad_studies, clad_studies = "n_distinct(study)")
test_x <- left_join(test_x, alt_studies, by = "sampler_location")
test_x <- left_join(test_x, clad_studies, by = "sampler_location")
# Remove NULL studies to avoid errors
int <- which(is.na(test_x$clad_studies))
test_x$clad_studies[int] <- 0
# Reorder the new variable("sampler_location-country-(alt:clad studies)) by latitude
test_x <- test_x %>% 
  mutate(sampler_location3 = paste0(sampler_location2,"    ",alt_studies,":",clad_studies))
test_x$sampler_location3 <- fct_reorder(test_x$sampler_location3, test_x$latitude, min)
# Plot the figure (across the dummy year of 2021)
test_x %>% 
  filter(seasonal_date <= dmy("28-12-2021")) %>% 
  ggplot(aes(y = sampler_location3, x = seasonal_date, shape = seasonal_date_type,
             colour = seasonal_date_type, group = sampler_location)) +
  geom_point() + facet_grid(~ spore_taxa) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month",
               limits = as.Date(c("2021-01-01","2021-12-31"))) +
  theme_bw() + theme(legend.title=element_blank(), 
                     strip.text = element_text(face = "bold.italic"),
                     axis.text.x = element_text(angle = 45)) +
  labs(x = "Month", y = "Sampler location (North to South)") +
  scale_color_brewer(palette = "Dark2", labels = c("Peak","End","Start")) +
  scale_shape_manual(values=c(17, 19, 19), labels = c("Peak","End","Start")) +
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black')) +
  annotate("text", x = dmy(31-12-2020), y = 1:10, label = "Some text")

ggsave("OUTPUT.tiff")


# FIG. 4. Peak concentrations by location =====================================

# Import extracted data from studies
INPUT <- read_excel("extracted_data.xlsx", sheet = "extracted_data")

# Bind coords dataframe (from Fig. 2) together with INPUT data for access to geographical data
study_lats <- cbind(coords, INPUT) %>% 
  dplyr::select(study, sampler_location, country, latitude, longitude)
# Create new col with location + country (for appending text to Y axis)
test_lat <- study_lats %>% 
  mutate(sampler_location2 = paste0(sampler_location," (",country, ")"))
# Join with data
test_lat <- full_join(INPUT, test_lat, by = c("study", "sampler_location"))
# Order y-axis (sampler_locations) by latitude
test_lat$sampler_location2 <- fct_reorder(test_lat$sampler_location2, test_lat$latitude, min)

# Data wrangling
# Filter for relevant factors
test_max <- test_lat %>% 
  dplyr::select(study, sampler_location, spore_taxa, Year, mean_spores_m3, 
                mean_spores_m3_type,
                `mean_spore_during_season/multiyear_1y`, max_spores_m3, 
                max_spores_m3_type, latitude) %>% 
  filter(spore_taxa %in% c("Alternaria", "Cladosporium")) %>% 
  pivot_longer(cols = c(mean_spores_m3, max_spores_m3), names_to = "stat_type",
               values_to = "stat_val") %>% 
  filter(!is.na(stat_val)) %>% 
  filter(max_spores_m3_type %in% c(">1y_or_<1y","1y",">1y")) %>%
  filter(stat_type == "max_spores_m3")
# Number of studies
alt_studies <-test_max %>% 
  filter(spore_taxa == "Alternaria") %>% 
  group_by(sampler_location) %>% 
  summarise(n_distinct(study))
clad_studies <-test_max %>% 
  filter(spore_taxa == "Cladosporium") %>% 
  group_by(sampler_location) %>% 
  summarise(n_distinct(study))
alt_studies <- rename(alt_studies, alt_studies = "n_distinct(study)")
clad_studies <- rename(clad_studies, clad_studies = "n_distinct(study)")
test_max <- left_join(test_max, alt_studies, by = "sampler_location")
test_max <- left_join(test_max, clad_studies, by = "sampler_location")
# Remove NULL studies to avoid errors
int <- which(is.na(test_max$clad_studies))
test_max$clad_studies[int] <- 0
# Create new var and re-reorder by lat again
test_max <- test_max %>% 
  mutate(sampler_location3 = paste0(sampler_location,"    ",alt_studies,":",clad_studies))
test_max$sampler_location3 <- fct_reorder(test_max$sampler_location3, test_max$latitude, min)
# Plot the figure
ggplot(test_max, aes(x = stat_val, y = sampler_location3)) + geom_point(colour = "#606060") +
  geom_vline(data=filter(test_lat, spore_taxa=="Alternaria"), aes(xintercept=100),
             colour="red", alpha = 0.3, size = 1) + 
  geom_vline(data=filter(test_lat, spore_taxa=="Cladosporium"), aes(xintercept=2000),
             colour="red", alpha = 0.3, size = 1) + 
  facet_wrap(~ spore_taxa, scales = "free") +
  theme_bw() +
  theme(legend.title=element_blank(), strip.text = element_text(face = "bold.italic")) +
  labs(x = expression(Peak~concentration~"("~spores~per~m^{"3"}~")"),
       y = "Sampler location (North to South)") +
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black')) #+
#scale_x_continuous(labels = function(x) format(x, scientific = TRUE))

ggsave("OUTPUT.tiff", height = 9, width = 7.20)


# FIG. 5. K-nearest neighbors (KNN) to group meteorological correlation heat maps  =====
# Packages used
library(cluster)
library(factoextra)
library(NbClust)
library(ggprism)

# Import extracted data from studies
INPUT <- read_excel("extracted_data.xlsx", sheet = "extracted_data")

# select meteorological variables with greatest average correlation coefficients
# against fungal concentrations (all taxa)
cor_table <- INPUT %>% 
 filter(corr %in% c("Precipitation","Air.humidity","Max.temp",
                    " Min.temp","Mean.wind.speed","Air.pressure"))

# Calculate mean for each taxa to ensure one meteorological observation per spore taxa
cor_table <- cor_table %>% 
  group_by(classification, corr) %>% 
  summarise(mean_r = mean(r, na.rm = TRUE))
# Remove NaN values
int <- which(is.nan(cor_table$mean_r))
cor_table <- cor_table[-int,]
# pivot to wide format
cor_table <- cor_table %>% 
  pivot_wider(names_from = corr, values_from = mean_r)
# Remove select groups (not taxonomic classification)
int1 <- which(cor_table$classification == "basidiospores")
int2 <- which(cor_table$classification == "rusts")
int3 <- which(cor_table$classification == "smuts")
int <- c(int1, int2, int3)
cor_table <- cor_table[-int,]
# scale to a matrix
cor_table <- as.data.frame(cor_table)
rowNames <- purrr::pluck(cor_table, 1)
row.names(cor_table) <- rowNames
cor_mtx <- as.matrix(cor_table)
# set seed
set.seed(123)
# k-means clustering (after assessing ideal cluster number (see below))
km.res <- kmeans(cor_mtx, 6, nstart = 25) # 6 clusters to start with
dd <- cbind(cor_mtx, cluster = km.res$cluster)
pam.res <- cluster::pam(dd, 4)

# Assess ideal number of clusters
# Elbow
fviz_nbclust(dd, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
ggsave("k-ELBOW.tiff", height = 3.02, width = 3.91)
# Gap statistic
fviz_nbclust(dd, kmeans, nstart = 25,  method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")

# Export the PCoA plot
ggsave("OUTPUT1.tiff", height = 3.02, width = 3.91)

# Select the most relevant meteorological factors
cor_table <- INPUT %>% 
  filter(corr %in% c("Precipitation","Air.humidity","Max.temp"))
# (!) repeat code above to generate new 'cor_mtx' with these new variables

# Repeat k-means clustering (after assessing ideal cluster number)
km.res <- kmeans(cor_mtx, 4, nstart = 25) # 4 clusters determiend as ideal
dd <- cbind(cor_mtx, cluster = km.res$cluster)
pam.res <- cluster::pam(dd, 4)

# Produce a PCoA plot
fviz_cluster(km.res, dd, repel = TRUE, show.clust.cent = FALSE) + ggprism::theme_prism()
fviz_cluster(km.res, dd, repel = TRUE, show.clust.cent = FALSE, 
             ellipse = TRUE, axes = c(1,2)) + ggprism::theme_prism()
fviz_cluster(pam.res, geom = "point", ellipse.type = "norm")

# Attach KNN data to extracted data
km.res.tbl <- km.res %>% 
  dplyr::select(spore_taxa, cluster)
corr_unique_lon <- left_join(INPUT, km.res.tbl, by = "spore_taxa")
# filter out spore_taxa without a cluster (see "k-means clustering.R")
int <- which(is.na(corr_unique_lon$cluster))
corr_unique_lon <- corr_unique_lon[-int,]

# Order meteorological variables of interest
corr_unique_lon$corr_grouped <- factor(corr_unique_lon$corr_grouped) #corr_grouped is a col containing the meteo vars
corr_unique_lon$corr_grouped <- fct_relevel(corr_unique_lon$corr_grouped, c("Mean temperature",
                                                                  "Maximum temperature",
                                                                  "Minimum temperature",
                                                                  "Relative humidity",
                                                                  "Precipitation",
                                                                  "Air pressure",
                                                                  "Dew point temperature",
                                                                  "Mean wind speed",
                                                                  "Wind direction",
                                                                  "Solar radiation"))

# Plot heat maps (faceted by cluster)
corr_x_avg %>% 
  ggplot(aes(x = corr_grouped, y = reorder(spore_taxa, desc(spore_taxa)),
             fill = mean_r)) + geom_tile() +
  geom_text(aes(label = round(r_avg, 1)), size = 4, colour = "black") +
  scale_fill_gradient2(name = expression(paste(italic("r coefficient"))), low="#0066CC",
                       mid = "white", high="#CC0000", limits=c(-1, 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x ="Meteorological variable", y = "Fungal taxa") +
  facet_wrap(~ cluster, scales = "free") +
  theme(axis.text.y = element_text(face = "italic"))

# Export the heatmap
ggsave("OUTPUT2.tiff") # Merge with the PCoA plot manually


# FIG. 6. Quality scoring summary  =========================
# Packages used
library(robvis) # note: functions were edited to fit specifications

# Import extracted data from studies
INPUT <- read_excel("extracted_data.xlsx", sheet = "quality_scores")

tiff("OUTPUT.tiff", units="in", width=10, height=4, res=300)
rob_summary(INPUT, colour = "cochrane")
dev.off()

# Note: rob_traffic_light() used to generate quality score plots for individual studies


# MULTIPLE LINEAR REGRESSION #############################################
# Packages used
library(geosphere)

# Import extracted data from studies
INPUT <- read_excel("extracted_data.xlsx", sheet = "extracted_data")

# Bind coords dataframe (from Fig. 2) together with INPUT data for access to geographical data
coords_res_x2 <- cbind(coords, INPUT) %>% 
  dplyr::select(study, sampler_location, country, latitude, longitude, Alternaria, Cladosporium)

# Extract latitude coordinate data
coords_res_x2_extLats <- coords_res_x2 %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])


# Convert latitude and longitude decimal values to kilometres from a fixed point
# using the haversine formula
# 1. Find furthest geographical point from all sampling locations 
min_lat <- min(coords_res_x2_extLats$lat)
max_lat <- max(coords_res_x2_extLats$lat)
min_lon <- min(coords_res_x2_extLats$lon)
max_lon <- max(coords_res_x2_extLats$lon)
# transform into a matrix of extreme values, where col 1 = lat, col 2 = lon,
# row 1 = min, row 2 = max
lat_lon_matrix <- matrix(c(min_lat, max_lat, min_lon, max_lon), ncol = 2)
# Define a function able to utilise the haversine formua
lat_to_Haversine <- function(lat, matrix){
  haver_lat_value <-  geosphere::distHaversine(
    c(matrix[1,2],matrix[1,1]),
    c(matrix[1,2],lat),
    r = 6378) # in km (r = 6378137 for meters)
  return(haver_lat_value)
}
# Mutate a new col using the lat_to_Haversine formula
id <- seq(1:nrow(coords_res_x2_extLats))
coords_res_x2_extLats <- cbind(id, coords_res_x2_extLats)
coords_res_x2_extLats <- coords_res_x2_extLats %>% 
  group_by(id) %>% 
  mutate(haver_lat = lat_to_Haversine(lat, matrix = lat_lon_matrix))
# Repeat for longitudinal values
lon_to_Haversine <- function(lon, matrix){
  haver_lon_value <-  distHaversine(
    c(matrix[1,2],matrix[1,1]),
    c(lon,matrix[1,1]),
    r = 6378)
  return(haver_lon_value)
}
coords_res_x2_extLats <- coords_res_x2_extLats %>% 
  group_by(id) %>% 
  mutate(haver_lon = lon_to_Haversine(lon, matrix = lat_lon_matrix))

# Run the regression models
mod_sl <- lm(season_length ~ haver_lat + haver_lon, data = coords_res_x2_extLats)
mod_max <- lm(max_spores_m3 ~ haver_lat + haver_lon , data = coords_res_x2_extLats)
mod_ann <- lm(annual_spores_m3 ~ haver_lat + haver_lon , data = coords_res_x2_extLats)
mod_ss <- lm(season_dates_start_doy ~ haver_lat + haver_lon, data = coords_res_x2_extLats)
mod_se <- lm(season_dates_end_doy ~ haver_lat + haver_lon , data = coords_res_x2_extLats)
mod_peak <- lm(peak_dates_doy ~ haver_lat + haver_lon , data = coords_res_x2_extLats)

