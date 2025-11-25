################################################################################
# Title:        Generate Maps of predicted baselines with zoomed panels for Europe, Scandinavia, and North America
# Description:  This scripts uses data-driven models to generate maps of isotope baselines 
#                for all sites , including standard errors
#               in the dataset
#               Six maps: d13C pelagic, d13C benthic, d15N pelagic, d15N benthic, SE d13C and SE d15N
# Date:         2025-05-05
# Version:      6.0
# Notes:        Any additional information or context
# Dependencies: dplyr, ggplot2, tidyr, mgcv
################################################################################


################################################################################
# 1. SETUP ----------
################################################################################
## Clear environment
rm(list = ls())

## Load required packages

library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(viridis)
library(grid)
library(gridExtra)
library(ggpubr)
library(rprojroot)


## Set the root directory for the project
root.dir = find_rstudio_root_file()
script.dir = paste0(root.dir,'/scripts')
models.dir = paste0(root.dir,'/models')
models.dir = paste0(root.dir,'/data')
setwd(root.dir)
################################################################################
# Load predicted baselines data
load("data/processed/Isotope_baselines_predictions_all_sites.RData")
predictions_all<-Isotope_baselines_predictions_all_sites
dim(predictions_all)
predictions_all<-unique(predictions_all)
dim(predictions_all)


################################################################################
# 2. MAPS for baseline values ----------
################################################################################


# Prepare data - spread to get pelagic and benthic as columns
map_data <- predictions_all %>%
  filter(!is.na(d13C_baseline) & !is.na(d15N_baseline)) %>%
  select(FW_ID, Habitat, Latitude, Longitude, d13C_baseline, d15N_baseline, Ecosystem_Type) %>%
  tidyr::pivot_wider(
    id_cols = c(FW_ID, Ecosystem_Type,Latitude,Longitude),
    names_from = Habitat,
    values_from = c(d13C_baseline, d15N_baseline),
    names_glue = "{.value}_{tolower(Habitat)}",
    values_fill = NA
  ) %>%
  rename(d13C_pelagic = d13C_baseline_pelagic, d13C_benthic = d13C_baseline_benthic,
         d15N_pelagic = d15N_baseline_pelagic, d15N_benthic = d15N_baseline_benthic)

# Get world map
world <- map_data("world")

# Define regions for zoomed panels (same as enhanced_map_v2)
europe_bounds <- list(long = c(-10, 15), lat = c(42, 60))
namerica_bounds <- list(long = c(-130, -65), lat = c(30, 55))
scandinavia_bounds <- list(long = c(5, 35), lat = c(65, 71))

# Border colors from enhanced_map_v2
border_colors <- list(
  europe = "chocolate2",
  namerica = "darkorchid2",
  scandinavia = "#31583c"
)

# Function to create base map with zoom regions highlighted
create_base_map <- function(data, value_col, color_scale = "viridis", scale_limits = NULL) {
  
  # Set color palette with fixed limits
  if(color_scale == "viridis") {
    color_func <- scale_color_viridis_c(option = "viridis", name = expression("δ"^13*"C (‰)"), limits = scale_limits)
  } else {
    color_func <- scale_color_viridis_c(option = "inferno", name = expression("δ"^15*"N (‰)"), limits = scale_limits)
  }
  
  p <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "lightgrey", color = "darkgrey", linewidth = 0.2) +
    geom_point(data = data, 
               aes(x = Longitude, y = Latitude, 
                   color = .data[[value_col]]),
  #                 shape = Ecosystem_Type),
               size = 0.8, alpha = 0.7) +
    color_func +
#    scale_shape_manual(values = c("Lentic" = 16, "Lotic" = 17), 
 #                      name = "Ecosystem Type") +
    # Add rectangles for zoom regions (same colors as enhanced_map_v2)
    geom_rect(aes(xmin = europe_bounds$long[1], xmax = europe_bounds$long[2],
                  ymin = europe_bounds$lat[1], ymax = europe_bounds$lat[2]),
              fill = NA, color = border_colors$europe, linewidth = 0.6, linetype = "solid") +
    geom_rect(aes(xmin = namerica_bounds$long[1], xmax = namerica_bounds$long[2],
                  ymin = namerica_bounds$lat[1], ymax = namerica_bounds$lat[2]),
              fill = NA, color = border_colors$namerica, linewidth = 0.6, linetype = "solid") +
    geom_rect(aes(xmin = scandinavia_bounds$long[1], xmax = scandinavia_bounds$long[2],
                  ymin = scandinavia_bounds$lat[1], ymax = scandinavia_bounds$lat[2]),
              fill = NA, color = border_colors$scandinavia, linewidth = 0.6, linetype = "solid") +
    coord_fixed(ratio = 1.3) +
    theme_classic() +
    labs(fill = "", x = "Longitude", y = "Latitude") +
    theme(legend.position = "right",
          plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          text = element_text(size = 10),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
  
  return(p)
}

# Function to create zoomed inset
create_zoom_inset <- function(data, bounds, value_col, color_scale = "viridis", border_color = "lightgoldenrod", scale_limits = NULL) {
  
  # Filter data for region
  region_data <- data %>%
    filter(Longitude >= bounds$long[1] & Longitude <= bounds$long[2] &
           Latitude >= bounds$lat[1] & Latitude <= bounds$lat[2])
  
  if(nrow(region_data) == 0) return(NULL)
  
  # Set color palette with fixed limits
  if(color_scale == "viridis") {
    color_func <- scale_color_viridis_c(option = "viridis", guide = "none", limits = scale_limits)
  } else {
    color_func <- scale_color_viridis_c(option = "inferno", guide = "none", limits = scale_limits)
  }
  
  # Calculate ratio based on enhanced_map_v2
  if(border_color == border_colors$namerica) {
    ratio <- 1/cos(mean(bounds$lat) * pi/180)
  } else {
    ratio <- 0.75*1/cos(mean(bounds$lat) * pi/180)
  }
  
  p <- ggplot() +
    geom_polygon(data = world %>% filter(long >= bounds$long[1] & 
                                         long <= bounds$long[2] &
                                         lat >= bounds$lat[1] & 
                                         lat <= bounds$lat[2]), 
                 aes(x = long, y = lat, group = group),
                 fill = "lightgrey", color = "darkgrey", linewidth = 0.2) +
    geom_point(data = region_data, 
               aes(x = Longitude, y = Latitude, 
                   color = .data[[value_col]]),
  #                 shape = Ecosystem_Type),
               size = 1.2, alpha = 0.6, stroke = 0.4) +
    color_func +
#    scale_shape_manual(values = c("Lentic" = 16, "Lotic" = 17)) +
    coord_fixed(ratio = ratio, 
                xlim = bounds$long, ylim = bounds$lat) +
    theme_void() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = border_color, linewidth = 0.8),
          plot.margin = margin(0.3, 0.3, 0.3, 0.3))
  
  return(p)
}

# Function to create complete map with insets
create_map_with_insets <- function(data, value_col, color_scale, scale_limits = NULL) {
  
  # Create base map
  base_map <- create_base_map(data, value_col, color_scale, scale_limits)
  
  # Create insets
  europe_inset <- create_zoom_inset(data, europe_bounds, value_col, color_scale, border_colors$europe, scale_limits)
  namerica_inset <- create_zoom_inset(data, namerica_bounds, value_col, color_scale, border_colors$namerica, scale_limits)
  scandinavia_inset <- create_zoom_inset(data, scandinavia_bounds, value_col, color_scale, border_colors$scandinavia, scale_limits)
  
  # Add insets to base map (same positions as enhanced_map_v2)
  map_with_insets <- base_map
  
  if(!is.null(namerica_inset)) {
    map_with_insets <- map_with_insets +
      annotation_custom(ggplotGrob(namerica_inset), 
                        xmin = -190, xmax = -82, ymin = -55, ymax = 25)
  }
  
  if(!is.null(europe_inset)) {
    map_with_insets <- map_with_insets +
      annotation_custom(ggplotGrob(europe_inset), 
                        xmin = 30, xmax = 120, ymin = 50, ymax = 100)
  }
  
  if(!is.null(scandinavia_inset)) {
    map_with_insets <- map_with_insets +
      annotation_custom(ggplotGrob(scandinavia_inset), 
                        xmin = -15, xmax = 29, ymin = 60, ymax = 100)
  }
  
  return(map_with_insets)
}

# Calculate scale limits for consistent ranges
d13C_limits <- range(c(map_data$d13C_pelagic, map_data$d13C_benthic), na.rm = TRUE)
#d15N_limits <- range(c(map_data$d15N_pelagic, map_data$d15N_benthic), na.rm = TRUE
d15N_limits <- c(-2,17)

# Create the four maps with consistent scales
cat("Creating d13C Pelagic map...\n")
map1 <- create_map_with_insets(map_data, "d13C_pelagic", "viridis", d13C_limits)

cat("Creating d15N Pelagic map...\n")
map2 <- create_map_with_insets(map_data, "d15N_pelagic", "inferno", d15N_limits)

cat("Creating d13C Benthic map...\n")
map3 <- create_map_with_insets(map_data, "d13C_benthic", "viridis", d13C_limits)

cat("Creating d15N Benthic map...\n")
map4 <- create_map_with_insets(map_data, "d15N_benthic", "inferno", d15N_limits)


################################################################################
# 3. MAPS for baseline SE ----------
################################################################################

# Prepare data - spread to get pelagic and benthic as columns
map_data_SE <- predictions_all %>%
  filter(!is.na(d13C_baseline_SE) & !is.na(d15N_baseline_SE)) %>%
  select(FW_ID, Habitat, Latitude, Longitude, d13C_baseline_SE, d15N_baseline_SE, Ecosystem_Type) %>%
  tidyr::pivot_wider(
    id_cols = c(FW_ID, Ecosystem_Type,Latitude,Longitude),
    names_from = Habitat,
    values_from = c(d13C_baseline_SE, d15N_baseline_SE),
    names_glue = "{.value}_{tolower(Habitat)}",
    values_fill = NA
  ) %>%
  rename(d13C_pelagic = d13C_baseline_SE_pelagic, d13C_benthic = d13C_baseline_SE_benthic,
         d15N_pelagic = d15N_baseline_SE_pelagic, d15N_benthic = d15N_baseline_SE_benthic)


# Function to create base map with zoom regions highlighted
create_base_map <- function(data, value_col, color_scale = "viridis", scale_limits = NULL) {
  
  # Set color palette with fixed limits
  if(color_scale == "viridis") {
    color_func <- scale_color_viridis_c(option = "viridis", name = expression("SE δ"^13*"C (‰)"), limits = scale_limits)
  } else {
    color_func <- scale_color_viridis_c(option = "inferno", name = expression("SE δ"^15*"N (‰)"), limits = scale_limits)
  }
  
  p <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "lightgrey", color = "darkgrey", linewidth = 0.2) +
    geom_point(data = data, 
               aes(x = Longitude, y = Latitude, 
                   color = .data[[value_col]]),
               #                 shape = Ecosystem_Type),
               size = 0.8, alpha = 0.7) +
    color_func +
    #    scale_shape_manual(values = c("Lentic" = 16, "Lotic" = 17), 
    #                      name = "Ecosystem Type") +
    # Add rectangles for zoom regions (same colors as enhanced_map_v2)
    geom_rect(aes(xmin = europe_bounds$long[1], xmax = europe_bounds$long[2],
                  ymin = europe_bounds$lat[1], ymax = europe_bounds$lat[2]),
              fill = NA, color = border_colors$europe, linewidth = 0.6, linetype = "solid") +
    geom_rect(aes(xmin = namerica_bounds$long[1], xmax = namerica_bounds$long[2],
                  ymin = namerica_bounds$lat[1], ymax = namerica_bounds$lat[2]),
              fill = NA, color = border_colors$namerica, linewidth = 0.6, linetype = "solid") +
    geom_rect(aes(xmin = scandinavia_bounds$long[1], xmax = scandinavia_bounds$long[2],
                  ymin = scandinavia_bounds$lat[1], ymax = scandinavia_bounds$lat[2]),
              fill = NA, color = border_colors$scandinavia, linewidth = 0.6, linetype = "solid") +
    coord_fixed(ratio = 1.3) +
    theme_classic() +
    labs(fill = "", x = "Longitude", y = "Latitude") +
    theme(legend.position = "right",
          plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          text = element_text(size = 10),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
  
  return(p)
}

# Function to create zoomed inset
create_zoom_inset <- function(data, bounds, value_col, color_scale = "viridis", border_color = "lightgoldenrod", scale_limits = NULL) {
  
  # Filter data for region
  region_data <- data %>%
    filter(Longitude >= bounds$long[1] & Longitude <= bounds$long[2] &
             Latitude >= bounds$lat[1] & Latitude <= bounds$lat[2])
  
  if(nrow(region_data) == 0) return(NULL)
  
  # Set color palette with fixed limits
  if(color_scale == "viridis") {
    color_func <- scale_color_viridis_c(option = "viridis", guide = "none", limits = scale_limits)
  } else {
    color_func <- scale_color_viridis_c(option = "inferno", guide = "none", limits = scale_limits)
  }
  
  # Calculate ratio based on enhanced_map_v2
  if(border_color == border_colors$namerica) {
    ratio <- 1/cos(mean(bounds$lat) * pi/180)
  } else {
    ratio <- 0.75*1/cos(mean(bounds$lat) * pi/180)
  }
  
  p <- ggplot() +
    geom_polygon(data = world %>% filter(long >= bounds$long[1] & 
                                           long <= bounds$long[2] &
                                           lat >= bounds$lat[1] & 
                                           lat <= bounds$lat[2]), 
                 aes(x = long, y = lat, group = group),
                 fill = "lightgrey", color = "darkgrey", linewidth = 0.2) +
    geom_point(data = region_data, 
               aes(x = Longitude, y = Latitude, 
                   color = .data[[value_col]]),
               #                 shape = Ecosystem_Type),
               size = 1.2, alpha = 0.6, stroke = 0.4) +
    color_func +
    #    scale_shape_manual(values = c("Lentic" = 16, "Lotic" = 17)) +
    coord_fixed(ratio = ratio, 
                xlim = bounds$long, ylim = bounds$lat) +
    theme_void() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = border_color, linewidth = 0.8),
          plot.margin = margin(0.3, 0.3, 0.3, 0.3))
  
  return(p)
}

# Function to create complete map with insets
create_map_with_insets <- function(data, value_col, color_scale, scale_limits = NULL) {
  
  # Create base map
  base_map <- create_base_map(data, value_col, color_scale, scale_limits)
  
  # Create insets
  europe_inset <- create_zoom_inset(data, europe_bounds, value_col, color_scale, border_colors$europe, scale_limits)
  namerica_inset <- create_zoom_inset(data, namerica_bounds, value_col, color_scale, border_colors$namerica, scale_limits)
  scandinavia_inset <- create_zoom_inset(data, scandinavia_bounds, value_col, color_scale, border_colors$scandinavia, scale_limits)
  
  # Add insets to base map (same positions as enhanced_map_v2)
  map_with_insets <- base_map
  
  if(!is.null(namerica_inset)) {
    map_with_insets <- map_with_insets +
      annotation_custom(ggplotGrob(namerica_inset), 
                        xmin = -190, xmax = -82, ymin = -55, ymax = 25)
  }
  
  if(!is.null(europe_inset)) {
    map_with_insets <- map_with_insets +
      annotation_custom(ggplotGrob(europe_inset), 
                        xmin = 30, xmax = 120, ymin = 50, ymax = 100)
  }
  
  if(!is.null(scandinavia_inset)) {
    map_with_insets <- map_with_insets +
      annotation_custom(ggplotGrob(scandinavia_inset), 
                        xmin = -15, xmax = 29, ymin = 60, ymax = 100)
  }
  
  return(map_with_insets)
}

# Calculate scale limits for consistent ranges
#d13C_limits <- range(c(map_data$d13C_pelagic, map_data$d13C_benthic), na.rm = TRUE)
d13C_limits <- c(0,2.5)

#d15N_limits <- range(c(map_data$d15N_pelagic, map_data$d15N_benthic), na.rm = TRUE
d15N_limits <- c(0,2)

# Create the four maps with consistent scales
cat("Creating d13C Pelagic map...\n")
map1_SE <- create_map_with_insets(map_data_SE, "d13C_pelagic", "viridis", d13C_limits)

cat("Creating d15N Pelagic map...\n")
map2_SE <- create_map_with_insets(map_data_SE, "d15N_pelagic", "inferno", d15N_limits)


################################################################################
# 4. COMBINE all MAPS ----------
################################################################################


combined_maps_1 <- ggarrange(map1, map3,ncol = 1, nrow = 2,
labels = c("A", "B"), common.legend=TRUE, legend = "top") 

combined_maps_2 <- ggarrange(map2, map4,ncol = 1, nrow = 2,
                             labels = c("C", "D"), common.legend=TRUE, legend = "top") 
combined_maps <- ggarrange(combined_maps_1, combined_maps_2, 
                           ncol = 2, nrow = 1, common.legend=TRUE, legend = "top")

combined_maps_SE_1 <- ggarrange(map1_SE, map2_SE,ncol = 2, nrow = 1,
                                labels = c("E", "F"), legend = "top") 

combined_maps_b<-ggarrange(combined_maps, combined_maps_SE_1, 
                           nrow = 2,heights=c(2,1.2))


jpeg("figures/Fig6.jpg",
     width = 21, height = 21, units = "cm", res = 1200)
combined_maps_b
dev.off()