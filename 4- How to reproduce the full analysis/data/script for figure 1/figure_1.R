

################################################################################
# Title:        Map of available baseline data - Figure 1 
# Date:         2025-09-05
# Version:      6.0
# Notes:        Any additional information or context
################################################################################


################################################################################
# 1. SETUP ----------
################################################################################
## Clear environment
rm(list = ls())

## Load required packages#figures for papers
library(ggplot2)
library(ggpubr)
library(maps)
library(mapdata)
library(dplyr)
library(ggrepel)
library(grid)
library(gridExtra)

#files
load("data/raw/DF_lentic_models.RData")
load("data/raw/DF_lotic_models.RData")

#colors
pal<-hcl.colors(6, "RdBu")

################################################################################
# 2. DATA PREP ----------
################################################################################

df1<-DF_lentic_models %>%
  select (
    FW_ID,
    Ecosystem_Type=Ecosystem_Type,
    lat=Latitude,
    long=Longitude,
    Hab=Resource_habitat,
    d13C=d13C_baseline,
    d15N=d15N_baseline,
    Trophic=Resource_trophic_group
  )
df2<-DF_lotic_models %>%
  select (
    FW_ID,
    Ecosystem_Type=Ecosystem_Type,
    lat=Latitude,
    long=Longitude,
    Hab=Resource_habitat,
    d13C=d13C_baseline,
    d15N=d15N_baseline,
    Trophic=Resource_trophic_group
  )

df<-rbind(df1,df2)  


# Remove duplicate rows
df <- distinct(df)
str(df)
dim(df)
df_b<-df %>%
  select(FW_ID,Ecosystem_Type,lat,long) %>%
  distinct()
dim(df_b)
################################################################################
# 3. PLOT MAP with INSERTS ----------
################################################################################

# Get the world map data
world_map <- map_data("world")


# Function to spread overlapping points radially (Nature paper style)
spread_points_radial <- function(df, region_bounds, spread_dist = 1.5, overlap_threshold = 0.8) {
  # Filter points in region
  region_df <- df %>%
    filter(long >= region_bounds$long[1] & long <= region_bounds$long[2] &
             lat >= region_bounds$lat[1] & lat <= region_bounds$lat[2])
  
  if(nrow(region_df) == 0) return(NULL)
  
  # Store original positions
  region_df$long_spread <- region_df$long
  region_df$lat_spread <- region_df$lat
  region_df$long_orig <- region_df$long
  region_df$lat_orig <- region_df$lat
  
  # Iteratively push apart overlapping points
  max_iterations <- 50
  for(iter in 1:max_iterations) {
    moved <- FALSE
    
    for(i in seq_len(nrow(region_df))) {
      # Find all points too close to point i
      for(j in seq_len(nrow(region_df))) {
        if(i != j) {
          dist <- sqrt((region_df$long_spread[i] - region_df$long_spread[j])^2 + 
                         (region_df$lat_spread[i] - region_df$lat_spread[j])^2)
          
          # If points overlap, push them apart radially
          if(dist < overlap_threshold && dist > 0.001) {
            # Calculate direction vector from j to i
            dx <- region_df$long_spread[i] - region_df$long_spread[j]
            dy <- region_df$lat_spread[i] - region_df$lat_spread[j]
            
            # Normalize
            norm <- sqrt(dx^2 + dy^2)
            dx <- dx / norm
            dy <- dy / norm
            
            # Push point i away from j
            push_amount <- (overlap_threshold - dist) / 2
            region_df$long_spread[i] <- region_df$long_spread[i] + dx * push_amount
            region_df$lat_spread[i] <- region_df$lat_spread[i] + dy * push_amount
            
            moved <- TRUE
          }
        }
      }
    }
    
    # If no points moved, we're done
    if(!moved) break
  }
  
  # Add flag for which points were moved significantly
  region_df$was_moved <- sqrt((region_df$long_spread - region_df$long_orig)^2 + 
                                (region_df$lat_spread - region_df$lat_orig)^2) > 0.1
  
  return(region_df)
}

# Define regions for zoomed panels
# Western Europe (France, UK, Benelux, Germany)
europe_bounds <- list(long = c(-10, 15), lat = c(40, 58))
# North America
namerica_bounds <- list(long = c(-130, -65), lat = c(30, 55))
# Northern Scandinavia (Norway, Sweden, Finland)
scandinavia_bounds <- list(long = c(5, 35), lat = c(65, 71))

# Spread points for each region
df_europe <- spread_points_radial(df_b, europe_bounds, spread_dist = 1.5, overlap_threshold = 0.6)
df_namerica <- spread_points_radial(df_b, namerica_bounds, spread_dist = 2.0, overlap_threshold = 1.0)
df_scandinavia <- spread_points_radial(df_b, scandinavia_bounds, spread_dist = 1.0, overlap_threshold = 0.5)

# Western Europe zoomed panel
europe_map <- ggplot() +
  geom_polygon(data = world_map %>% filter(long >= europe_bounds$long[1] & 
                                             long <= europe_bounds$long[2] &
                                             lat >= europe_bounds$lat[1] & 
                                             lat <= europe_bounds$lat[2]), 
               aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "darkgrey", linewidth = 0.2) +
  # Draw lines from original to spread positions (subtle, like Nature paper)
  {if(!is.null(df_europe) && any(df_europe$was_moved))
    geom_segment(data = df_europe %>% filter(was_moved),
                 aes(x = long_orig, y = lat_orig, xend = long_spread, yend = lat_spread),
                 linewidth = 0.3, color = "#31583c", alpha = 0.5)
  } +
  # Draw points at spread positions
  {if(!is.null(df_europe))
    geom_point(data = df_europe, 
               aes(x = long_spread, y = lat_spread, fill = Ecosystem_Type),
               pch = 21, size = 0.5, alpha = 0.6, stroke = 0.4, color = "black")
  } +
  scale_fill_manual(values = c("Lentic" = pal[5], "Lotic" = pal[2])) +
  coord_fixed(ratio = 0.75*1/cos(mean(europe_bounds$lat) * pi/180), 
              xlim = europe_bounds$long, ylim = europe_bounds$lat) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "chocolate1", linewidth = 0.6),
        plot.margin = margin(2, 2, 2, 2))

# North America zoomed panel
namerica_map <- ggplot() +
  geom_polygon(data = world_map %>% filter(long >= namerica_bounds$long[1] & 
                                             long <= namerica_bounds$long[2] &
                                             lat >= namerica_bounds$lat[1] & 
                                             lat <= namerica_bounds$lat[2]), 
               aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "darkgrey", linewidth = 0.2) +
  # Draw lines from original to spread positions
  {if(!is.null(df_namerica) && any(df_namerica$was_moved))
    geom_segment(data = df_namerica %>% filter(was_moved),
                 aes(x = long_orig, y = lat_orig, xend = long_spread, yend = lat_spread),
                 linewidth = 0.3, color = "black", alpha = 0.5)
  } +
  # Draw points at spread positions
  {if(!is.null(df_namerica))
    geom_point(data = df_namerica, 
               aes(x = long_spread, y = lat_spread, fill = Ecosystem_Type),
               pch = 21, size = 0.5, alpha = 0.6, stroke = 0.4, color = "black")
  } +
  scale_fill_manual(values = c("Lentic" = pal[5], "Lotic" = pal[2])) +
  coord_fixed(ratio = 1/cos(mean(namerica_bounds$lat) * pi/180), 
              xlim = namerica_bounds$long, ylim = namerica_bounds$lat) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "darkorchid2", linewidth = 0.6),
        plot.margin = margin(2, 2, 2, 2))

# Northern Scandinavia zoomed panel
scandinavia_map <- ggplot() +
  geom_polygon(data = world_map %>% filter(long >= scandinavia_bounds$long[1] & 
                                             long <= scandinavia_bounds$long[2] &
                                             lat >= scandinavia_bounds$lat[1] & 
                                             lat <= scandinavia_bounds$lat[2]), 
               aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "darkgrey", linewidth = 0.2) +
  # Draw lines from original to spread positions
  {if(!is.null(df_scandinavia) && any(df_scandinavia$was_moved))
    geom_segment(data = df_scandinavia %>% filter(was_moved),
                 aes(x = long_orig, y = lat_orig, xend = long_spread, yend = lat_spread),
                 linewidth = 0.3, color = "#48697c", alpha = 0.5)
  } +
  # Draw points at spread positions
  {if(!is.null(df_scandinavia))
    geom_point(data = df_scandinavia, 
               aes(x = long_spread, y = lat_spread, fill = Ecosystem_Type),
               pch = 21, size = 0.5, alpha = 0.6, stroke = 0.4, color = "black")
  } +
  scale_fill_manual(values = c("Lentic" = pal[5], "Lotic" = pal[2])) +
  coord_fixed(ratio = 0.75*1/cos(mean(scandinavia_bounds$lat) * pi/180), 
              xlim = scandinavia_bounds$long, ylim = scandinavia_bounds$lat) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", color = "#31583c", linewidth = 0.6),
        plot.margin = margin(2, 2, 2, 2))

# Create main map with rectangles showing zoomed regions
MAP_enhanced <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "darkgrey", linewidth = 0.2) +
  geom_point(data = df_b, aes(x = long, y = lat, fill = Ecosystem_Type),
             pch = 21, size = 1.0, alpha = 0.3) +
  # Add rectangles for zoom regions
  geom_rect(aes(xmin = europe_bounds$long[1], xmax = europe_bounds$long[2],
                ymin = europe_bounds$lat[1], ymax = europe_bounds$lat[2]),
            fill = NA, color = "chocolate1", linewidth = 0.8, linetype = "solid") +
  geom_rect(aes(xmin = namerica_bounds$long[1], xmax = namerica_bounds$long[2],
                ymin = namerica_bounds$lat[1], ymax = namerica_bounds$lat[2]),
            fill = NA, color = "darkorchid2", linewidth = 0.8, linetype = "solid") +
  geom_rect(aes(xmin = scandinavia_bounds$long[1], xmax = scandinavia_bounds$long[2],
                ymin = scandinavia_bounds$lat[1], ymax = scandinavia_bounds$lat[2]),
            fill = NA, color = "#31583c", linewidth = 0.8, linetype = "solid") +
  coord_fixed(ratio = 1.3) +
  theme_classic() +
  labs(fill = "", x = "Longitude", y = "Latitude") +
  scale_fill_manual(values = c("Lentic" = pal[5], "Lotic" = pal[2])) +
  theme(legend.position = "top",
        plot.margin = margin(0.75, 0.5, 0.5, 0.5, "cm"),
        text = element_text(size = 10),
        title = element_text(size = 10))

# Combine main map with inset panels positioned in ocean areas
# North America inset in the Pacific Ocean (left side)
# Western Europe inset in the central Pacific
# Northern Scandinavia inset in the South Pacific
MAP_with_insets <- MAP_enhanced +
  annotation_custom(ggplotGrob(namerica_map), 
                    xmin = -190, xmax = -82, ymin = -55, ymax = 25) +
  annotation_custom(ggplotGrob(europe_map), 
                    xmin = 28, xmax = 120, ymin = 50, ymax = 90) +
  annotation_custom(ggplotGrob(scandinavia_map), 
                    xmin = -15, xmax = 29, ymin = 60, ymax = 100)

# Display the map
print(MAP_with_insets)

# Save the enhanced map
ggsave("figures/MAP_figure1.jpg",
       plot = MAP_with_insets,
       width = 25, height = 12, units = "cm", dpi = 600)

################################################################################
# 3. PLOT HISTOGRAMS ----------
################################################################################


dflotic<-df %>% filter(Ecosystem_Type=="Lotic")
Lotic_C2<-ggplot(dflotic, aes(x = d13C,fill=Hab,color=Hab)) +
  geom_histogram(binwidth = 2, position = "stack", alpha=0.5) +
  scale_color_manual(values = c("Benthic"=pal[1], "Pelagic"=pal[2]))+
  scale_fill_manual(values = c("Benthic"=pal[1], "Pelagic"=pal[2]))+
  labs(x = expression(paste(delta^13,"C (\u2030)")),
       y = "Count",fill="",col="") +xlim(-42,-8)+
  theme_classic2()+
  theme(legend.position = c(0.8, 0.8),text = element_text(size = 8),title = element_text(size = 8),legend.key.size = unit(0.3, 'cm'))
 # theme(legend.position = "top")

dflentic<-df %>% filter(Ecosystem_Type=="Lentic")
Lentic_C2<-ggplot(dflentic, aes(x = d13C,fill=Hab,color=Hab)) +
  geom_histogram(binwidth = 2, position = "stack", alpha=0.5) +
  scale_color_manual(values = c("Benthic"=pal[6], "Pelagic"=pal[5]))+
  scale_fill_manual(values = c("Benthic"=pal[6], "Pelagic"=pal[5]))+
  labs(x = expression(paste(delta^13,"C (\u2030)")),
       y = "Count",fill="",col="") +xlim(-42,-8)+
  theme_classic2()+
  theme(legend.position = c(0.8, 0.8),text = element_text(size = 8),title = element_text(size = 8),legend.key.size = unit(0.3, 'cm'))
  #theme(legend.position = "top")


Lotic_N2<-ggplot(dflotic, aes(x = d15N,fill=Hab,color=Hab)) +
  geom_histogram(binwidth = 2, position = "stack", alpha=0.5) +
  scale_color_manual(values = c("Benthic"=pal[1], "Pelagic"=pal[2]))+
  scale_fill_manual(values = c("Benthic"=pal[1], "Pelagic"=pal[2]))+
  labs(x = expression(paste(delta^15,"N (\u2030)")),
       y = "Count",fill="Habitat",col="Habitat") +xlim(-4,20)+
  theme_classic2()+
 # theme(legend.position = c(0.8, 0.8))
  theme(legend.position = "none",text = element_text(size = 8),title = element_text(size = 8),legend=element_text(size=8))

Lentic_N2<-ggplot(dflentic, aes(x = d15N,fill=Hab,color=Hab)) +
  geom_histogram(binwidth = 2, position = "stack", alpha=0.5) +
  scale_color_manual(values = c("Benthic"=pal[6], "Pelagic"=pal[5]))+
  scale_fill_manual(values = c("Benthic"=pal[6], "Pelagic"=pal[5]))+
  labs(x = expression(paste(delta^15,"N (\u2030)")),
       y = "Count",fill="Habitat",col="Habitat" +xlim(-4,20))+
  theme_classic2()+
#  theme(legend.position = c(0.8, 0.8))
  theme(legend.position = "none",text = element_text(size = 8),title = element_text(size = 8),legend=element_text(size=8))

Hist<-ggarrange(Lentic_C2,Lotic_C2,Lentic_N2,Lotic_N2,ncol=2,nrow=2,labels = c("B", "C", "D","E"),font.label = list(size = 9, color = "black", face = "bold", family = NULL))

Hist


################################################################################
# 4. ARRANGE PANNELS ----------
################################################################################

Fig1<-ggarrange(MAP_with_insets,Hist,ncol=1,nrow=2,labels = c("A", ""),heights =c(4,1.9),font.label = list(size = 9, color = "black", face = "bold", family = NULL))
Fig1

# Save original version
jpeg("figures/Fig1.jpg",width=18,height=19.5,units="cm",res=800)
Fig1
dev.off()


