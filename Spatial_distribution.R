setwd("D:/Review paper/Analysis/Distribution")

# 1. Install and Load Required Packages
# ------------------------------------------------------------------------------
required_packages <- c("tidyverse", "rnaturalearth", "rnaturalearthdata", 
                       "sf", "ggplot2", "scatterpie", "maps", "patchwork", "openxlsx")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all packages silently
invisible(lapply(required_packages, library, character.only = TRUE))


# 2. Get Country Coordinates (Calculated Once)
# ------------------------------------------------------------------------------
# We use a world map to find the center (centroid) of each country
world <- ne_countries(scale = "medium", returnclass = "sf")

# Calculate centroids safely and extract coordinates
country_centroids <- suppressWarnings(st_centroid(world)) %>%
  select(name, geometry) %>%
  mutate(
    long = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  ) %>%
  st_drop_geometry() # Convert back to a standard dataframe


# 3. Load and Prep the Original Data
# ------------------------------------------------------------------------------
# Load data 
df <- read.csv("Extracted_data.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Fix country name mismatches upfront so they apply to all subsequent subsets
df <- df %>%
  mutate(Nation = case_when(
    Nation == "USA" ~ "United States of America",
    Nation == "UK"  ~ "United Kingdom",
    TRUE ~ Nation
  ))


# 4. Data Processing Function
# ------------------------------------------------------------------------------
# A unified function to filter, count, reshape, join coordinates, and save to CSV
process_trend_data <- function(data, target_vars, output_file, centroids) {
  
  processed_df <- data %>%
    filter(Variables %in% target_vars) %>%
    group_by(Nation, Trend) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    pivot_wider(names_from = Trend, values_from = Count, values_fill = list(Count = 0))
  
  # Safeguard: Ensure +, -, and 0 columns exist even if a trend is missing from the subset
  if (!"+" %in% names(processed_df)) processed_df$`+` <- 0
  if (!"-" %in% names(processed_df)) processed_df$`-` <- 0
  if (!"0" %in% names(processed_df)) processed_df$`0` <- 0
  
  # Rename columns, calculate total, and join coordinates
  final_df <- processed_df %>%
    dplyr::rename(Increase = `+`, Decrease = `-`, Stable = `0`) %>%
    mutate(Total = Increase + Decrease + Stable) %>%
    left_join(centroids, by = c("Nation" = "name"))
  
  # Save to CSV
  write.csv(final_df, output_file, row.names = FALSE)
  message(paste("File", output_file, "has been created successfully."))
  
  return(final_df)
}


# 5. Process All Variables and Generate CSVs
# ------------------------------------------------------------------------------
# Define the large vector of primary producer variables
primary_producers <- c("Phytoplankton biomass", "Cyanobacteria biomass", 
                       "Phytoplankton species richness", "Phytoplankton genus richness", 
                       "HAB cell abundance", "Photosynthetic nanoplankton abundance",
                       "Diatoms abundance", "Submerged aquatic vegetation species composition index",
                       "Submerged aquatic vegetation abundance", "Submerged aquatic vegetation evenness",
                       "Submerged aquatic vegetation Shannon diversity", "Periphyton biomass",
                       "Periphyton primary productivity")

# Generate CSV datasets
tn_data        <- process_trend_data(df, "Total nitrogen", "tn_data.csv", country_centroids)
tp_data        <- process_trend_data(df, "Total phosphorus", "tp_data.csv", country_centroids)
si_data        <- process_trend_data(df, "Silicate", "si_data.csv", country_centroids)
discharge_data <- process_trend_data(df, "Discharge", "discharge_data.csv", country_centroids)
temp_data      <- process_trend_data(df, "Water temperature", "temp_data.csv", country_centroids)
chla_data      <- process_trend_data(df, "Chlorophyll a", "chla_data.csv", country_centroids)
com_data       <- process_trend_data(df, primary_producers, "com_data.csv", country_centroids)


# 6. Define Mapping Parameters and Plotting Function
# ------------------------------------------------------------------------------
GLOBAL_SCALING_FACTOR <- 2.5 
world_map <- map_data("world")

generate_trend_map <- function(csv_file, variable_name, output_file, add_europe_inset = FALSE) {
  
  # Load data
  data <- read.csv(csv_file)
  
  # Calculate logarithmic radius using the GLOBAL scaling factor
  data$radius <- log(data$Total + 1) * GLOBAL_SCALING_FACTOR
  
  # Filter base map for highlighted countries
  highlighted_countries <- subset(world_map, region %in% data$Nation)
  
  # Create the MAIN map
  plot_main <- ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill = "whitesmoke", color = "lightgrey", linewidth = 0.2) +
    geom_polygon(data = highlighted_countries, aes(x = long, y = lat, group = group), 
                 fill = "lightyellow", color = "black", linewidth = 0.2) +
    geom_scatterpie(data = data, aes(x = long, y = lat, r = radius), 
                    cols = c("Decrease", "Stable", "Increase"), 
                    color = "black", linewidth = 0.2, alpha = 0.85) +
    scale_fill_manual(
      values = c("Decrease" = "#2166ac", "Stable" = "grey", "Increase" = "#1a9641"), 
      labels = c("Decrease" = "decreasing (-)", "Stable" = "no trend (0)", "Increase" = "increasing (+)"), 
      name = "trend"
    ) +
    coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-60, 90)) +
    theme_minimal() +
    labs(x = "longitude", y = "latitude") +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),
      legend.key.size = unit(1, "cm")
    )
  
  # Handle Europe Inset if required
  if (add_europe_inset) {
    plot_europe <- ggplot() +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                   fill = "whitesmoke", color = "lightgrey", linewidth = 0.2) +
      geom_polygon(data = highlighted_countries, aes(x = long, y = lat, group = group), 
                   fill = "lightyellow", color = "black", linewidth = 0.2) +
      geom_scatterpie(data = data, aes(x = long, y = lat, r = radius), 
                      cols = c("Decrease", "Stable", "Increase"), 
                      color = "black", linewidth = 0.2, alpha = 0.85) +
      scale_fill_manual(
        values = c("Decrease" = "#2166ac", "Stable" = "grey", "Increase" = "#1a9641"),
        labels = c("Decrease" = "decreasing (-)", "Stable" = "no trend (0)", "Increase" = "increasing (+)")
      ) +
      coord_fixed(ratio = 1, xlim = c(-25, 45), ylim = c(34, 72)) +
      theme_void() +
      theme(
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Bounding Box around Europe
    plot_main <- plot_main +
      annotate("rect", xmin = -25, xmax = 45, ymin = 34, ymax = 72, 
               color = "black", fill = NA, linewidth = 0.8) 
    
    # Combine maps using patchwork
    final_plot <- plot_main + inset_element(plot_europe, left = 0.01, bottom = 0.05, right = 0.35, top = 0.55)
    
    # Print and Save
    print(final_plot)
    ggsave(output_file, plot = final_plot, width = 12, height = 8, dpi = 300, bg = "white")
    
  } else {
    # Print and Save standard map
    print(plot_main)
    ggsave(output_file, plot = plot_main, width = 12, height = 8, dpi = 300, bg = "white")
  }
}


# 7. Generate All Maps
# ------------------------------------------------------------------------------
generate_trend_map("tn_data.csv", "TN", "tn_map.png")
generate_trend_map("tp_data.csv", "TP", "tp_map.png", add_europe_inset = TRUE)
generate_trend_map("si_data.csv", "DSi", "Dsi_map.png", add_europe_inset = TRUE)
generate_trend_map("discharge_data.csv", "discharge", "discharge_map.png", add_europe_inset = TRUE)
generate_trend_map("temp_data.csv", "water temperature", "temp_map.png", add_europe_inset = TRUE)
generate_trend_map("chla_data.csv", "Chlorophyll-a", "chla_map.png")
generate_trend_map("com_data.csv", "primary producer", "com_map.png")
