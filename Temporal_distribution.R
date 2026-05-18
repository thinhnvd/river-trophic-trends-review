# --- 1. Setup and Libraries ---
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(scales)) install.packages("scales")
library(tidyverse)
library(scales)

# --- 2. Load Data ---
raw_data <- read.csv("Extracted_data.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

# --- 3. Clean, Process, and Global Scale ---
all_parameters <- c(
  "nitrate", "total nitrogen", "phosphate", "total phosphorus", 
  "dissolved silicon", "water discharge", "water temperature", "Chlorophyll-a"
)

processed_data <- raw_data %>%
  dplyr::select(Parameter = 2, Trend = 3, Period = 5) %>%
  filter(Parameter %in% all_parameters) %>%
  
  # Handle Periods
  separate_rows(Period, sep = ",\\s*") %>%
  separate(Period, into = c("Start_Year", "End_Year"), sep = "-", convert = TRUE) %>%
  drop_na(Start_Year, End_Year) %>%
  
  # Expand Years
  rowwise() %>%
  mutate(Year_Point = list(seq(from = Start_Year, to = End_Year))) %>%
  unnest(Year_Point) %>%
  ungroup() %>%
  
  # --- GLOBAL SCALING ---
  group_by(Parameter, Trend) %>%
  mutate(obs_count = n()) %>%
  ungroup() %>%
  mutate(
    global_max = max(obs_count), 
    scaled_width = obs_count / global_max
  ) %>%
  
  # --- Rename and Set Factors ---
  mutate(
    # Rename the trends for the facet labels
    Trend = case_when(
      Trend == "-" ~ "decreasing (-)",
      Trend == "0" ~ "no trend (0)",
      Trend == "+" ~ "increasing (+)",
      TRUE ~ as.character(Trend)
    ),
    # This order dictates the left-to-right columns
    Trend = factor(Trend, levels = c("increasing (+)", "no trend (0)", "decreasing (-)")),
    Parameter = factor(Parameter, levels = all_parameters) 
  )

# --- 4. Generate Plot ---
# Updated color mapping to match the new Trend names
trend_colors <- c(
  "increasing (+)" = "#1a9641", 
  "decreasing (-)" = "#2166ac", 
  "no trend (0)" = "#bdbdbd"
)

combined_plot <- ggplot(processed_data, aes(x = Year_Point, y = "", color = Trend, fill = Trend)) +
  
  # Added a black vertical reference line at the year 1900
  geom_vline(xintercept = 1900, color = "black", linewidth = 0.5, linetype = "dashed") +
  
  # Violin
  geom_violin(
    aes(width = scaled_width), 
    alpha = 0.2, color = "black", linewidth = 0.3, scale = "width", trim = TRUE
  ) +
  # Points
  geom_point(
    size = 0.5, alpha = 0.3, 
    position = position_jitter(height = 0.15, width = 0.2)
  ) +
  # Facet by Parameter (rows) AND Trend (columns)
  facet_grid(Parameter ~ Trend, switch = "y") +
  
  # Formatting
  scale_x_continuous(name = "time (year)", breaks = scales::pretty_breaks(n = 6)) + 
  scale_y_discrete(name = NULL) + 
  scale_color_manual(values = trend_colors) +
  scale_fill_manual(values = trend_colors) +
  
  theme_minimal() +
  theme(
    # Row Strips (Parameter Names) on the LEFT
    strip.text.y.left = element_text(face = "bold", size = 12, angle = 0, hjust = 1), 
    strip.placement = "outside",
    
    # Column Strips (Trends) on the TOP
    strip.text.x = element_text(face = "bold", size = 16), 
    strip.background = element_blank(), 
    
    # Axes
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 14), 
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)), 
    
    # Grid/Legend
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.spacing = unit(0.5, "lines") 
  )

print(combined_plot)
