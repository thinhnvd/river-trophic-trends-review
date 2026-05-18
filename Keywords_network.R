# ==============================================================================
# 1. Load Necessary Libraries & Data
# ==============================================================================
# Install missing packages automatically
# ADDED patchwork for subfigure layout
required_packages <- c("readr", "dplyr", "tidyr", "widyr", "igraph", "ggraph", "patchwork")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(widyr)
library(igraph)
library(ggraph)
library(patchwork) 

# Load the dataset
df <- read_csv("Bibliographic.csv")

# ==============================================================================
# 2. Clean, Combine, and Standardize Keywords
# ==============================================================================
tidy_keywords <- df %>%
  select(`No.`, Year, `Author Keywords`, `Index Keywords`) %>%
  unite("Combined_Keywords", `Author Keywords`, `Index Keywords`, sep = "; ", na.rm = TRUE) %>%
  filter(Combined_Keywords != "") %>%
  separate_rows(Combined_Keywords, sep = ";\\s*") %>%
  mutate(Cleaned_Keyword = trimws(tolower(Combined_Keywords))) %>%
  filter(Cleaned_Keyword != "") %>%
  mutate(
    Cleaned_Keyword = case_when(
      grepl("eutrophication", Cleaned_Keyword) ~ "eutrophication",
      Cleaned_Keyword == "agricultural catchments" ~ "agricultural catchment",
      Cleaned_Keyword %in% c("agricultural policies", "agricultural policy measures") ~ "agricultural policy",
      Cleaned_Keyword %in% c("agricultural practices", "agricultural procedures") ~ "agricultural practice",
      Cleaned_Keyword == "agricultural land use" ~ "agricultural land",
      Cleaned_Keyword == "alga" ~ "algae",
      Cleaned_Keyword == "algal blooms" ~ "algal bloom",
      Cleaned_Keyword == "animals" ~ "animal",
      Cleaned_Keyword == "anthropogenic sources" ~ "anthropogenic source",
      Cleaned_Keyword %in% c("aquatic ecosystems", "aquatic system") ~ "aquatic ecosystem",
      Cleaned_Keyword == "aquatic environments" ~ "aquatic environment",
      Cleaned_Keyword == "atmospheric depositions" ~ "atmospheric deposition",
      Cleaned_Keyword %in% c("bacteria (microorganisms)", "bacterium") ~ "bacteria",
      Cleaned_Keyword == "bays" ~ "bay",
      Cleaned_Keyword == "big river" ~ "large rivers",
      Cleaned_Keyword == "case-studies" ~ "case study",
      Cleaned_Keyword == "best management practices" ~ "best management practice",
      Cleaned_Keyword == "biogeochemical cycling" ~ "biogeochemical cycle",
      Cleaned_Keyword == "carbon isotopes" ~ "carbon isotope",
      Cleaned_Keyword == "catchments" ~ "catchment",
      Cleaned_Keyword == "catchment area (hydrology)" ~ "catchment area",
      Cleaned_Keyword == "change-point analysis" ~ "change point analysis",
      Cleaned_Keyword %in% c("changjiang yangtze river", "changjiang yangtze-river", "changjiang river", "changjiang", "changjiang (yangtze river)", "changjiang estuary", "changjiang river basin", "changjiang river estuary", "yangtze basin", "yangtze estuary") ~ "yangtze river",
      Cleaned_Keyword == "chlorides" ~ "chloride",
      Cleaned_Keyword == "chemical and biological variables" ~ "chemical and biologicals",
      Cleaned_Keyword %in% c("chlorophyll", "chlorophyll-a", "chlorophyll content") ~ "chlorophyll a",
      Cleaned_Keyword %in% c("climate-change", "climate change impact", "climatic changes") ~ "climate change",
      Cleaned_Keyword %in% c("climate", "climate effect", "climate forcing", "climatic effects", "climatology") ~ "climate variability",
      Cleaned_Keyword == "coastal ecosystems" ~ "coastal ecosystem",
      Cleaned_Keyword == "coastal waters" ~ "coastal water",
      Cleaned_Keyword %in% c("coastal zones", "coastal zone management") ~ "coastal zone",
      Cleaned_Keyword == "community structures" ~ "community structure",
      Cleaned_Keyword %in% c("concentration parameters", "concentration parameter", "concentration (composition)", "concentration (parameter)", "concentration (parameters)", "concentration (process)", "concentration of", "concentration patterns") ~ "concentration",
      Cleaned_Keyword == "cyanobacteria blooms" ~ "cyanobacterial blooms",
      Cleaned_Keyword == "conservation lands" ~ "conservation areas",
      Cleaned_Keyword == "copepods" ~ "copepoda",
      Cleaned_Keyword == "copper compounds" ~ "copper",
      Cleaned_Keyword == "corbicula" ~ "corbicula clams",
      Cleaned_Keyword == "cyanobacterium" ~ "cyanobacteria",
      Cleaned_Keyword %in% c("dam (barrier)", "dam construction", "damming effect", "dams") ~ "dam",
      Cleaned_Keyword == "danube basin" ~ "danube",
      Cleaned_Keyword == "decomposition technique" ~ "decomposition",
      Cleaned_Keyword %in% c("diffuse pollution control", "diffusion pollution control") ~ "diffuse pollution",
      Cleaned_Keyword == "data resources" ~ "data resource",
      Cleaned_Keyword == "data sets" ~ "data set",
      Cleaned_Keyword == "diatoms" ~ "diatom",
      Cleaned_Keyword %in% c("dissolved inorganic nitrogens", "dissolved inorganic n") ~ "dissolved inorganic nitrogen",
      Cleaned_Keyword %in% c("ecological restoration project", "ecological restoration projects") ~ "ecological restoration",
      Cleaned_Keyword == "ecosystems" ~ "ecosystem",
      Cleaned_Keyword %in% c("e-coli concentrations", "escherichia coli") ~ "e. coli",
      Cleaned_Keyword %in% c("estuaries", "estuary") ~ "estuarine",
      Cleaned_Keyword == "elbe estuary" ~ "elbe river",
      Cleaned_Keyword == "effluents" ~ "effluent",
      Cleaned_Keyword == "environmental factors" ~ "environmental factor",
      Cleaned_Keyword == "environmental impact assessment" ~ "environmental impact",
      Cleaned_Keyword == "environmental modelling" ~ "environmental modeling",
      Cleaned_Keyword == "environmental pollutions" ~ "environmental pollution",
      Cleaned_Keyword == "eutrophic rivers" ~ "eutrophic river",
      Cleaned_Keyword %in% c("fertilizers", "fertilizer application") ~ "fertilizer",
      Cleaned_Keyword == "flooding" ~ "flood",
      Cleaned_Keyword == "florida [united states]" ~ "florida",
      Cleaned_Keyword == "flow of water" ~ "flow",
      Cleaned_Keyword == "fishes" ~ "fish",
      Cleaned_Keyword %in% c("fresh water", "fresh water resources", "freshwater environment") ~ "freshwater ecosystem",
      Cleaned_Keyword %in% c("geographic information systems", "geographic information system analyse", "gis analysis") ~ "geographic information system",
      Cleaned_Keyword == "great lakes [north america]" ~ "great lakes region",
      Cleaned_Keyword %in% c("harmful algae bloom", "harmful algal blooms", "harmful algal bloom (hab)") ~ "harmful algal bloom",
      Cleaned_Keyword == "han river [far east]" ~ "han river",
      Cleaned_Keyword == "high temperature effects" ~ "high temperature",
      Cleaned_Keyword == "humans" ~ "human",
      Cleaned_Keyword == "human activity" ~ "human activities",
      Cleaned_Keyword %in% c("invertebrata", "invertebrates") ~ "invertebrate",
      Cleaned_Keyword == "irrigation (agriculture)" ~ "irrigation",
      Cleaned_Keyword == "jiulong basin" ~ "jiulong river",
      Cleaned_Keyword == "knowledge gaps" ~ "knowledge gap",
      Cleaned_Keyword == "lag-time" ~ "lag time",
      Cleaned_Keyword %in% c("land-use changes", "land use", "land use and land cover", "land use and land cover change", "land use change", "land-use/land cover change") ~ "land use/land cover change",
      Cleaned_Keyword == "land management practices" ~ "land management",
      Cleaned_Keyword == "law" ~ "laws and legislation",
      Cleaned_Keyword == "light-attenuation coefficients" ~ "light attenuation",
      Cleaned_Keyword == "livestock farming" ~ "livestock",
      Cleaned_Keyword == "lakes" ~ "lake",
      Cleaned_Keyword == "legacy nutrients" ~ "legacy nutrient",
      Cleaned_Keyword == "loading" ~ "load",
      Cleaned_Keyword == "loire" ~ "loire river",
      Cleaned_Keyword %in% c("long term change", "long-term changes") ~ "long-term change",
      Cleaned_Keyword %in% c("long term monitoring", "long-term monitoring datum") ~ "long-term monitoring",
      Cleaned_Keyword %in% c("long term study", "long-term studies") ~ "long-term study",
      Cleaned_Keyword %in% c("long term trends", "long-term trends") ~ "long-term trend",
      Cleaned_Keyword == "longitudinal studies" ~ "longitudinal study",
      Cleaned_Keyword %in% c("mann-kendall test", "mann-kendall trend analysis", "mann-kendall trends") ~ "mann-kendall",
      Cleaned_Keyword == "macroinvertebrates" ~ "macroinvertebrate",
      Cleaned_Keyword == "maumee" ~ "maumee river",
      Cleaned_Keyword == "mediterranean sea" ~ "mediterranean",
      Cleaned_Keyword %in% c("mercury (element)", "mercury (metal)") ~ "mercury",
      Cleaned_Keyword == "mississippi basin" ~ "mississippi",
      Cleaned_Keyword == "macrophytes" ~ "macrophyte",
      Cleaned_Keyword == "multiple linear regressions" ~ "multiple linear regression",
      Cleaned_Keyword == "multivariate autoregressive model" ~ "multivariate autoregressive",
      Cleaned_Keyword == "n deposition" ~ "n-deposition",
      Cleaned_Keyword %in% c("nitrates", "nitrate-n", "nitrate concentration", "nitrate levels", "nitrate pollution", "nitrate flux") ~ "nitrate",
      Cleaned_Keyword == "nitrate stable isotopes" ~ "nitrate stable isotope",
      Cleaned_Keyword %in% c("nitrogen cycles", "nitrogen cycle", "nitrogen cycling") ~ "nitrogen cycle",
      Cleaned_Keyword %in% c("nonpoint sources", "nonpoint source", "nonpoint source pollution") ~ "non-point source",
      Cleaned_Keyword == "nutrients" ~ "nutrient",
      Cleaned_Keyword %in% c("nutrient budgets", "nutrient availability") ~ "nutrient budget",
      Cleaned_Keyword %in% c("nutrient concentrations", "nutrient levels") ~ "nutrient concentration",
      Cleaned_Keyword == "nutrient enrichments" ~ "nutrient enrichment",
      Cleaned_Keyword == "nutrient limitations" ~ "nutrient limitation",
      Cleaned_Keyword %in% c("nutrient loading", "nutrient loads") ~ "nutrient load",
      Cleaned_Keyword == "nutrient management efficiency" ~ "nutrient management",
      Cleaned_Keyword == "nutrient transportation" ~ "nutrient transport",
      Cleaned_Keyword == "nutrient use" ~ "nutrient use efficiency",
      Cleaned_Keyword == "parana [brazil]" ~ "parana river",
      Cleaned_Keyword %in% c("patuxent estuary", "patuxent river estuary") ~ "patuxent river",
      Cleaned_Keyword == "periphytons" ~ "periphyton",
      Cleaned_Keyword %in% c("ph effects", "ph measurement") ~ "ph",
      Cleaned_Keyword %in% c("phosphates", "orthophosphate") ~ "phosphate",
      Cleaned_Keyword %in% c("phosphorous", "phosphorus compounds", "phosphorus concentration", "phosphorus contents") ~ "phosphorus",
      Cleaned_Keyword == "phosphorus balances" ~ "phosphorus balance",
      Cleaned_Keyword == "phytoplankton composition" ~ "phytoplankton community",
      Cleaned_Keyword %in% c("point sources", "point source", "point-source pollution", "point source pollution", "point source emissions")  ~ "point-sources",
      Cleaned_Keyword == "polychlorinated biphenyls" ~ "polychlorinated biphenyl",
      Cleaned_Keyword == "poland [central europe]" ~ "poland",
      Cleaned_Keyword %in% c("policy implementation", "policy making") ~ "policy",
      Cleaned_Keyword == "potomac estuary" ~ "potomac river",
      Cleaned_Keyword == "prairie" ~ "prairie catchment",
      Cleaned_Keyword %in% c("precipitation (climatology)", "precipitation (meteorology)", "precipitation assessment", "rain", "rainfall intensity", "rainfall") ~ "precipitation",
      Cleaned_Keyword == "principal components analysis" ~ "principal component analysis",
      Cleaned_Keyword %in% c("reservoir impoundment", "reservoir management", "reservoirs (water)") ~ "reservoir",
      Cleaned_Keyword == "residuals analysis" ~ "residual analysis",
      Cleaned_Keyword %in% c("rivers", "stream (river)", "stream water", "stream", "river basins", "river basin", "rivers and streams", "rivers/streams", "river catchment", "river channel", "river systems", "river system", "river ecosystem", "river system managements") ~ "river",
      Cleaned_Keyword == "river runoffs" ~ "river runoff",
      Cleaned_Keyword == "river water" ~ "river water quality",
      Cleaned_Keyword == "rural areas" ~ "rural area",
      Cleaned_Keyword == "saskatchewan" ~ "saskatchewan river",
      Cleaned_Keyword == "scheldt basin" ~ "scheldt river",
      Cleaned_Keyword %in% c("sea water", "seawater") ~ "sea",
      Cleaned_Keyword %in% c("seasons", "season", "seasonal and interannual variability", "seasonal variability", "seasonal variation", "seasonality") ~ "seasonal changes",
      Cleaned_Keyword %in% c("sediments", "sedimentation") ~ "sediment",
      Cleaned_Keyword == "seine basin" ~ "seine river",
      Cleaned_Keyword %in% c("sewage treatment plant (stp)", "sewage treatment plants", "sewage treatment works") ~ "sewage treatment",
      Cleaned_Keyword %in% c("silicates", "silica", "silicate flux", "silicon") ~ "silicate",
      Cleaned_Keyword %in% c("soil-tests", "soil testing") ~ "soil test",
      Cleaned_Keyword == "soils" ~ "soil",
      Cleaned_Keyword == "soluble reactive phosphorus (srp)" ~ "soluble reactive phosphorus",
      Cleaned_Keyword %in% c("soil and water assessment tool model", "soil and water assessment tools") ~ "soil and water assessment tool",
      Cleaned_Keyword == "spatial variations" ~ "spatial variation",
      Cleaned_Keyword == "southern africa" ~ "south africa",
      Cleaned_Keyword == "stable isotopes" ~ "stable isotope",
      Cleaned_Keyword == "statistics" ~ "statistical analysis",
      Cleaned_Keyword == "streamflow" ~ "stream flow",
      Cleaned_Keyword == "streamwater" ~ "stream water",
      Cleaned_Keyword %in% c("submerged aquatic vegetations", "submersed aquatic vegetation", "submerged vegetation") ~ "submerged aquatic vegetation",
      Cleaned_Keyword == "subtropical reservoirs" ~ "subtropical reservoir",
      Cleaned_Keyword %in% c("surface waters", "surface water quality") ~ "surface water",
      Cleaned_Keyword %in% c("suspended solid", "suspended solids", "suspended load", "suspended matters", "suspended particulate matter", "suspended sediment") ~ "total suspended solids",
      Cleaned_Keyword == "time factors" ~ "time factor",
      Cleaned_Keyword == "temperature measurement" ~ "temperature",
      Cleaned_Keyword %in% c("time-series analysis", "time series") ~ "time series analysis",
      Cleaned_Keyword == "toxic materials" ~ "toxic material",
      Cleaned_Keyword %in% c("trend detection method", "trend study", "trend test") ~ "trend analysis",
      Cleaned_Keyword == "tributaries" ~ "tributary",
      Cleaned_Keyword == "trophic status" ~ "trophic state",
      Cleaned_Keyword == "uk" ~ "united kingdom",
      Cleaned_Keyword == "urban waste water treatment directives" ~ "urban waste water treatment directive",
      Cleaned_Keyword %in% c("urban waters", "urban stream", "urban water") ~ "urban river",
      Cleaned_Keyword == "usa" ~ "united states",
      Cleaned_Keyword %in% c("usa north carolina pamlico river estuary", "usa, north carolina, pamlico estuary", "usa, north carolina, pamlico river estuary") ~ "usa north carolina pamlico estuary",
      Cleaned_Keyword == "vaal rivers" ~ "vaal river",
      Cleaned_Keyword == "vegetation type" ~ "vegetation",
      Cleaned_Keyword %in% c("waste water", "waste waters") ~ "wastewater",
      Cleaned_Keyword %in% c("waste water treatment plant", "waste water treatment", "wastewater treatment plant") ~ "wastewater treatment",
      Cleaned_Keyword == "water levels" ~ "water level",
      Cleaned_Keyword == "water pollutants" ~ "water pollutant",
      Cleaned_Keyword %in% c("water pollution - water quality", "water pollution control", "water pollution control strategies", "water pollution, chemical") ~ "water pollution",
      Cleaned_Keyword %in% c("water resources", "water resources management") ~ "water resource",
      Cleaned_Keyword == "water temperatures" ~ "water temperature",
      Cleaned_Keyword %in% c("watersheds", "watershed management") ~ "watershed",
      Cleaned_Keyword == "watersheds management" ~ "watershed management",
      TRUE ~ Cleaned_Keyword 
    )
  ) %>%
  distinct(`No.`, Year, Cleaned_Keyword)

# ==============================================================================
# 3. Define Keyword Categories
# ==============================================================================
n_terms <- c(
  "nitrogen", "nitrate", "nitrite", "ammonia", "ammonium", "denitrification", 
  "nitrification", "\\btn\\b", "dsi:din ratios", "n-deposition", "n mineralization", 
  "n pollutions", "n surplus", "net anthropogenic n", "nitric acid derivative", 
  "nitrous oxide"
)

p_terms <- c(
  "phosphorus", "phosphate", "\\btp\\b", "orthophosphate", "srp", "tdp", "dip", 
  "p availabilities", "p surplus", "phosphoric acid"
)

chl_terms <- c(
  "chl a", "chl-a", "chlorophyll a", "chlorophyll"
)

comm_terms <- c(
  "community", "community structure", "assemblage", "anabaena", "aquatic organisms", 
  "aquatic plant", "attached algae", "atyaephyra desmarestii", "bacillariophyceae", 
  "bacillariophyta", "bacteria", "bacterial count", "benthic fish", "benthos", 
  "biodiversity", "bivalve", "bivalvia", "blue green algae", "brachycentridae", 
  "chlorophyta", "ciliophora", "cladophora glomerata", "coliform bacterium", 
  "copepoda", "corbicula clams", "crocodylidae \\(all crocodiles\\)", "crustacean", 
  "cyanobacteria", "cyprinid", "diatom", "didymo", "didymosphenia geminata", 
  "dinoflagellate", "dinophyceae", "e\\. coli", "eukaryota", "filamentous algae", 
  "fish", "freshwater macrophytes", "fundulus diaphanus", "fungal biomass", 
  "green alga", "harmful algae", "harmful algal bloom", "invasive species", 
  "invertebrate", "macroalga", "macroinvertebrate", "macrophyte", "macrozoobenthos", 
  "mastigophora \\(flagellates\\)", "microbial biomass", "microbial growth", 
  "microcystis", "microcystis aeruginosa", "microorganisms", "micropterus", 
  "micropterus salmoides", "mnemiopsis leidyi", "molluscs", "morone americana", 
  "perciform", "periphyton", "phytoplankton", "phytoplankton abundances", 
  "phytoplankton biomass", "phytoplankton growth", "stream fish", "plankton", 
  "polychaete", "species composition", "species diversity", "stephanodiscus hantzschii", 
  "population abundance", "population density", "potamanthidae", "potamoplankton", 
  "primary producers", "primary production", "prorocentrum dentatum", 
  "pseudomonas aureofaciens", "salmo salar", "salmonid", "satanoperca pappaterra", 
  "shellfish", "submerged aquatic vegetation", "skeletonema costatum", "taxonomy", 
  "zooplankton", "algal biomass", "toxic microorganisms", "trachydoras paraguayensis"
)

clim_terms <- c(
  "climate", "climate change", "climate variability", "temperature", "precipitation", 
  "warming", "weather", "rainfall", "wind", "meteorolog", "atmospheric", "drought", 
  "season", "seasonal changes", "monsoon", "el nino", "solar radiation", 
  "concentration-discharge", "discharge", "water discharges", "water level", 
  "water flow", "\\bflow\\b", "flow measurement", "flow patterns", "runoff", 
  "hydraulics", "streamflow regimes", "stream flow", "hydrodynamics", 
  "hydrological condition", "hydrological factors", "hydrological regime", 
  "hydrological response", "high flow", "flow rate", "global change", 
  "greenhouse effect", "river discharge", "river runoff"
)

n_regex    <- paste(n_terms, collapse = "|")
p_regex    <- paste(p_terms, collapse = "|")
chl_regex  <- paste(chl_terms, collapse = "|")
comm_regex <- paste(comm_terms, collapse = "|")
clim_regex <- paste(clim_terms, collapse = "|")
all_categories_regex <- paste(c(n_regex, p_regex, chl_regex, comm_regex, clim_regex, "\\beutrophication\\b", "\\boligotrophication\\b"), collapse = "|")

# ==============================================================================
# 4. Calculate Co-occurrence and Filter
# ==============================================================================
keyword_pairs <- tidy_keywords %>%
  pairwise_count(item = Cleaned_Keyword, feature = `No.`, sort = TRUE)

# ORIGINAL PAIRS
filtered_pairs_all <- keyword_pairs %>%
  filter(
    grepl(all_categories_regex, item1, ignore.case = TRUE) &
      grepl(all_categories_regex, item2, ignore.case = TRUE)
  ) %>%
  mutate(
    edge_category = case_when(
      item1 == "eutrophication" | item2 == "eutrophication" ~ "eutrophication link",
      item1 == "oligotrophication" | item2 == "oligotrophication" ~ "oligotrophication link",
      TRUE ~ "normal"
    )
  )

# PAIRWISE N >= 2
filtered_pairs_n2 <- filtered_pairs_all %>%
  filter(n >= 2)

# ==============================================================================
# 5. Assign Categories to Nodes and Plot Network
# ==============================================================================

# Helper function to generate nodes to avoid repeating code
generate_nodes <- function(pairs_df) {
  data.frame(name = unique(c(pairs_df$item1, pairs_df$item2))) %>%
    mutate(
      category = case_when(
        name == "eutrophication" ~ "eutrophication",
        name == "oligotrophication" ~ "oligotrophication",
        grepl(n_regex, name, ignore.case = TRUE) ~ "nitrogen",
        grepl(p_regex, name, ignore.case = TRUE) ~ "phosphorus",
        grepl(chl_regex, name, ignore.case = TRUE) ~ "chlorophyll",
        grepl(comm_regex, name, ignore.case = TRUE) ~ "community",
        grepl(clim_regex, name, ignore.case = TRUE) ~ "climate",
        TRUE ~ "Other"
      ),
      node_size = ifelse(name %in% c("eutrophication", "oligotrophication"), 15, 3.5),
      text_size = ifelse(name %in% c("eutrophication", "oligotrophication"), 4, 3.5),
      display_name = ifelse(name %in% c("eutrophication", "oligotrophication"), toupper(name), name)
    )
}

nodes_all <- generate_nodes(filtered_pairs_all)
nodes_n2 <- generate_nodes(filtered_pairs_n2)

# Create Graph Objects
set.seed(2024) 
graph_all <- graph_from_data_frame(d = filtered_pairs_all, directed = FALSE, vertices = nodes_all)
set.seed(2024) 
graph_n2 <- graph_from_data_frame(d = filtered_pairs_n2, directed = FALSE, vertices = nodes_n2)


# Helper function to generate identical plot styling 
create_plot <- function(graph_obj) {
  ggraph(graph_obj, layout = "fr") +  
    geom_edge_link(aes(edge_alpha = n, edge_width = n, edge_color = edge_category)) +
    geom_node_point(aes(color = category, size = node_size)) +
    geom_node_text(aes(label = display_name, size = text_size), repel = TRUE, max.overlaps = Inf, fontface = "bold", color = "black") +
    scale_edge_width(range = c(0.5, 2)) + 
    scale_edge_color_manual(values = c(
      "eutrophication link" = "#1a9641", 
      "oligotrophication link" = "#2166ac", 
      "normal" = "gray50"
    )) +
    scale_size_identity() +
    scale_color_manual(values = c(
      "eutrophication"    = "#1a9641", 
      "oligotrophication" = "#2166ac", 
      "nitrogen"          = "#E41A1C", 
      "phosphorus"        = "#8c564b", 
      "chlorophyll"       = "#bcbd22", 
      "community"         = "#984EA3", 
      "climate"           = "#FF7F00", 
      "Other"             = "gray70"   
    )) +
    theme_void() + 
    labs(
      color = "keyword category",
      edge_color = "edge category"
    ) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 14),
      legend.key.size = unit(1.2, "cm") 
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 6)) 
    )
}

# Create individual plots
plot_all <- create_plot(graph_all)
plot_n2 <- create_plot(graph_n2)

# Combine plots using patchwork and collect the legend
combined_plot <- plot_all + plot_n2 + plot_layout(guides = "collect") & theme(legend.position = "right")

