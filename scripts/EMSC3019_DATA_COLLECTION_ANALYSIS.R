################################################################################
### EMSC3019 - DATA COLLECTION & ANALYSIS                                    ###
################################################################################

# Author(s): Johann Wagner

# Purpose: To load, clean, visualise, and statistically model collected mangrove
# data from the EMSC3019 Coral Reef Field Trip 2022.





# Setup & Configuration --------------------------------------------------------

library(readxl)
library(tidyverse)
library(sf)
library(ggmap)
library(ggthemes)
library(basemaps)




# Data Loading -----------------------------------------------------------------

FOREST_RAW  <- read_xlsx("data/forest_polygon.xlsx")
SAPLING_RAW <- read_xlsx("data/sapling_points.xlsx")
# EXTENT_RAW  <- read_sf("extent.shp")
# 
# 
# BASEMAP <- basemap_ggplot(
#   ext = EXTENT_RAW
# )
# 
# x <- draw_ext()
# 
# write_sf(x, "extent.shp")

# Data Cleaning ----------------------------------------------------------------

#* Raw into Cleaned ------------------------------------------------------------
FOREST_CLEANED <- FOREST_RAW %>%
  
  # Keep only relevant data and prepare data into tidy format
  transmute(
    geo_data = forest_edge %>% 
      str_split(
        pattern = ";"
      )
  ) %>% 
  
  unnest(geo_data) %>% 
  
  separate(
    col   = geo_data,
    into  = c("lat", "lon", "var_1", "var_2"),
    sep   = " ",
    remove = TRUE,
    convert = TRUE
  ) %>% 
  
  mutate(type = "Perimeter")

SAPLING_CLEANED <- SAPLING_RAW %>% 
  
  # Ensure correct data type for each variable
  transmute(
    lon        = `geopoint-Longitude` %>% as.numeric(),
    lat        = `geopoint-Latitude`  %>% as.numeric(),
    leaf_count = leaf                 %>% as.integer(),
    height     = height               %>% as.numeric(),
    alive      = alive                %>% factor(
      levels = c(
        "Alive",
        "Dead"
      )
    )
  )

#* Spatial Calculations ------------------------------------------------------------

# Calculate the forest centroid point
FOREST_CLEANED_CENTROID_SF <- FOREST_CLEANED %>%
  
  # Convert into sf class
  st_as_sf(
    coords = c("lon", "lat"),
    crs    = 4326
  ) %>% 
  
  summarise(
    geometry = geometry %>%
      st_combine()
  ) %>%
  
  st_centroid()

FOREST_CLEANED_CENTROID <- FOREST_CLEANED_CENTROID_SF %>% 
  
  # Extract longitude and latitude from sf class
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    type = "Centroid"
  )
  

SAPLING_CLEANED_COMBINED <- SAPLING_CLEANED %>% 
  
  mutate(
    distance = {
      SAPLING_CLEANED %>% 
        
        # Convert into sf class
        st_as_sf(
          coords = c("lon", "lat"),
          crs    = 4326
        ) %>% 
        
        # Calculate Euclidian distance
        st_distance(
          FOREST_CLEANED_CENTROID_SF
        ) %>% 
        
        # Tibble is best!
        as_tibble() %>% 
        
        # Data cleaning
        transmute(
          distance = value %>% 
            str_remove(pattern = " [m]") %>% 
            as.numeric()
        ) %>% 
        
        pull()
      }
    )




# Data Visualisation -----------------------------------------------------------

# Create ggmap template
ggmap_template <- ggmap(BASEMAP) +
  theme_clean() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  )

# Create spatial visualisation of mangrove forest data.
ggmap_template +
  
  # Forest perimeter
  geom_point(
    data = FOREST_CLEANED,
    aes(
      x     = lon,
      y     = lat,
      color = type
    )
  ) +
  
  # Forest Centroid
  geom_point(
    data = FOREST_CLEANED_CENTROID,
    aes(
      x      = lon,
      y      = lat,
      color  = type
    ),
    size   = 5,
    shape  = 18
  ) +
  scale_color_manual(
    values = c(
      "Centroid"  = "#ADD8E6",
      "Perimeter" = "#FFCCCB"
    )
  ) +
  labs(
    color = "Mangrove Forest",
    title = "Spatial Visualisation of Mangrove Forest Centroid"
  )

# Create spatial visualisation of mangrove sapling data.
ggmap_template +
  
  # Forest Centroid
  geom_point(
    data = FOREST_CLEANED_CENTROID,
    aes(
      x = lon,
      y = lat
    ),
    color = "#ADD8E6",
    size   = 5,
    shape = 18,
    show.legend = FALSE
  ) +
  geom_label(
    data = FOREST_CLEANED_CENTROID,
    aes(
      x     = lon,
      y     = lat,
      label = "Centroid"
    ),
    vjust = 0,
    nudge_y = -0.00008,
    size = 4
  ) +
  
  # Sapling Distribution
  geom_point(
    data = SAPLING_CLEANED_COMBINED,
    aes(
      x     = lon,
      y     = lat,
      shape = alive,
      size  = height,
      color = leaf_count
    )
  ) +
  scale_color_gradient(
    low = "red",
    high = "yellow",
    breaks = c(3, 6, 9),
    guide = guide_colorbar(
      direction = "horizontal"
    )
  ) +
  labs(
    shape = "Survival Status",
    size  = "Height",
    color = "Leaf Count",
    title = "Spatial Visualisation of Mangrove Sapling Distribution"
  )





# Data Modelling -----------------------------------------------------------

SAPLING_CLEANED_COMBINED_MODEL <- SAPLING_CLEANED_COMBINED %>% 
  
  mutate(
    alive = case_when(
        alive == "Alive" ~ 1,
        alive == "Dead"  ~ 0
      )
  )
  
model_1 <- glm(
  data = SAPLING_CLEANED_COMBINED_MODEL,
  formula = alive ~ distance,
  family = binomial(link = "logit")
)

model_2 <- glm(
  data = SAPLING_CLEANED_COMBINED_MODEL,
  formula = height ~ distance
)

model_3 <- glm(
  data = SAPLING_CLEANED_COMBINED_MODEL,
  formula = leaf_count ~ distance,
  family = poisson(link = "log")
)

summary(model_1)
summary(model_2)
summary(model_3)
