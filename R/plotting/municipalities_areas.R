# libraries ----
library(sf)
library(tidyverse)

# load data ----
areas <- st_read(here::here("data", "areas.shp"))

# plot ----
ggplot() +
  geom_sf(data = areas, fill = hsl_cols("lightgray"),
          color = 'white', size = 1) +
  theme_maps +
  ggsave(
    here::here("results", "areas.png"),
    width = dimensions_map[1], 
    height = dimensions_map[2], 
    units = "cm")

# plot ----
ggplot() +
  geom_sf(data = areas %>%
            filter(!area %in% "peripheral"), 
          fill = hsl_cols("lightgray"),
          color = 'white', size = 1) +
  theme_maps +
  ggsave(
    here::here("results", "areas_mal.png"),
    width = dimensions_map[1], 
    height = dimensions_map[2], 
    units = "cm")

# load data ----
municipalities <- st_read(here::here("data", "municipalities.shp"))

# plot ----
ggplot() +
  geom_sf(data = municipalities, fill = hsl_cols("lightgray"),
          color = 'white', size = 1) +
  theme_maps +
  ggsave(here::here("results", "municipalities.png"),
         width = dimensions_map[1], 
         height = dimensions_map[2], 
         units = "cm")

# plot ----
ggplot() +
  geom_sf(data = municipalities %>% 
            filter(mcp %in% c("Helsinki", "Espoo", "Vantaa", "Kauniainen")), 
          fill = hsl_cols("lightgray"),
          color = 'white', size = 1) +
  theme_maps +
  ggsave(here::here("results", "municipalities_pks.png"),
         width = dimensions_map[1], 
         height = dimensions_map[2], 
         units = "cm")
