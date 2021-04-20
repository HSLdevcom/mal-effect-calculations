# libraries ----
library(sf)
library(tidyverse)

# load data ----
zones<- st_read(here::here("data", "helmet_zones_map.shp"))

# areas ----
areas <- zones %>%
  st_buffer(dist = 100) %>%
  group_by(area) %>%
  summarise(nr_zones = n())

areas <- areas %>% 
  st_union(by_feature = TRUE) %>%
  st_simplify(dTolerance = 100)

areas %>% 
  st_write(here::here("data", "areas.shp"),
           delete_dsn = TRUE)

# municipalities ----
municipalities <- zones %>%
  st_buffer(dist = 100) %>%
  group_by(mcp) %>%
  summarise(nr_zones = n())

municipalities <- municipalities %>% 
  st_union(by_feature = TRUE) %>%
  st_simplify(dTolerance = 100)

municipalities %>% 
  st_write(here::here("data", "municipalities.shp"),
           delete_dsn = TRUE)
