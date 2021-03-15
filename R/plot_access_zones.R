library("tidyverse")
library("config")
library("here")
library("sf")

# Read files ----

file_path <- here("results", get("projected_scenario"), "agents.txt")
agents <- read_delim(file_path, delim = "\t", col_names = TRUE) 
zones <- st_read(here("data", "helmet_zones_map.shp"))

# Parameters for plotting ----

limit_trips <- 100
limit_txt <- paste0("Zones with over ", limit_trips, " tours")
plot_areas <- c("helsinki_cbd", "helsinki_other", "espoo_vant_kau")

# Join agents tables ----

res_vars <- c("nr_tours0", "nr_tours1",
              "sustainable_access0", "sustainable_access1",
              "car_access0", "car_access1",
              "total_access0", "total_access1",
              "persons1", "persons0")

agents <- agents %>%
  group_by(number) %>%
  summarise_at(res_vars, sum, na.rm = TRUE) %>%
  filter(nr_tours1 > limit_trips)

# Calc differences ----

agents <- agents %>%
  mutate(tour_access1 = total_access1 / nr_tours1, 
         tour_access0 = total_access0 / nr_tours0,
         tour_sust_access1 = sustainable_access1 / nr_tours1, 
         tour_sust_access0 = sustainable_access0 / nr_tours0,
         tour_car_access1 = car_access1 / nr_tours1, 
         tour_car_access0 = car_access0 / nr_tours0,
         tour_access_dif = tour_access1 - tour_access0,
         tour_sust_access_dif = tour_sust_access1 - tour_sust_access0,
         tour_car_access_dif = tour_car_access1 - tour_car_access0)  

# Combine to shapefile ----

zones <- zones %>%
  left_join(agents, by=c("zone" = "number"))

# Plot accessibility ----

# all tours
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = tour_access1)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(high = hsl_cols("red"),
                      low = hsl_cols("white"),
                      na.value = hsl_cols("lightgray")) +
  labs(fill = "Utility (eur) / tour",
       title = "Accessibility of zone residents",
       subtitle = paste("All tours.", limit_txt)) +
  ggsave(here("results", get("projected_scenario"), "zones_access.png"),
         width = dimensions_map[1], 
         height = dimensions_map[2], 
         units = "cm")

# car tours
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = tour_car_access1)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(high = hsl_cols("red"),
                      low = hsl_cols("white"),
                      na.value = hsl_cols("lightgray")) +
  labs(fill = "Utility (eur) / tour",
       title = "Accessibility of zone residents",
       subtitle = paste("Car tours.", limit_txt)) +
  ggsave(here("results", get("projected_scenario"), "zones_car_access.png"),
         width = dimensions_map[1], 
         height = dimensions_map[2], 
         units = "cm")

# walk, bike and transit tours
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = tour_sust_access1)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(high = hsl_cols("red"),
                      low = hsl_cols("white"),
                      na.value = hsl_cols("lightgray")) +
  labs(fill = "Utility (eur) / tour",
       title = "Accessibility of zone residents",
       subtitle = paste("Walk, bike and transit", limit_txt)) +
  ggsave(here("results", get("projected_scenario"), "zones_sust_access.png"),
         width = dimensions_map[1], 
         height = dimensions_map[2], 
         units = "cm")

# Plot differences ----

# all tours
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = tour_access_dif)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient2(high = hsl_cols("red"),
                       low = hsl_cols("blue"),
                       mid = hsl_cols("white"),
                       na.value = hsl_cols("lightgray")) +
  labs(fill = "Utility change (eur)",
       title = "Change in average tour’s accessibility",
       subtitle = paste("All tours.", limit_txt)) +
  ggsave(here("results", get("projected_scenario"), "zones_access_dif.png"),
         width = dimensions_map[1], 
         height = dimensions_map[2], 
         units = "cm")

# car tours
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = tour_car_access_dif)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient2(high = hsl_cols("red"),
                       low = hsl_cols("blue"),
                       mid = hsl_cols("white"),
                       na.value = hsl_cols("lightgray")) +
  labs(fill = "Utility change (eur)",
       title = "Change in average tour’s accessibility",
       subtitle = paste("Car tours.", limit_txt)) +
  ggsave(here("results", get("projected_scenario"), "zones_car_access_dif.png"),
         width = dimensions_map[1], 
         height = dimensions_map[2], 
         units = "cm")

# walk, bike and transit tours
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = tour_sust_access_dif)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient2(high = hsl_cols("red"),
                       low = hsl_cols("blue"),
                       mid = hsl_cols("white"),
                       na.value = hsl_cols("lightgray")) +
  labs(fill = "Utility change (eur)",
       title = "Change in average tour’s accessibility",
       subtitle = paste("Walk, bike and transit.", limit_txt)) +
  ggsave(here("results", get("projected_scenario"), "zones_sust_access_dif.png"),
         width = dimensions_map[1], 
         height = dimensions_map[2], 
         units = "cm")

