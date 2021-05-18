library(tidyverse)
library(config)
library(here)
library(sf)

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

zones <- st_read(here("data", "helmet_zones_map.shp"))

# Parameters for plotting ----

plot_areas <-
  c("helsinki_cbd",
    "helsinki_other",
    "espoo_vant_kau",
    "surround_train",
    "surround_other")

# Calc zone residents ----

zone_persons <- agents %>%
  group_by(number) %>%
  summarise(n = n())


# Sum results ----

agents <- agents %>%
  mutate(tour_access = total_access / nr_tours)

agents_0 <- agents_0 %>%
  mutate(tour_access = total_access / nr_tours)

agents_1 <- agents_1 %>%
  mutate(tour_access = total_access / nr_tours)

# Calc differences ----

low_limit <-
  quantile(agents_0$tour_access, probs = 0.05, na.rm = TRUE)[[1]]

calc_low_access <- function(df, name) {
  df <- df %>%
    mutate(low_access = (tour_access < low_limit)) %>%
    group_by(number) %>%
    summarise(!!name := sum(low_access, na.rm = TRUE),
              nr_tours = sum(nr_tours, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(nr_tours > 100) %>%
    select(-nr_tours)
}

low_access_0 <- agents_0 %>%
  calc_low_access("baseline")

low_access_1 <- agents_1 %>%
  calc_low_access("projected")

# Combine to shapefile ----

zones <- zones %>%
  left_join(low_access_0, by = c("zone" = "number")) %>%
  left_join(low_access_1, by = c("zone" = "number"))

zones <- zones %>%
  mutate(change = projected - baseline)

# Plot accessibility ----

zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = projected)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  labs(
    fill = "asukasta",
    title = get("projected_name"),
    subtitle = "Vertailutason alle jäävän väestön määrä"
  )

ggsave(
  here("figures",
       get("projected_scenario"),
       "zones_access_poor.png"
       ),
  width = dimensions_map[1],
  height = dimensions_map[2],
  units = "cm"
)
