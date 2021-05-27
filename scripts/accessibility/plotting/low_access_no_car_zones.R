library(tidyverse)
library(config)
library(here)
library(sf)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

zones <- st_read(here("data", "helmet_zones_map.shp"))

# Join tours to agents data ----

agents <- agents %>%
  join_purpose_tours(tours, "sustainable_access", "ho")

agents_1 <- agents_1 %>%
  join_purpose_tours(tours_1, "sustainable_access", "ho")

# Filter persons with no car use ----

agents <- agents %>%
  filter(is_car_user)

agents_1 <- agents_1 %>%
  filter(is_car_user)

# Parameters for plotting ----

areas <-
  c("Helsingin kantakaupunki",
    "Muu Helsinki",
    "Espoo, Vantaa, Kau")

plot_areas <-
  c("helsinki_cbd",
    "helsinki_other",
    "espoo_vant_kau")

# Calculate tour access ----

agents <- agents %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = sustainable_access / nr_tours) %>%
  filter(area %in% areas)

agents_1 <- agents_1 %>%
  filter(nr_tours > 0) %>%
  mutate(tour_access = sustainable_access / nr_tours) %>%
  filter(area %in% areas)

# Calc differences ----

low_limit <-
  quantile(agents_1$tour_access, probs = 0.05, na.rm = TRUE)[[1]]

calc_low_access <- function(df, name) {
  df <- df %>%
    mutate(low_access = (tour_access < low_limit)) %>%
    group_by(number) %>%
    summarise(nr_tours = sum(nr_tours, na.rm = TRUE),
              low_access = sum(low_access, na.rm = TRUE),
              !!name := low_access / nr_tours) %>%
    ungroup() %>%
    filter(nr_tours > 50)
}

low_access_1 <- agents_1 %>%
  calc_low_access("projected")

# Combine to shapefile ----

zones <- zones %>%
  left_join(low_access_1, by = c("zone" = "number"))

# Plot accessibility ----

zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = projected)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray"),
    labels = scales::percent
  ) +
  labs(
    fill = "% autottomista asukkaista",
    title = get("projected_name"),
    subtitle = "Vertailutason alle jäävän autottoman väestön määrä"
  )

ggsave(
  here("figures",
       get("projected_scenario"),
       "zones_access_poor_no_car.png"
  ),
  width = dimensions_map[1],
  height = dimensions_map[2],
  units = "cm"
)
