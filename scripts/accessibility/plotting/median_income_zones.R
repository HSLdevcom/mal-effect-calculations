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

# Parameters for plotting ----

plot_areas <-
  c("helsinki_cbd",
    "helsinki_other",
    "espoo_vant_kau",
    "surround_train",
    "surround_other")

# Join agents tables ----

agents <- agents %>%
  group_by(number) %>%
  summarise(persons = n(),
            income = median(income)) %>%
  ungroup()

# Combine to shapefile ----

zones <- zones %>%
  left_join(agents, by = c("zone" = "number"))

# Plot accessibility ----

# all tours
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = income)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  labs(
    fill = "eur / kk",
    title = config::get("present_name"),
    subtitle = "Mediaanitulot"
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "median_income_persons.png"
       ),
  width = dimensions_map[1],
  height = dimensions_map[2],
  units = "cm"
)
