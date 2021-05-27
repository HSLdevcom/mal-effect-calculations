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

agents_sums <- agents %>%
  filter(income_group %in% 7:10) %>%
  group_by(number) %>%
  summarise(high_income_persons = n()) %>%
  ungroup()

# Combine to shapefile ----

zones <- zones %>%
  left_join(agents_sums, by = c("zone" = "number"))

# Plot accessibility ----

# all tours
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = high_income_persons)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  labs(
    fill = "asukasta",
    title = config::get("present_name"),
    subtitle = "Väestö ylimmissä tulodesiileissä 70-100 %"
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "persons_high_income.png"
       ),
  width = dimensions_map[1],
  height = dimensions_map[2],
  units = "cm"
)
