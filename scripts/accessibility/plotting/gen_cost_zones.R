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

file_path <- here("data", config::get("housing_cost"))
housing_cost <- read_delim(file_path, delim = "\t")

zones <- st_read(here("data", "helmet_zones_map.shp"))

# Parameters for plotting ----

plot_areas <-
  c("helsinki_cbd",
    "helsinki_other",
    "espoo_vant_kau",
    "surround_train",
    "surround_other")

# Cost to month ----

agents <- agents %>%
  mutate(gen_cost = gen_cost * 30,
         cost = cost * 30)

# Join agents tables ----

res_var <- c("gen_cost", "cost", "time_cost")

agent_sums <- agents %>%
  mutate(time_cost = gen_cost - cost) %>%
  group_mean("number", res_var) %>%
  filter_outliers("gen_cost")

# Select vars for housing cost ----

housing_cost <- housing_cost %>%
  select(zone, askust_kalib)

# for housing cost zeros are actually missing values
housing_cost<- housing_cost %>%
  mutate(askust_kalib = if_else(
    askust_kalib <0.01, NA_real_, askust_kalib))

# Combine to shapefile ----

zones <- zones %>%
  left_join(agent_sums,
            by = c("zone" = "number"),
            suffix = c("", "1")) %>%
  left_join(housing_cost, by = c("zone" = "zone"))

# Plot costs in zones ----

# transport costs
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = gen_cost)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(
    high = hsl_cols("blue"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  labs(
    fill = "eur / asukas / kk",
    title = config::get("present_name"),
    subtitle = "Liikkumisen yleistetyt kustannukset (eur)"
  )

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "gen_cost_transport_zone.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)

# housing + transport costs
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = gen_cost + askust_kalib)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  labs(
    fill = "eur / asukas / kk",
    title = config::get("present_name"),
    subtitle =
      "Asumisen ja liikkumisen yleistetyt kustannukset (eur)"
  )

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "gen_cost_housing_transport.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)

# transport costs
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = cost)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(
    high = hsl_cols("blue"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  labs(
    fill = "eur / asukas / kk",
    title = config::get("present_name"),
    subtitle = "Liikkumisen suorat kustannukset (eur)"
  )

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "cost_transport_zone.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)


# housing costs
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = askust_kalib)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  labs(
    fill = "eur / asukas / kk",
    title = config::get("present_name"),
    subtitle = "Asumisen kustannukset (eur)"
  )

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "cost_housing_zone.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)

# housing + transport costs
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = cost + askust_kalib)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  labs(
    fill = "eur / asukas / kk",
    title = config::get("present_name"),
    subtitle =
      "Asumisen ja liikkumisen suorat kustannukset (eur)"
  )

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "cost_housing_transport_zone.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)
