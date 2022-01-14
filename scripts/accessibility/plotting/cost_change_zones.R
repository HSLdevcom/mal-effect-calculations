library(tidyverse)
library(config)
library(here)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")
source(here::here("scripts", "basemap", "functions_map.R"),
       encoding = "utf-8")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

zones <- readr::read_rds(here::here("results", "zones.rds"))

zones <- zones %>%
  dplyr::rename(zone = SIJ2019)

# Calc cost per month ----

agents <- agents %>%
  mutate(cost = cost * 30)

agents_0 <- agents_0 %>%
  mutate(cost = cost * 30)

agents_1 <- agents_1 %>%
  mutate(cost = cost * 30)

# Summarise by area ----

res_var <- c("cost", "income")

zones_agents <- agents %>%
  group_mean("number", res_var)

zones_agents_0 <- agents_0 %>%
  group_mean("number", res_var)

zones_agents_1 <- agents_1 %>%
  group_mean("number", res_var)

zones <- zones %>%
  left_join(zones_agents,
            by = c("zone" = "number")
            ) %>%
  left_join(zones_agents_0,
            by = c("zone" = "number"),
            suffix = c("", "_0")
            ) %>%
  left_join(zones_agents_1,
            by = c("zone" = "number"),
            suffix = c("", "_1")
            )

# Calculate measures ----

zones <- zones %>%
  mutate(cost_diff = cost_1 - cost_0) %>%
  filter_outliers("cost_diff")

# Plot costs in zones ----

breaks <- c(100, 75, 50, 25, 10)
breaks <- sort(c(breaks, -breaks))

# transport costs

ggplot() +
  geom_sf(mapping = aes(fill = cost_diff),
          data = zones, color = NA) +
  scale_fill_steps2(
    name = "eur / asukas / kk",
    limits = c(-100, 100),
    breaks = breaks,
    high = hsl_cols("red"),
    mid = hsl_cols("white"),
    low = hsl_cols("blue"),
    na.value = hsl_cols("lightgray")
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Liikkumisen suorat kustannukset (eur)",
    subtitle = paste0(
      "Muutos: ",
      config::get("projected_name"),
      " - ",
      config::get("baseline_name")
      )
    ) +
  theme_mal_map()

ggsave_map(
  here(
    "figures",
    config::get("projected_scenario"),
    "cost_change_transport.png"
  )
)
