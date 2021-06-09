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

areas <-
  c("Helsingin kantakaupunki",
    "Muu Helsinki",
    "Espoo, Vantaa, Kau")

plot_areas <-
  c("helsinki_cbd",
    "helsinki_other",
    "espoo_vant_kau")

# Sum results ----

res_var <- c("nr_tours", "sustainable_access", "total_access")
group_var <- c("area", "number")

agent_sums <- agents %>%
  group_sum(group_var, res_var) %>%
  mutate(tour_access = total_access / nr_tours,
         tour_sust_access = total_access / nr_tours
         )

agent_sums_0 <- agents_0 %>%
  group_sum(group_var, res_var) %>%
  mutate(
    tour_access = total_access / nr_tours,
    tour_sust_access = sustainable_access / nr_tours
    )

agent_sums_1 <- agents_1 %>%
  group_sum(group_var, res_var) %>%
  mutate(
    tour_access = total_access / nr_tours,
    tour_sust_access = sustainable_access / nr_tours
    )

# Filter results ----

agent_sums <- agent_sums %>%
  filter(area %in% areas) %>%
  filter_outliers("tour_access")

agent_sums_0 <- agent_sums_0 %>%
  filter(area %in% areas) %>%
  filter_outliers("tour_access")

agent_sums_1 <- agent_sums_1 %>%
  filter(area %in% areas) %>%
  filter_outliers("tour_access")

# Join tables ----

agent_sums <- full_join(agent_sums,
                        agent_sums_0,
                        by = group_var,
                        suffix = c("", "0"))

agent_sums <- full_join(agent_sums,
                        agent_sums_1,
                        by = group_var,
                        suffix = c("", "1"))

# Calc differences ----

agent_sums <- agent_sums %>%
  mutate(
    tour_access_dif = tour_access1 - tour_access0,
    tour_sust_access_dif = tour_sust_access1 - tour_sust_access0
    )

# Combine to shapefile ----

zones <- zones %>%
  left_join(agent_sums,
            by = c("zone" = "number"),
            suffix = c("", "1"))

# Plot differences ----

# all tours
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = tour_access_dif)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient2(
    high = hsl_cols("red"),
    low = hsl_cols("blue"),
    mid = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  labs(
    fill = "eur / kiertomatka",
    title = config::get("projected_name"),
    subtitle = "Muutos matkan hyödyissä (kaikki kulkutavat)"
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "zones_access_dif.png"
       ),
  width = dimensions_map[1],
  height = dimensions_map[2],
  units = "cm"
)

# walk, bike and transit tours
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = tour_sust_access_dif)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient2(
    high = hsl_cols("red"),
    low = hsl_cols("blue"),
    mid = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  labs(
    fill = "eur / kiertomatka",
    title = config::get("projected_name"),
    subtitle = "Muutos matkan hyödyissä (kävely, pyöräily, jl)"
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "zones_sust_access_dif.png"
       ),
  width = dimensions_map[1],
  height = dimensions_map[2],
  units = "cm"
)

