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

# Join tours to agents data ----

agents <- agents %>%
  join_purpose_tours(tours,
                     c("total_access", "sustainable_access"),
                     "hw")

agents_0 <- agents_0 %>%
  join_purpose_tours(tours_0,
                     c("total_access", "sustainable_access"),
                     "hw")

agents_1 <- agents_1 %>%
  join_purpose_tours(tours_1,
                     c("total_access", "sustainable_access"),
                     "hw")

# Sum results ----

res_var <- c("nr_tours_hw", "total_access_hw", "sustainable_access_hw")
group_var <- c("area", "number")

agent_sums <- agents %>%
  group_sum(group_var, res_var) %>%
  mutate(
    tour_access_hw = total_access_hw / nr_tours_hw,
    tour_sust_access_hw = sustainable_access_hw / nr_tours_hw
    )

agent_sums_0 <- agents_0 %>%
  group_sum(group_var, res_var) %>%
  mutate(
    tour_access_hw = total_access_hw / nr_tours_hw,
    tour_sust_access_hw = sustainable_access_hw / nr_tours_hw
  )

agent_sums_1 <- agents_1 %>%
  group_sum(group_var, res_var) %>%
  mutate(
    tour_access_hw = total_access_hw / nr_tours_hw,
    tour_sust_access_hw = sustainable_access_hw / nr_tours_hw
  )

# Filter areas and outliers ----

agent_sums <- agent_sums %>%
  filter(area %in% areas) %>%
  filter_outliers("tour_access_hw")

agent_sums_0 <- agent_sums_0 %>%
  filter(area %in% areas) %>%
  filter_outliers("tour_access_hw")

agent_sums_1 <- agent_sums_1 %>%
  filter(area %in% areas) %>%
  filter_outliers("tour_access_hw")

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
  mutate(tour_access_gap_hw1 =
           tour_access_hw1 - mean(tour_access_hw1, na.rm = TRUE))

# Combine to shapefile ----

zones <- zones %>%
  filter(area %in% plot_areas) %>%
  left_join(agent_sums,
            by = c("zone" = "number"),
            suffix = c("", "1"))

# Plot accessibility ----

# all tours
zones %>%
  ggplot(aes(fill = tour_access_hw1)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  labs(
    fill = "eur / kiertomatka",
    title = config::get("projected_name"),
    subtitle = "Työkiertomatkoista saatava hyöty (kaikki kulkutavat)"
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "zones_access.png"
  ),
  width = dimensions_map[1],
  height = dimensions_map[2],
  units = "cm"
)

# walk, bike and transit tours
zones %>%
  ggplot(aes(fill = tour_sust_access_hw1)) +
  geom_sf(size = 0.1, color = "gray") +
  theme_maps +
  scale_fill_gradient(
    high = hsl_cols("red"),
    low = hsl_cols("white"),
    na.value = hsl_cols("lightgray")
  ) +
  labs(
    fill = "eur / kiertomatka",
    title = config::get("projected_name"),
    subtitle = "Työkiertomatkoista saatava hyöty (kävely, pyöräily, jl)"
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "zones_sust_access.png"
  ),
  width = dimensions_map[1],
  height = dimensions_map[2],
  units = "cm"
)

# Plot gap ----

# all tours
zones %>%
  ggplot(aes(fill = tour_access_gap_hw1)) +
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
    subtitle = "Ero alueen keskiarvoon matkoista saatavassa hyödyssä"
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "zones_access_gap.png"
       ),
  width = dimensions_map[1],
  height = dimensions_map[2],
  units = "cm"
)
