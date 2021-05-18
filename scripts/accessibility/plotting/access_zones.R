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

res_var <- c("nr_tours", "sustainable_access", "total_access")
group_var <- c("number")

sum_agents <- function(df){
  df <- df %>%
    group_by(!!!syms(group_var)) %>%
    summarise(across(all_of(res_var), sum, na.rm = TRUE)) %>%
    ungroup()
  }

agents <- agents %>%
  sum_agents %>%
  slice(which(zone_persons$n > 50))

agents_0 <- agents_0 %>%
  sum_agents %>%
  slice(which(zone_persons$n > 50))

agents_1 <- agents_1 %>%
  sum_agents %>%
  slice(which(zone_persons$n > 50))

# Join tables ----

agents <- full_join(agents,
                    agents_0,
                    by = group_var,
                    suffix = c("", "0"))

agents <- full_join(agents,
                    agents_1,
                    by = group_var,
                    suffix = c("", "1"))

# Calc differences ----

agents <- agents %>%
  mutate(
    tour_access1 = total_access1 / nr_tours1,
    tour_access0 = total_access0 / nr_tours0,
    tour_access_gap1 = tour_access1 - mean(tour_access1, na.rm = TRUE),
    tour_access_gap0 = tour_access0 - mean(tour_access0, na.rm = TRUE),
    tour_sust_access1 = sustainable_access1 / nr_tours1,
    tour_sust_access0 = sustainable_access0 / nr_tours0,
    tour_access_dif = tour_access1 - tour_access0,
    tour_sust_access_dif = tour_sust_access1 - tour_sust_access0
    )

# Combine to shapefile ----

zones <- zones %>%
  left_join(agents, by = c("zone" = "number"))

# Plot accessibility ----

# all tours
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = tour_access1)) +
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
    subtitle = "Matkustamisesta saatava hyöty (kaikki kulkutavat)"
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
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = tour_sust_access1)) +
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
    subtitle = "Matkustamisesta saatava hyöty (kävely, pyöräily, jl)"
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

# Plot gap ----

# all tours
zones %>%
  filter(area %in% plot_areas) %>%
  ggplot(aes(fill = tour_access_gap1)) +
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
    subtitle = "Ero seudun keskiarvoon matkoista saatavassa hyödyssä"
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
