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

mcp <- st_read(here("data", "municipalities.shp"))

# Remove aakkoset ----

housing_cost <- housing_cost %>%
  mutate(kunta = str_replace_all(kunta, "ä", "a"),
         kunta = str_replace_all(kunta, "ö", "o"))

mcp <- mcp %>%
  mutate(mcp = str_replace_all(mcp, "ä", "a"),
         mcp = str_replace_all(mcp, "ö", "o"))

# Transport costs to month ----

agents <- agents %>%
  mutate(gen_cost = gen_cost * 30,
         cost = cost * 30)

# Join agents tables ----

res_var <- c("gen_cost", "cost", "time_cost")

agent_sums <- agents %>%
  mutate(time_cost = gen_cost - cost) %>%
  group_mean("municipality", res_var) %>%
  filter_outliers("time_cost")

# Select vars for housing cost ----

housing_cost <- housing_cost %>%
  group_by(kunta) %>%
  summarise(askust_kalib = mean(askust_kalib, na.rm = TRUE))

# Combine to shapefile ----

mcp <- mcp %>%
  left_join(agent_sums,
            by = c("mcp" = "municipality"),
            suffix = c("", "1")) %>%
  left_join(housing_cost, by = c("mcp" = "kunta"))

# Write shapefile ----

mcp <- mcp %>%
  filter(mcp %in% agent_sums$municipality)

mcp %>%
  st_write(here::here("results",
                      config::get("projected_scenario"),
                      "municipalities_costs.shp"),
           delete_dsn = TRUE)

# Plot costs in zones ----

# transport costs
mcp %>%
  ggplot(aes(fill = cost)) +
  geom_sf(size = 0.1, color = "gray") +
  #geom_sf_text(aes(label = mcp), colour = "black", size = 2) +
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
    "cost_transport_municipality.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)

# housing costs
mcp %>%
  ggplot(aes(fill = askust_kalib)) +
  geom_sf(size = 0.1, color = "gray") +
  #geom_sf_text(aes(label = mcp), colour = "black", size = 2) +
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
    "cost_housing_municipality.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)

# housing + transport costs
mcp %>%
  mutate(h_t_cost = cost + askust_kalib) %>%
  ggplot(aes(fill = h_t_cost)) +
  geom_sf(size = 0.1, color = "gray") +
  #geom_sf_text(aes(label = mcp), colour = "black", size = 2) +
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
    "cost_housing_transport_municipality.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)

# transport costs
mcp %>%
  ggplot(aes(fill = gen_cost)) +
  geom_sf(size = 0.1, color = "gray") +
  #geom_sf_text(aes(label = mcp), colour = "black", size = 2) +
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
    "gen_cost_transport_municipality.png"
  ),
  width = dimensions_long[1],
  height = dimensions_long[2],
  units = "cm"
)
