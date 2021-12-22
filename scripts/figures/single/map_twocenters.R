# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
source(here::here("scripts", "utils.R"), encoding = "utf-8")


plot_twocenters <- function(data, fill, title) {
  ggplot() +
    geom_sf(mapping = aes(fill = {{ fill }}),
            data = data, color = NA) +
    scale_fill_viridis_d(
      option="plasma",
      name = "indeksi",
      direction = -1
    ) +
    geom_basemap() +
    coord_sf_mal() +
    annotate_map(
      title = title,
      subtitle = sprintf("%d %s", config::get("year"), config::get("scenario_name"))
    ) +
    theme_mal_map()
}


plot_twocenters(.data, bins, title)
ggsave_map(here::here("figures", sprintf("map_twocenters_%s_%s.png", mode, config::get("scenario"))))


# Plot --------------------------------------------------------------------

results <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("scenario"))))

car <- twocenters(results, mode = "car", title="Kahden keskuksen matka-aikasaavutettavuus henkilöautolla")
transit <- twocenters(results, mode = "transit", title="Kahden keskuksen matka-aikasaavutettavuus joukkoliikenteellä")
bike <- twocenters(results, mode = "bike", title="Kahden keskuksen matka-aikasaavutettavuus polkupyörällä")
walk <- twocenters(results, mode = "walk", title="Kahden keskuksen matka-aikasaavutettavuus kävellen")

pdata <- data.frame(value = c(car, transit, bike, walk),
                    mode = rep(c("car", "transit", "bike", "walk"), each = length(car)))
ggplot(pdata, aes(value)) +
  geom_histogram(binwidth = 1, boundary = 0, closed = "right") +
  facet_wrap(vars(mode), nrow = 1)
ggsave_graph(here::here("figures", sprintf("twocenters_distribution_%s.png", config::get("scenario"))))

mode_shares <- readr::read_rds(here::here("results", sprintf("zones_%s.rds", config::get("baseline_scenario"))))

results <- results %>%
  dplyr::mutate(ttime_twocenters_all = mode_shares$mode_share_car * car +
                  mode_shares$mode_share_transit * transit +
                  mode_shares$mode_share_bike * bike +
                  mode_shares$mode_share_walk * walk)

# Now, we are handling already normalized travel times but I do not think that
# is an issue. They are normalized again to fit [1, 100].
all <- twocenters(results, mode = "all", title="Kahden keskuksen matka-aikasaavutettavuus kaikilla kulkutavoilla")
