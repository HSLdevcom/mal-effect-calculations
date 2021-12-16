# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")
source(here::here("scripts", "utils.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

# Scaling to 1-100
a <- 1
b <- 100

# Dropping Suomenlinna because it distorts the results too much.
outlier_zones <- c(1531)

scale_and_squish <- function(x, xrange, a, b) {
  x_normal <- scale_to_range(x, xrange[1], xrange[2], a, b)
  x_normal_ab <- pmax(a, pmin(x_normal, b))
  return(x_normal_ab)
}

break_twocenters <- function(x) {
  unique(quantile(x, probs = seq.int(0, 1, by = 1 / 5)))
}

cut_twocenters <- function(x, breaks) {
  cut(x, breaks = breaks, include.lowest = TRUE)
}

ttime_to_bins <- function(x, xrange, a, b, breaks) {
  x_normal_ab <- scale_and_squish(x, xrange, a, b)
  return(cut_twocenters(x_normal_ab, breaks = breaks))
}

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

twocenters <- function(.data, mode, title) {
  column <- sprintf("ttime_twocenters_%s", mode)
  # If we are on baseline scenario, calculate range of the variable. Otherwise
  # read it from the result folder.
  .data_to_range <- .data %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(!(zone %in% outlier_zones))
  if (config::get("scenario") == config::get("baseline_scenario")) {
    xrange <- range(.data_to_range[[column]])
    readr::write_rds(xrange, file = here::here("results", sprintf("twocenters_range_%s.rds", mode)))
  } else {
    message("twocenters: read ranges...")
    xrange <- readr::read_rds(here::here("results", sprintf("twocenters_range_%s.rds", mode)))
  }
  # Normalise variable.
  ttime_twocenters_normal = scale_and_squish(.data[[column]],
                                             xrange = xrange,
                                             a = a,
                                             b = b)
  # If we are on baseline scenario, calculate breaks of the variable for the
  # plotting. Otherwise read it from the result folder.
  if (config::get("scenario") == config::get("baseline_scenario")) {
    breaks <- break_twocenters(ttime_twocenters_normal)
    readr::write_rds(breaks, file = here::here("results", sprintf("twocenters_breaks_%s.rds", mode)))
  } else {
    message("twocenters: read breaks...")
    breaks <- readr::read_rds(here::here("results", sprintf("twocenters_breaks_%s.rds", mode)))
  }
  # Cut variable into bins and plot.
  .data$bins = cut_twocenters(ttime_twocenters_normal,
                              breaks = breaks)
  plot_twocenters(.data, bins, title)
  ggsave_map(here::here("figures", sprintf("map_twocenters_%s_%s.png", mode, config::get("scenario"))))
  return(ttime_twocenters_normal)
}


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

results <- results %>%
  dplyr::mutate(ttime_twocenters_all = mode_share_car * car +
                  mode_share_transit * transit +
                  mode_share_bike * bike +
                  mode_share_walk * walk)

# Now, we are handling already normalized travel times but I do not think that
# is an issue. They are normalized again to fit [1, 100].
all <- twocenters(results, mode = "all", title="Kahden keskuksen matka-aikasaavutettavuus kaikilla kulkutavoilla")
