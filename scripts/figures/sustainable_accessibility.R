library(tidyverse)
library(here)
library(sf)
library(viridis)
source(here("scripts", "utils.R"),
       encoding = "utf-8")
source(here("scripts", "basemap", "functions_map.R"),
       encoding = "utf-8")

scenario_colors <- grDevices::colorRampPalette(c("#bee4f8", "#007AC9"))(3)

# Read data ----

scenario_definitions <- readr::read_tsv(
  here("scenarios.tsv"),
  comment = "#",
  col_types = cols(
    year = col_integer(),
    present = col_logical(),
    baseline = col_logical(),
    projected = col_logical(),
    sensitivity = col_logical(),
    .default = col_character()
  )
) %>%
  filter(present | baseline | projected) %>%
  mutate(scenario_name = sprintf("%d %s", year, name)) %>%
  select(
    scenario, year, name, forecast, results, present, baseline, projected,
    scenario_name
  )

if (sum(scenario_definitions$present) != 1 ||
    sum(scenario_definitions$baseline) != 1 ||
    sum(scenario_definitions$projected) != 1) {
  rlang::abort("Expected one present, baseline, and projected aggregate scenario.")
}

read_sustainable_accessibility <- function(results_dir) {
  read_tsv_helmet(
    here("results", results_dir, "sustainable_accessibility.txt"),
    col_types = "idddddddddddd",
    first_col_name = zone
  ) %>%
    transmute(
      zone = as.integer(zone),
      sustainable_accessibility = -all
    )
}

hs15_geometry <- sf::st_read(
  here("data", "aluejaot_2023_SHP", "sijoittelualueet2023.shp"),
  quiet = TRUE
) %>%
  mutate(zone = as.integer(SIJ2023)) %>%
  filter(zone < 16000)
hs15_zone_ids <- hs15_geometry$zone

read_population <- function(forecast) {
  population_files <- list.files(
    here("scenarios", forecast),
    pattern = "\\.pop$",
    full.names = TRUE
  )

  if (length(population_files) != 1) {
    rlang::abort(sprintf(
      "Expected one population file for forecast %s, found %d.",
      forecast,
      length(population_files)
    ))
  }

  population <- read_tsv_helmet(
    population_files,
    col_types = "idddddd",
    first_col_name = zone,
    n_max = length(hs15_zone_ids)
  ) %>%
    transmute(
      zone = as.integer(zone),
      total_pop = total
    )

  if (
    nrow(population) != length(hs15_zone_ids) ||
      anyDuplicated(population$zone) > 0 ||
      !setequal(population$zone, hs15_zone_ids)
  ) {
    rlang::abort(sprintf(
      "Population zones do not match the HS15 zones for forecast %s.",
      forecast
    ))
  }

  population
}

present_scenario <- scenario_definitions %>%
  filter(present)
present_population_zones <- read_population(present_scenario$forecast[[1]]) %>%
  select(zone)

reference_accessibility <- read_sustainable_accessibility(
  present_scenario$results[[1]]
) %>%
  semi_join(present_population_zones, by = "zone")
reference_range <- range(
  reference_accessibility$sustainable_accessibility,
  na.rm = TRUE
)

if (any(!is.finite(reference_range)) || diff(reference_range) <= 0) {
  rlang::abort("Could not calculate the present-scenario accessibility range.")
}

scenario_results <- purrr::pmap_dfr(
  scenario_definitions,
  function(scenario, year, name, forecast, results, present, baseline,
           projected, scenario_name) {
    read_sustainable_accessibility(results) %>%
      inner_join(read_population(forecast), by = "zone") %>%
      mutate(
        accessibility_index = scale_to_range(
          sustainable_accessibility,
          xmin = reference_range[[1]],
          xmax = reference_range[[2]],
          a = 0,
          b = 100
        ),
        area = case_when(
          zone %in% 0:999 ~ "Helsingin kantakaupunki",
          zone %in% 1000:1999 ~ "Muu Helsinki",
          zone %in% 2000:5999 ~ "Muu pääkaupunkiseutu",
          zone %in% c(6000:6999, 10000:11999, 13000:14999, 15500:15999) ~
            "Junaliikenteen kehyskunnat",
          zone %in% c(7000:9999, 12000:12999, 15000:15499) ~
            "Bussiliikenteen kehyskunnat",
          TRUE ~ NA_character_
        ),
        scenario_id = scenario,
        scenario = scenario_name,
        projected = projected
      )
  }
)

if (anyNA(scenario_results$total_pop)) {
  rlang::abort("Population data is missing for one or more accessibility zones.")
}

area_levels <- c(
  "Helsingin kantakaupunki",
  "Muu Helsinki",
  "Muu pääkaupunkiseutu",
  "Junaliikenteen kehyskunnat",
  "Bussiliikenteen kehyskunnat"
)
scenario_levels <- scenario_definitions$scenario_name

aggregate_accessibility <- function(data) {
  data %>%
    filter(!is.na(area)) %>%
    group_by(area, scenario) %>%
    summarise(
      accessibility_index = weighted.mean(
        accessibility_index,
        w = total_pop,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    mutate(
      area = factor(area, levels = area_levels),
      scenario = factor(scenario, levels = scenario_levels)
    )
}

area_results <- aggregate_accessibility(scenario_results)

# Vulnerable-area subset ----

vulnerable_zones <- sf::st_read(
  here("data", "sij2023_hsy", "sijoittelualueet2023_hsy.shp"),
  quiet = TRUE
) %>%
  sf::st_drop_geometry() %>%
  transmute(
    zone = as.integer(SIJ2023),
    des_gt_1ha
  ) %>%
  filter(des_gt_1ha == "T") %>%
  distinct(zone)
vulnerable_zone_ids <- vulnerable_zones$zone

vulnerable_geometry <- sf::st_read(
  here("data", "sij2023_hsy", "sijoittelualueet2023_hsy.shp"),
  quiet = TRUE
) %>%
  mutate(zone = as.integer(SIJ2023)) %>%
  filter(zone %in% vulnerable_zone_ids) %>%
  sf::st_make_valid() %>%
  sf::st_union()

vulnerable_bbox <- sf::st_bbox(vulnerable_geometry)
hatch_offsets <- seq(
  unname(vulnerable_bbox[["ymin"]] - vulnerable_bbox[["xmax"]]),
  unname(vulnerable_bbox[["ymax"]] - vulnerable_bbox[["xmin"]]),
  by = 800
)
hatch_segments <- lapply(hatch_offsets, function(offset) {
  x_start <- max(
    unname(vulnerable_bbox[["xmin"]]),
    unname(vulnerable_bbox[["ymin"]] - offset)
  )
  x_end <- min(
    unname(vulnerable_bbox[["xmax"]]),
    unname(vulnerable_bbox[["ymax"]] - offset)
  )

  if (x_start >= x_end) {
    return(NULL)
  }

  sf::st_linestring(rbind(
    c(x_start, x_start + offset),
    c(x_end, x_end + offset)
  ))
}) %>%
  Filter(Negate(is.null), .)
vulnerable_hatching <- sf::st_sfc(
  hatch_segments,
  crs = sf::st_crs(vulnerable_geometry)
) %>%
  sf::st_intersection(vulnerable_geometry) %>%
  sf::st_sf()

vulnerable_results <- scenario_results %>%
  semi_join(vulnerable_zones, by = "zone") %>%
  aggregate_accessibility()

# Plot helpers ----

plot_area_results <- function(data, title, filename) {
  plot <- ggplot(data, aes(x = area, y = accessibility_index, fill = scenario)) +
    geom_col(position = position_dodge2()) +
    geom_text(
      aes(
        y = accessibility_index / 2,
        label = scales::label_number(accuracy = 1)(accessibility_index),
        color = scenario
      ),
      position = position_dodge2(width = 0.9),
      size = points2mm(8)
    ) +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 1),
      limits = c(0, 100),
      oob = scales::squish,
      expand = expansion(mult = c(0.025, 0.1))
    ) +
    scale_x_discrete(labels = scales::label_wrap(5)) +
    scale_fill_manual(name = NULL, values = scenario_colors) +
    scale_color_manual(
      guide = "none",
      values = rep("#333333", length(scenario_levels))
    ) +
    labs(
      x = NULL,
      y = "indeksi",
      title = stringr::str_wrap(title, width = 52)
    ) +
    theme_mal_graph()

  ggsave_graph(here("figures", filename), plot = plot)
}

# Plot ----

projected_scenario <- scenario_definitions %>%
  filter(projected)
projected_index <- scenario_results %>%
  filter(projected) %>%
  select(zone, accessibility_index)

hs15_zones <- hs15_geometry %>%
  semi_join(present_population_zones, by = "zone") %>%
  sf::st_make_valid() %>%
  left_join(projected_index, by = "zone") %>%
  mutate(vulnerable = zone %in% vulnerable_zone_ids)

if (anyNA(hs15_zones$accessibility_index)) {
  rlang::abort("Sustainable accessibility is missing for one or more HSY zones.")
}

ggplot() +
  geom_sf(
    data = hs15_zones,
    mapping = aes(fill = accessibility_index),
    color = NA
  ) +
  scale_fill_viridis(
    option = "magma",
    name = "indeksi",
    labels = scales::label_number(accuracy = 1),
    limits = c(0, 100),
    direction = -1,
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Kestävän saavutettavuuden indeksi",
    subtitle = sprintf(
      "%d %s",
      projected_scenario$year[[1]],
      projected_scenario$name[[1]]
    )
  ) +
  theme_mal_map()

ggsave_map(here(
  "figures",
  sprintf("map_sustainable-accessibility_%s.png", projected_scenario$scenario[[1]])
))

ggplot() +
  geom_sf(
    data = hs15_zones,
    mapping = aes(fill = accessibility_index),
    color = NA
  ) +
  scale_fill_viridis(
    option = "magma",
    name = "indeksi",
    labels = scales::label_number(accuracy = 1),
    limits = c(0, 100),
    direction = -1,
    oob = scales::squish
  ) +
  geom_basemap() +
  geom_sf(
    data = vulnerable_hatching,
    color = "#FFFFFF",
    alpha = 0.8,
    linewidth = 0.25
  ) +
  geom_sf(
    data = vulnerable_geometry,
    fill = NA,
    color = "#FFFFFF",
    linewidth = 0.35
  ) +
  coord_sf_mal() +
  annotate_map(
    title = "Kestävän saavutettavuuden indeksi ja matalan sosioekonomisen statuksen alueet",
    subtitle = sprintf(
      "%d %s; haavoittuvat alueet viivoitettu ja rajattu valkoisella",
      projected_scenario$year[[1]],
      projected_scenario$name[[1]]
    )
  ) +
  theme_mal_map()

ggsave_map(here(
  "figures",
  sprintf("map_sustainable-accessibility_vulnerable_%s.png", projected_scenario$scenario[[1]])
))

plot_area_results(
  area_results,
  "Kestävän saavutettavuuden indeksi väestöpainotettuna",
  "graph_sustainable-accessibility_areas.png"
)

plot_area_results(
  vulnerable_results,
  "Kestävän saavutettavuuden indeksi haavoittuvilla alueilla",
  "graph_sustainable-accessibility_vulnerable-areas.png"
)
