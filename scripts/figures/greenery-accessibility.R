# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(terra)

source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")


# Data --------------------------------------------------------------------

grid <- sf::st_read(
  here::here(
    "data",
    "Tavoiteverkkojen yhdistetty maankäyttöaineisto",
    "aineistopyynto_siuntio_ruudukko_yhdistetty.gpkg"
  ),
  quiet = TRUE
)

required_fields <- c("asukkaat_24", "asukkaat_45")
if (!all(required_fields %in% names(grid))) {
  rlang::abort(sprintf(
    "The population grid is missing required fields: %s",
    paste(setdiff(required_fields, names(grid)), collapse = ", ")
  ))
}

grid <- grid %>%
  sf::st_transform(3067) %>%
  dplyr::mutate(
    grid_id = dplyr::row_number(),
    population_2024 = as.numeric(asukkaat_24),
    population_2045_ve0 = as.numeric(asukkaat_45),
    population_2045_plan = as.numeric(asukkaat_45)
  )

population_fields <- c(
  "population_2024",
  "population_2045_ve0",
  "population_2045_plan"
)
if (anyNA(sf::st_drop_geometry(grid)[population_fields])) {
  rlang::abort("The population grid contains missing population values.")
}
if (any(as.matrix(sf::st_drop_geometry(grid)[population_fields]) < 0)) {
  rlang::abort("The population grid contains negative population values.")
}

corine_path <- here::here("data", "Corine2018", "Clc2018_FI20m.tif")
corine_classes_path <- paste0(corine_path, ".vat.dbf")
corine_classes <- foreign::read.dbf(corine_classes_path)

# Parks, forests, natural grasslands, heathland, scrub, and wetlands.
# Agricultural land, bare ground, beaches, and water are excluded.
green_level3 <- c(141, 311, 312, 313, 321, 322, 324, 411, 412, 421)
green_values <- corine_classes$Value[
  corine_classes$LEVEL3 %in% green_level3
]
if (length(green_values) == 0) {
  rlang::abort("No CORINE pixels matched the configured greenery classes.")
}

corine <- terra::rast(corine_path)
if (!isTRUE(terra::same.crs(corine, terra::vect(grid)))) {
  rlang::abort("The population grid and CORINE raster must use the same CRS.")
}

greenery_region <- sf::st_union(c(region, sf::st_geometry(bonus_region)))
if (!any(sf::st_is_valid(greenery_region))) {
  rlang::abort("The Helsinki region boundary is invalid.")
}
region_sf <- sf::st_sf(geometry = greenery_region)
bbox <- sf::st_bbox(greenery_region)

active_grid <- grid %>%
  dplyr::filter(
    .data$population_2024 > 0 |
      .data$population_2045_ve0 > 0 |
      .data$population_2045_plan > 0
  )
if (nrow(active_grid) == 0) {
  rlang::abort("The population grid has no residents in the selected years.")
}

centroids <- sf::st_centroid(sf::st_geometry(active_grid))
centroid_coordinates <- sf::st_coordinates(centroids)
grid_bbox <- sf::st_bbox(grid)
plot_bbox <- sf::st_bbox(
  sf::st_transform(sf::st_as_sfc(bbox), sf::st_crs(corine))
)


# Nearest greenery --------------------------------------------------------

# Crop around the populated area. Expanding until every measured distance is
# shorter than its distance to the crop edge guarantees the crop cannot hide a
# closer green pixel outside it.
green_distance <- NULL
for (buffer_m in c(10000, 20000, 40000, 80000)) {
  corine_crop <- terra::crop(
    corine,
    terra::ext(
      grid_bbox[["xmin"]] - buffer_m,
      grid_bbox[["xmax"]] + buffer_m,
      grid_bbox[["ymin"]] - buffer_m,
      grid_bbox[["ymax"]] + buffer_m
    ),
    snap = "out"
  )
  green_mask <- terra::ifel(corine_crop %in% green_values, 1, NA)
  green_cell_count <- terra::global(green_mask, "sum", na.rm = TRUE)[1, 1]
  if (is.na(green_cell_count) || green_cell_count == 0) {
    next
  }

  distance_raster <- terra::distance(green_mask)
  sampled_distances <- terra::extract(
    distance_raster,
    terra::vect(centroids)
  )[[2]]
  crop_bounds <- as.vector(terra::ext(corine_crop))
  distance_to_crop_edge <- pmin(
    centroid_coordinates[, 1] - crop_bounds[1],
    crop_bounds[2] - centroid_coordinates[, 1],
    centroid_coordinates[, 2] - crop_bounds[3],
    crop_bounds[4] - centroid_coordinates[, 2]
  )

  if (all(
    is.finite(sampled_distances) &
      sampled_distances < distance_to_crop_edge
  )) {
    green_distance <- sampled_distances
    break
  }
}

if (is.null(green_distance)) {
  rlang::abort(
    paste(
      "Could not guarantee nearest-greenery distances",
      "within the 80 km raster crop."
    )
  )
}

greenery_map_crop <- terra::crop(
  corine,
  terra::ext(
    plot_bbox[["xmin"]] - 500,
    plot_bbox[["xmax"]] + 500,
    plot_bbox[["ymin"]] - 500,
    plot_bbox[["ymax"]] + 500
  ),
  snap = "out"
)
greenery_map_mask <- terra::ifel(
  greenery_map_crop %in% green_values,
  1,
  NA
)
greenery_map_raster <- terra::project(
  greenery_map_mask,
  "EPSG:3879",
  method = "near",
  res = 100
)
greenery_map_raster <- terra::mask(
  greenery_map_raster,
  terra::vect(region_sf)
)
greenery_matrix <- as.matrix(greenery_map_raster, wide = TRUE)
greenery_fill <- grDevices::adjustcolor("#258344", alpha.f = 0.4)
greenery_rgba <- matrix(
  ifelse(is.na(greenery_matrix), NA_character_, greenery_fill),
  nrow = nrow(greenery_matrix)
)
greenery_image <- grDevices::as.raster(greenery_rgba)
greenery_image_extent <- as.vector(terra::ext(greenery_map_raster))

distance_by_grid <- rep(NA_real_, nrow(grid))
distance_by_grid[active_grid$grid_id] <- green_distance

# The CORINE 2018 greenery mask is intentionally reused for all years and
# scenarios. Replace the corresponding assignments when future greenery layers
# are available; asukkaat_45 is reused for both 2045 population fields.
grid <- grid %>%
  dplyr::mutate(
    greenery_distance_2024_m = distance_by_grid,
    greenery_distance_2045_ve0_m = distance_by_grid,
    greenery_distance_2045_plan_m = distance_by_grid,
    greenery_distance_plan_effect_m =
      .data$greenery_distance_2045_plan_m -
      .data$greenery_distance_2045_ve0_m
  )


# Population-weighted summary --------------------------------------------

summary_data <- dplyr::bind_rows(
  grid %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(.data$population_2024 > 0) %>%
    dplyr::transmute(
      scenario = "2024",
      population = .data$population_2024,
      distance_m = .data$greenery_distance_2024_m
    ),
  grid %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(.data$population_2045_ve0 > 0) %>%
    dplyr::transmute(
      scenario = "2045 vertailupohja",
      population = .data$population_2045_ve0,
      distance_m = .data$greenery_distance_2045_ve0_m
    ),
  grid %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(.data$population_2045_plan > 0) %>%
    dplyr::transmute(
      scenario = "2045 MAL-suunnitelma",
      population = .data$population_2045_plan,
      distance_m = .data$greenery_distance_2045_plan_m
    )
) %>%
  dplyr::group_by(.data$scenario) %>%
  dplyr::summarise(
    total_population = sum(.data$population),
    population_weighted_distance_m =
      sum(.data$distance_m * .data$population) / sum(.data$population),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    plan_effect_vs_reference_m = dplyr::if_else(
      .data$scenario == "2045 MAL-suunnitelma",
      .data$population_weighted_distance_m[
        .data$scenario == "2045 MAL-suunnitelma"
      ] -
        .data$population_weighted_distance_m[
          .data$scenario == "2045 vertailupohja"
        ],
      NA_real_
    )
  )

readr::write_csv(
  summary_data,
  here::here("results", "greenery-accessibility-summary.csv")
)
print(summary_data)


# Maps --------------------------------------------------------------------

distance_limits <- range(distance_by_grid, na.rm = TRUE)

clip_to_helsinki_region <- function(data) {
  region_in_grid_crs <- sf::st_transform(region_sf, sf::st_crs(data))
  clipped <- sf::st_intersection(data, region_in_grid_crs)
  if (nrow(clipped) == 0) {
    rlang::abort("No populated grid cells intersect the Helsinki region.")
  }
  clipped
}

plot_distance_map <- function(data, distance_field, title, subtitle) {
  ggplot2::ggplot() +
    geom_basemap(show_roads = FALSE) +
    ggplot2::geom_sf(
      data = data,
      mapping = ggplot2::aes(fill = .data[[distance_field]]),
      color = NA,
      linewidth = 0
    ) +
    ggplot2::annotation_raster(
      greenery_image,
      xmin = greenery_image_extent[1],
      xmax = greenery_image_extent[2],
      ymin = greenery_image_extent[3],
      ymax = greenery_image_extent[4],
      interpolate = FALSE
    ) +
    ggplot2::scale_fill_viridis_c(
      name = "Etäisyys (m)",
      limits = distance_limits,
      labels = scales::label_number(accuracy = 100, decimal.mark = ",")
    ) +
    coord_sf_mal() +
    annotate_map(
      title = title,
      subtitle = paste(subtitle, "Vihreä overlay: CORINE 2018", sep = "; ")
    ) +
    theme_mal_map()
}

map_2024 <- grid %>%
  dplyr::filter(.data$population_2024 > 0) %>%
  clip_to_helsinki_region()
map_2045_ve0 <- grid %>%
  dplyr::filter(.data$population_2045_ve0 > 0) %>%
  clip_to_helsinki_region()
map_2045_plan <- grid %>%
  dplyr::filter(.data$population_2045_plan > 0) %>%
  clip_to_helsinki_region()
map_plan_effect <- grid %>%
  dplyr::filter(
    .data$population_2045_ve0 > 0 |
      .data$population_2045_plan > 0
  ) %>%
  clip_to_helsinki_region()

plot_2024 <- plot_distance_map(
  map_2024,
  "greenery_distance_2024_m",
  "Etäisyys lähimpään viheralueeseen",
  "2024 asukkaat; gridin keskipisteestä CORINE-ruutuun"
)
ggsave_map(
  here::here("figures", "greenery", "map_greenery_accessibility_2024.png"),
  plot = plot_2024
)

plot_2045_ve0 <- plot_distance_map(
  map_2045_ve0,
  "greenery_distance_2045_ve0_m",
  "Etäisyys lähimpään viheralueeseen",
  "2045 vertailupohja; gridin keskipisteestä CORINE-ruutuun"
)
ggsave_map(
  here::here("figures", "greenery","map_greenery_accessibility_2045_ve0.png"),
  plot = plot_2045_ve0
)

plot_2045_plan <- plot_distance_map(
  map_2045_plan,
  "greenery_distance_2045_plan_m",
  "Etäisyys lähimpään viheralueeseen",
  "2045 MAL-suunnitelma; gridin keskipisteestä CORINE-ruutuun"
)
ggsave_map(
  here::here("figures", "greenery","map_greenery_accessibility_2045_plan.png"),
  plot = plot_2045_plan
)

max_effect_m <- max(
  abs(map_plan_effect$greenery_distance_plan_effect_m),
  na.rm = TRUE
)
effect_limit_m <- if (max_effect_m == 0) 1 else max_effect_m
effect_breaks <- if (max_effect_m == 0) 0 else pretty(
  c(-effect_limit_m, effect_limit_m),
  n = 5
)
plot_plan_effect <- ggplot2::ggplot() +
  geom_basemap(show_roads = FALSE) +
  ggplot2::geom_sf(
    data = map_plan_effect,
    mapping = ggplot2::aes(fill = .data$greenery_distance_plan_effect_m),
    color = NA,
    linewidth = 0
  ) +
  ggplot2::annotation_raster(
    greenery_image,
    xmin = greenery_image_extent[1],
    xmax = greenery_image_extent[2],
    ymin = greenery_image_extent[3],
    ymax = greenery_image_extent[4],
    interpolate = FALSE
  ) +
  ggplot2::scale_fill_gradient2(
    name = "Muutos (m)",
    low = "#7b1154",
    mid = "#FFFFFF",
    high = "#3E8606",
    midpoint = 0,
    breaks = effect_breaks,
    limits = c(-effect_limit_m, effect_limit_m),
    oob = scales::squish
  ) +
  coord_sf_mal() +
  annotate_map(
    title = "MAL-suunnitelman vaikutus viheralueiden etäisyyteen",
    subtitle = paste(
      "2045 MAL-suunnitelma - 2045 vertailupohja",
      "Vihreä overlay: CORINE 2018",
      sep = "; "
    )
  ) +
  theme_mal_map()
ggsave_map(
  here::here(
    "figures",
    "greenery",
    "map_diff_greenery_accessibility_2045_ve0-2045_plan.png"
  ),
  plot = plot_plan_effect
)
