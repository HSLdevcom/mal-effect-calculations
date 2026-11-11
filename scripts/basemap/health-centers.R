# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)
library(sf)
library(readxl)

source(here::here("scripts", "basemap", "functions_map.R"), encoding = "utf-8")

# Data --------------------------------------------------------------------

health_codes <- sprintf("06%02d", 10:19)
building_file <- here::here(
  "data", "sotepe_alueet",
  "Sotepe rakennukset HELMET-alue.xlsx"
)

health_centers <- readxl::read_excel(
  building_file,
  sheet = "Toimipaikat"
) %>%
  dplyr::mutate(
    id = sprintf("HC-%04d", dplyr::row_number())
  ) %>%
  dplyr::transmute(
    id,
    municipality = as.character(Kunta),
    status = as.character(Tila),
    name = as.character(Kohde),
    address = as.character(Osoite),
    purpose = as.character(`Käyttötarkoitus`),
    latitude = as.numeric(Leveysaste),
    longitude = as.numeric(Pituusaste)
  ) %>%
  dplyr::filter(
    stringr::str_sub(purpose, 1, 4) %in% health_codes,
    status %in% c("Nykyinen", "Suunniteltu")
  ) %>%
  dplyr::mutate(
    purpose_code = stringr::str_sub(purpose, 1, 4),
    purpose_name = stringr::str_trim(stringr::str_remove(purpose, "^[0-9]+\\s+")),
    purpose_name = dplyr::case_when(
      purpose_code %in% c("0612", "0613") ~ "Muut sairaalat",
      purpose_code %in% c("0610", "0619") ~ "Terveys- ja hyvinvointilaitokset",
      TRUE ~ purpose_name
    )
  )

missing_coordinates <- health_centers %>%
  dplyr::filter(is.na(latitude) | is.na(longitude))

health_centers <- health_centers %>%
  dplyr::filter(!is.na(latitude), !is.na(longitude)) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_transform(3879)

zones <- readr::read_rds(here::here("results", "zones.rds")) %>%
  dplyr::select(zone = SIJ2023, geometry)

# Assign each facility to the nearest TAZ. Nearest-feature matching also
# handles facilities located just outside a TAZ polygon boundary.
health_centers <- health_centers %>%
  sf::st_join(zones, join = sf::st_nearest_feature, left = TRUE)

stopifnot(inherits(health_centers, "sf"))
stopifnot(all(!sf::st_is_empty(health_centers)))
stopifnot(all(!is.na(health_centers$zone)))

readr::write_rds(
  health_centers,
  file = here::here("results", "health-centers.rds")
)
readr::write_rds(
  missing_coordinates,
  file = here::here("results", "health-centers-missing-coordinates.rds")
)

# Service-area summaries --------------------------------------------------

service_areas <- readxl::read_excel(
  here::here("data", "sotepe_alueet", "Terveysasemien aluejako.xlsx"),
  sheet = "Terveysasemien aluejako"
) %>%
  dplyr::select(Terveysasema, `Terveysasema-alue`, Peruspiiri, `Osa-alue`) %>%
  dplyr::distinct()

service_area_summary <- service_areas %>%
  dplyr::count(Terveysasema, `Terveysasema-alue`, name = "subareas")

readr::write_rds(
  service_areas,
  file = here::here("results", "health-centre-service-areas.rds")
)

# Map ---------------------------------------------------------------------

health_centers_map_data <- health_centers %>%
  dplyr::filter(.data$purpose_name != "Kuntoutuslaitokset") %>%
  dplyr::mutate(
    map_group = dplyr::if_else(
      is.na(.data$address) | stringr::str_squish(.data$address) == "",
      paste0("facility:", .data$id),
      paste(
        .data$municipality,
        stringr::str_to_lower(stringr::str_squish(.data$address)),
        .data$purpose_name,
        sep = "|"
      )
    )
  ) %>%
  dplyr::group_by(.data$map_group) %>%
  dplyr::summarise(
    municipality = dplyr::first(.data$municipality),
    address = dplyr::first(.data$address),
    purpose_name = dplyr::first(.data$purpose_name),
    status = if (dplyr::n_distinct(.data$status) == 1) {
      dplyr::first(.data$status)
    } else {
      "Nykyinen ja suunniteltu"
    },
    facility_names = paste(
      sort(unique(.data$name[!is.na(.data$name) & .data$name != ""])),
      collapse = ", "
    ),
    geometry = sf::st_centroid(sf::st_combine(.data$geometry)),
    .groups = "drop"
  ) %>%
  dplyr::arrange(.data$purpose_name, .data$municipality, .data$address) %>%
  dplyr::mutate(
    facility_number = dplyr::row_number(),
    facility_names = dplyr::if_else(
      .data$facility_names == "",
      .data$address,
      .data$facility_names
    )
  ) %>%
  dplyr::mutate(
    map_point_size = dplyr::if_else(
      .data$purpose_name == "Terveys- ja hyvinvointilaitokset",
      1.2,
      1.7
    )
  )

facility_coordinates <- sf::st_coordinates(health_centers_map_data)
facility_label_coordinates <- matrix(
  NA_real_,
  nrow = nrow(health_centers_map_data),
  ncol = 2,
  dimnames = list(NULL, c("label_x", "label_y"))
)
label_angles <- seq(0, 2 * pi, length.out = 25)[-25]
label_radii <- seq(300, 4200, by = 300)

for (i in seq_len(nrow(health_centers_map_data))) {
  candidates <- rbind(
    facility_coordinates[i, ],
    do.call(
      rbind,
      lapply(label_radii, function(radius) {
        cbind(
          facility_coordinates[i, 1] + radius * cos(label_angles),
          facility_coordinates[i, 2] + radius * sin(label_angles)
        )
      })
    )
  )

  if (i == 1) {
    selected_candidate <- 1
  } else {
    candidate_distances <- vapply(
      seq_len(nrow(candidates)),
      function(candidate) {
        sqrt(rowSums(
          sweep(
            facility_label_coordinates[seq_len(i - 1), , drop = FALSE],
            2,
            candidates[candidate, ],
            "-"
          )^2
        ))
      },
      numeric(i - 1)
    )
    candidate_distances <- matrix(
      candidate_distances,
      nrow = i - 1,
      ncol = nrow(candidates)
    )
    suitable_candidates <- which(
      apply(candidate_distances, 2, min) >= 1200
    )
    selected_candidate <- if (length(suitable_candidates) > 0) {
      suitable_candidates[[1]]
    } else {
      which.max(apply(candidate_distances, 2, min))
    }
  }

  facility_label_coordinates[i, ] <- candidates[selected_candidate, ]
}

facility_label_data <- health_centers_map_data %>%
  dplyr::mutate(
    point_x = facility_coordinates[, 1],
    point_y = facility_coordinates[, 2],
    label_x = facility_label_coordinates[, 1],
    label_y = facility_label_coordinates[, 2],
    label_size = dplyr::if_else(
      .data$purpose_name == "Terveys- ja hyvinvointilaitokset",
      4.2,
      4.5
    )
  ) %>%
  sf::st_drop_geometry()

facility_colors <- c(
  "Keskussairaalat" = "#159E78",
  "Muut sairaalat" = "#E57200",
  "Terveys- ja hyvinvointilaitokset" = "#756BB1"
)

health_centers_map <- ggplot() +
  geom_sf(data = zones, fill = "#F3F1E8", color = "#D0CCC0", linewidth = 0.12) +
  geom_basemap(show_roads = TRUE) +
  geom_segment(
    data = facility_label_data,
    aes(x = point_x, y = point_y, xend = label_x, yend = label_y),
    color = "#777777",
    linewidth = 0.15
  ) +
  geom_sf(
    data = health_centers_map_data,
    aes(shape = status, color = purpose_name, size = map_point_size),
    stroke = 0.2
  ) +
  geom_point(
    data = facility_label_data,
    aes(
      x = label_x,
      y = label_y,
      size = label_size,
      color = purpose_name
    ),
    shape = 21,
    fill = "#FFFFFF",
    stroke = 0.45,
    show.legend = FALSE
  ) +
  geom_text(
    data = facility_label_data,
    aes(x = label_x, y = label_y, label = facility_number),
    size = 2.7,
    color = "#111111",
    fontface = "bold"
  ) +
  scale_shape_manual(
    values = c(
      "Nykyinen" = 21,
      "Suunniteltu" = 24,
      "Nykyinen ja suunniteltu" = 23
    ),
    name = "Tila"
  ) +
  scale_color_manual(values = facility_colors, name = "Palvelutyyppi") +
  scale_fill_manual(values = facility_colors, guide = "none") +
  scale_size_identity(guide = "none") +
  coord_sf_mal() +
  theme_mal_map() +
  theme(
    plot.margin = margin(0, 2, 0, 0),
    legend.position = c(0, 1),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.key.height = unit(0.35, "cm"),
    legend.spacing.y = unit(0.08, "cm"),
    legend.background = element_rect(fill = "white", colour = NA)
  )

# A widescreen slide, with names at more than twice the former 1.8 mm size.
# Service types are already encoded by colour and explained in the map legend.
index_font_size <- 4 * 72.27 / 25.4
index_line_height <- 0.85
index_entry_gap <- 3
index_columns <- 3L

facility_index <- health_centers_map_data %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    facility_number,
    purpose_name,
    facility_name = stringr::str_wrap(
      paste0(facility_number, ". ", facility_names),
      width = 38
    ),
    line_count = stringr::str_count(.data$facility_name, "\n") + 1,
    # grid uses a 1.2 line-spacing multiplier when drawing multiline text.
    entry_height = .data$line_count * index_font_size * index_line_height * 1.2 +
      index_entry_gap
  ) %>%
  # Balance by rendered text height rather than the number of facilities.
  dplyr::mutate(
    index_column = pmin(
      index_columns,
      floor((cumsum(.data$entry_height) - .data$entry_height / 2) /
        (sum(.data$entry_height) / index_columns)) + 1L
    )
  ) %>%
  dplyr::group_by(.data$index_column) %>%
  dplyr::mutate(
    offset = dplyr::lag(cumsum(.data$entry_height), default = 0)
  ) %>%
  dplyr::ungroup()

# Physical text offsets keep rows compact without introducing scale padding.
facility_index_plot <- grid::gTree(children = do.call(grid::gList, c(
  list(grid::textGrob(
    "Kohdenumerot ja nimet",
    x = unit(6, "pt"), y = unit(1, "npc") - unit(2, "pt"),
    just = c("left", "top"),
    gp = grid::gpar(fontsize = 13, col = "#3E8606")
  )),
  lapply(seq_len(nrow(facility_index)), function(i) {
    entry <- facility_index[i, ]
    grid::textGrob(
      entry$facility_name,
      x = unit((entry$index_column - 1) / index_columns, "npc") + unit(6, "pt"),
      y = unit(1, "npc") - unit(24 + entry$offset, "pt"),
      just = c("left", "top"),
      gp = grid::gpar(
        fontsize = index_font_size, lineheight = index_line_height,
        col = facility_colors[[entry$purpose_name]]
      )
    )
  })
)))

health_centers_map_with_index <- gridExtra::arrangeGrob(
  facility_index_plot,
  health_centers_map,
  ncol = 2,
  widths = c(0.58, 0.42),
  top = grid::textGrob(
    paste0(
      "Helsingin seudun terveys- ja hyvinvointilaitokset\n",
      "Nykyiset ja suunnitellut kohteet sijoittelualueilla"
    ),
    x = unit(6, "pt"), just = "left",
    gp = grid::gpar(fontsize = 17, lineheight = 1.15, col = "#3E8606")
  ),
  bottom = grid::textGrob(
    paste0(
      "Maastotietokanta: Maanmittauslaitos 2021 | ",
      "Maastokartta 1:100 000: Maanmittauslaitos 2021"
    ),
    x = unit(1, "npc") - unit(6, "pt"), just = "right",
    gp = grid::gpar(fontsize = 8, col = "#333333")
  ),
  padding = unit(5, "pt")
)

ggsave_map(
  here::here("figures", "map_health-centers.png"),
  plot = health_centers_map_with_index,
  width = 16,
  height = 9,
  units = "in",
  dpi = 120,
  bg = "white"
)

message(
  "Health-centre workflow complete: ", nrow(health_centers),
  " mapped facilities; ", nrow(missing_coordinates),
  " health-related records without coordinates."
)
