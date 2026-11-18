# Rebuild heavy-rail coverage for the MAL 2027 scenarios.
# Run from the project: source(here::here("scripts", "create_centers_and_stations.R"))
# Requires the project's sf, readr, dplyr, here and config packages.
#
# Keep the original GeoPackage as the source of center coverage. Outputs:
#   data/centers_and_stations.gpkg
#   results/centers-and-stations.rds
# scripts/basemap/centers-and-stations.R reads the generated GeoPackage and
# refreshes the RDS used by figures/squares.R. Run this generator first when
# the scenario networks or center coverage change.

station_buffer_m <- 1000 # Inferred from the old coverage; straight-line distance.
network_crs <- 3879     # HELMET links.txt coordinates; units are metres.
source_path <- here::here("data", "centers_and_stations2.gpkg")
output_path <- here::here("data", "centers_and_stations.gpkg")
results_path <- here::here("results", "centers-and-stations.rds")

# The baseline is named 2045_ve0 in scenarios.tsv. Expose the requested name
# in the output, and retain ve0/ve1 aliases for existing figure consumers.
scenario_ids <- c("2023", "2045_ve0", "2045_tavoiteverkko", "2045_suunnitelma")
scenario_names <- c("2023", "2045_vertailupohja", "2045_tavoiteverkko",
                    "2045_suunnitelma")
station_columns <- paste0("station_", scenario_names)
scenarios <- readr::read_tsv(here::here("scenarios.tsv"), comment = "#",
                            show_col_types = FALSE)
stopifnot(!anyDuplicated(scenarios$scenario),
          all(scenario_ids %in% scenarios$scenario),
          is.finite(station_buffer_m), station_buffer_m > 0)
network_root <- config::get("helmet_data")
link_paths <- here::here(network_root,
                        scenarios$results[match(scenario_ids, scenarios$scenario)],
                        "links.txt")
if (any(!file.exists(link_paths))) {
  stop("Missing scenario networks: ", paste(link_paths[!file.exists(link_paths)],
                                            collapse = ", "))
}

original <- sf::read_sf(source_path)
stopifnot("center" %in% names(original), !is.na(sf::st_crs(original)))
centers <- original[!is.na(original$center) & original$center == 1, "center"]
stopifnot(nrow(centers) > 0L)
centers <- sf::st_transform(centers, network_crs)
# Historical rows split center polygons by old station flags. Dissolve those
# subdivisions to retain precisely the same center footprint.
centers <- sf::st_sf(center = TRUE,
                     geometry = sf::st_union(sf::st_make_valid(centers)))
for (column in station_columns) centers[[column]] <- FALSE

read_station_nodes <- function(path) {
  links <- readr::read_tsv(path, show_col_types = FALSE, progress = FALSE,
                          col_types = readr::cols_only(
                            Link = readr::col_character(),
                            node_i = readr::col_character(),
                            node_j = readr::col_character(),
                            type = readr::col_integer()))
  stopifnot(all(c("Link", "node_i", "node_j", "type") %in% names(links)))
  # Types 3 = metro, 4 = train. Exclude tram/light rail (2, 5, 6).
  # These are network station nodes, not stop-level boarding records: links.txt
  # alone cannot prove which trains stop at each node. Do not filter passenger
  # volumes: a zero-demand station still belongs to the scenario network.
  links <- links[!is.na(links$type) & links$type %in% c(3L, 4L), ]
  if (!nrow(links)) stop("No train/metro links in ", path)
  stopifnot(!anyNA(links$node_i), !anyNA(links$node_j))
  lines <- sf::st_as_sfc(links$Link, crs = network_crs)
  stopifnot(all(sf::st_geometry_type(lines) == "LINESTRING"),
            !any(sf::st_is_empty(lines)))
  # Only endpoints represent nodes; intermediate WKT vertices describe tracks.
  coordinates <- sf::st_coordinates(lines)
  first <- !duplicated(coordinates[, "L1"])
  last <- !duplicated(coordinates[, "L1"], fromLast = TRUE)
  nodes <- dplyr::bind_rows(
    data.frame(node = links$node_i, x = coordinates[first, "X"],
               y = coordinates[first, "Y"]),
    data.frame(node = links$node_j, x = coordinates[last, "X"],
               y = coordinates[last, "Y"]))
  stopifnot(all(is.finite(nodes$x)), all(is.finite(nodes$y)))
  # Reversed links repeat nodes. Reject inconsistent endpoint coordinates.
  extent <- nodes |>
    dplyr::group_by(node) |>
    dplyr::summarise(dx = max(x) - min(x), dy = max(y) - min(y),
                     .groups = "drop")
  if (any(extent$dx > 1 | extent$dy > 1)) {
    stop("Node coordinates differ by more than 1 m in ", path)
  }
  nodes <- dplyr::distinct(nodes, node, .keep_all = TRUE)
  sf::st_as_sf(nodes, coords = c("x", "y"), crs = network_crs)
}

coverage <- list(centers)
for (i in seq_along(scenario_ids)) {
  stations <- read_station_nodes(link_paths[i])
  message(scenario_names[i], ": ", nrow(stations), " train/metro nodes")
  # Keep the full model network. Downstream grids determine the study area;
  # clipping at its border would discard coverage from nearby stations.
  buffers <- sf::st_union(sf::st_buffer(stations, dist = station_buffer_m))
  feature <- sf::st_sf(center = FALSE, geometry = buffers)
  for (column in station_columns) feature[[column]] <- FALSE
  feature[[station_columns[i]]] <- TRUE
  coverage[[i + 1L]] <- feature
}

# Separate, overlapping coverage features are intentional. When assigning
# flags to grid centroids, combine ALL intersecting rows with any(), as in
# scripts/figures/squares.R. A single spatial-join match is insufficient.
centers_and_stations <- dplyr::bind_rows(coverage)
centers_and_stations$station_2045_ve0 <-
  centers_and_stations$station_2045_vertailupohja
centers_and_stations$station_2045_ve1 <-
  centers_and_stations$station_2045_tavoiteverkko
stopifnot(!any(sf::st_is_empty(centers_and_stations)),
          all(sf::st_is_valid(centers_and_stations)))

dir.create(dirname(results_path), recursive = TRUE, showWarnings = FALSE)
sf::st_write(centers_and_stations, output_path, layer = "centers_and_stations",
             delete_layer = TRUE, quiet = TRUE)
readr::write_rds(centers_and_stations, results_path)
message("Wrote ", output_path, " and ", results_path)
