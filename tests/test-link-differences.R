# Run from the repository root: Rscript tests/test-link-differences.R
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
source("scripts/figures/link_difference_blacklist.R", encoding = "UTF-8")

# Exclude the old and replacement geometries, while retaining shared links,
# unlisted links, and a genuinely new connection in the same neighbourhood.
blacklist <- readr::read_tsv("utilities/link_difference_blacklist.tsv",
                            col_types = "cccdd")
links <- data.frame(
  id = c("removed", "added", "shared", "new_connection", "unlisted"),
  node_i0 = c(40307, NA, 40307, NA, 1),
  node_j0 = c(181834, NA, 181834, NA, 2),
  node_i1 = c(NA, 40307, 40307, 322769, NA),
  node_j1 = c(NA, 322914, 181834, 322770, NA)
)
for (pair in list(c("2045_ve0", "2045_suunnitelma"),
                 c("2045_suunnitelma", "2045_ve0"))) {
  filtered <- filter_link_difference_blacklist(links, pair[1], pair[2], blacklist)
  stopifnot(identical(filtered$id, c("shared", "new_connection", "unlisted")))
}
stopifnot(identical(
  filter_link_difference_blacklist(links, "2023", "2045_ve0", blacklist),
  links
))

for (expr in parse("scripts/utils.R", encoding = "UTF-8")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("matrix_col_to_vector"))) eval(expr)
}

# Exercise the actual map transformations without reading cached scenario data
# or rendering the unrelated basemap layers.
for (file in c("map_diff_volume-aht.R", "map_diff_volume-car-aht.R",
               "map_diff_volume-car-aht2.R", "map_diff_relative_speed.R")) {
  expressions <- parse(file.path("scripts/figures/difference", file),
                       encoding = "UTF-8")
  speed <- grepl("relative_speed", file)
  metric <- if (speed) "relative_speed" else if (grepl("car", file)) "car_aht" else "volume_aht"
  difference <- paste0("diff_", metric)
  env <- new.env()
  env$scenario0 <- "2023"
  env$scenario1 <- "2045_ve0"
  env$results0 <- data.frame(
    node_i = c(1, 2), node_j = c(2, 3),
    wkt = c("LINESTRING (0 0, 100 0)", "LINESTRING (0 100, 100 100)")
  )
  env$results1 <- data.frame(
    node_i = c(1, 3), node_j = c(2, 4),
    wkt = c("LINESTRING (0 0, 100 0)", "LINESTRING (0 200, 100 200)")
  )
  env$results0[[metric]] <- if (speed) c(0.6, 0.5) else c(100, 200)
  env$results1[[metric]] <- if (speed) c(0.7, 0.8) else c(150, 300)
  if (speed) {
    env$results0$volume_aht <- c(100, 200)
    env$results1$volume_aht <- c(150, 300)
  }
  env$region <- st_as_sfc(st_bbox(c(xmin = -1000, ymin = -1000,
                                  xmax = 1000, ymax = 1000), crs = st_crs(3879)))
  for (expr in expressions) {
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        as.character(expr[[2]]) %in% c("results", "buffers")) {
      eval(expr, env)
    }
  }

  expected <- if (speed) c(0.1, -0.5, 0.8) else c(50, -200, 300)
  stopifnot(nrow(env$results) == 3L,
            isTRUE(all.equal(env$results[[difference]], expected)),
            nrow(env$buffers) == 3L,
            !any(st_is_empty(env$buffers)))
  if (speed) {
    stopifnot(identical(env$results$volume_aht, c(150, 200, 300)))
  }

  # Inspect the actual fill scale: removed links must be greener than red,
  # and added links redder than green.
  plot_expr <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1]], as.name("+"))
  }, expressions)[[1]]
  scale_expr <- function(expr) {
    if (!is.call(expr)) return(NULL)
    if (identical(expr[[1]], as.name("scale_fill_fermenter"))) return(expr)
    for (part in as.list(expr)[-1]) {
      found <- scale_expr(part)
      if (!is.null(found)) return(found)
    }
    NULL
  }
  fill <- eval(scale_expr(plot_expr))
  fill$train(env$buffers[[difference]])
  colours <- col2rgb(fill$map(c(min(expected), max(expected))))
  stopifnot(colours["green", 1] > colours["red", 1],
            colours["red", 2] > colours["green", 2])
  cat(file, "passed.\n")
}
