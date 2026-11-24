# Run from the repository root with the project R libraries loaded.
library(dplyr)
source("scripts/figures/transfers_helpers.R", encoding = "UTF-8")

od <- data.frame(
  origin = c("1", "1", "2", "1", "external", "2"),
  destination = c("1", "2", "1", "external", "1", "2"),
  transit_demand = c(10, 10, 80, 1000, 1000, 0),
  average_boardings = c(0, 2, 1, 100, 20, NA_real_)
)
od_totals <- summarise_transfer_origins(od, c("1", "2"))
stopifnot(sum(od_totals$regional_transit_demand) == 100)
stopifnot(sum(od_totals$regional_boardings) == 100)
stopifnot(sum(od_totals$regional_transfer_events) == 10)
stopifnot(sum(od_totals$transit_demand) == 2100)

zone_areas <- data.frame(zone = c("1", "2", "3", "4"), area = c("a", "a", "b", "empty"))
origins <- data.frame(
  scenario = "base", origin = c("1", "2", "3", "4", "external"),
  regional_transit_demand = c(10, 90, 100, 0, 0),
  regional_boardings = c(20, 90, 130, 0, 0),
  regional_transfer_events = c(10, 10, 30, 0, 0)
)
areas <- summarise_transfer_areas(origins, zone_areas)
# Demand weighting gives 1.1, not the unweighted mean 1.5. The second origin
# includes zero-boarding trips, so 1 + transfers/trips is also incorrect.
stopifnot(areas$boardings_per_trip[areas$area == "a"] == 1.1)
stopifnot(areas$boardings_per_trip[areas$area == "helsinki_region"] == 1.2)
stopifnot(areas$transfer_events[areas$area == "helsinki_region"] == 50)
stopifnot(is.na(areas$boardings_per_trip[areas$area == "empty"]))
groups <- filter(areas, area != "helsinki_region")
region <- filter(areas, area == "helsinki_region")
stopifnot(sum(groups$transit_demand) == region$transit_demand)
stopifnot(sum(groups$boardings) == region$boardings)

expect_error <- function(expr) stopifnot(inherits(tryCatch(
  { force(expr); NULL }, error = identity
), "error"))
expect_error(summarise_transfer_areas(origins, bind_rows(zone_areas, zone_areas[1, ])))
expect_error(summarise_transfer_areas(bind_rows(origins, origins[1, ]), zone_areas))
expect_error(summarise_transfer_areas(bind_rows(
  origins, mutate(origins[-1, ], scenario = "missing_origin")
), zone_areas))
cat("Transfer aggregation checks passed\n")
