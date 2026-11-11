# Aggregate forecast population and employment for the health workbook.
# Called automatically by terveys.py; no interactive R session is required.
# Requires the yaml package in the Rscript installation's library.
# Rscript --vanilla scripts/terveysmittarit/asukkaat_terveyksiin.R manifest.tsv totals.tsv
# manifest.tsv columns: sheet, forecast (folder under forecast_zonedata_path).

health_area <- function(zone) {
  area <- rep("other", length(zone))
  area[zone %in% 0:999] <- "helsinki_cbd"
  area[zone %in% 1000:1999] <- "helsinki_other"
  area[zone %in% 2000:5999] <- "espoo_vant_kau"
  area[zone %in% c(6000:6999, 10000:11999, 13000:14999, 15500:15999)] <- "surround_train"
  area[zone %in% c(7000:9999, 12000:12999, 15000:15499)] <- "surround_other"
  area
}

read_zone_totals <- function(folder, extension) {
  files <- list.files(folder, pattern = paste0("[.]", extension, "$"), full.names = TRUE)
  if (length(files) != 1L) {
    stop("Expected exactly one .", extension, " file in ", folder, call. = FALSE)
  }
  data <- read.delim(files[[1]], comment.char = "#", check.names = FALSE)
  names(data)[1] <- "zone"
  if (!all(c("zone", "total") %in% names(data)) ||
      anyDuplicated(data$zone) || !is.numeric(data$zone) ||
      !is.numeric(data$total) || anyNA(data$zone) ||
      any(!is.finite(data$total)) || any(data$total < 0)) {
    stop("Invalid zone IDs or totals in ", files[[1]], call. = FALSE)
  }
  data[c("zone", "total")]
}

aggregate_health_forecast <- function(folder) {
  pop <- read_zone_totals(folder, "pop")
  wrk <- read_zone_totals(folder, "wrk")
  if (!setequal(pop$zone, wrk$zone)) {
    stop("Population and employment zone IDs differ in ", folder, call. = FALSE)
  }
  area_order <- c("helsinki_cbd", "helsinki_other", "espoo_vant_kau",
                  "surround_train", "surround_other", "other")
  pop_totals <- tapply(pop$total, health_area(pop$zone), sum)
  wrk_totals <- tapply(wrk$total, health_area(wrk$zone), sum)
  result <- data.frame(area = area_order,
                       total_pop = as.numeric(pop_totals[area_order]),
                       total_wrk = as.numeric(wrk_totals[area_order]))
  if (anyNA(result)) stop("Missing health area in ", folder, call. = FALSE)
  result
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) != 2L) stop("Usage: Rscript asukkaat_terveyksiin.R manifest.tsv totals.tsv")
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  root <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), "../.."),
                        winslash = "/", mustWork = TRUE)
  # This helper uses only base R and yaml, without the full plotting pipeline.
  forecast_root <- yaml::read_yaml(file.path(root, "config.yml"))$default$forecast_zonedata_path
  if (is.null(forecast_root)) stop("Missing default.forecast_zonedata_path in config.yml")
  if (!grepl("^([A-Za-z]:|/|\\\\)", forecast_root)) forecast_root <- file.path(root, forecast_root)
  manifest <- read.delim(args[[1]], colClasses = "character", check.names = FALSE)
  stopifnot(nrow(manifest) > 0, all(c("sheet", "forecast") %in% names(manifest)),
            !anyNA(manifest), !anyDuplicated(manifest$sheet))
  results <- lapply(seq_len(nrow(manifest)), function(i) {
    message("Aggregate land use: ", manifest$sheet[i], " <- ", manifest$forecast[i])
    totals <- aggregate_health_forecast(file.path(forecast_root, manifest$forecast[i]))
    data.frame(sheet = manifest$sheet[i], totals)
  })
  write.table(do.call(rbind, results), args[[2]], sep = "\t", row.names = FALSE,
              quote = FALSE, fileEncoding = "UTF-8")
}

if (sys.nframe() == 0L) main()
