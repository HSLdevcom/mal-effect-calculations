# Read the named noise bands shared by the old and new Helmet exports.
# Report exposure at 55 dB and above; *_old is a different legacy measure.
read_noise_areas <- function(path) {
  noise <- readr::read_tsv(path, show_col_types = FALSE, name_repair = "minimal")
  names(noise)[1] <- "area"
  names(noise) <- sub("_dB$", "", names(noise))
  bands <- c("55_59", "60_64", "65_69")
  if (all(c("area_70_75", "population_70_75") %in% names(noise))) {
    bands <- c(bands, "70_75")
  } else {
    bands <- c(bands, "70_74", "75_plus")
  }
  area_columns <- paste0("area_", bands)
  population_columns <- paste0("population_", bands)
  required <- c(area_columns, population_columns)
  if (!all(required %in% names(noise))) {
    stop("Missing noise bands in ", path, ": ",
         paste(setdiff(required, names(noise)), collapse = ", "))
  }
  values <- as.matrix(noise[, required])
  if (!is.numeric(values) || any(!is.finite(values)) || any(values < 0) ||
      anyNA(noise$area) || anyDuplicated(noise$area)) {
    stop("Invalid noise values or duplicate areas in ", path)
  }
  tibble::tibble(
    area = noise$area,
    noise_area_km2 = rowSums(noise[, area_columns]),
    noise_population = rowSums(noise[, population_columns])
  )
}
