# Run from the repository root: Rscript --vanilla tests/test-basemap.R
# Extract the helpers without loading unrelated cached basemap datasets.
expressions <- parse("scripts/basemap/functions_map.R", encoding = "UTF-8")
for (expr in expressions) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      as.character(expr[[2]]) %in% c("train_for_scenarios", "geom_basemap")) {
    eval(expr)
  }
}

train <- data.frame(id = 1:4, suunnitelma = c(0, 1, NA, 0))
without_plan <- c(1L, 3L, 4L)
for (ids in list(character(), "2023", "2045_ve0", "2045_tavoiteverkko",
                 c("2023", "2045_ve0"),
                 "Tavoiteverkko_2045_Ruuhkainen_joukkoliikenne_H51")) {
  stopifnot(identical(train_for_scenarios(ids)$id, without_plan))
}
for (ids in list("2045_suunnitelma", "Suunnitelmaluonnos_2045_H51_agentti",
                 "Suunnitelma_2045_Ruuhkainen_tieverkko_H51",
                 c("2045_ve0", "2045_suunnitelma"),
                 c("2045_suunnitelma", "2045_ve0"))) {
  stopifnot(identical(train_for_scenarios(ids), train))
}
train$suunnitelma <- as.character(train$suunnitelma)
stopifnot(identical(train_for_scenarios("2023")$id, without_plan))

# A previous plan plot must not leak its railway into a later comparison.
scenario <- scenario1 <- "2045_suunnitelma"
scenario_attributes <- list(scenario = scenario)
stopifnot(identical(train_for_scenarios(c("2023", "2045_ve0"))$id, without_plan))

# Both strokes of the railway must use the same filtered features.
geom_sf <- function(data, ...) list(data = data)
water <- municipalities <- roads <- metro <- region <- bonus_region <- data.frame()
layers <- geom_basemap(scenario_ids = c("2023", "2045_ve0"))
stopifnot(identical(layers[[4]]$data$id, without_plan),
          identical(layers[[5]]$data$id, without_plan))
layers <- geom_basemap(show_roads = FALSE, scenario_ids = "2045_suunnitelma")
stopifnot(is.null(layers[[3]]), identical(layers[[4]]$data, train),
          identical(layers[[5]]$data, train))

names(train)[names(train) == "suunnitelma"] <- "Suunnitelma"
stopifnot(identical(train_for_scenarios("2023")$id, without_plan))
train$Suunnitelma <- NULL
stopifnot(inherits(tryCatch(train_for_scenarios(), error = identity), "error"))
cat("Basemap scenario tests passed.\n")
