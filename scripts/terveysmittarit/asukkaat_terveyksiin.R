# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

lf <- list.files(here::here("results"), pattern = "^areas_.*rds", full.names = TRUE)

for (i in 1:length(lf)) {
  areas <- readr::read_rds(lf[i])
  print(lf[i])
  areas %>% dplyr::select(area, total_pop, total_wrk) %>% print()
}

translations <- here::here("utilities", "areas.tsv") %>%
  readr::read_tsv(col_types = "cc") %>%
  dplyr::add_row(level =
                   "other", label = "Muu mallialue")

for (scenario in scenarios$scenario) {
  scenario_attributes <- set_scenario(scenario)
  message(sprintf("Prepare data in scenario %s...", scenario))

  pop <- read_tsv_helmet(
    list.files(file.path(config::get("forecast_zonedata_path"),
                         scenario_attributes[["forecast"]]),
               pattern = ".pop$",
               full.names = TRUE),
    col_types = "idddddd",
    first_col_name = "zone"
  )
  wrk <- read_tsv_helmet(
    list.files(file.path(config::get("forecast_zonedata_path"),
                         scenario_attributes[["forecast"]]),
               pattern = ".wrk$",
               full.names = TRUE),
    col_types = "iidddd",
    first_col_name = "zone"
  )
  pop <- pop %>%
    dplyr::rename(total_pop = total)
  wrk <- wrk %>%
    dplyr::rename(total_wrk = total)

  zones <- pop %>%  dplyr::left_join(wrk, by = "zone")

  zones <- zones %>%
    dplyr::mutate(
      area = dplyr::case_when(
        zone %in% 0:999 ~ "helsinki_cbd",
        zone %in% 1000:1999 ~ "helsinki_other",
        zone %in% 2000:5999 ~ "espoo_vant_kau",
        zone %in% c(6000:6999, 10000:11999, 13000:14999, 15500:15999) ~ "surround_train",
        zone %in% c(7000:9999, 12000:12999, 15000:15499) ~ "surround_other",
        TRUE ~ "other"
      ),
    )

  zones <- zones %>%
    dplyr::mutate(area = factor(area, levels = translations$level, labels = translations$label)) %>%
    dplyr::arrange(area)

  zones %>% dplyr::group_by(area) %>% dplyr::summarise(total_pop = sum(total_pop), total_wrk = sum(total_wrk)) %>% print()

}
