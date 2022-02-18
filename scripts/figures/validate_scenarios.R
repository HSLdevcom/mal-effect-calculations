# -*- coding: utf-8-unix -*-
library(here)
library(tidyverse)

scenarios <- readr::read_tsv(
  here::here("scenarios.tsv"),
  col_types = list(
    year = readr::col_integer(),
    co2 = readr::col_integer(),
    present = readr::col_logical(),
    baseline = readr::col_logical(),
    projected = readr::col_logical(),
    sensitivity = readr::col_logical(),
    .default = readr::col_character()
  )
)

stopifnot(all(!duplicated(scenarios$uid)))
stopifnot(sum(scenarios$present) == 1)
stopifnot(sum(scenarios$baseline) == 1)
stopifnot(sum(scenarios$projected) >= 1)
stopifnot(all(rowSums(dplyr::select(scenarios, present, baseline, projected, sensitivity)) == 1))
stopifnot(all(scenarios$root %in% scenarios$uid))
