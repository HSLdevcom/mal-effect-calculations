# -*- coding: utf-8-unix -*-
library(here)

source(here::here("scripts", "figures", "zones.R"), encoding = "utf-8")

verbose_source <- function(file, ...) {
  message(sprintf("Running analysis in %s...", basename(file)))
  invisible(source(file, ...))
}

# When running maps and graphs, the order of the files does not matter.
files_in <- list.files(here::here("scripts", "figures"),
                       pattern = "^map.*R$|^graph.*R$",
                       full.names = TRUE)

lapply(files_in, verbose_source, encoding = "utf-8")
