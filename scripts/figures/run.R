# -*- coding: utf-8-unix -*-
library(here)

verbose_source <- function(file, ...) {
  message(sprintf("Running analysis in %s...", basename(file)))
  invisible(source(file, ...))
}

# When running maps and graphs, the order of the files does not matter.
files_in <- list.files(here::here("scripts", "figures"),
                       pattern = ".R$",
                       full.names = TRUE)

lapply(files_in, verbose_source, encoding = "utf-8")
