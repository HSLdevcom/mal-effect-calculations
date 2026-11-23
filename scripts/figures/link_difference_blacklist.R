# Hide explicitly listed geometry replacements in link difference maps.
# Entries use directed network node IDs, never cache row numbers or buffers.
# Edit utilities/link_difference_blacklist.tsv to add or remove exclusions.
# List each direction separately, including replacement segments after a split.
# Both scenarios must match; links with matching geometry remain visible.
filter_link_difference_blacklist <- function(
    results, scenario0, scenario1,
    blacklist = readr::read_tsv(
      here::here("utilities", "link_difference_blacklist.tsv"),
      col_types = "cccdd"
    )) {
  comparison <- (blacklist$scenario0 == scenario0 & blacklist$scenario1 == scenario1) |
    (blacklist$scenario0 == scenario1 & blacklist$scenario1 == scenario0)
  blacklist <- blacklist[comparison, ]
  if (nrow(blacklist) == 0L) return(results)

  excluded_ids <- paste(blacklist$node_i, blacklist$node_j, sep = ":")
  excluded <- paste(results$node_i0, results$node_j0, sep = ":") %in% excluded_ids |
    paste(results$node_i1, results$node_j1, sep = ":") %in% excluded_ids
  only_one_network <- is.na(results$node_i0) | is.na(results$node_i1)
  results[!(only_one_network & excluded), ]
}
