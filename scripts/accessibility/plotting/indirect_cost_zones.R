library(tidyverse)
library(here)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")
source(here("scripts", "basemap", "functions_map.R"),
       encoding = "utf-8")

# Read the same agent and zone data as the direct-cost maps ----

load(here("results", config::get("projected_scenario"), "agents.Rdata"))
zones <- readr::read_rds(here("results", "zones.rds"))

# Current zone data use SIJ2023; older datasets use SIJ2019.
zone_column <- if ("SIJ2023" %in% names(zones)) "SIJ2023" else "SIJ2019"
zones <- zones %>%
  dplyr::rename(zone = all_of(zone_column))

# Calculate indirect costs per agent before averaging by zone.
# Agent costs are daily; multiply by 30 to match the direct-cost maps.
zone_indirect_cost <- function(agent_data) {
  agent_data %>%
    mutate(indirect_cost = (gen_cost - cost) * 30) %>%
    group_mean("number", "indirect_cost")
}

zones <- zones %>%
  left_join(zone_indirect_cost(agents), by = c("zone" = "number")) %>%
  left_join(zone_indirect_cost(agents_0), by = c("zone" = "number"),
            suffix = c("", "_0")) %>%
  left_join(zone_indirect_cost(agents_1), by = c("zone" = "number"),
            suffix = c("", "_1")) %>%
  mutate(indirect_cost_diff = indirect_cost_1 - indirect_cost_0)

# Present and projected costs ----

plot_indirect_cost <- function(cost_column, scenario_name, scenario_id) {
  plot <- ggplot() +
    geom_sf(data = zones, aes(fill = .data[[cost_column]]), color = NA) +
    scale_fill_steps(
      name = "eur / asukas / kk",
      limits = c(0, 600),
      breaks = seq(100, 500, 100),
      high = hsl_cols("blue"),
      low = hsl_cols("white"),
      na.value = hsl_cols("lightgray"),
      oob = scales::squish
    ) +
    geom_basemap() +
    coord_sf_mal() +
    annotate_map(
      title = "Liikkumisen epäsuorat kustannukset (eur)",
      subtitle = scenario_name
    ) +
    theme_mal_map()

  ggsave_map(
    here("figures", config::get("projected_scenario"),
         paste0("indirect_cost_transport_", scenario_id, ".png")),
    plot = plot
  )
}

plot_indirect_cost("indirect_cost", config::get("present_name"),
                   config::get("present_scenario"))
plot_indirect_cost("indirect_cost_1", config::get("projected_name"),
                   config::get("projected_scenario"))

# Projected minus baseline, matching the direct-cost change map ----

ggplot() +
  geom_sf(data = filter_outliers(zones, "indirect_cost_diff"),
          aes(fill = indirect_cost_diff), color = NA) +
  scale_fill_steps2(
    name = "eur / asukas / kk",
    limits = c(-55, 55),
    breaks = seq(-45, 45, 10),
    high = hsl_cols("red"),
    mid = hsl_cols("white"),
    low = hsl_cols("blue"),
    na.value = hsl_cols("lightgray"),
    oob = scales::squish
  ) +
  geom_basemap() +
  coord_sf_mal() +
  annotate_map(
    title = "Liikkumisen epäsuorat kustannukset (eur)",
    subtitle = paste0("Muutos: ", config::get("projected_name"),
                      " - ", config::get("baseline_name"))
  ) +
  theme_mal_map()

ggsave_map(
  here("figures", config::get("projected_scenario"),
       "indirect_cost_change_transport.png")
)
