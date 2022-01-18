library(tidyverse)
library(config)
library(here)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

file_path <-
  here("results",
       config::get("projected_scenario"),
       "housing_costs_income.Rdata")

load(file_path)


# Join housing cost ----

areas_zones <- agents %>%
  distinct(number, area)

housing_cost <- housing_cost %>%
  left_join(areas_zones, by = c("sij2019" = "number")) %>%
  filter(!is.na(area))

housing_cost <- housing_cost %>%
  mutate(muut_asmenot =  asmenot_kalib_hlo - askust_kalib_hlo)

# Summarise by area ----

res_vars <- c("cost", "gen_cost")

agents_sums <- agents %>%
  group_mean("area", res_vars) %>%
  mutate(scenario = config::get("present_name"))

agents_sums_0 <- agents_0 %>%
  group_mean("area", res_vars) %>%
  mutate(scenario = config::get("baseline_name"))

agents_sums_1 <- agents_1 %>%
  group_mean("area", res_vars) %>%
  mutate(scenario = config::get("projected_name"))

areas_housing_cost <- housing_cost %>%
  group_mean("area", c("askust_kalib_hlo", "muut_asmenot"))

areas <- agents_sums %>%
  bind_rows(agents_sums_0, agents_sums_1) %>%
  mutate(scenario = factor(scenario, levels = c(config::get("present_name"),
                                                config::get("baseline_name"),
                                                config::get("projected_name")))) %>%
  left_join(areas_housing_cost, by = "area")

# Cost data to month ----

areas <- areas %>%
  mutate(cost = 30 * cost,
         gen_cost = 30 * gen_cost,
         time_cost = gen_cost - cost
         )

# Gen cost plot ----

results <- areas %>%
  gather("type", "cost",
         askust_kalib_hlo, muut_asmenot, cost, time_cost) %>%
  mutate(type = case_when(
    type %in% "askust_kalib_hlo" ~ "Asumisen kustannus",
    type %in% "muut_asmenot" ~ "Asuntolainan lyhennykset",
    type %in% "cost" ~ "Liikkumisen suora kustannus",
    type %in% "time_cost" ~ "Liikkumisen aikakustannus"))

plot_cols <- c(
  hsl_pal("blues")(2), hsl_pal("greens")(2)
)

results %>%
  ggplot(aes(
    x = scenario,
    y = cost,
    fill = type,
    label = round(cost, -1)
  )) +
  geom_bar(
    stat = "identity",
    position = "stack",
    color = "white",
    width = 0.8
  ) +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  facet_wrap(~ area, nrow = 1) +
  scale_fill_manual(values = plot_cols) +
  theme_wide +
  geom_abline(slope = 0) +
  labs(
    fill = NULL,
    y = "eur / asukas / kk",
    x = NULL,
    title = "Liikkumisen ja asumisen keskimääräiset kustannukset"
    )

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "gen_cost_housing_transport_areas.png"
    ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)

# Direct cost plot ----

plot_cols <- c(
  hsl_pal("blues")(2), hsl_cols("green")
)

results <- areas %>%
  gather("type", "cost",
         askust_kalib_hlo, muut_asmenot, cost) %>%
  mutate(type = case_when(
    type %in% "askust_kalib_hlo" ~ "Asumisen kustannus",
    type %in% "muut_asmenot" ~ "Asuntolainan lyhennykset",
    type %in% "cost" ~ "Liikkumisen suora kustannus"))

results %>%
  ggplot(aes(
    x = scenario,
    y = cost,
    fill = type,
    label = round(cost, -1)
  )) +
  geom_bar(
    stat = "identity",
    position = "stack",
    color = "white",
    width = 0.8
  ) +
  geom_text(size = 2, position = position_stack(vjust = 0.5)) +
  facet_wrap(~ area, nrow = 1) +
  scale_fill_manual(values = plot_cols) +
  theme_wide +
  geom_abline(slope = 0) +
  labs(
    fill = NULL,
    y = "eur / asukas / kk",
    x = NULL,
    title =  "Liikkumisen ja asumisen keskimääräiset kustannukset"
    )

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "cost_housing_transport_areas.png"
  ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)

