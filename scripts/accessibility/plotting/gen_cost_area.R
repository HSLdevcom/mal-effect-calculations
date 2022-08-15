library(tidyverse)
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
    type %in% "time_cost" ~ "Liikkumisen aikakustannus")) %>%
  mutate(type = factor(type,
                       levels = c("Liikkumisen aikakustannus",
                                  "Liikkumisen suora kustannus",
                                  "Asumisen kustannus",
                                  "Asuntolainan lyhennykset")))

ggplot(results, aes(x = scenario, y = cost)) +
  facet_grid(cols = vars(area), switch = "both", labeller = labeller(.cols = scales::label_wrap(10))) +
  geom_col(fill = "white") +
  geom_col(aes(fill = type, alpha = forcats::fct_rev(scenario))) +
  geom_text(
    aes(label = scales::label_number(accuracy = 1)(cost), group = type),
    position = position_stack(vjust = 0.5),
    size = points2mm(8),
    color = "#333333"
  ) +
  # geom_text(data = results_total,
  #           aes(label = scales::label_percent(accuracy = 1, suffix = "")(cost)),
  #           vjust = -0.5,
  #           size = points2mm(8),
  #           fontface = "bold",
  #           color = "#333333") +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 10),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(
    labels = NULL
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Asumisen kustannus" = "#BEE4F8",
               "Asuntolainan lyhennykset" = "#007AC9",
               "Liikkumisen suora kustannus" = "#64BE1E",
               "Liikkumisen aikakustannus" = "#D0E6BE"),
    guide = guide_legend(order = 1)
  ) +
  scale_alpha_discrete(
    name = NULL,
    range = c(0.333, 1),
    guide = guide_legend(order = 2, reverse = TRUE)
  ) +
  labs(
    title = "Liikkumisen ja asumisen keskimääräiset kustannukset",
    x = NULL,
    y = "eur / asukas / kk"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures",
                        config::get("projected_scenario"),
                        "graph_gen_cost_housing_transport_areas.png"),
             width = 234,
             height = 107)

# Direct cost plot ----

results <- areas %>%
  gather("type", "cost",
         askust_kalib_hlo, muut_asmenot, cost) %>%
  mutate(type = case_when(
    type %in% "askust_kalib_hlo" ~ "Asumisen kustannus",
    type %in% "muut_asmenot" ~ "Asuntolainan lyhennykset",
    type %in% "cost" ~ "Liikkumisen suora kustannus")) %>%
  mutate(type = factor(type,
                       levels = c("Liikkumisen suora kustannus",
                                  "Asumisen kustannus",
                                  "Asuntolainan lyhennykset")))

ggplot(results, aes(x = scenario, y = cost)) +
  facet_grid(cols = vars(area), switch = "both", labeller = labeller(.cols = scales::label_wrap(10))) +
  geom_col(fill = "white") +
  geom_col(aes(fill = type, alpha = forcats::fct_rev(scenario))) +
  geom_text(
    aes(label = scales::label_number(accuracy = 1)(cost), group = type),
    position = position_stack(vjust = 0.5),
    size = points2mm(8),
    color = "#333333"
  ) +
  # geom_text(data = results_total,
  #           aes(label = scales::label_percent(accuracy = 1, suffix = "")(cost)),
  #           vjust = -0.5,
  #           size = points2mm(8),
  #           fontface = "bold",
  #           color = "#333333") +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 10),
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_x_discrete(
    labels = NULL
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Asumisen kustannus" = "#BEE4F8",
               "Asuntolainan lyhennykset" = "#007AC9",
               "Liikkumisen suora kustannus" = "#64BE1E"),
    guide = guide_legend(order = 1)
  ) +
  scale_alpha_discrete(
    name = NULL,
    range = c(0.333, 1),
    guide = guide_legend(order = 2, reverse = TRUE)
  ) +
  labs(
    title = "Liikkumisen ja asumisen keskimääräiset kustannukset",
    x = NULL,
    y = "eur / asukas / kk"
  ) +
  theme_mal_graph()

ggsave_graph(here::here("figures",
                        config::get("projected_scenario"),
                        "graph_cost_housing_transport_areas.png"),
             width = 234,
             height = 107)
