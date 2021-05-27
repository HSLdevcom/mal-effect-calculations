library(tidyverse)
library(config)
library(here)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

file_path <- here("data", config::get("housing_cost"))
housing_cost <- read_delim(file_path, delim = "\t")

# Join housing cost ----

housing_cost <- housing_cost %>%
  select(zone, askust_kalib)

agents <- agents %>%
  left_join(housing_cost, by = c("number" = "zone"))

agents_0 <- agents_0 %>%
  left_join(housing_cost, by = c("number" = "zone"))

agents_1 <- agents_1 %>%
  left_join(housing_cost, by = c("number" = "zone"))

# Add time cost variable ----

agents <- agents %>%
  mutate(
    cost = cost * 30,
    gen_cost = gen_cost * 30,
    time_cost = gen_cost - cost
    )

agents_0 <- agents_0 %>%
  mutate(
    cost = cost * 30,
    gen_cost = gen_cost * 30,
    time_cost = gen_cost - cost
  )

agents_1 <- agents_1 %>%
  mutate(
    cost = cost * 30,
    gen_cost = gen_cost * 30,
    time_cost = gen_cost - cost
  )

# Summarise by area ----

res_var <- c(
  "askust_kalib",
  "cost",
  "time_cost",
  "income",
  "gen_cost"
  )

group_var <- "area"

agent_sums <- agents %>%
  group_mean(group_var, res_var) %>%
  mutate(scenario = config::get("present_name"))

agent_sums_0 <- agents_0 %>%
  group_mean(group_var, res_var) %>%
  mutate(scenario = config::get("baseline_name"))

agent_sums_1 <- agents_1 %>%
  group_mean(group_var, res_var) %>%
  mutate(scenario = config::get("projected_name"))

# Join tables ----

agent_sums <- bind_rows(agent_sums,
                        agent_sums_0,
                        agent_sums_1)

# Gen cost plot ----

results <- agent_sums %>%
  gather("type", "cost", askust_kalib, cost, time_cost) %>%
  mutate(type = case_when(
    type %in% "askust_kalib" ~ "Asuminen",
    type %in% "cost" ~ "Liikkumisen suora kustannus",
    type %in% "time_cost" ~ "Liikkumisen aikakustannus"))

results %>%
  ggplot(aes(x = scenario, y = cost, fill = type)) +
  geom_bar(
    stat = "identity",
    position = "stack",
    color = "white",
    width = 0.8
  ) +
  facet_wrap( ~ area, nrow = 1) +
  scale_fill_manual(values = hsl_pal("blues")(3)) +
  theme_wide +
  geom_abline(slope = 0) +
  labs(
    fill = NULL,
    y = "eur / asukas / kk",
    x = NULL,
    title =
      paste0(
        "Asumisen ja liikkumisen kustannukset: ",
        config::get("present_name")
      )
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

results <- agent_sums %>%
  gather("type", "cost", askust_kalib, cost) %>%
  mutate(type = case_when(
    type %in% "askust_kalib" ~ "Asuminen",
    type %in% "cost" ~ "Liikkuminen"))

results %>%
  ggplot(aes(x = scenario, y = cost, fill = type)) +
  geom_bar(
    stat = "identity",
    position = "stack",
    color = "white",
    width = 0.8
  ) +
  facet_wrap( ~ area, nrow = 1) +
  scale_fill_manual(values = hsl_pal("blues")(2)) +
  theme_wide +
  geom_abline(slope = 0) +
  labs(
    fill = NULL,
    y = "eur / asukas / kk",
    x = NULL,
    title =
      paste0(
        "Asumisen ja liikkumisen suorat kustannukset: ",
        config::get("present_name")
      )
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
