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

# Calc cost per month ----

agents <- agents %>%
  mutate(cost = cost * 30)

# Join housing cost ----

areas_zones <- agents %>%
  distinct(number, area)

housing_cost <- housing_cost %>%
  left_join(areas_zones, by = c("sij2019" = "number")) %>%
  filter(!is.na(area))

housing_cost <- housing_cost %>%
  mutate(muut_asmenot =  asmenot_kalib_hlo - askust_kalib_hlo)

income_data <- income_data %>%
  left_join(areas_zones, by = c("sij2019" = "number")) %>%
  filter(!is.na(area))

# Summarise by area ----

areas_agents <- agents %>%
  group_mean("area", c("cost"))

areas_housing_cost <- housing_cost %>%
  group_mean("area", c("askust_kalib_hlo", "muut_asmenot"))

areas_income_data <- income_data %>%
  group_mean("area", c("hr_mtu"))

areas <- areas_agents %>%
  left_join(areas_housing_cost, by = "area")

areas_income_data <- areas_income_data %>%
  left_join(areas, by = "area") %>%
  mutate(total_cost = cost + askust_kalib_hlo + muut_asmenot,
         cost_income = total_cost / hr_mtu
         ) %>%
  select(area, hr_mtu, cost_income)

# Plot ----

results <- areas %>%
  gather("type", "cost",
         askust_kalib_hlo, muut_asmenot, cost) %>%
  mutate(type = case_when(
    type %in% "askust_kalib_hlo" ~ "Asumisen kustannus",
    type %in% "muut_asmenot" ~ "Asuntolainan lyhennykset",
    type %in% "cost" ~ "Liikkumisen suora kustannus"))

plot_cols <- c(
  hsl_pal("blues")(2), hsl_cols("green")
)

results %>%
  ggplot(aes(x = area, y = cost, fill = type)) +
  geom_bar(
    stat = "identity",
    position = "stack",
    color = "white",
    width = 0.8
  ) +
  scale_fill_manual(values = plot_cols) +
  geom_point(
    data = areas_income_data,
    aes(x = area, y = hr_mtu, color = "Mediaanitulot"),
    size = 5,
    pch = 1,
    inherit.aes = FALSE
  ) +
  scale_color_manual(NULL, values = "black") +
  theme_fig +
  geom_abline(slope = 0) +
  labs(
    fill = NULL,
    y = "eur / asukas / kk",
    x = NULL,
    title =
      paste0(
        "Liikkumisen ja asumisen suorat kustannukset: ",
        config::get("present_name")
      )
  )

ggsave(
  here(
    "figures",
    config::get("projected_scenario"),
    "cost_income_housing_transport.png"
    ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)

