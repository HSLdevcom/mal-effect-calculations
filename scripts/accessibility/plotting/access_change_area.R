library(tidyverse)
library(config)
library(here)
source(here("scripts", "accessibility", "helpers.R"),
       encoding = "utf-8")

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

# Group agents tables ----

res_var <- c("total_access")
group_var <- c("area")

agent_sums <- agents %>%
  group_mean(group_var, res_var)

agent_sums_0 <- agents_0 %>%
  group_mean(group_var, res_var)

agent_sums_1 <- agents_1 %>%
  group_mean(group_var, res_var)

# Join tables ----

agent_sums <- full_join(agent_sums,
                        agent_sums_0,
                        by = group_var,
                        suffix = c("", "0"))

agent_sums <- full_join(agent_sums,
                        agent_sums_1,
                        by = group_var,
                        suffix = c("", "1"))

# Calc differences ----

agent_sums <- agent_sums %>%
  mutate(
    projected = total_access1,
    baseline = total_access0,
    util_dif = projected - baseline
  )

# Plot ----
max_dif <- 10

agent_sums %>%
  ggplot(aes(x = area, y = util_dif)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    fill = hsl_cols("blue"),
    width = 0.8
  ) +
  ylim(-max_dif, max_dif) +
  theme_fig +
  geom_abline(slope = 0) +
  labs(
    y = "eur / asukas",
    x = NULL,
    title = paste0(
      "Muutos asukkaan matkojen saavutettavuudessa: ",
      config::get("projected_name")
    )
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "access_change_area.png"
       ),
  width = dimensions_fig[1],
  height = dimensions_fig[2],
  units = "cm"
)
