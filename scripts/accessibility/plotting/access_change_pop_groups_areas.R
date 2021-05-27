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
group_var <- c("age_group",
               "gender",
               "area")

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

max_dif <- 3

agent_sums %>%
  ggplot(aes(x = age_group, y = util_dif, fill = gender)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "white",
    width = 0.8
  ) +
  scale_fill_manual(values = hsl_cols("blue", "green")) +
  facet_wrap( ~ area, nrow = 1) +
  theme_wide +
  geom_abline(slope = 0) +
  ylim(-max_dif, max_dif) +
  labs(
    fill = "Sukupuoli",
    y = "eur / asukas",
    x = "Ikäryhmät",
    title = paste0(
      "Muutos asukkaan tekemien matkojen saavutettavuudessa: ",
      config::get("projected_name")
    )
  )

ggsave(
  here("figures",
       config::get("projected_scenario"),
       "access_change_pop_group_areas.png"
       ),
  width = dimensions_wide[1],
  height = dimensions_wide[2],
  units = "cm"
)
