library(tidyverse)
library(config)
library(here)

# Read files ----

file_path <-
  here("results", config::get("projected_scenario"), "agents.Rdata")

load(file_path)

# Group agents tables ----

res_var <- c("total_access")
group_var <- c("area")

mean_agents <- function(df){
  df <- df %>%
    group_by(!!!syms(group_var)) %>%
    summarise(across(all_of(res_var), mean, na.rm = TRUE)) %>%
    ungroup()
}

agents <- agents %>%
  mean_agents()

agents_0 <- agents_0 %>%
  mean_agents()

agents_1 <- agents_1 %>%
  mean_agents()

# Join tables ----

agents <- full_join(agents,
                    agents_0,
                    by = group_var,
                    suffix = c("", "0"))

agents <- full_join(agents,
                    agents_1,
                    by = group_var,
                    suffix = c("", "1"))

# Calc differences ----

agents <- agents %>%
  mutate(
    projected = total_access1,
    baseline = total_access0,
    util_dif = projected - baseline
  )

# Plot ----
max_dif <- 3

agents %>%
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
