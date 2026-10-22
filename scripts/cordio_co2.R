pacman::p_load(
  tidyverse,
  lme4)

df_co2 <- read_csv("data/co2.csv") |>
  janitor::clean_names() |>
  select(site, species, status, flow, area_cm) |>
  drop_na()


lmer(flow ~ area_cm * species * status + (1|site), data = df_co2)

ggplot(df_co2, aes(x = area_cm, y = flow, color = status)) +
  geom_point() +
  geom_smooth(method = "lm") #+
  facet_wrap(~ species)
