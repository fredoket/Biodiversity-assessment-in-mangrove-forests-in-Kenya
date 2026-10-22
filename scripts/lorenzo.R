df <- data.frame(
  site = c("Bamburi", "Gazi Bay", "General", "Kanamai", "Kilifi Creek",
           "Malindi", "Mida Creek", "Mkomani", "Mtwapa Creek", "Ngomeni",
           "Sabaki", "Shimoni", "Tudor Creek", "Ungwana Bay", "Vanga"),
  studies = c(1, 13, 2, 1, 2, 1, 8, 1, 1, 1, 1, 1, 3, 1, 1)
)

library(ineq)

gini_value <- Gini(df$studies)
gini_value


library(dplyr)
library(ggplot2)

lc <- Lc(df$studies)

lorenz_df <- data.frame(
  cumulative_sites   = lc$p,
  cumulative_studies = lc$L
)


lor <- ggplot(lorenz_df, aes(cumulative_sites, cumulative_studies)) +
  geom_line(color = "black", linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey40") +
  labs(
    x = "Cumulative proportion of sites",
    y = "Cumulative proportion of studies"
  ) +
  theme_classic()
