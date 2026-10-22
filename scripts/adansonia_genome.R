library(tidyverse)
library(ggrepel)

# === Read the data ===
pi2 <- read.table("data/pi_results",
                  header = FALSE,
                  sep = "\t",
                  col.names = c("Region", "Pi")) |>
  filter(Region != "trnM-CAU")


pi2 <- pi2 |> 
  mutate(Index = 1:n()) 

pi2$Index <- seq_len(nrow(pi2))
# Ensure Region is in genome order
pi2$Region <- factor(pi2$Region, levels = pi2$Region)

# Plot
ggplot(pi2, aes(x = Region, y = Pi, group = 1)) +
  geom_line(color = "grey60", linewidth = 1) +
  geom_point(color = "grey60", size = 1) +
  geom_text_repel(
    data = subset(pi2, Pi > 0.0125),
    aes(label = Region),
    size = 3,
    nudge_y = 0.002,
    color = "black"
  )+
  theme_minimal(base_size = 14) +
  labs(
    x = "Intergenic Regions",
    y = expression(Pi)
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


pi <- read.table("data/pi_genic",
                 header = FALSE,
                 sep = "\t",
                 col.names = c("Region", "Pi"))


pi <- pi %>%
  mutate(Index = 1:n()) 


# Ensure Region is in genome order
pi$Region <- factor(pi$Region, levels = pi$Region)

# Plot
ggplot(pi, aes(x = Region, y = Pi, group = 1)) +
  geom_line(color = "grey60", linewidth = 1) +
  geom_point(color = "grey60", size = 1) +
  geom_text_repel(
    data = subset(pi, Pi > 0.004),
    aes(label = Region),
    size = 3,
    nudge_y = 0.002,
    color = "black"
  )+
  theme_minimal(base_size = 14) +
  labs(
    x = "Genes Regions",
    y = expression(pi)
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

