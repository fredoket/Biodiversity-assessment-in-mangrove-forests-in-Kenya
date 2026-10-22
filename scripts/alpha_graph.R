alp3 <- ggplot(
  alpha_table,
  aes(x = site, y = Value, fill = sample_type)
) +
  geom_boxplot(
    position = position_dodge(width = 2),
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = sample_type),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 1),
    alpha = 0.6,
    size = 2
  ) +
  facet_grid(rows = vars(Measure), scales = "free_y") +
  scale_fill_manual(
    values = c("Sediment" = "blue", "Water" = "#52180D")
  ) +
  scale_color_manual(
    values = c("Sediment" = "blue", "Water" = "#52180D")
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      size = 12, face = "bold",
      angle = 90, hjust = 1
    ),
    axis.text.y = element_text(size = 12, face = "bold"),
    strip.text.y = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid = element_blank(),
    legend.title = element_text(face = "bold")
  ) +
  labs(
    x = "Study site",
    y = "Alpha diversity value",
    fill = "Sample type",
    color = "Sample type"
  )
alp3

