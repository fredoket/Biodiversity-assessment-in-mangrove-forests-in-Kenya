#Load packages
library(ggplot2)
library(ggpubr)
library(patchwork)
# Assign colours
sample_cols <- c(
  "Water"    = "#1F78B4",  # blue
  "Sediment" = "#8B4513"   # brown
)




plot_alpha <- function(measure_name) {
  ggplot(
    filter(alpha_summary, Measure == measure_name),
    aes(x = sample_type, y = mean_value, fill = sample_type)
  ) +
    geom_col(
      width = 0.6,
      color = "black"
    ) +
    geom_errorbar(
      aes(
        ymin = mean_value - se_value,
        ymax = mean_value + se_value
      ),
      width = 0.2,
      linewidth = 0.6
    ) +
    scale_fill_manual(values = sample_cols) +
    ggpubr::theme_classic2() +
    theme(
      axis.text.x = element_text(
        size = 11, face = "bold", angle = 0, hjust = 1
      ),
      axis.text.y = element_text(size = 11, face = "bold"),
      axis.title  = element_text(size = 12, face = "bold"),
      plot.title  = element_text(size = 13, face = "bold", hjust = 0.5),
      legend.position = "none"  # remove repeated legends
    ) +
    labs(
      x = "Sample type",
      y = "Mean value (± SE)",
      title = measure_name
    )
}


p_chao1   <- plot_alpha("Chao1")
p_shannon <- plot_alpha("Shannon")
p_simpson <- plot_alpha("Simpson")

