
library(tidyverse)
library(ggsignif)
library(tidyverse)
library(ggsignif)

# Data: summarized mean ± SE
data <- tribble(
  ~Season, ~Treatment, ~Variable, ~Mean, ~SE, ~Letters,
  # --- Dry season ---
  "Dry", "T1", "Disease severity (%)", 28.14, 0.16, "c",
  "Dry", "T2", "Disease severity (%)", 43.40, 0.32, "b",
  "Dry", "T3", "Disease severity (%)", 71.16, 1.03, "a",
  
  "Dry", "T1", "Grain Yield (Kg/ha)", 4262.00, 3.18, "a",
  "Dry", "T2", "Grain Yield (Kg/ha)", 3023.00, 59.57, "b",
  "Dry", "T3", "Grain Yield (Kg/ha)", 2723.67, 116.86, "c",
  
  "Dry", "T1", "Yellowing (No.)", 2.00, 0.33, "b",
  "Dry", "T2", "Yellowing (No.)", 2.00, 0.33, "b",
  "Dry", "T3", "Yellowing (No.)", 4.00, 0.33, "a",
  
  "Dry", "T1", "Wilting (No.)", 1.00, 0.33, "c",
  "Dry", "T2", "Wilting (No.)", 3.00, 0.33, "b",
  "Dry", "T3", "Wilting (No.)", 4.00, 0.33, "a",
  
  "Dry", "T1", "Root rot (No.)", 1.00, 0.33, "b",
  "Dry", "T2", "Root rot (No.)", 3.00, 0.33, "b",
  "Dry", "T3", "Root rot (No.)", 5.00, 0.58, "a",
  
  "Dry", "T1", "No. of seeds per pod", 8.00, 0.33, "a",
  "Dry", "T2", "No. of seeds per pod", 4.00, 0.58, "b",
  "Dry", "T3", "No. of seeds per pod", 3.00, 0.88, "b",
  
  "Dry", "T1", "No. of pods per plant", 11.00, 1.20, "a",
  "Dry", "T2", "No. of pods per plant", 7.00, 0.58, "b",
  "Dry", "T3", "No. of pods per plant", 5.00, 0.33, "b",
  
  # --- Rainy season ---
  "Rainy", "T1", "Disease severity (%)", 33.92, 1.40, "c",
  "Rainy", "T2", "Disease severity (%)", 47.67, 0.22, "b",
  "Rainy", "T3", "Disease severity (%)", 66.91, 0.88, "a",
  
  "Rainy", "T1", "Grain Yield (Kg/ha)", 4818.33, 11.89, "a",
  "Rainy", "T2", "Grain Yield (Kg/ha)", 3206.33, 17.79, "b",
  "Rainy", "T3", "Grain Yield (Kg/ha)", 2740.00, 33.15, "c",
  
  "Rainy", "T1", "Yellowing (No.)", 3.00, 0.58, "a",
  "Rainy", "T2", "Yellowing (No.)", 3.00, 0.00, "a",
  "Rainy", "T3", "Yellowing (No.)", 4.00, 0.58, "a",
  
  "Rainy", "T1", "Wilting (No.)", 3.00, 0.00, "b",
  "Rainy", "T2", "Wilting (No.)", 4.00, 0.58, "b",
  "Rainy", "T3", "Wilting (No.)", 5.00, 0.33, "a",
  
  "Rainy", "T1", "Root rot (No.)", 2.00, 0.58, "b",
  "Rainy", "T2", "Root rot (No.)", 3.00, 0.33, "ab",
  "Rainy", "T3", "Root rot (No.)", 5.00, 0.58, "a",
  
  "Rainy", "T1", "No. of seeds per pod", 9.00, 0.58, "a",
  "Rainy", "T2", "No. of seeds per pod", 6.00, 0.88, "b",
  "Rainy", "T3", "No. of seeds per pod", 5.00, 0.33, "b",
  
  "Rainy", "T1", "No. of pods per plant", 13.00, 1.20, "a",
  "Rainy", "T2", "No. of pods per plant", 9.00, 0.33, "b",
  "Rainy", "T3", "No. of pods per plant", 7.00, 0.58, "b"
)

# (Use the same dataset definition as before)
# If you've already run the `data <- tribble(...)` block earlier, skip redefining it.

# --- Reorder factors for nicer display ---
data <- data %>%
  mutate(
    Treatment = factor(Treatment, levels = c("T1", "T2", "T3")),
    Variable = factor(Variable, 
                      levels = c("Disease severity (%)", "Grain Yield (Kg/ha)",
                                 "Yellowing (No.)", "Wilting (No.)", "Root rot (No.)",
                                 "No. of seeds per pod", "No. of pods per plant"))
  )

ggplot(data, aes(x = Treatment, y = Mean, fill = Treatment)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                width = 0.2, position = position_dodge(0.8)) +
  geom_text(aes(label = Letters),
            position = position_dodge(0.8), vjust = -0.5, size = 3.5) +
  facet_wrap(~Variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("T1" = "#009E73", "T2" = "#E69F00", "T3" = "#D55E00")) +
  labs(x = "Treatment", y = "Mean ± SE", fill = "Treatment") +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )

t1 <- ggplot(data, aes(x = Treatment, y = Mean)) +
  geom_col(fill = "grey60", width = 0.4) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                width = 0.2) +
  geom_text(aes(label = Letters),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~Variable, scales = "free_y", ncol = 3) +
  labs(x = "Treatment", y = "Mean ± SE") +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )
t1

ggplot(data, aes(x = Treatment, y = Mean, fill = Treatment)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  geom_text(aes(label = Letters), vjust = -0.5, size = 3.5) +
  facet_wrap(~Variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("T1" = "#009E73", "T2" = "#E69F00", "T3" = "#D55E00")) +
  labs(x = "Treatment", y = "Mean ± SE", fill = "Treatment") +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )

ty <- ggplot(data, aes(x = Treatment, y = Mean, fill = Season)) +
  geom_col(position = position_dodge(0.8), width = 0.5) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE, group = Season),
                width = 0.2, position = position_dodge(0.8)) +
  geom_text(aes(label = Letters, group = Season),
            position = position_dodge(0.8), vjust = -0.5, size = 3.5) +
  facet_wrap(~Variable, scales = "free_y", ncol = 3)


ty <- ggplot(data, aes(x = Treatment, y = Mean, fill = Season)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE, group = Season),
    width = 0.5,
    position = position_dodge(0.8)
  ) +
  geom_text(
    aes(label = Letters, group = Season),
    position = position_dodge(0.8),
    vjust = -1.2,   # moves labels further above error bars
    size = 3.5
  ) +
  facet_wrap(~Variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("Dry" = "grey50", "Rainy" = "#0B2D6B")) +
  labs(x = "Treatment", y = "Mean ± SE", fill = "Season") +
  ggsankey::theme_alluvial(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

ty

ty
