# Streamgraph — manual construction (no ggstream dependency; see note below)
# Self-contained: data is embedded below.

library(tidyverse)

df_raw <- tribble(
  ~year, ~phylum,        ~count,
  1990,  "Arthropoda",   32,
  1990,  "Others",       18,
  1990,  "Teleostei",    5,
  1991,  "Tracheophyta", 5,
  1995,  "Aves",         62,
  1995,  "Teleostei",    2,
  1996,  "Arthropoda",   25,
  1996,  "Aves",         67,
  1996,  "Others",       3,
  1996,  "Others",       1,
  1996,  "Teleostei",    59,
  1996,  "Tracheophyta", 27,
  1998,  "Teleostei",    1,
  1998,  "Tracheophyta", 6,
  1999,  "Arthropoda",   5,
  1999,  "Others",       1,
  1999,  "Teleostei",    234,
  2004,  "Others",       1,
  2004,  "Teleostei",    35,
  2004,  "Tracheophyta", 5,
  2005,  "Teleostei",    93,
  2007,  "Teleostei",    21,
  2007,  "Tracheophyta", 1,
  2009,  "Arthropoda",   5,
  2009,  "Others",       2,
  2009,  "Others",       1,
  2009,  "Nematoda",     73,
  2009,  "Teleostei",    54,
  2010,  "Aves",         109,
  2010,  "Others",       1,
  2010,  "Teleostei",    115,
  2010,  "Tracheophyta", 5,
  2013,  "Teleostei",    80,
  2014,  "Teleostei",    26,
  2017,  "Others",       27,
  2017,  "Teleostei",    1,
  2021,  "Others",       3,
  2021,  "Arthropoda",   2,
  2021,  "Aves",         5,
  2021,  "Others",       13,
  2021,  "Others",       1,
  2021,  "Teleostei",    37,
  2021,  "Others",       2,
  2021,  "Tracheophyta", 11,
  2022,  "Arthropoda",   11,
  2022,  "Nematoda",     29,
  2022,  "Teleostei",    59,
  2023,  "Others",       1,
  2024,  "Others",       27,
  2024,  "Teleostei",    1
)
custom_colors <- c(
  "Arthropoda"   = "#E69F00",  # Vivid Orange
  "Aves"         = "#56B4E9",  # Sky Blue
  "Nematoda"     = "#009E73",  # Bluish Green
  "Others"       = "#999999",  # Neutral Grey (perfect for an "Others" category)
  "Teleostei"    = "#0072B2",  # Dark Blue
  "Tracheophyta" = "#D55E00"   # Vermilion/Red-Orange
)
# Collapse duplicate year-phylum rows, then complete year x phylum grid
# (missing combos = 0 records that year).
df_stream <- df_raw |>
  group_by(year, phylum) |>
  summarise(count = sum(count), .groups = "drop") |>
  complete(year = full_seq(year, 1), phylum, fill = list(count = 0)) |>
  mutate(phylum = factor(phylum, levels = names(custom_colors))) |>
  arrange(year, phylum)

# "Silhouette" streamgraph: stack is centered around 0 each year
df_stream <- df_stream |>
  group_by(year) |>
  mutate(
    ymax = cumsum(count) - sum(count) / 2,
    ymin = ymax - count
  ) |>
  ungroup()

# Smooth each phylum's ribbon onto a finer year grid so it flows rather
# than looking jagged between sparse data years.
df_smooth <- df_stream |>
  group_by(phylum) |>
  group_modify(~ {
    yrs <- seq(min(.x$year), max(.x$year), length.out = 200)
    tibble(
      year = yrs,
      ymin = spline(.x$year, .x$ymin, xout = yrs, method = "natural")$y,
      ymax = spline(.x$year, .x$ymax, xout = yrs, method = "natural")$y
    )
  }) |>
  ungroup()

stream_plot <- ggplot(
  df_smooth,
  aes(x = year, ymin = ymin, ymax = ymax, fill = phylum, group = phylum)
) +
  geom_ribbon(colour = NA, alpha = 0.95) +
  labs(x = "Year", y = "Records per year", fill = "") +
  ggtitle(expression(bold("(c)"))) +
  scale_x_continuous(limits = c(1990, 2026), expand = c(0, 0)) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  ) +
  scale_fill_manual(values = custom_colors)

stream_plot

# ggsave("streamgraph.png", stream_plot, width = 8, height = 5, dpi = 300)