# Count unique species per class and compute percent
pacman::p_load(
  dplyr,
  stringr
)


# Step 1: Count unique species per class and compute percent
class_counts <- df_sankey |>
  select(
    class,
    species
  ) |>
  distinct() |>
  group_by(class) |>
  summarise(
    n = n_distinct(species),
    .groups = "drop"
  ) |>
  mutate(
    percent = round(
      (n / sum(n)) * 100,
      1
    )
  )


# Step 2: Store total for low-count classes (< 20)
total_n <- class_counts |>
  filter(
    n < 1
  ) |>
  summarise(
    total_n = sum(n)
  ) |>
  pull(total_n)

total_percent <- class_counts |>
  filter(
    n < 1
  ) |>
  summarise(
    percent = round(
      sum(n) / sum(class_counts$n) * 100,
      1
    )
  ) |>
  pull(percent)

others_label <- paste0(
  "Others ",
  total_n,
  " (",
  total_percent,
  "%)"
)


# Step 3: Apply label conditionally
class_counts <- class_counts |>
  mutate(
    class_c = if_else(
      n < 1,
      others_label,
      paste0(
        class,
        " ",
        n,
        " (",
        percent,
        "%)"
      )
    )
  )


# Step 4: Join with original data
df_sank <- df_sank |>
  left_join(
    class_counts |>
      select(
        class,
        class_c
      ),
    by = "class"
  ) |>
  select(-class)