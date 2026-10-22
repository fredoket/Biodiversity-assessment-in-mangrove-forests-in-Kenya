
chao <- alpha_table |>
  select(Measure, sample_type, Value) |>
  filter(Measure == "Chao1") |>
  drop_na()

kruskal.test(Value ~ sample_type, data = chao)

# Or pairwise Wilcoxon
pairwise.wilcox.test(chao$Value, chao$Value, p.adjust.method = "BH")

# Dunn’s test
library(FSA)
dunnTest(Value ~ sample_type, data = chao, method = "bonferroni")


shanon <- alpha_table |>
  select(Measure, sample_type, Value) |>
  filter(Measure == "Shannon") |>
  drop_na()

kruskal.test(Value ~ sample_type, data = shanon)


# Dunn’s test
library(FSA)
dunnTest(Value ~ sample_type, data = shanon, method = "bonferroni")


simpson <- alpha_table |>
  select(Measure, sample_type, Value) |>
  filter(Measure == "Simpson") |>
  drop_na()

kruskal.test(Value ~ sample_type, data = simpson)


# Dunn’s test
library(FSA)
dunnTest(Value ~ sample_type, data = simpson, method = "bonferroni")
