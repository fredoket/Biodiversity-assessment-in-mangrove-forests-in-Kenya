pacman::p_load(
  lubridate,  
  tidyverse,  
  phyloseq,
  microeco,
  file2meco,
  ggh4x
)


# Load the required data
df_phylo <- readRDS("data/df_phylo.rds")
df_micro <- readRDS("data/df_microeco.rds")

df_comp <- clone(df_micro)


df_comp$tidy_dataset()

df_map <- clone(df_comp)

df_map$filter_pollution(taxa = c("Bacteria"))
df_map %<>%
  tidy_taxonomy()

set.seed(123)
df_map$rarefy_samples(sample.size = 1500)

df_map$sample_table <- subset(df_map$sample_table, system == "Marine")

df_map$tidy_dataset()

t1 <- trans_abund$new(dataset = df_map, taxrank = "kingdom", ntaxa = 8)

t1$plot_bar(others_color = "grey70", facet = c("county", "abr"),
            xtext_keep = FALSE, legend_text_italic = FALSE,
            barwidth = 0.9)

t2 <- trans_abund$new(dataset = df_map, taxrank = "Occurance", ntaxa = 8)

t2$plot_bar(others_color = "grey70", facet = c("county", "abr"),
            xtext_keep = FALSE, legend_text_italic = FALSE)
