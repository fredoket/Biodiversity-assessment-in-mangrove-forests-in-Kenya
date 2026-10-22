pacman::p_load(
  microeco,
  phyloseq
)

df_divstat <- clone(df_microeco)

df_divstat$sample_table <- subset(
  df_divstat$sample_table,
  manuscript == "Yes"
)

df_divstat$tidy_dataset()

df_divstat$filter_taxa(
  rel_abund = 0.00001, freq = 0.001)


# subset bacteria

bac <- clone(df_divstat)
  
bac$tax_table <- base::subset(bac$tax_table,
                              kingdom == "Bacteria")

bac$tidy_dataset()


# Subset Eukaryotes 
euk <- clone(df_divstat)

euk$tax_table <- base::subset(euk$tax_table,
                              kingdom != "Bacteria")


p1 <- trans_abund$new(dataset = bac,
                      taxrank = "family")


# Relative abundance by at phylum level
p1$plot_bar(others_color = "grey70",
            facet = "site",
            xtext_keep = FALSE,
            legend_text_italic = FALSE)

# show 40 taxa at Genus level
p1 <- trans_abund$new(dataset = bac,
                      taxrank = "family",
                      ntaxa = 100)

g1 <- p1$plot_heatmap(facet = "site",
                      xtext_keep = FALSE,
                      withmargin = FALSE,
                      plot_breaks = c(0.01, 0.1, 1, 10))
g1
g1 + theme(axis.text.y = element_text(face = 'italic'))
