# Load the required package
pacman::p_load(
  microeco,
  phyloseq,
  gt, dplyr,
  ggplot2,
  file2meco
)

# Load marine data
df_marine <- df_marine

## Obtain and transform the data to correct format
cat <- df_marine[["sample_table"]]

cat <- cat |>
  select(sample_type, site)

cat$sample_id <- rownames(cat)
rownames(cat) <- NULL

cat <- cat |>
  dplyr::select(sample_id, sample_type)

sample_data(physeq) <- sample_data(cat)

sample_data(physeq)
dds <- phyloseq_to_deseq2(physeq, ~ sample_type)
sample_data(physeq)$sample_type <- factor(sample_data(physeq)$sample_type)
dds <- DESeq2::DESeq(dds, sfType = "poscounts")
res <- DESeq2::results(dds, contrast = c("sample_type", "Water", "Sediment"))
res <- res[order(res$padj), ]
head(res)

log2FC_cutoff <- 1
padj_cutoff <- 0.05

res_df <- as.data.frame(res)
res_df$taxa <- rownames(res_df)
res_df <- res_df %>%
  filter(!is.na(padj))



padj_cutoff <- 0.05
log2FC_cutoff <- 1

res_df <- res_df %>%
  mutate(
    Significant = case_when(
      padj < padj_cutoff & log2FoldChange >  log2FC_cutoff  ~ "Enriched in Water",
      padj < padj_cutoff & log2FoldChange < -log2FC_cutoff ~ "Enriched in Sediment",
      TRUE ~ "Not significant"
    )
  )

res_family <- df_marine[["tax_table"]]
res_family$taxa <- rownames(res_family)

res_family <- res_family |>
  dplyr::select(taxa, class, family)

red_df <- merge(res_family, res_df, by = "taxa")


volcano_plot <- ggplot(red_df, aes(x = log2FoldChange, y = -log10(padj))) +
  
  geom_point(aes(color = Significant), alpha = 0.8, size = 2) +
  
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "grey40") +
  
  geom_text_repel(
    data = subset(red_df, Significant != "Not significant"),
    aes(label = family),
    size = 3,
    max.overlaps = 20,
    box.padding = 0.4,
    point.padding = 0.3
  ) +
  
  scale_color_manual(
    values = c(
      "Enriched in Water" = "#1f78b4",
      "Enriched in Sediment" = "#e31a1c",
      "Not significant" = "grey70"
    )
  ) +
  
  theme_classic() +
  
  labs(
    x = "Log2 fold change (Water vs Sediment)",
    y = "-log10 adjusted p-value",
    color = "Differential abundance"
  )


volcano_plot
