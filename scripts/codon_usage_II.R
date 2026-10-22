# --- Load libraries ---
library(ggplot2)
library(dplyr)

# --- Step 1: Create sample COA data (as if from CodonW) ---

# Codon coordinates (example: extracted from codon.coa)
codon_positions <- read.table("data/pca.txt", header = TRUE)
  
  

# Gene coordinates (example: extracted from genes.coa)
gene_positions <- read.table("data/pca_gene.txt",
                             header = TRUE)


# --- Step 2: Plot COA biplot ---
ggplot() +
  # Codons (red points)
  geom_point(data = codon_positions, aes(x = Axis1, y = Axis2), 
             color = "#E64B35", size = 3, alpha = 0.8) +
  geom_text(data = codon_positions, aes(x = Axis1, y = Axis2, label = Codon),
            vjust = -1, color = "#E64B35", size = 3.5) +
  
  # Genes (blue points)
  geom_point(data = gene_positions, aes(x = Axis1, y = Axis2),
             color = "#4DBBD5", size = 3, alpha = 0.8) +
  geom_text(data = gene_positions, aes(x = Axis1, y = Axis2, label = Gene),
            vjust = -1, color = "#4DBBD5", size = 3.5) +
  
  # Axes and style
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dashed") +
  labs(
    x = "Axis 1 (major codon usage trend)",
    y = "Axis 2 (secondary codon usage trend)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )
