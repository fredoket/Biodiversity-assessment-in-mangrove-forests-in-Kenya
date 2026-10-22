pacman::p_load(
  tidyverse,
  viridis,
  reshape2,
  ggpubr
)

# Gene locus identification and characterization--------------------------------

gene <- read_csv("data/gene_coordinates_full.csv") |>
  janitor::clean_names()

sgenes <- gene |>
  filter(copy_status == "Single")

dgenes <- gene |>
  filter(copy_status == "Duplicate") |>
  select(gene) |>
  unique()

lsc <- gene |>
  filter(region == "LSC") |>
  select(gene) |> 
  unique()

irb <- gene |>
  filter(region == "IRb") |>
  select(gene) |>
  unique()

ira <- gene |>
  filter(region == "IRa") |>
  select(gene) |>
  unique()

ssc <- gene |>
  filter(region == "SSC") |>
  select(gene) |>
  unique()


uirs <- bind_rows(ira, irb) %>% 
  distinct(gene, .keep_all = TRUE)

# Load data---------------------------------------------------------------------
codon <- read_csv("data/codon_df.csv")

codon$GC12 <- (3 * codon$GC - codon$GC3s) / 2
# Compute PR2-bias indices
codon$AT_bias <- codon$A3s / (codon$A3s + codon$T3s)
codon$GC_bias <- codon$G3s / (codon$G3s + codon$C3s)
codon$eNC <- 2 + codon$GC3s + 29 /
  ((codon$GC3s^2) + ((1 - codon$GC3s)^2))
codon$ENC_diff_ratio <- (codon$eNC - codon$Nc)/
  codon$eNC



# Compute density-based smooth CDFs --------------------------------------------
dens_obs <- density(codon$Nc, na.rm = TRUE)
dens_exp <- density(codon$eNC, na.rm = TRUE)

cdf_obs <- cbind(dens_obs$x, cumsum(dens_obs$y) / sum(dens_obs$y))
cdf_exp <- cbind(dens_exp$x, cumsum(dens_exp$y) / sum(dens_exp$y))

# Get starting and ending points
x_min <- min(c(codon$Nc, codon$eNC), na.rm = TRUE)
x_max <- max(c(codon$Nc, codon$eNC), na.rm = TRUE)

# Extend observed curve to start 5 units before lowest ENC
x_start_obs <- x_min - 0
cdf_obs <- rbind(c(x_start_obs, 0), cdf_obs)

# Extend expected curve to start from 0
cdf_exp <- rbind(c(0, 0), cdf_exp)

# Combine into one data frame for ggplot
df_cdf <- data.frame(
  x = c(cdf_obs[,1], cdf_exp[,1]),
  y = c(cdf_obs[,2], cdf_exp[,2]),
  Type = rep(c("Observed ENC", "Expected eNC"),
             each = nrow(cdf_obs))
)

# Perform Kolmogorov–Smirnov test ---
ks_result <- ks.test(codon$Nc, codon$eNC)
p_value <- formatC(ks_result$p.value, format = "e", digits = 2)
D_value <- round(ks_result$statistic, 3)

# Plot with extended lines and classic theme ---
p1 <- ggplot(df_cdf, aes(x = x, y = y, color = Type)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("blue", "red")) +
  theme_classic(base_size = 14) +
  labs(
    x = "Value",
    y = "Cumulative Distribution Function",
    subtitle = paste0("K–S Test: D = ", D_value, ",  P = ", p_value)
  ) +
  coord_cartesian(xlim = c(x_start_obs, x_max), ylim = c(0, 1)) +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.subtitle = element_text(size = 12, face = "italic")
  )

# NC ratio differenrence--------------------------------------------------------
# Define bin width
bin_width <- 0.08

# Create histogram (without plotting)
hist_data <- hist(
  codon$ENC_diff_ratio,
  breaks = seq(
    floor(min(codon$ENC_diff_ratio, na.rm = TRUE)),
    ceiling(max(codon$ENC_diff_ratio, na.rm = TRUE)),
    by = bin_width
  ),
  plot = FALSE
)

# Convert to data frame
bin_df <- data.frame(
  Bin_Start = head(hist_data$breaks, -1),
  Bin_End = tail(hist_data$breaks, -1),
  Frequency = hist_data$counts
)

# Calculate percentage
bin_df$Percentage <- (bin_df$Frequency / sum(bin_df$Frequency)) * 100

# Remove bins with zero frequency
bin_df <- subset(bin_df, Frequency > 0)

# Add midpoint for plotting
bin_df$Midpoint <- (bin_df$Bin_Start + bin_df$Bin_End) / 2

# Show cleaned table
bin_df

# Plot the frequency distribution histogram
p2 <- ggplot(bin_df, aes(x = Midpoint, y = Percentage)) +
  geom_bar(stat = "identity", fill = "grey50", color = "black",
           width = bin_width * 0.95) +
  geom_text(aes(label = sprintf("%.1f", Percentage)),
            vjust = -0.3, size = 3.5) +
  theme_classic(base_size = 14) +
  labs(
    x = expression((eNC - NC)/eNC),
    y = "Frequency"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black")
#-------------------------------------------------------------------------------
library(dplyr)

library(ggrepel)
library(ggplot2)

# Select 5 genes with strongest positive codon bias and 5 with weakest
label_genes <- codon %>%
  arrange(desc((eNC-Nc)/eNC)) %>% 
  slice_head(n = 10) %>%
  bind_rows(
    codon %>%
      arrange((eNC-Nc)/eNC) %>%
      slice_head(n = 6)
  )

# Create theoretical curve
expected_curve <- data.frame(
  GC3s = seq(0, 1, length.out = 200)
)

expected_curve$Nc <- 2 + expected_curve$GC3s +
  29 / ((expected_curve$GC3s^2) + ((1 - expected_curve$GC3s)^2))

# Plot
p3 <- ggplot() +
  geom_line(data = expected_curve,
            aes(x = GC3s, y = Nc),
            color = "red", size = 1, linetype = "dashed") +
  
  geom_point(data = codon,
             aes(x = GC3s, y = Nc),
             color = "black", size = 1.5, alpha = 0.7) +
  
  geom_text_repel(data = label_genes,
                  aes(x = GC3s, y = Nc, label = Genes),
                  size = 3) +
  
  theme_classic(base_size = 14) +
  labs(x = expression(GC[3]),
       y = "NC")

#Neutrality plot with regression equation--------------------------------------
p4 <- ggplot(codon, aes(x = GC3s, y = GC12)) +
  geom_point(size = 1.5, color = "black", alpha = 0.4) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  
# Regression line equation (from ggpubr)
  stat_regline_equation(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    label.x = 0.2, label.y = 0.6,
    size = 4, parse = TRUE
  ) +
  
# Correlation coefficient and p-value
  stat_cor(
    method = "pearson",
    label.x = 0.2, label.y = 0.55,
    size = 4
  ) +
  
  theme(
    axis.text.x = element_text(size = 12, angle = 0, hjust = 1),
    axis.text.y = element_text(size = 12),
    strip.text.y = element_text(size = 12, face = "bold"),
    axis.title  = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  ) +
  labs(
    x = "GC3s",
    y = "GC12"
  )


# PR2 plot ---------------------------------------------------------------------
p5 <- ggplot(codon, aes(x = GC_bias, y = AT_bias)) +
  geom_point(size = 2, alpha = 0.6, color = "darkblue") +
  geom_density_2d(color = "gray40") +
  geom_vline(xintercept = 0.5,
             linetype = "dotted", color = "black", linewidth = 0.6) +
  geom_hline(yintercept = 0.5,
             linetype = "dotted", color = "black", linewidth = 0.6) +
  coord_fixed() +
  theme(
    axis.text.x = element_text(size = 12, angle = 0, hjust = 1),
    axis.text.y = element_text(size = 12),
    strip.text.y = element_text(size = 12, face = "bold"),
    axis.title  = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  )+
  labs(x = "G3 / (G3 + C3)",
    y = "A3 / (A3 + T3)"
  )+
  guides(fill = "none") 

#RSCU---------------------------------------------------------------------------

rscu <- read.table("data/rscu.txt",
                   header = TRUE)


# Create composite axis label: "Codon (AA)"
rscu <- rscu %>%
  mutate(Codon_AA = paste0(Codon, " (", AminoAcid, ")")) %>%
  arrange(AminoAcid, Codon) %>%
  mutate(Codon_AA = factor(Codon_AA, levels = Codon_AA))

# Identify the preferred codon per amino acid (highest RSCU)
top_rscu <- rscu %>%
  group_by(AminoAcid) %>%
  slice_max(RSCU, n = 1) %>%
  ungroup() %>%
  mutate(Label = paste0(AminoAcid, " (", round(RSCU, 2), ")"))

# Plot — uniform bar color, no legend, label only top codons
p6 <- ggplot(rscu, aes(x = Codon_AA, y = RSCU)) +
  geom_bar(stat = "identity", fill = "grey40", color = "white") +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.3) +
  
  geom_text(
    data = top_rscu,
    aes(label = Label),
    vjust = -0.2,
    fontface = "bold",
    size = 3.8
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) +
  labs(
    x = "Codon (Amino Acid)",
    y = "RSCU"
  )
#-----------------COA_ANALYSIS--------------------------------------------------
library(ade4)
library(ggplot2)
library(ggrepel)
library(ggbreak)

coa1 <- read.table("data/coa.txt",
                   sep = "\t",          
                   stringsAsFactors = FALSE,
                   header = TRUE)

codons <- read.table("data/pca.txt",
                     header = TRUE,
                     sep = "",            # auto-detect whitespace
                     stringsAsFactors = FALSE,
                     fill = TRUE)


scale_factor <- 8   # adjust visually if needed
codons$Axis1 <- codons$Axis1 * scale_factor
codons$Axis2 <- codons$Axis2 * scale_factor


p7 <- ggplot(coa1, aes(x = Axis1, y = Axis2, color = Role)) +  # map color to Role
  geom_point(size = 2, alpha = 0.7) +
  geom_text_repel(aes(label = Gene), size = 3, max.overlaps = 15) +
  
  # Add x and y axis lines at zero
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.6) +
  
  theme(
    axis.text.x = element_text(size = 12, angle = 0, hjust = 1),
    axis.text.y = element_text(size = 12),
    strip.text.y = element_text(size = 12, face = "bold"),
    axis.title  = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  ) +
  labs(
    x = "Axis 1 (17.48%)",
    y = "Axis 2 (10.51%)",
    color = "Gene Function"  # legend title
  ) +
  theme(
    plot.title = element_text(face = "bold")
  )
# Calculate A3(A3+T3) and G3(G3+C3) ratio---------------------------------------
aa_ratio <- codon
aa_ratio$A3s <- aa_ratio$A3s/(aa_ratio$A3s+aa_ratio$T3s)
mean_A3_ratio <- mean(aa_ratio$A3s, na.rm = TRUE)
sd_A3_ratio   <- sd(aa_ratio$A3s, na.rm = TRUE)

cat("Mean A3/(A3+T3):", round(mean_A3_ratio, 3), "\n")
cat("Standard Deviation:", round(sd_A3_ratio, 3), "\n")


gg_ratio <- codon
gg_ratio$G3s <- gg_ratio$G3s/(gg_ratio$G3s + gg_ratio$C3s)
mean_G3_ratio <- mean(gg_ratio$G3s, na.rm = TRUE)
sd_G3_ratio   <- sd(gg_ratio$G3s, na.rm = TRUE)

cat("Mean G3/(G3+C3):", round(mean_G3_ratio, 3), "\n")
cat("Standard Deviation:", round(sd_G3_ratio, 3), "\n")

# Combined graphs---------------------------------------------------------------

library(patchwork)
library(ggbreak)


combined <- (p2 | p3) / p6+
  plot_annotation(tag_levels = 'a')

combined

combined1 <- (p4 | p1) / p5+
  plot_annotation(tag_levels = 'a')
combined1

combined2 <- (p5 | p7) +
  plot_annotation(tag_levels = 'a')
combined2

# Top row: p5 and p7
top_row <- p5 | p7

# Combine top row with p3 below
combined2 <- top_row /
  p3 +
  plot_layout(heights = c(1, 1.2)) +  
  plot_annotation(tag_levels = "a")
combined2

# Nucleotide diversity ---------------------------------------------------------

nu_gene <- read.table("data/gene_sort_as_cp_order.txt",
                     header = TRUE,
                     sep = "",            
                     stringsAsFactors = FALSE,
                     fill = TRUE)

nu_gene$pi <- as.numeric(nu_gene$Pi)

loc <- read.table("data/gene_location.txt",
                      header = TRUE,
                      sep = "",            
                      stringsAsFactors = FALSE,
                      fill = TRUE)


nu_gene <- merge(nu_gene,
                 loc[, c("Gene", "Location")],
                 by = "Gene", all.x = TRUE)

# Plot
p19 <- ggplot(nu_gene, aes(x = Gene, y = Pi, group = 1)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(size = 1.2) +
  geom_text_repel(
    data = subset(nu_gene, Pi > 0.003),  
    aes(label = Gene),
    size = 3
  ) +
  
  geom_hline(yintercept = 0.0025, 
             linetype = "dashed", 
             linewidth = 0.5, 
             color = "red") +
  
  theme_classic() +
  labs(x = "Protein and tRNA coding genes", y = expression(pi)) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

data1 <- nu_gene |>
  rename("Locus" = "Location") |> 
  mutate(role = "Protein-coding genes") |>
  unique()

data2 <- nu_igs |>
  rename("Gene" = "IGS") |> 
  mutate(role = "Intron and Intergenic region") |>
  unique()

box2 <- bind_rows(data1, data2)


# Boxplot
p20 <- ggplot(box2, aes(x = role, y = pi)) +
  geom_boxplot(fill = "grey30", color = "black", outlier.color = "red") +
  theme_classic() +
  labs(x = "", y = "Nucleotide diversity") +
  theme(axis.text.x = element_text(hjust = 1))

# intergenic region ------------------------------------------------------------

nu_igs <- read.table("data/IGS_sort_as_cp_order.txt",
                      header = TRUE,
                      sep = "",            
                      stringsAsFactors = FALSE,
                      fill = TRUE)

nu_igs$pi <- as.numeric(nu_igs$Pi)


library(ggbreak)

# Plot
p21 <- ggplot(nu_igs, aes(x = IGS, y = pi, group = 1)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(size = 1.2) +
  geom_text_repel(
    data = subset(nu_igs, pi > 0.008),
    aes(label = IGS),
    size = 3
  ) +
  geom_hline(yintercept = 0.008, 
             linetype = "dashed", 
             linewidth = 0.5, 
             color = "red") +
  theme_classic() +
  labs(x = "Intergenic and intron containing regions", y = expression(pi)) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.2, size = 7)
  ) +
  scale_y_break(c(0.027, 0.38))   # <-- Break the y-axis here

# make data
df1 <- nu_gene |>
  rename("Locus" = "Location")

df3 <- nu_igs |>
  rename("Gene" = "IGS")

box1 <- bind_rows(df1, df3) |>
  unique()

# Box plot
p22 <- ggplot(box1, aes(x = Locus, y = pi)) +
  geom_boxplot(fill = "grey30", color = "black", outlier.color = "red") +
  theme_classic() +
  labs(x = "", y = "Nucleotide diversity") +
  theme(axis.text.x = element_text(hjust = 1))

library(patchwork)

panel <- p19 + p21 + p22 + p20 +
  plot_layout(design = c(
    area(t = 1, l = 1, b = 2, r = 9),   
    area(t = 3, l = 1, b = 4, r = 9),   
    area(t = 1, l = 10, b = 2, r = 10), 
    area(t = 3, l = 10, b = 4, r = 10)  
  ))

panel

# Evolution and nucleotide diversity--------------------------------------------
ds_dn <- read_csv("data/codeml_summary.csv") |>
  select(-dS, -dN)

ds1 <- ggplot(ds_dn, aes(x = Gene, y = kappa, group = 1)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(size = 1.2) +
  geom_text_repel(
    data = subset(ds_dn, kappa > 3),  
    aes(label = Gene),
    size = 3
  ) +
  
  geom_hline(yintercept = 3, 
             linetype = "dashed", 
             linewidth = 0.5, 
             color = "red") +
  
  theme_classic() +
  labs(x = "Protein coding genes", y = "substitution rate ratios (kappa)") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ds2 <- ggplot(ds_dn, aes(x = Gene, y = omega, group = 1)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(size = 1.2) +
  geom_text_repel(
    data = subset(ds_dn, omega > 1),  
    aes(label = Gene),
    size = 3
  ) +
  
  geom_hline(yintercept = 1, 
             linetype = "dashed", 
             linewidth = 0.5, 
             color = "red") +
  
  theme_classic() +
  labs(x = "Protein coding genes", y = "Nonsynonymous to synonymous ratios (ω)") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
combinedx <- (ds2 / ds1)+
  plot_annotation(tag_levels = 'a')

#-----------------Phylogenetic tree---------------------------------------------
library(ape)
library(ggplot2)
library(treeio)
library(ggtree)   # load last


tre <- read.tree("data/newick.txt")

p <- ggtree(tre)

scaleClade(p, 23, .2) %>% collapse(23, 'min', fill="darkgreen")  
ggplot(tre, aes(x, y))+
  geom_tree()+
  theme_tree()

library(tidyverse)

mal <- read_csv("data/malvacea.csv") 

library(dplyr)

df_clean <- mal %>%
  mutate(
    # Extract the numeric part of the Accession string
    acc_num = as.numeric(gsub("\\D", "", Accession))
  ) %>%
  # Group by species
  group_by(species) %>%
  # Keep the row with the highest accession number
  slice_max(acc_num, n = 1) %>%
  ungroup() %>%
  # Remove the helper column
  select(-acc_num) |>
  select(Accession)

