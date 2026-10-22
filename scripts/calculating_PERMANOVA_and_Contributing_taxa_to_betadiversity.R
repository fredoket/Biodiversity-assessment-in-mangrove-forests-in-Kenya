#Load packages
pacman::p_load(
 dplyr, tidyr, vegan, ggplot2,
 viridis, microeco, phyloseq,
 gt, dplyr, ggplot2
)

# load the required data( in S4 or R6 format)-----------------------------------
df_microeco <- readRDS("data/df_microeco_2.rds")

# Remove OTUs with fewer than 20 contigs
df_divstat <- clone(df_microeco)

df_divstat$sample_table <- subset(
  df_divstat$sample_table,
  system == "Mangrove"
)

df_divstat$filter_taxa(
  rel_abund = 0.00000102,
  freq = 1
)
df_divstat$tidy_dataset()

# Remove "Unexpected" and none marine taxa--------------------------------------
df_marine <- clone(df_divstat)

df_marine$tax_table <- subset(
  df_marine$tax_table,
  Habitat != "Terrestrial" &
    Habitat != "Freshwater" &
    status == "Expected"
)

df_marine$tidy_dataset()


# Process data------------------------------------------------------------------
otu_df <- df_marine$otu_table
tax_df <- df_marine$tax_table
meta_df <- df_marine$sample_table
tax_df <- tax_df |>
  tibble::rownames_to_column("OTU")



otu_long <- otu_df |>
  tibble::rownames_to_column("OTU") |>
  pivot_longer(
    cols = -OTU,
    names_to = "sample",
    values_to = "abundance"
  )

comm <- t(otu_df)

# Calculate PERMANOVA 

dist_bc <- vegdist(comm,
                   method = "jaccard")  # or "jaccard"


# Run PERMANOVA
# PERMANOVA with two factors (site and sample type)
permanova_res <- adonis2(dist_bc ~ site + sample_type,
                         data = meta_df,
                         permutations = 999)

# View results
permanova_res

# Run NMDS plot

# NMDS Ordination
nmds <- metaMDS(comm,
                distance = "jaccard",
                k = 2, trymax = 100)

# Check stress (should be < 0.2 ideally)
nmds$stress


# Prepare NMDS coordinates for plotting

# Extract NMDS site scores
nmds_scores <- as.data.frame(scores(nmds,
                                    display = "sites"))

# Add metadata for plotting
nmds_scores <- cbind(nmds_scores,
                     meta_df)

# Draw NMDS plot
p3 <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = site,
                        shape = sample_type)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "black", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dotted",
             color = "black", linewidth = 0.6) +
  #scale_x_continuous(expand = c(0,0), limits = c(0, 30)) +  
  scale_color_viridis_d(option = "magma") +
  theme_bw()+
  labs(
    x = "NMDS1",
    y = "NMDS2",
    color = "Site"
  ) +
  theme(
    axis.text.x = element_text(size = 12, angle = 0, hjust = 1),
    axis.text.y = element_text(size = 12),
    strip.text.y = element_text(size = 12, face = "bold"),
    axis.title  = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  )


# Aggregate at a higher taxonomic level
sim <- simper(comm, group = meta_df$site, permutations = 999)
sim_df <- summary(sim)$Kilifi_Kililana  # change contrast as needed
simper_sig <- sim_df |>
  as.data.frame() |>
  tibble::rownames_to_column("OTU") |>
  left_join(tax_df, by = "OTU") |>
  arrange(desc(average))


# Calculate simper

simper_plot_df <- simper_sig |>
  group_by(family) |>
  summarise(average = sum(average)) |>
  arrange(desc(average)) |>
  slice_head(n = 33) |> 
  ungroup()


# Convert taxon to a factor to control order
simper_plot_df <- simper_plot_df |>
  mutate(family = factor(family, levels = family))


# Draw the final graph
sig <- ggplot(simper_plot_df, aes(
  x = family,  
  y = average,
  fill = family  
)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = c(0, 0)) +  # bars start at 0
  scale_fill_viridis_d(option = "magma") +
  ggpubr::theme_classic2()+
  labs(
    x = "Family (OTU)",
    y = "Average contribution"
  ) +
  theme(
    axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 12, face = "italic", colour = "black"),
    axis.title.x = element_text(size = 12, face = "bold", colour = "black"),
    axis.title.y = element_text(size = 12, face = "bold", colour = "black"),
    panel.grid = element_blank()
  )

