pacman::p_load(
  tidyverse, janitor, ggsankeyfier,
  ggpubr, ggpmisc, ggrepel,  broom,
  patchwork, ggsankey, viridis,
  stringr, dplyr, gtsummary, vcd,
  FactoMineR, factoextra, ggExtra,
  patchwork
)

# Data cleaning/transformation and verifying in GBIF and WORMS------------------ 
#source("scripts/match_data_in_WORMS_GBIF.R")

df_NA <- read_csv("data/species.csv") |>
  janitor::clean_names()

df_clean <- read_csv("data/df_shaq.csv")

df_shaq <- df_clean 

#----------------------------------------
pubtrend <- df_clean |>
  select(year_of_publication, document_title) |>
  unique() |>
  drop_na() |>
  filter(!grepl("^Integration", document_title,
                ignore.case = TRUE)) |>
  rename("year" = "year_of_publication") |>
  group_by(year) |>
  summarise(
    count = n_distinct(document_title),
    .groups = "drop"
  )

# Fit the model and extract statistics
model <- lm(count ~ year, data = pubtrend)

p_val <- summary(model)$coefficients["year", "Pr(>|t|)"]
r2_val <- summary(model)$r.squared

# Formatting 
label_text <- paste0(
  "P = ", signif(p_val, 3),
  ", R² = ", round(r2_val, 2)
)

# Get R squre when appropriate
r2_label <- ifelse(r2_val < 0.01, "< 0.01", round(r2_val, 2))

label_text <- paste0(
  "P = ", signif(p_val, 3),
  ", R² ", ifelse(r2_val < 0.01, "< ", "= "), r2_label
)


t1 <- ggplot(pubtrend, aes(x = year, y = count)) +
  geom_point(colour = "black", size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "black", linetype = "dashed") +
  annotate(
    "text",
    x = min(pubtrend$year) + 5,
    y = max(pubtrend$count),
    label = label_text,
    hjust = 0,
    size = 3.5
  ) +
  labs(
    x = "Year",
    y = "Number of studies"
  ) +
  theme_classic()


p_bar <- ggplot(pubtrend, aes(x = year, y = count)) +
  geom_col(fill = "grey20", colour = "grey70", width = 1) +
  labs(
    x = "Year",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 9),
    axis.text.y  = element_text(size = 8),
    plot.margin  = margin(t = 0)
  )

p_line <- ggplot(pubtrend, aes(x = year, y = count, group = 1)) +
  geom_line(colour = "grey20", linewidth = 0.8) +
  geom_point(colour = "grey20", size = 2) +
  labs(
    x = "Year",
    y = "Count"
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 9),
    axis.text.y  = element_text(size = 8),
    plot.margin  = margin(t = 0)
  )

# Number of studies by location
stuloc <- df_clean |>
  select(site, document_title, document_class) |>
  group_by(site) |>
  summarise(
    count = n_distinct(document_title)
  ) |>
  mutate(site = ifelse(count < 2, "Others", site)) |>
  group_by(site) |>
  summarise(count = sum(count)) |> 
  ungroup()
  
s1 <-stuloc |>
  arrange(desc(count)) |> 
  ggplot(aes(x = reorder(site, -count), y =count))+
  geom_col(fill = "grey40", width = 0.7)+
  geom_text(aes(label = count), hjust = 0.5, site = 3)+
  labs(
    x = "Mangrove Forest",
    y = "Number of studies"
  )+
  theme_classic()


# Graph panels
source("scripts/lorenzo.R")

p_line + s1 + t1 + lor +
  plot_layout(
    design = "
    AB
    CD
    "
  )
#Extract the regression equation
coef(model)

intercept <- coef(model)[1]
slope <- coef(model)[2]

equation <- paste0(
  "y = ", round(intercept, 2),
  " + ", round(slope, 4), " × year"
)

equation


ggplot(pubtrend, aes(x = year, y = count)) +
  geom_point(colour = "black", size = 2) +
  geom_smooth(method = "lm", se = TRUE, colour = "black", linetype = "dashed") +
  annotate(
    "text",
    x = min(pubtrend$year) + 5,
    y = max(pubtrend$count),
    label = equation,
    hjust = 0,
    size = 3.5
  ) +
  labs(
    x = "Year",
    y = "Number of studies"
  ) +
  theme_classic()

#------------------------Study bias---------------------------------------------
bias <- df_clean |>
  select(site, species, class, document_title) |>
  unique() |>
  drop_na()

site_summary <- bias |>
  group_by(site) |>
  summarise(taxa_richness = n_distinct(species),
            surveys = n_distinct(document_title))

# Chisqure test on survey efforts---------------------------------------------
chis <- site_summary |>
  mutate(site = ifelse(surveys == 1, "Others", site)) #|> 
  group_by(site) |>
  summarise(site = sum(surveys), .groups = "drop") 

chisq.test(chis$surveys)

# Correlation between survey effort and taxa richness
cor.test(site_summary$surveys,
         site_summary$taxa_richness,
         method = "spearman")


taxa_table <- table(bias$site, bias$document_title)
chisq.test(taxa_table)

#------------------------Sankey-------------------------------------------------

# Count the number of taxa under each variable
source("scripts/taxa_count_manual.R")


# Construct sankey plot
df_sank_long <- df_sankf |>
  make_long(
    `Year of publication`, `year_c`, `Forest location`,
    `Survey technique`,
    `Documented taxa`) 


# Sankey plot
t1 <- ggplot(df_sank_long, aes(x = x,
                               next_x = next_x,
                               node = node,
                               next_node = next_node,
                               fill = factor(node))) +
  geom_sankey(flow.alpha = 0.4,
              show.legend = FALSE) +
  geom_sankey(
    flow.alpha = 0.3,   # reduce opacity of flows
    node.color = "grey90",  # optional: outline color for nodes
    node.alpha = 0.4,   # reduce opacity of nodes
    node.width = 0.001,  # reduce node thickness (default is ~0.3)
    show.legend = FALSE)+
  geom_sankey_label(
    aes(label = node,
        hjust = case_when(
          x == "Year of publication" ~ 1.5,
          x == "year_c" ~ 1.5,
          x == "Document type" ~ -0.1,
          x == "Forest location" ~ -0.1,
          x == "Survey technique" ~ -0.1,
          x == "Taxa number" ~ -0.1,
          x == "Documented taxa" ~ -0.15,
          TRUE ~ -0.2
        ),
        vjust = 0.7
    ),
    size = 4.2,
    fill = "white",
    color = "black"
  ) +
  theme_sankey(base_size = 18) +
  scale_fill_viridis_d(option = "cividis", begin = 0.1, end = 0.9) +
  theme(
    panel.spacing = unit(17, "lines"), 
    plot.margin = margin(0.5, 0.5, 2, 2)    
  ) +
  labs(x = NULL) 

t1
# ------------------------------------------------------------------------------














# The number of studies carried out in mangrove forests
df_pub <- df_clean |>
  select(document_title) |>
  unique()

# The number of studies by site
df_study <- df_clean |>
  select(site, document_title, document_class) |>
  distinct() |>
  count(site, name = "number_of_studies") |>
  mutate(
    site = ifelse(number_of_studies <= 1, "Others", site)
  ) |>
  group_by(site) |>
  summarise(
    number_of_studies = sum(number_of_studies),
    .groups = "drop"
  ) |>
  arrange(desc(number_of_studies))


# Bar graph
mean_studies <- mean(df_study$number_of_studies)

ggplot(df_study,
       aes(x = reorder(site, number_of_studies),
           y = number_of_studies)) +
  geom_col(fill = "grey50") +
  geom_hline(yintercept = mean_studies,
             linetype = "dashed", color = "red")+
  labs(
    x = "Mangrove Forest",
    y = "Number of studies"
  ) +
  theme_minimal()
#-------------------------------------------------------------------------------






df_site <- df_lite |> 
  filter(assessment_method != "eDNA") |>                            
  select(site, document_title) |> 
  distinct() |>                                                     
  group_by(site) |> 
  summarise(Publication = n_distinct(document_title), .groups = "drop") |> 
  mutate(site = ifelse(Publication == 1, "Others", site)) |> 
  group_by(site) |> 
  summarise(Publication = sum(Publication), .groups = "drop") |> 
  rename(Site = site) |> 
  mutate(
    Percentage = round((Publication / sum(Publication)) * 100, 1),
    Label = paste0(Publication)
  )

# Plot the bar graph
ggplot(df_site, aes(x = reorder(Site, -Publication), y = Publication)) +
  geom_bar(stat = "identity", fill = "grey40") +
  geom_text(aes(label = Label), vjust = -0.3, size = 4.5) +
  labs(x = "Site",
       y = "Number of Studies") +
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold", size = 16,
                                hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", size = 13)
  )
# ------------------------------------------------------------------------------  
df_techniq <- df_lite |>
  select(document_title, Method) |>
  distinct() |>
  group_by(Method) |> 
  summarise(Number = n_distinct(document_title), .groups = "drop") |>
  mutate(
    Percentage = round((Number / sum(Number)) * 100, 1),
    Label = paste0(Number)
  ) |>
  rename("Metabarcoding" == "eDNA")

# Plot the bar graph
ggplot(df_techniq, aes(x = reorder(Method, -Number), y = Number)) +
  geom_bar(stat = "identity", fill = "grey40") +
  geom_text(aes(label = Label), vjust = -0.3, size = 4.5) +
  labs(x = "Survey technique",
       y = "Number") +
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold", size = 16,
                                hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", size = 13)
  )

# Number of documents reviewed
df_studies <- df_lite |>
  select(year_of_publication, document_title) |>
  unique() |>
  group_by(year_of_publication) |> 
  summarise(count = n_distinct(document_title)) |>
  rename("Year" = "year_of_publication",
         "Publication" = "count")


plot_a <- ggplot(df_studies, aes(x = Year, y = Publication)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black",
              fill = "grey30", se = TRUE, size = 1.5) +  
  stat_poly_eq(
    aes(label = paste("bold(", after_stat(rr.label), ")~~~bold(",
                      after_stat(p.value.label), ")")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "middle",
    label.y = "middle",
    color = "black"
  ) +
  labs(x = "Year", y = "Number of Publications") +
  ggtitle(expression(bold("(a)")))+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold", size = 16,
                                hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", size = 13)
  )

## Trend in the number of taxa surveyed over the years

df_taxatrend <- df_lite |> 
  select(year_of_publication, species) |>
  unique() |>
  group_by(year_of_publication) |> 
  summarise(count = n_distinct(species))

plot_b <- ggplot(df_taxatrend, aes(x = year_of_publication, y = count)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", formula = y ~ x, color = "black",
              fill = "grey30", se = TRUE, size = 1.5) + 
  stat_poly_eq(
    aes(label = paste("bold(", after_stat(rr.label), ")~~~bold(",
                      after_stat(p.value.label), ")")),
    formula = y ~ x,
    parse = TRUE,
    label.x = "middle",
    label.y = "middle",
    color = "black"
  ) +
  labs(x = "Year", y = "Number of Taxa") +
  ggtitle(expression(bold("(b)")))+
  theme_classic()+
  theme(
    axis.title.x = element_text(face = "bold", size = 16,
                                hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold", size = 13)
  )

# Place the plots in a grid
plot_a + plot_b


# Count the number of taxa under each variable
source("scripts/taxa_count_manual.R")


# Construct sankey plot
df_sank_long <- df_sankf |>
  make_long(
    `Year of publication`, `Documet type`, `Forest location`,
    `Survey technique`,
    `Documented taxa`) 

# Sankey plot
ggplot(df_sank_long, aes(x = x,
                         next_x = next_x,
                         node = node,
                         next_node = next_node,
                         fill = factor(node))) +
  geom_sankey(flow.alpha = 0.9,
              show.legend = FALSE) +
  
  geom_sankey_label(
    aes(label = node,
        hjust = case_when(
          x == "Year of publication" ~ 1.5,
          x == "Document type" ~ -0.1,
          x == "Forest location" ~ -0.1,
          x == "Survey technique" ~ -0.1,
          x == "Taxa number" ~ -0.1,
          x == "Documented taxa" ~ -0.15,
          TRUE ~ -0.2
        ),
        vjust = 0.7
    ),
    size = 4.2,
    fill = "white",
    color = "black"
  ) +
  theme_sankey(base_size = 18) +
  scale_fill_viridis_d(option = "viridis") +
  theme(
    panel.spacing = unit(17, "lines"), 
    plot.margin = margin(0.5, 0.5, 2, 2)    
  ) +
  labs(x = NULL) 

trial <- df_sankey |>
  select(site, class, phylum) |>
  distinct() |> 
  tbl_summary(by= site, include = c(phylum)) |>
  add_overall()
# Techniques used in taxa survey------------------------------------------------
df_tools <- df_clean |>
  left_join(df_NA, by = "species", suffix = c("", ".ref")) |> 
  mutate(class = ifelse(is.na(class), class.ref, class)) |>
  mutate(class = replace_na(class, "Teleostei")) |>
  filter(class != "Unknown") |>
  filter(!grepl("^Integration", document_title,
                ignore.case = TRUE)) |>
  distinct() |>
  drop_na(class)|>
  mutate(phylum = case_when(
    phylum == "Chordata" & class == "Teleostei" ~ "Teleostei",
    phylum == "Chordata" & class == "Aves" ~ "Aves",
    phylum == "Chordata" & class == "Mammalia" ~ "Mammalia",
    TRUE ~ phylum
  )) |>
  mutate(phylum = case_when(
    kingdom == "Bacteria" ~ "Bacteria",
    kingdom == "Fungi" ~ "Fungi",
    TRUE ~ phylum
  )) |>
  mutate(tools = if_else(
    str_detect(tools, regex("net|nets|sein|seins|gillnet|gillnets",
                            ignore_case = TRUE)),
    "Use of net",
    tools
  )) |>
  mutate(tools = case_when(
    tools == "DNA based technique" ~ "eDNA",
    tools == "Direct observation" ~ "Observation",
    tools == "Review of existing Primary data" ~ "Review",
    TRUE ~ tools   
  )) |>
  unique()

df_tech <- df_tools |>
  select(-1, 2:12, class) |> 
  unique() |> 
  select(tools, class) |>
  drop_na() |>
  unique()


tab <- table(df_tech$tools,
             df_tech$class)
tab

chisq.test(tab)


assostats(tab)



ggplot(data, aes(x = class, fill = tools)) +
  geom_bar(position = "fill") +
  ylab("Proportion") +
  theme_classic()
library(FactoMineR)
library(factoextra)

ca <- CA(tab, graph = TRUE)
fviz_ca_biplot(ca, repel = TRUE)+
  theme_classic()

df_trend <- df_clean |>
  select(year_of_publication, species, site, document_title) |>
  unique() |>
  rename("year" = "year_of_publication") |> 
  mutate(
    year_of_publication = as.integer(year),
    Species = trimws(species),
    Site = trimws(site)
  )

records_year <- df_trend |>
  select(year, document_title) |>
  unique() |>
  group_by(year) |>
  summarise(
    Records = n_distinct(document_title)
  )

ggplot(records_year, aes(x = year, y = Records)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Number of records",
    title = "Survey effort over time"
  ) +
  theme_classic()

species_year <- df_trend %>%
  group_by(year) %>%
  summarise(
    Species_richness = n_distinct(species),
    .groups = "drop"
  )

ggplot(species_year, aes(x = year, y = Species_richness)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Number of species",
    title = "Species richness over time"
  ) +
  theme_classic()

efficiency <- records_year |> 
  left_join(species_year, by = "year") |> 
  mutate(Species_per_record = Species_richness / Records)

ggplot(efficiency, aes(x = year, y = Species_per_record)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Species per record",
    title = "Survey efficiency through time"
  ) +
  theme_classic()

cumulative_species <- df_trend %>%
  arrange(year) %>%
  mutate(
    Cum_species = sapply(seq_len(n()),
                         function(i) n_distinct(df_trend$species[1:i]))
  ) %>%
  distinct(year, Cum_species)

ggplot(cumulative_species, aes(x = year, y = Cum_species)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Year",
    y = "Cumulative species",
    title = "Cumulative species accumulation curve"
  ) +
  theme_classic()

first_records <- df_trend |> 
  group_by(species) |> 
  summarise(First_year = min(year), .groups = "drop")

new_species_year <- first_records |> 
  count(First_year, name = "New_species") |> 
  rename(year = First_year)

ggplot(new_species_year, aes(x = year, y = New_species)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "New species recorded",
    title = "Rate of new species detection"
  ) +
  theme_classic()

library(Kendall)

MannKendall(records_year$Records)
MannKendall(species_year$Species_richness)
MannKendall(efficiency$Species_per_record)
MannKendall(new_species_per_year$new_species)


taxa_location <- table(df_trend$species, df_trend$site)
chisq_test <- chisq.test(taxa_location)
chisq_test

chisq_test <- chisq.test(taxa_location, simulate.p.value = TRUE)
residuals <- chisq_test$stdres

library(pheatmap)

pheatmap(residuals,
         color = colorRampPalette(c("blue", "white", "red"))(100),
         main = "Taxonomic over- and under-representation by location")

prop_table <- prop.table(taxa_location, margin = 2)

prop_table

expected <- rowSums(taxa_location) / sum(taxa_location)
expected
bias_index <- sweep(prop_table, 1, expected, "-")
bias_index
library(vegan)

ca <- cca(taxa_location)
plot(ca)


library(vegan)

# CA
ca <- cca(taxa_location)

# Extract scores
site_scores <- scores(ca, display = "sites")
taxa_scores <- scores(ca, display = "species")

# Plot empty canvas
plot(site_scores, type = "n",
     xlab = "CA1", ylab = "CA2",
     main = "Correspondence Analysis of Taxa by Location")

# Add sites (locations)
text(site_scores, labels = rownames(site_scores),
     col = "blue", cex = 0.9)

# Add taxa (species)
text(taxa_scores, labels = rownames(taxa_scores),
     col = "red", cex = 0.6)

library(ggplot2)

sites <- as.data.frame(site_scores)
sites$Name <- rownames(sites)
sites$Type <- "Location"

taxa <- as.data.frame(taxa_scores)
taxa$Name <- rownames(taxa)
taxa$Type <- "Taxon"

scores_df <- rbind(sites, taxa)

ggplot(scores_df, aes(CA1, CA2, label = Name, color = Type)) +
  geom_text(size = 3) +
  theme_minimal() +
  labs(title = "Correspondence Analysis of Taxa–Location Associations")

plot(ca, display = "sites", type = "n",
     main = "CA showing location associations")

text(ca, display = "sites",
     labels = rownames(site_scores),
     col = "blue", cex = 1)


# Species over-representation---------------------------------------------------

df_unique <- df_clean |>
  select(class, species) |>
  distinct() |>
  drop_na()

class_counts <- df_unique %>%
  count(class, name = "Observed")

n_classes <- nrow(class_counts)
total_species <- sum(class_counts$Observed)

class_counts$Expected <- total_species / n_classes

chisq_test <- chisq.test(class_counts$Observed,
                         p = rep(1 / n_classes, n_classes))

chisq_test

class_counts$StdResiduals <- chisq_test$stdres
library(ggplot2)

ggplot(class_counts, aes(x = reorder(class, Observed), y = Observed)) +
  geom_col(fill = "steelblue") +
  geom_hline(aes(yintercept = Expected), linetype = "dashed", color = "red") +
  coord_flip() +
  labs(y = "Number of species",
       x = "Taxonomic class") +
  theme_minimal()

ggplot(class_counts, aes(x = reorder(class, StdResiduals),
                         y = StdResiduals,
                         fill = StdResiduals > 0)) +
  geom_col() +
  coord_flip() +
  labs(y = "Standardized residuals",
       x = "Taxonomic class",
       title = "Over- and under-representation of taxonomic classes") +
  theme_classic() +
  guides(fill = "none")
# Species discovery commulation curve
df <- df_clean |>
  select(year_of_publication, species) |>
  distinct() |> 
  arrange(year_of_publication)

first_records <- df |> 
  group_by(species) |> 
  summarise(first_year = min(year_of_publication), .groups = "drop")

# Calculate cumulative species discovery
cum_species <- first_records |> 
  count(first_year, name = "new_species") |> 
  arrange(first_year) |> 
  mutate(
    cumulative_species = cumsum(new_species)
  )

# Visualize cumulative discovery through time
ggplot(cum_species, aes(x = first_year, y = cumulative_species)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Cumulative number of species",
    title = "Cumulative species discovery in mangrove surveys (1990–2024)"
  ) +
  theme_minimal()

model <- lm(cumulative_species ~ first_year, data = cum_species)
summary(model)


MannKendall(cum_species$cumulative_species)

ggplot(cum_species, aes(x = first_year, y = new_species)) +
  geom_col(fill = "grey60") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Year",
    y = "Number of newly recorded species"
  ) +
  theme_minimal()

ggplot(cum_species, aes(x = first_year, y = cumulative_species)) +
  geom_point(size = 2) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year",
    y = "Cumulative number of species"
  ) +
  theme_classic()

# Step 1: Identify the first record year for each species
library(dplyr)

first_records <- df_clean |>
  select(year_of_publication, species, site) |>
  unique() |> 
  group_by(species) |> 
  summarise(first_year = min(year_of_publication)) |> 
  ungroup() |>
  unique()

# Step 1: Identify the first record year for each species
new_species_per_year <- first_records |> 
  count(first_year, name = "new_species")

# Step 3: Visualize independent discovery over time

ggplot(new_species_per_year,
       aes(x = first_year, y = new_species)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    x = "Year",
    y = "Number of new species discovered"
  ) +
  theme_classic()

MannKendall(new_species_per_year$new_species)

glm_model <- glm(new_species ~ first_year,
                 family = poisson,
                 data = new_species_per_year)

summary(glm_model)


df_taxa <- df_clean |>
  select(site, species) |>
  drop_na() |>
  distinct(site, species) |>
  count(site, name = "species_richness")

kruskal.test(species_richness ~ site, data = df_taxa)

glm_model <- glm(species_richness ~ site,
                 family = poisson,
                 data = df_taxa)

summary(glm_model)


library(vegan)
# Create the presence/absence matrix
comm <- df_clean |> 
  dplyr::select(site, class, species) |> 
  distinct(site, class, species) |>
  drop_na() |>
  summarise(
    count = n_distinct(species)
  )
  tidyr::pivot_wider(names_from = species, values_from = species,
                     values_fn = length, values_fill = 0)

# Extract site names
sites <- comm$site

# Remove the site column for distance calculation
comm_matrix <- comm %>%
  dplyr::select(-site) %>%
  as.matrix()

# Convert to presence/absence (0/1)
comm_matrix[comm_matrix > 0] <- 1

# Perform PERMANOVA
adonis2(comm_matrix ~ sites, method = "jaccard")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)
library(circlize)

Heatmap(
  resid_mat,
  name = "Std. residual",
  col = colorRamp2(c(-4, 0, 4), c("blue", "white", "red")),
  row_names_side = "left",
  row_names_gp = gpar(fontsize = 12, fontface = "italic"),
  column_names_rot = 90,
  column_names_gp = gpar(fontsize = 12),
  cluster_rows = FALSE,
  cluster_columns = FALSE
)

