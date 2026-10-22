# Save the data for offline analysis
write.csv(df_project, "data/df_project.csv")

# Data inspection
df_status <- df_project |>
  select(scientific_name, status)


# Exploratory bar graph
occ1 <- ggplot(df_project, aes(x = status,
                               fill = status)) +
  geom_bar(width = 0.3, fill = "grey40") +
  ylab("Number of OTUs") +
  xlab("Occurance status") +
  geom_text(stat = 'count', aes(label =
                                  ..count..),
            vjust = -0.5) +
  theme_classic() +
  theme(legend.position = "none")

# Species habitat
df_habitat <- df_project |>
  select(scientific_name, Habitat)



hh1 <- ggplot(df_habitat, aes(x = Habitat,
                              fill = Habitat))+
  geom_bar(width = 0.6, fill = "grey40")+
  xlab("Habitats")+
  ylab("Number of OTUs")+
  geom_text(stat = 'count', aes(label =
                                  ..count..),
            vjust = -0.3) +
  theme_classic() +
  theme(legend.position = "none")

# Import the data
#df_project <- read_csv("data/df_project.csv")