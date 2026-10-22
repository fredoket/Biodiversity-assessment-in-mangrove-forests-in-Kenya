# Chao1
chao_sub <- subset(alpha_table, Measure == "Chao1")
by(chao_sub$Value, chao_sub$sample_type, shapiro.test)

# Shannon
shan_sub <- subset(alpha_table, Measure == "Shannon")
by(shan_sub$Value, shan_sub$sample_type, shapiro.test)

# Simpson
simp_sub <- subset(alpha_table, Measure == "Simpson")
by(simp_sub$Value, simp_sub$sample_type, shapiro.test)


# Chao1 richness by sample type
chao_sub <- subset(alpha_table, Measure == "Chao1")
wilcox.test(Value ~ sample_type, data = chao_sub,
            exact = FALSE)

# Shannon diversity
shan_sub <- subset(alpha_table, Measure == "Shannon")
wilcox.test(Value ~ sample_type, data = shan_sub)

# Simpson evenness
simp_sub <- subset(alpha_table, Measure == "Simpson")
wilcox.test(Value ~ sample_type, data = simp_sub)

