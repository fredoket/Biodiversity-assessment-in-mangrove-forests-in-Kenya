# General function to test alpha diversity across a grouping variable
alpha_test <- function(df, group_var = "site") {
  
  measures <- unique(df$Measure)   # get all measures in the dataset
  
  for (measure in measures) {
    cat("\n====================\n")
    cat("Testing:", measure, "by", group_var, "\n")
    cat("====================\n")
    
    # Subset data
    df_sub <- subset(df, Measure == measure)
    group <- df_sub[[group_var]]
    
    # Shapiro-Wilk test (normality)
    shapiro <- shapiro.test(df_sub$Value)
    cat("Shapiro-Wilk test: W =", round(shapiro$statistic, 3), 
        "p =", shapiro$p.value, "\n")
    
    # Bartlett test (homogeneity of variances)
    bartlett <- bartlett.test(df_sub$Value ~ group)
    cat("Bartlett test: K-sq =", round(bartlett$statistic, 3), 
        "p =", bartlett$p.value, "\n")
    
    # Decide which test to use
    if (shapiro$p.value > 0.05 & bartlett$p.value > 0.05) {
      cat("✅ Assumptions met → Running ANOVA\n")
      result <- summary(aov(Value ~ group, data = df_sub))
    } else {
      cat("❌ Assumptions failed → Running Kruskal-Wallis\n")
      result <- kruskal.test(Value ~ group, data = df_sub)
    }
    
    print(result)
  }
}

# Example usage:
alpha_test(alpha_table, "site")      # test by site
alpha_test(alpha_table, "sample_type")    # test by sample type
