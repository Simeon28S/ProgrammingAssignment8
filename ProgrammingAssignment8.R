# cop2073c-pa8.R
# Simeon Sipes
# 07/09/26
# Performs one-way ANOVA on zombie cookie recipes

# Response variable: transformation significance values
# Predictor variable: cookie recipe group
# H0: The mean transformation values are equal for all recipes.
# HA: At least one recipe mean is different.

RecipeA <- c(0, 4, 4, 5, 4, 1, 2, 3, 3, 5)
RecipeB <- c(7, 6, 4, 2, 3, 0, 8, 6, 2, 3)
RecipeC <- c(1, 0, 1, 0, NA, 1, 1, NA, 1, 0)

df <- data.frame(RecipeA, RecipeB, RecipeC)

newdf <- na.omit(stack(df))

cat("stacked data frame:\n")
print(newdf)

# Run the initial summary on the stacked data.
initial_summary <- summary(newdf)

cat("\ninitial summary:\n")
print(initial_summary)

# Run the one-way ANOVA test.
anova_test <- aov(values ~ ind, data = newdf)

anova_summary <- summary(anova_test)

cat("\nanova summary:\n")
print(anova_summary)

# Extract the p-value and compare it to alpha.
pval <- anova_summary[[1]]$`Pr(>F)`[1]
signif <- 0.05

cat("\np_value is", pval, "\n")

if (pval < signif) {
  cat("Sufficient evidence to reject the null hypothesis.\n")
  cat("Ingredient X may have an effect on the results.\n")
} else {
  cat("Insufficient evidence to reject the null hypothesis.\n")
  cat("Ingredient X may not have an effect on the results.\n")
}