
# ============================================================
# DSC6008 Assignment 3 - Question 3(d)
# Fractional Factorial Design
# ============================================================

rm(list = ls())

# 1. Enter the experimental data
data <- data.frame(
  A = c(-1, 1, -1, 1, -1, 1, -1, 1,
        -1, 1, -1, 1, -1, 1, -1, 1),

  B = c(-1, -1, 1, 1, -1, -1, 1, 1,
        -1, -1, 1, 1, -1, -1, 1, 1),

  C = c(-1, -1, -1, -1, 1, 1, 1, 1,
        -1, -1, -1, -1, 1, 1, 1, 1),

  D = c(rep(-1, 8), rep(1, 8)),

  E = c(-1, 1, 1, -1, 1, -1, -1, 1,
        -1, 1, 1, -1, 1, -1, -1, 1),

  F = c(-1, -1, 1, 1, 1, 1, -1, -1,
        1, 1, -1, -1, -1, -1, 1, 1),

  y = c(4, 6, 7, 2, 3, 1, 5, 9,
        3, 2, 8, 5, 4, 4, 4, 6)
)

# 2. Calculate six main effects
main_names <- c("A", "B", "C", "D", "E", "F")

main_effects <- sapply(main_names, function(x) {
  sum(data[[x]] * data$y) / 8
})

# 3. Calculate seven two-factor interaction alias effects
interaction_columns <- list(
  AB_CE = data$A * data$B,
  AC_BE = data$A * data$C,
  AD_EF = data$A * data$D,
  AE_BC_DF = data$A * data$E,
  AF_DE = data$A * data$F,
  BD_CF = data$B * data$D,
  BF_CD = data$B * data$F
)

interaction_effects <- sapply(interaction_columns, function(x) {
  sum(x * data$y) / 8
})

# Combine all 13 effects
effects <- c(main_effects, interaction_effects)

cat("13 ESTIMABLE FACTORIAL EFFECTS\n")
print(effects)

# 4. Lenth's method
m <- length(effects)
abs_effects <- abs(effects)

s0 <- 1.5 * median(abs_effects)

PSE <- 1.5 * median(
  abs_effects[abs_effects < 2.5 * s0]
)

df <- m / 3

ME <- qt(1 - 0.05 / 2, df = df) * PSE

SME <- qt(
  1 - 0.05 / (2 * m),
  df = df
) * PSE

cat("\nLENTH'S METHOD\n")
cat("Initial scale estimate:", s0, "\n")
cat("Pseudo standard error:", PSE, "\n")
cat("Degrees of freedom:", df, "\n")
cat("ME:", ME, "\n")
cat("SME:", SME, "\n")

significant <- names(effects)[abs(effects) > SME]

cat("\nSignificant effects at alpha = 0.05:\n")
print(significant)

# 5. Fit reduced linear model
model <- lm(y ~ B + E + F + A:C + A:F, data = data)

cat("\nREDUCED LINEAR MODEL\n")
print(summary(model))

cat("\nModel coefficients:\n")
print(coef(model))

# 6. Half-normal plot
ordered_effects <- sort(abs_effects)

half_normal_scores <- qnorm(
  (1 + ppoints(m)) / 2
)

plot(
  half_normal_scores,
  ordered_effects,
  main = "Half-Normal Plot of Q3 Factorial Effects",
  xlab = "Half-Normal Scores",
  ylab = "Absolute Factorial Effects",
  pch = 19
)

text(
  half_normal_scores,
  ordered_effects,
  labels = names(ordered_effects),
  pos = 3,
  cex = 0.7
)
