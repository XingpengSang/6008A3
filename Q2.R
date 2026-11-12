# ============================================================
# Q2(e): Factorial Effects and Lenth's Method
# ============================================================

# 1. Enter the factorial runs
design <- data.frame(
  A = c(-1, 1, -1, 1, -1, 1, -1, 1),
  B = c(-1, -1, 1, 1, -1, -1, 1, 1),
  C = c(-1, -1, -1, -1, 1, 1, 1, 1),
  Y = c(60, 62, 82, 86, 85, 85, 61, 61)
)

# 2. Calculate interaction columns
design$AB  <- design$A * design$B
design$AC  <- design$A * design$C
design$BC  <- design$B * design$C
design$ABC <- design$A * design$B * design$C

# 3. Calculate seven factorial effects
effect_names <- c("A", "B", "C", "AB", "AC", "BC", "ABC")

effects <- sapply(effect_names, function(x) {
  sum(design[[x]] * design$Y) / 4
})

cat("Factorial Effects:\n")
print(effects)

# 4. Lenth's method
m <- length(effects)
abs_effects <- abs(effects)

s0 <- 1.5 * median(abs_effects)

PSE <- 1.5 * median(
  abs_effects[abs_effects < 2.5 * s0]
)

df <- m / 3

cat("\nInitial scale estimate (s0):", s0, "\n")
cat("Pseudo standard error (PSE):", PSE, "\n")
cat("Approximate degrees of freedom:", df, "\n")

# 5. Calculate ME and SME
alpha <- c(0.05, 0.01)

ME <- qt(1 - alpha / 2, df = df) * PSE

SME <- qt(
  1 - alpha / (2 * m),
  df = df
) * PSE

results <- data.frame(
  Alpha = alpha,
  ME = ME,
  SME = SME
)

cat("\nLenth's Method Thresholds:\n")
print(results)

# 6. Identify significant effects using SME
for (i in seq_along(alpha)) {
  significant <- names(effects)[abs(effects) > SME[i]]

  cat("\nSignificant effects at alpha =", alpha[i], ":\n")
  print(significant)
}

# 7. Approximate p-value of the largest effect
largest_effect <- max(abs(effects))
t_value <- largest_effect / PSE

approx_p <- 2 * pt(-t_value, df = df)

cat("\nLargest absolute effect:", largest_effect, "\n")
cat("Approximate t statistic:", t_value, "\n")
cat("Approximate p-value:", approx_p, "\n")

# 8. Half-normal plot
ordered_effects <- sort(abs_effects)

half_normal_scores <- qnorm(
  (1 + ppoints(m)) / 2
)

plot(
  half_normal_scores,
  ordered_effects,
  main = "Half-Normal Plot of Factorial Effects",
  xlab = "Half-Normal Scores",
  ylab = "Absolute Factorial Effects",
  pch = 19
)

text(
  half_normal_scores,
  ordered_effects,
  labels = names(ordered_effects),
  pos = 3,
  cex = 0.8
)
