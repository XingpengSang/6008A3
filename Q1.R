# DSC6008 Assignment 3
# Question 1: Two-way ANOVA

# 1. Enter the experimental data
data <- data.frame(
  Oil = rep(c("Oil1", "Oil2", "Oil3"), each = 10),

  Truck = rep(
    rep(paste0("Truck", 1:5), each = 2),
    times = 3
  ),

  Fuel = c(
    0.480, 0.520, 0.614, 0.654, 0.467, 0.507,
    0.309, 0.349, 0.492, 0.532,

    0.515, 0.555, 0.655, 0.695, 0.500, 0.540,
    0.415, 0.455, 0.520, 0.560,

    0.493, 0.533, 0.575, 0.615, 0.468, 0.508,
    0.380, 0.420, 0.490, 0.530
  )
)

# Convert Oil and Truck into categorical variables
data$Oil <- as.factor(data$Oil)
data$Truck <- as.factor(data$Truck)

# Check the dataset
print(data)

# 2. Fit the two-way ANOVA model
model <- aov(Fuel ~ Oil * Truck, data = data)

# 3. Display the ANOVA table
print(summary(model))

# Q1(c): Interaction Plot

interaction.plot(
  x.factor = data$Truck,
  trace.factor = data$Oil,
  response = data$Fuel,
  fun = mean,
  type = "b",
  pch = c(1, 2, 3),
  col = c("blue", "red", "darkgreen"),
  xlab = "Truck Engine",
  ylab = "Mean Fuel Consumption",
  trace.label = "Oil Type",
  main = "Interaction Plot of Oil and Truck"
)

# Q1(d): Tukey's HSD Test
print(TukeyHSD(model, which = "Oil"))