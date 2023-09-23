means <- seq(-5, 5, by = 1)

# Generate state space values
x <- seq(-10, 10, length = 500)

# Create a new plotting area
par(mfrow=c(1, length(means)))  # Adjust the layout

# Plot a sequence of normal densities with different means
for (i in 1:length(means)) {
  density_values <- dnorm(x, mean = means[i], sd = 1)
  plot(density_values, x, type = "l", col = "blue", ylab = "State Space", xlab = paste("t=",round(i, 1)),
       main = paste("Density for Mean =", round(means[i], 1)))
}

# Reset the plotting area to the default
par(mfrow=c(1, 1))