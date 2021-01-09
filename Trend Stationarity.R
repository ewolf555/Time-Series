# Trend Stationarity
# Simulation shows the results of Excercise 1 d) on the first worksheet.

# Simulate the process
x <- numeric(length = 150)
a <- 2
b <- 0.5
alpha <- 0.3

# Set starting value
x[1] <- a + b +  rnorm(1, 0, 5)

# Generate the time series recursively
for (i in 2:150){
  x[i] <- a + b*i + 0.3*x[i-1] + rnorm(1, 0, 5)
}

# Calculate the values for the trend
t <- 1:150
mu0 <- (a)/(1-alpha) - alpha/(1-alpha)^2
mu1 <- b/(1-alpha)

# Plot of simualted series with constants and true time trend
t <- 1:150
plot(x, type="l", main = "AR(1) with time trend")
lines(a + b*t, col="blue")
lines(mu0 + mu1*t, col="red")
grid()
legend("bottomright", legend = c("simulated series", "a + bt", "mu0 + mu1t"), 
       col = c("black", "blue", "red"), lty=1)

# Look at detrended Series to see stationarity
trend <- mu0 + mu1*t
z <- x - trend

# plot series
plot(z, type = "l", main = "AR(1) detrended")
grid()
# add mean of detrended series to show that it is constant and approximately 0 
abline(h = mean(z), col = "red")

