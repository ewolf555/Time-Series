library(readxl)
# Solution to Excercise 4 on the first Worksheet

# Load and select data. 
data <- read_excel("...")
hicp <- data$HICP

data$TIME <- as.Date(data$TIME, format = "%Y-%m-%d")

plot(data$HICP ~ data$TIME, type="l", main = "HICP - Euro Area",
     xlab = "Time", ylab = "HICP")
grid()

# Add a time trend
linTrend <- lm(data$HICP ~ c(1:nrow(data)))

plot(data$HICP ~ data$TIME, type="l", main = "HICP - Euro Area",
     xlab = "Time", ylab = "HICP")
lines(linTrend$fitted.values~ data$TIME, col = "red")
grid()

# Plot detrended component
plot(linTrend$residuals ~ data$TIME, type = "l", main = "Cyclical Component",
     xlab = "Time", ylab = "HICP")
grid()

# Calculate and plot Year-on-Year Changes
deltaHicp <- diff(log(hicp), lag = 12)*100

# Mean of the series
xBar <- mean(deltaHicp)
print(xBar)

# Plot with mean and H0
plot(deltaHicp ~ data$TIME[13:208], type = "l", xlab = "Time", 
     ylab = "Inflation", main = "Inflation (YoY) - Euro Area", lwd=2)
grid()
abline(h = 2, col = "red")
abline(h = xBar, col = "blue")
legend("bottomleft", legend = c("H0: mu = 2", paste("Mean = ", round(xBar, 2))), 
                                col = c("red", "blue"), lty = 1)

# T-test
t.test(deltaHicp, mu = 2, alternative = c("two.sided"), conf.level = 0.95)

# rejcet Null-Hyphothesis if p<0.05 
