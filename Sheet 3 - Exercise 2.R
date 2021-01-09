# Frequency Approach: Summing up Cosines 

wn <- c(1, 1, 1, 1, 1, 1, 1, 1)  # white noise
low <- c(4, 3.8, 3.6, 3.4, 3, 2.4, 1.8, 1)  # low frequency dominates
hi <- c(0, 0.1, 0.5, 1.5, 2.2, 3, 3.8, 4)  # high frequency dominates

# Choose weights
w <- wn
t <- seq(0, 10, 0.01)
# Sum up the cosines with different cycle frequencies
process <- w[1]* cos(0.5*t) + w[2]*cos(7*t) + w[3]*cos(13*t) + w[4]*cos(17*t) + 
          w[5]*cos(37*t) + w[6]*cos(51*t) + w[7]*cos(61*t) + w[8]*cos(91*t)

# Plot the resulting process and the chosen weights for cosines with different 
# frequencies
dev.new()
layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE))
plot(process, type = "l", main = "Summed up Cosines")
grid()
plot(w, type = "l", main = "Coefficients", xaxt="n", xlab = "Frequency")
grid()
axis(1, at=1:8, labels=1:8)

# Theoretical Properties of an AR(1) - ACF vs. Spectrum

alpha <- -0.7
sigma <- 1

# Simulate AR-Process
ar1 = numeric(length = 500)
ar1[1] <- rnorm(1, 0, sigma)
for (i in 2:500){
  ar1[i] <- alpha*ar1[i-1] + rnorm(1, 0, sigma)
}

# Function to compute the Spectrum
ar1_spectrum <- function(omega, alpha){
  spc <- sigma/(2*pi*(1-2*alpha*cos(omega) + alpha^2))
  return(spc)
}

# Function to compute the ACF
ar1_acf <- function(lag, alpha){
  acc <- alpha^lag
  return(acc)
}

# Compute the Spectrum for alpha 
freqs <- seq(from = 0, to = 0.5, by = 0.001)
spectrum_1 <- ar1_spectrum(freqs, alpha)

# Compute the ACF for 10 lags
lags <- 0:10
acf_1 <- ar1_acf(lags, alpha)

# Plot Time Series, Spectrum and ACF to compare properties. 
dev.new()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(ar1, type = "l")
grid()
title(main = paste("AR(1)- Process, alpha = ", alpha))
plot(spectrum_1, type = "l", main = "Spectrum (theoretical)", xaxt="n")
axis(1, at = c(0, 100, 200, 300, 400, 500), 
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5"))
barplot(acf_1, main = "ACF (theoretical)", xlab = "Lag")


# A positive AR(1)-Coefficient means that two observations are postively
# correlated and yields a spectrum with high values near zero and low values 
# at 0.5. This indicates that the process is smooth in the time domain.
# The autocorrelation function is postive and decays geometrically towards zero.
# A negative AR(1)-coefficientreflects a negative correlation between two 
# realizations. This implies high values of the spectrum close to 0.5 and yields
# a process that is rough in the time domain. Stationarity implies that the 
# autocorrelation function also decays but it now shows an oscillating behavior. 

# Exercise 2 iv)
setwd("...")
data <- read_csv("mom.csv", col_names = FALSE)
momHicp <- data$X1

specHicp <- spectrum(momHicp, method = "ar", 
                     main = "Spectrum month-to-month Inflation")

# Pick value that maximizes the spectrum
peakIdx <- which.max(specHicp$spec)
peakFreq <- specHicp$freq[peakIdx]
print(peakFreq)
peakPeriod <- 1/peakFreq
print(peakPeriod)



