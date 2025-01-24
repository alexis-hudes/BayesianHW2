# Bayesian HW Set 2: Monte Carlo/ Convergence/ Seeds/ Scripts/ Plotting
# January 2025

######################################################
##  author: Alexis Hudes
##  copyright by the author
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)
######################################################

# Use a Monte Carlo simulation method to determine the mean and 95th percentile
# from a known univariate normal distribution with a mean of 0 and standard deviation of 1
# with your estimated uncertainties

# Normal Dist: convergence of standard deviation -----------------------------------------
# Define parameters
mu <- 0
sigma <- 1 
seeds <- c(1, 42, 237, 684, 705) # Different seeds
colors <- c("red", "blue", "purple",  "darkgoldenrod1", "darkolivegreen") # Assigning colors for each seed

# Create an empty plot to start
pdf("hw2_normal_convergence_std.pdf", height = 6, width = 6)
plot(NULL, xlim = c(10, 10^7), ylim = range(0, 2), main = 'Standard Deviation',
     xlab = 'Number of Draws', ylab = 'Observed Standard Deviation', log = 'x')

abline(h=1,col="black", lty =2)

# Loop over different seeds
for (seed_index in 1:length(seeds)) {
  set.seed(seeds[seed_index])
  
  #initializing
  sd_by_draws = c()
  
  # Loop over different numbers of draws
  for (n in 1:7) {
    num <- 10 ^ n
    x <- rnorm(n = num, mean = mu, sd = sigma)
    sd_by_draws[n] <- sd(x)
  }
  
  # Plot points (pch=19 means solid points)
  points(c(10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), 
         sd_by_draws, col = colors[seed_index], pch = 19)
}

# Add a legend
legend("topright", legend = paste("Seed =", seeds), col = colors, pch = 19)

dev.off()

# Normal Dist: convergence of standard error -----------------------------------------
# Empyt plot
pdf("hw2_normal_convergence_se.pdf", height = 6, width = 6)
plot(NULL, xlim = c(10, 10^7), ylim = range(0, 0.5), main = 'Standard Error',
     xlab = 'Number of Draws', ylab = 'Standard Error', log = 'x')

# Loop over different seeds
for (seed_index in 1:length(seeds)) {
  set.seed(seeds[seed_index])
  
  #initializing
  se_by_draws = c()
  
  # Loop over different numbers of draws
  for (n in 1:7) {
    num <- 10 ^ n
    x <- rnorm(n = num, mean = mu, sd = sigma)
    se_by_draws[n] <- sd(x)/sqrt(num)
  }
  
  # Plot standard error for the current seed using the corresponding color
  points(c(10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), 
         se_by_draws, col = colors[seed_index], pch = 19)
}

# Add a legend
legend("topright", legend = paste("Seed =", seeds), col = colors, pch = 19)

dev.off()

# Normal Dist: convergence of means -----------------------------------------
# Empty plot
pdf("hw2_normal_convergence.pdf", height = 6, width = 6)
plot(NULL, xlim = c(10, 10^7), ylim = range(-0.7, 0.7),
     xlab = 'Number of Draws', main = 'Mean', ylab = 'Observed Mean', log = 'x')

abline(h=0,col="black", lty =2)

# Loop over different seeds
for (seed_index in 1:length(seeds)) {
  set.seed(seeds[seed_index])
  
  #initializing
  means_by_draws = c()
  
  # Loop over different numbers of draws
  for (n in 1:7) {
    num <- 10 ^ n
    x <- rnorm(n = num, mean = mu, sd = sigma)
    means_by_draws[n] <- mean(x)
  }
  
  # Plot means for the current seed using the corresponding color
  points(c(10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), 
         means_by_draws, col = colors[seed_index], pch = 19)
}

# Add a legend
legend("topright", legend = paste("Seed =", seeds), col = colors, pch = 19)

dev.off()

# Normal Dist: Convergence of 95th percentile ------------------------------------------------
# Empty plot
pdf("hw2_normal_95th_convergence.pdf", height = 6, width = 6)
plot(NULL, xlim = c(10, 10^7), ylim = range(0, 3), main = '95th Percentile',
     xlab = 'Number of Draws', ylab = '95th Percentile', log = 'x')

abline(h=1.645,col="black", lty =2)

# Loop over different seeds
for (seed_index in 1:length(seeds)) {
  set.seed(seeds[seed_index])
  means_by_draws = c()
  CI_by_draws = c()
  
  # Loop for draws
  for (n in 1:7) {
    num <- 10 ^ n
    x <- rnorm(n = num, mean = mu, sd = sigma)
    CI_by_draws[n] <- quantile(x,0.95)
  }
  
  # Plot points
  points(c(10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), 
         CI_by_draws, col = colors[seed_index], pch = 19)
}

# Add a legend
legend("topright", legend = paste("Seed =", seeds), col = colors, pch = 19)

dev.off()

# Normal dist: histogram of a single trial -----------------------------------------
# Set some values.  
num <- 10^5   # number of random draws
set.seed(10)  # setting a seed for reproducible randomization

# Sample randomly from a normal distribution
x <- rnorm(n = num, mean = mu, sd = sigma)

sample_mu <- mean(x)
sample95_plus <- quantile(x,0.95)
sample95_minus <- quantile(x,0.05)

# Plot the results as a histogram.  Also plot a red line showing the mean
# of the sample and blue lines showing the 95th percentile

pdf("hw2_normal_hist.pdf", height = 6, width = 6)
hist(x, main = "Histogram of a single trial", xlab = "Values of Samples", ylab = "Frequency (n = 10^5)")
abline(v = sample_mu, lwd = 2, col = "red")
abline(v = sample95_minus, lwd = 2, col = "blue")
abline(v = sample95_plus, lwd = 2, col = "blue")

legend("topleft", c("Mean",
                    "95th percentile"),
       lwd=c(1,2,2),  col=c("red","blue"), cex=0.75)
dev.off()


# Convergence of pi ----------------------------------------------------------
# Empty plot
pdf("hw2_pi_calcs.pdf", height = 6, width = 6)
plot(NULL, xlim = c(10, 10^7), ylim = range(2, 4), main = 'Calculating Pi',
     xlab = 'Number of Draws', ylab = 'Calculate Value', log = 'x')

abline(h=pi,col="black", lty = 2)

for (seed_index in 1:length(seeds)) {
  set.seed(seeds[seed_index])
  pi_calcs = c()
  for(n_exp in 1:7){
    n = 10^n_exp
    xrand = runif(n, -1, 1) #sampling from a uniform dist for x coords
    yrand = runif(n, -1, 1) #sampling from a uniform dist for y coords

    incircle = c()
    for (i in 1:n){
      # Checking whether the generated points are in the circle
      if(sqrt(xrand[i]^2 + yrand[i]^2)<=1){
        incircle[i] <- 1
      }else{
        incircle[i] <- 0
      }
    }
    # Calculating pi
    pi_calcs[n_exp]=4*sum(incircle)/n
  }
  # Plotting points
  points(c(10, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7), pi_calcs, col = colors[seed_index],pch=19)
}

# Add a legend
legend("topright", legend = paste("Seed =", seeds), col = colors, pch = 19)

dev.off()


# Calculating pi ----------------------------------------------------------
# Getting an average of the 5 seeds for a sample size of 10^6
pi_calcs = c()
for (seed_index in 1:length(seeds)) {
  set.seed(seeds[seed_index])
  n = 10^6
  xrand = runif(n, -1, 1)
  yrand = runif(n, -1, 1)
    
  incircle = c()
  for (i in 1:n){
    if(sqrt(xrand[i]^2 + yrand[i]^2)<=1){
      incircle[i] <- 1
    }else{
      incircle[i] <- 0
    }
  }
  pi_calcs[seed_index] = 4*sum(incircle)/n
}
# Printing the results
print(mean(pi_calcs))
print(sd(pi_calcs))