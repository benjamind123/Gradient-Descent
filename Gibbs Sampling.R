# Gibbs sampling for Conjugate Model #
######################################

# Number of samples

N <- 1000

Sample <- matrix(c(rep(0, 3*N)), ncol = 3)
colnames(Sample) <- c("alpha", "beta", "theta")

# Binomial data

n <- 10
y <- rbinom(100, n, 0.15)

# Gibbs sampling with full conditional
# distributions of the model parameters

# Initial values

Sample[1, ] <- c(20, 20, 0.2)

for (i in 2:N){
  Sample[i ,1] <- (exp(-0.01 * Sample[i - 1, 1]) * Sample[i - 1, 3] ^ (Sample[i - 1, 1] - 1)) / beta(Sample[i - 1, 1], Sample[i - 1, 2])
  Sample[i, 2] <- (exp(-0.01 * Sample[i - 1, 2]) * (1 - Sample[i - 1, 3]) ^ (Sample[i - 1, 2] - 1)) / beta(Sample[i, 1], Sample[i - 1, 2])
  Sample[i ,3] <- rbeta(1, sum(y) + Sample[i, 1], n - sum(y) + Sample[i, 2])
}

for (i in 2:N){
  Sample[i ,3] <- rbeta(1, sum(y) + 4, n - sum(y) + 10) 
}

plot(Sample[, 3], type = "l", col = "blue", main = "Trace plot for Gibbs sampler")
