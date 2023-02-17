# Type-1 error rate is not a concern for Bayesian statistics

# Frank Farrell 2020a 

library(arm)
library(rstan)
library(bayesplot)

# Frequentist-oriented Bayesian designs
# Type-1 error rate control for group sequential tests, under the Bayesian framework

tau = 0.054 # prior se
sigma = 1 # data sd
N = 1e3 # sample size in each trial
K = 4 # 4 looks

zzz = lapply(1:1e4, function(x) { # simulate 1e4 trials
  
  zz1 = lapply(1:K, function(k) {
    
    y = rnorm(N, 0, sigma) # data

    theta_hat <- mean(y) * (N/sigma^2) / (N/sigma^2 + 1/tau^2)
    theta_hat_se <- sqrt(1 / (N/sigma^2 + 1/tau^2))
    
    abs(theta_hat) > 1.96 * theta_hat_se
  }) %>% unlist

  any(zz1)
  
})

mean(unlist(zzz))

y1 = rnorm(10, 0, 1)
y2 = rnorm(1, 0, 1/sqrt(10))

###################################################################

xpost <- function() { # sigma as s.d. 
  theta_hat <- mean(y) * (n/sigma^2) / (n/sigma^2 + 1/tau^2)
  theta_hat_se <- sqrt(1 / (n/sigma^2 + 1/tau^2))
  list(theta_hat, theta_hat_se)
}

xpost <- function() { # sigma as s.e.
  theta_hat <- mean(y) * (1/sigma^2) / (1/sigma^2 + 1/tau^2)
  theta_hat_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))
  list(theta_hat, theta_hat_se)
}

######### Andrew Gelman Blog: what he was doing?
# with small tau, he was assigning strong prior on theta; he was also using only 1 data point to adjust the prior
# so why multiple comparison was not a concern, and the small error rate?
# because of the known prior, which is null, and strong

spidey <- function(sigma, tau, N) { cat("sigma = ", sigma, ", tau = ", tau, ", N = ", N, "\n", sep="")
  
  theta <- rnorm(N, 0, tau)
  y <- theta + rnorm(N, 0, sigma)
  
  signif_classical <- abs(y) > 1.96 * sigma
  
  cat(sum(signif_classical), " (", fround(100*mean(signif_classical), 1), "%) of the 95% classical intervals exclude 0\n", sep="")
  cat("Mean absolute value of these classical estimates is", fround(mean(abs(y)[signif_classical]), 2), "\n")
  cat("Mean absolute value of the corresponding true parameters is", fround(mean(abs(theta)[signif_classical]), 2), "\n")
  cat(fround(100*mean((sign(theta)!=sign(y))[signif_classical]), 1), "% of these are the wrong sign (Type S error)\n", sep="")
  
  theta_hat_bayes <- y * (1/sigma^2) / (1/sigma^2 + 1/tau^2)
  theta_se_bayes <- sqrt(1 / (1/sigma^2 + 1/tau^2))
  signif_bayes <- abs(theta_hat_bayes) > 1.96 * theta_se_bayes
  
  cat(sum(signif_bayes), " (", fround(100*mean(signif_bayes), 1), "%) of the 95% posterior intervals exclude 0\n", sep="")
  cat("Mean absolute value of these Bayes estimates is", fround(mean(abs(theta_hat_bayes)[signif_bayes]), 2), "\n")
  cat("Mean absolute value of the corresponding true parameters is", fround(mean(abs(theta)[signif_bayes]), 2), "\n")
  cat(fround(100*mean((sign(theta)!=sign(theta_hat_bayes))[signif_bayes]), 1), "% of these are the wrong sign (Type S error)\n", sep="")
}

sigma <- 1
tau <- .5
N <- 1e2

spidey(sigma, tau, N)

#########

stan_data = list(N = N, x = x, y = y)

fit = stan_model("GitHub/xbayes/stan_model.stan")
