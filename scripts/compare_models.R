# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Code to compare estimation using MLE (quickfit) and MCMC (rstan)
# Adam Kucharski (2023)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

library(devtools)
library(rstan)
library(MASS)
devtools::install_github("adamkucharski/quickfit")

set.seed(10)

# Simulate small amount of data
sim_data <- rnorm(100, 4, 2)
truehist(sim_data, col="light gray")

setwd("~/Documents/GitHub/quickfit/scripts/")

# Fit with Bayesian model --------------------------------------------

# Compile stan model
# stan_input <- rstan::stanc(file="normal_dist.stan")
# model_1 <- rstan::stan_model(stanc_ret = stan_input)

# Fit stan model
fit_1 <- rstan::sampling(model_1, 
                         warmup=100, 
                         iter=1e3, 
                         seed=1,
                         data=list(y=sim_data, N=length(sim_data)))

# Plot outputs
rstan::stan_hist(fit_1)
print(fit_1, probs=c(0.025, 0.5, 0.975))

# Fit with MLE ------------------------------------------------------------

# Estimate 95% CI based on profile likelihood
sim_data <- rgamma(100, 4, 2)
log_l <- function(x,a,b){ dgamma(x,a,b,log=T) }


quickfit::estimate_MLE(log_l,data_in=sim_data,n_param=2,a_initial=3,b_initial=1)

aa <- Sys.time()
quickfit::calculate_profile(log_l,data_in=sim_data,n_param=2,a_initial = 3,b_initial = 1,precision=0.01)
Sys.time() - aa

# Fit with stats4 ---------------------------------------------------------

library(stats4)

log_l <- function(a,b) -sum(dgamma(sim_data, a, b, log = TRUE))

aa <- Sys.time()
x <- stats4::mle(log_l, start = list(a = 3, b = 1))
confint(x)
Sys.time() - aa
