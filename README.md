# quickfit

`R` package to estimate one and two parameter functions via maximum likelihood

## Installation

The easiest way to install the development version of `quickfit` is to use the `devtools` package:

```r
# install.packages("devtools")
library(devtools)
install_github("adamkucharski/quickfit")
library(quickfit)

# load dependencies
# install.packages("MASS")
library(MASS)
```

## Quick start

Generate some simulated data, define a likelihood, then estimate MLE (faster), or MLE and 95% confidence interval based on profile likelihood (slower):

```r
sim_data <- rnorm(15, 4, 2)

# Define likelihood function
log_l <- function(x,a,b){ dnorm(x,a,b,log=T) }

# Estimate MLE
estimate_MLE(log_l,sim_data,n_param=2,a_inital=3,b_initial=1)

# Estimate 95% CI based on profile likelihood
calculate_profile(log_l,data_in=sim_data,n_param=2,a_initial = 3,b_initial = 1,precision=0.1)

```
