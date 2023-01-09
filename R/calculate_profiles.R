#' Estimate profile likelihood and 95% CI
#'
#' This function estimate the profile likelihood and 95% CI for a one or two parameter model
#' @param log_likelihood Log-likelihood function in form \code{function(x,a)} (one parameter model) or \code{function(x,a,b)} (two parameter model)
#' @param data_in Vector of observations to be evaluated in log_likelihood, with overall likelihood given by sum(log_likelihood)
#' @param n_param Number of parameters in \code{log_likelihood} model
#' @param a_inital Initial guess for parameter \code{a}
#' @param b_inital Initial guess for parameter \code{b} (if a two parameter model, otherwise default is NULL)
#' @export
#' @examples
#' calculate_profile()

calculate_profile <- function(log_likelihood,
                         data_in,
                         n_param,
                         a_inital,
                         b_inital=NULL
){
  
  # Chi-squared value corresponding to 95% CI
  chi_1 <- qchisq(0.95,1)/2
  
  # One parameter model
  if(n_param==1){
    
    # Calculate MLE
    mle_estimate <- estimate_MLE(log_likelihood,data_in,n_param=1,a_inital)
    
    # Format for optimisation
    optim_profile <- function(theta){
      abs(sum(log_likelihood(data_in,theta[1])) - mle_estimate$log_likelihood + chi_1)
    }
    
    # Run optimisation and return
    # XX CHECK LOWER BOUND
    profile_out_1 <- optim(c(a=a_initial),optim_profile, method = "Brent", lower = mle_estimate$estimate-1e2, upper = mle_estimate$estimate)
    profile_out_2 <- optim(c(a=a_initial),optim_profile, method = "Brent", lower = mle_estimate$estimate, upper = mle_estimate$estimate+1e2)
    
    profile_out <- c(profile_out_1$par,profile_out_2$par)
  }
  
  # Two parameter model
  if(n_param==2){
    # Calculate MLE
    mle_estimate <- estimate_MLE(log_likelihood,data_in,n_param=2,a_inital,b_initial)
    
    # Format for optimisation
    optim_profile <- function(theta){
      abs(sum(log_likelihood(data_in,theta[1])) - mle_estimate$log_likelihood + chi_1)
    }
    
    # Run optimisation and return
    # XX CHECK LOWER BOUND
    profile_out_1 <- optim(c(a=a_initial),optim_profile, method = "Brent", lower = mle_estimate$estimate-1e2, upper = mle_estimate$estimate)
    profile_out_2 <- optim(c(a=a_initial),optim_profile, method = "Brent", lower = mle_estimate$estimate, upper = mle_estimate$estimate+1e2)
    
    profile_out <- c(profile_out_1$par,profile_out_2$par)
    
  }
  
  # Output estimates and likelihood
  output <- list(estimate = mle$par,log_likelihood = -mle$value)
  return(output)
  
}

# DEBUG
data_in <- rnorm(100,5,2)
#log_l <- function(x,a,b){ dnorm(x,a,b,log=T) }
log_l <- function(x,a){ dnorm(x,a,2,log=T) }
log_likelihood <- log_l
a_initial <- 4; b_initial=1

sum(dnorm(data_in,a_initial,b_initial))

xx <- seq(4,6,0.1)
plot(xx,sapply(xx,optim_likelihood))

#estimate_MLE(log_l,data_in,2,4,2)
