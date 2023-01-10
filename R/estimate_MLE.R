#' Estimate MLE
#'
#' This function calculates the maximum-likelihood estimate for a one or two parameter model
#' @param log_likelihood Log-likelihood function in form \code{function(x,a)} (one parameter model) or \code{function(x,a,b)} (two parameter model)
#' @param data_in Vector of observations to be evaluated in log_likelihood, with overall likelihood given by sum(log_likelihood)
#' @param n_param Number of parameters in \code{log_likelihood} model
#' @param a_initial Initial guess for parameter \code{a}
#' @param b_initial Initial guess for parameter \code{b} (if a two parameter model, otherwise default is NULL)
#' @export
#' @examples
#' estimate_MLE()

estimate_MLE <- function(log_likelihood,
                         data_in,
                         n_param,
                         a_initial,
                         b_initial=NULL
                         ){
  
  # One parameter model
  if(n_param==1){
    # Format for optimisation
    optim_likelihood <- function(theta){
      -sum(log_likelihood(data_in,as.numeric(theta[1])))
    }
    
    # Run optimisation and return
    # XX ADD tests for upper and lower bound
    mle <- optim(c(a=a_initial),optim_likelihood,method = "Brent", lower = -abs(a_initial)*10, upper = abs(a_initial)*10)
  }
  
  # Two parameter model
  if(n_param==2){
    # Format for optimisation
    optim_likelihood <- function(theta){
      -sum(log_likelihood(data_in,as.numeric(theta[1]),as.numeric(theta[2])))
    }
    
    # Run optimisation and return
    mle <- optim(c(a=a_initial,b=b_initial),optim_likelihood)
  }

  # Output estimates and likelihood
  output <- list(estimate = mle$par,log_likelihood = -mle$value)
  return(output)
  
}

# # DEBUG
# sim_data <- rnorm(100,5,2)
# log_l <- function(x,a,b){ dnorm(x,a,b,log=T) }
# log_likelihood <- log_l
# a_initial <- 4; b_initial=1
# 
# sum(dnorm(data_in,a_initial,b_initial))


