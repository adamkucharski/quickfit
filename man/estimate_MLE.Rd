% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_MLE.R
\name{estimate_MLE}
\alias{estimate_MLE}
\title{Estimate MLE}
\usage{
estimate_MLE(log_likelihood, data_in, n_param, a_inital, b_inital = NULL)
}
\arguments{
\item{log_likelihood}{Log-likelihood function in form \code{function(x,a)} (one parameter model) or \code{function(x,a,b)} (two parameter model)}

\item{data_in}{Vector of observations to be evaluated in log_likelihood, with overall likelihood given by sum(log_likelihood)}

\item{n_param}{Number of parameters in \code{log_likelihood} model}

\item{a_inital}{Initial guess for parameter \code{a}}

\item{b_inital}{Initial guess for parameter \code{b} (if a two parameter model, otherwise default is NULL)}
}
\description{
This function calculates the maximum-likelihood estimate for a one or two parameter model
}
\examples{
estimate_MLE()
}
