functions {
  // Normal log pdf
  real model_lpdf(real y, real mu, real sigma){
  	return normal_lpdf(y | mu, sigma);
  }
}
data {
  int N;
  real y[N];
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  // Priors
  mu ~ normal(0, 10);
  sigma ~ normal(0,10);
  // Likelihood
  for(n in 1:N){
    target +=  model_lpdf(y[n] | mu, sigma);
  }
}
