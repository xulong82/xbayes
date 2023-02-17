//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

data {
  int<lower=0> N;
  vector[N] y;
  real tau;
  real sigma;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real theta;
}

model {
  theta ~ normal(0, tau);
  y ~ normal(theta, sigma);
}
