data {
   int<lower=1> N_OBS;
   int<lower=0> Y_OBS[N_OBS];
}

parameters {
   real r[N_OBS];
   real<lower=0> s_r;
   real beta1;
}

model {
   for (j in 1:N_OBS)
      r[j] ~ normal(0, s_r);
   for (j in 1:N_OBS)
      Y_OBS[j] ~ poisson_log(beta1 + r[j]);
}

generated quantities {
   real<lower=0> lambda[N_OBS];
   for (j in 1:N_OBS)
      lambda[j] = exp(beta1 + r[j]);
}
