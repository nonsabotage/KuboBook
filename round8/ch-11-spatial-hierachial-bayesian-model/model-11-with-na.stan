data {
   int<lower=1> N_OBS;
   int<lower=0> Y_OBS[N_OBS];
   int<lower=1> INDEX_Y_OBS[N_OBS];
   int<lower=1> N_NA;
   int<lower=1> INDEX_Y_NA[N_NA];
}

parameters {
   real r[(N_OBS + N_NA)];
   real<lower=0> s_r;
   real beta1;
}

model {
   for (j in 1:(N_OBS + N_NA))
      r[j] ~ normal(0, s_r);
   for (j in 1:N_OBS)
      Y_OBS[j] ~ poisson_log(beta1 + r[INDEX_Y_OBS[j]]);
}

generated quantities {
   real<lower=0> lambda[(N_OBS + N_NA)];
   int<lower=0> y_na[N_NA];
   for (j in 1:(N_OBS + N_NA))
      lambda[j] = exp(beta1 + r[j]);
   for (j in 1:N_NA)
      y_na[j] = poisson_rng(lambda[INDEX_Y_NA[j]]);
}
