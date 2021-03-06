data {
   int<lower=1> N_LOC;
   int<lower=0> Y[N_LOC];
}

parameters {
   real r[N_LOC];
   real<lower=0> s_r;
   real beta;
}

model {
   for (j in 2:N_LOC)
      r[j] ~ normal(r[j-1], s_r);
   for (j in 1:N_LOC)
      Y[j] ~ poisson_log(beta + r[j]);
}

generated quantities {
   real<lower=0> lambda[N_LOC];
   for (j in 1:N_LOC)
      lambda[j] = exp(beta + r[j]);
}
