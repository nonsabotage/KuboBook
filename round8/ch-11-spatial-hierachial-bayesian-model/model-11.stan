data {
    int<lower=1> N_LOC;
    int<lower=0> Y[N_LOC];
}
parameters {
    real beta1;
    real s_r;
    vector[N_LOC] r;
}
transformed parameters {
    real<lower=0> lambda[N_LOC];
    for (i in 1:N_LOC) {
        lambda[i] = exp(beta1 + r[i]);
    }
}
model {

    r[1] ~ normal(r[2], s_r);
    for (i in 2:(N_LOC-1)) {
        r[i] ~ normal((r[i-1]+r[i+1])/2, s_r/sqrt2());
    }
    r[N_LOC] ~ normal(r[N_LOC-1], s_r);

    for (j in 1:N_LOC) {
        Y[j] ~ poisson(lambda[j]);
    }
}
