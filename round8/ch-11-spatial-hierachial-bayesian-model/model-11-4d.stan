data {
    int<lower=1> N_LOC;
    int<lower=0> Y[N_LOC];
}
parameters {
    real beta1;
    real<lower=0> s_r;
    vector[N_LOC] r;
}
transformed parameters {
    real<lower=0> lambda[N_LOC];
    for (i in 1:N_LOC) {
        lambda[i] = exp(beta1 + r[i]);
    }
}
transformed data {
    real sqrt6;
    sqrt6 = sqrt(6);
}
model {

    for (i in 3:(N_LOC-2)) {
        r[i] ~ normal((r[i-2]-4*r[i-1]-4*r[i+1]+r[i+2])/6, s_r/sqrt6);
    }
    for (j in 1:N_LOC) {
        Y[j] ~ poisson(lambda[j]);
    }
}
