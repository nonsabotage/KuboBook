data {
    int SIZE;
    int ID[SIZE];
    int POT[SIZE];
    int POT_SIZE;
    int F[SIZE];
    int Y[SIZE];
}
parameters {
    real beta1;
    real beta2;
    real r[SIZE];
    real rp[POT_SIZE];
    real<lower=0> s1;
    real<lower=0> s2;
}
transformed parameters {
    real lambda[SIZE];
    for (i in 1:SIZE) {
        lambda[i] = exp (beta1 + beta2 * F[i] + r[i] + rp[POT[i]]);
    }
}
model {

    for (i in 1:SIZE) {
        Y[i] ~ poisson(lambda[i]);
    }
    for (i in 1:SIZE) {
        r[i] ~ normal(0, s1);
    }
    for (i in 1:POT_SIZE) {
        rp[i] ~ normal(0, s2);
    }
    beta1 ~ normal(0, 100);
    beta2 ~ normal(0, 100);
    s1 ~ uniform(0, 10000);
    s2 ~ uniform(0, 10000);
}



