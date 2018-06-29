data {
    int<lower=0> SIZE;
    int<lower=0> OBS[SIZE];
    real X[SIZE];
    real XMEAN;
}
parameters {
    real beta1;
    real beta2;
}
transformed parameters {
    real<lower=0> lambda[SIZE];
    for (i in 1:SIZE) {
        lambda[i] = exp(beta1 + beta2 * (X[i]-XMEAN));}
}
model {
    for (i in 1:SIZE) {
        OBS[i] ~ poisson(lambda[i]);}
    beta1 ~ normal(0, 100);
    beta2 ~ normal(0, 100);
}
