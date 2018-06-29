rm(list=ls(all=TRUE))

libs <- c ("rstan", "purrr", "readr", "dplyr", "magrittr", "tidyr", "ggplot2", "gridExtra")
for(lib in libs) {
    if (!require(lib, character.only=TRUE)) {
        install.packages(lib, repos="http://cran.ism.ac.jp")
        require(lib, character.only=TRUE)
    }
}

### 9章の目的
# - 線形予測子を持つ統計モデルのベイズ化
# - 複数のパラメーターの事後分布からのMCMCサンプリング
obs <- read_csv ("ch-9.csv")
dat <- list (
        SIZE  = as.integer(nrow(obs)),
        OBS   = as.integer(obs$y),
        X     = obs$x,
        XMEAN = mean(obs$x)
    )
compiled_model <- stan_model(file="stan_model.stan")
ret            <- sampling(compiled_model, iter=1000, data=dat, warmup=100)

beta1 <- as_data_frame (
    rstan::extract(ret, "beta1", FALSE)) %>%
    mutate (x = 1:nrow(.)) %>%
    gather(chain, value, -x)
beta2 <- as_data_frame (
    rstan::extract(ret, "beta2", FALSE)) %>%
    mutate (x = 1:nrow(.)) %>%
    gather(chain, value, -x)

beta1_trace <-
    beta1 %>%
    ggplot(aes(x, value, group=chain, color=chain)) +
    geom_path() +
    ylab("beta1") +
    theme_minimal()
beta2_trace <-
    beta2 %>%
    ggplot(aes(x, value, group=chain, color=chain)) +
    geom_path() +
    ylab("beta2") +
    theme_minimal()

mcmc_graph <- grid.arrange (
    beta1_trace,
    beta2_trace,
    nrow=2
)
ggsave("trace_beta.pdf", plot=mcmc_graph, width=297, height=210, units="mm")








beta1_dens <-
    beta1 %>%
    ggplot(aes(value, group=chain, color=chain)) +
    geom_density()+
    theme_minimal()