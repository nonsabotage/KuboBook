rm(list=ls(all=TRUE))

libs <- c ("rstan", "purrr", "readr", "dplyr", "magrittr", "tidyr", "ggplot2", "gridExtra")
for(lib in libs) {
    if (!require(lib, character.only=TRUE)) {
        install.packages(lib, repos="http://cran.ism.ac.jp")
        require(lib, character.only=TRUE)
    }
}

### 10章 階層ポアソンモデル
obs     <- read_csv ("d1.csv")
pot2int <- set_names(seq_along(unique(obs$pot)), unique(obs$pot))
f2int   <- set_names(seq_along(unique(obs$f))-1,   unique(obs$f))
dat     <- list (
    SIZE = nrow(obs),
    ID   = obs$id,
    POT  = pot2int[obs$pot],
    POT_SIZE = length(pot2int),
    F    = f2int[obs$f],
    Y    = obs$y
)
compiled_model <- stan_model(file="stan_model_b.stan")
ret            <- sampling(compiled_model, iter=1000, data=dat, warmup=100)


