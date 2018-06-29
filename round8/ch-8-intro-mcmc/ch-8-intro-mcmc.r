rm(list=ls(all=TRUE))

libs <- c ("purrr", "readr", "dplyr", "magrittr", "tidyr", "ggplot2", "gridExtra")
for(lib in libs) {
    if (!require(lib, character.only=TRUE)) {
        install.packages(lib, repos="http://cran.ism.ac.jp")
        require(lib, character.only=TRUE)}
}

### 8.3 MCMCアルゴリズムのひとつ：メトロポリス法
compute_lglike <- (function() {
    N          <- 8L
    survived   <- scan("data.txt", what=integer())
    function (survived_prob) {
        sum (
            dbinom (
                x    = survived,
                size = N,
                prob = survived_prob,
                log  = TRUE))
    }
})()
initial_q_prob   <- function () {
    0.4
}
sampling <- function (q_prob) {
    max(0.01, min(0.99, sample(c(0.01,-0.01), 1) + q_prob))
}
sim_metropolis <- function (step_size) {
    sampling_holder    <- numeric(step_size)
    sampling_holder[1] <- initial_q_prob()
    cur_lglike         <- compute_lglike(sampling_holder[1])
    if (step_size <= 1) {
        return (sampling_holder)}
    for (step in 2:step_size) {
        cand_q_prob  <- sampling(sampling_holder[step-1])
        cand_lglike  <- compute_lglike(cand_q_prob)
        if (cur_lglike <= cand_lglike || runif(1) <= exp (cand_lglike - cur_lglike)) {
            sampling_holder[step] <- cand_q_prob
            cur_lglike <- cand_lglike
        }
        else {
            sampling_holder[step] <- sampling_holder[step-1]
        }
    }
    return (sampling_holder)
}

ret <- data_frame (
        v    = sim_metropolis(1e4),
        step = seq_along(v))

mcmc_path <-
    ret %>%
    ggplot(aes(step, v)) +
    geom_line() +
    ylim(c(0.2, 0.7))+
    theme_minimal()
mcmc_hist <-
    ret %>%
    ggplot(aes(v)) +
    geom_histogram(binwidth=.02, fill="lightblue", color="darkgrey") +
    coord_flip() +
    xlim(c(0.2, 0.7)) +
    theme_minimal()
mcmc_graph <- grid.arrange (
    mcmc_path,
    mcmc_hist,
    layout_matrix=matrix(c(1,1,1,2),nrow=1)
)
ggsave("sim_metropolis.pdf", plot=mcmc_graph, width=297, height=210, units="mm")


