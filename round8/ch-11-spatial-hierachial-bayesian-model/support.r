library(gridExtra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tufte)
library(tidyverse)
library(rstan)
library(RcppEigen)
library(readr)
library(ggmcmc)

if(file.exists("Y.RData")) load("Y.RData")



## 11章で考えるデータ
plot(Y, xlab="位置", ylab="個体数", ylim=c(0, 25))
abline(h =mean(Y), lty=2, lwd=5, col="skyblue")

plot(Y, xlab="位置", ylab="個体数", ylim=c(0, 25))
points(Y, type="l", col="skyblue", lwd=3, lty=2)




index_y_obs <- c (1:5, 7, 10:25, 31:50)
index_y_na  <- setdiff(1:50, index_y_obs)
dat <- list (
    N_OBS       = length (index_y_obs), 
    Y_OBS       = Y[index_y_obs], 
    INDEX_Y_OBS = index_y_obs, 
    N_NA        = length (index_y_na), 
    INDEX_Y_NA  = index_y_na
)

stan_obj_11   <- read_rds("model-11-with-na-sp.rds")
stan_obj_10   <- read_rds("model-11-with-na.rds")
stan_fits     <- 
    list (stan_obj_11, stan_obj_10) %>%
    map(function(obj) {
        stan_arg <- list(object=obj, iter = 2000, data=dat, warmup=1000, chain=1, thin=10)
        stan_fit <- lift(sampling)(stan_arg)
        stan_fit
    })
stan_summarys <- 
    stan_fits %>%
    map(function(fit){
        ggs(fit) %>%
            filter (grepl("lambda\\[[0-9]+\\]", Parameter)) %>%
            mutate (x = as.integer(gsub ("[^0-9]", "", Parameter))) %>%
            group_by (x) %>%
            summarise(
                q05 = quantile (value, .1), 
                q50 = quantile (value, .5), 
                q95 = quantile (value, .9)
            ) %>%
            ungroup()
    })
graph_base <- 
    ggplot() + 
    geom_point(data=data_frame(x = seq_along(Y), y = Y), aes(x, y), shape=1, size=2) +
    geom_point(data=data_frame(x = index_y_na,   y = Y[index_y_na]), aes(x, y), color="black", size=2)+
    labs(x = " j ", y = "Y[ j ]") +
    theme_classic()
map2 (stan_summarys, c ("red", "blue"), function(ggs, cl) {
        list (
            geom_ribbon(data=ggs, mapping=aes(x=x, ymin=q05, ymax=q95), fill=cl, alpha=.15),
            geom_line (data=ggs, mapping=aes(x=x, y=q50), color=cl, alpha=.5, size=3))
    }) %>%
    flatten() %>%
    Reduce("+", x=., init=graph_base)




dat     <- list (
    N_OBS = length(Y), # 場所数
    Y_OBS = Y              # 観測値
)

stan_fit <- stan_model (file="model-10.stan")
samples  <- sampling(stan_fit, iter=2000, data=dat, warmup=1000)
gmcmc1 <- ggs(samples)
gsum1  <- 
    gmcmc1 %>%
    filter (grepl ("lambda", Parameter)) %>%
    mutate (
        x = as.integer (gsub("[^0-9]", "", Parameter))
    ) %>%
    group_by (x) %>%
    summarise (
        "q10" = quantile (value, .05), 
        "q50" = quantile (value, .5),
        "q90" = quantile (value, .95)
    ) %>%
    ungroup()
plot(Y, xlab="位置", ylab="個体数", ylim=c(0, 25))
points(gsum1$q50, type="l", col="skyblue", lwd=3, lty=2)



