# Chapter7 一般化線形混合モデル(GLMM) 個体差のモデリング

libs <- c ("ggplot2", "dplyr", "tidyr", "purrr", "readr", "broom")
for (lib in libs) {
    library (lib, character.only = TRUE)
}


## 7.1 GLMでは説明できなカウントデータ
data   <- "../../最新版/kubobook2012/chapter07/data6b.csv"
obs    <- read_csv (data)

### fig 7.3.a
glmfit <- glm (cbind(y, N-y) ~ x, data = obs, family = binomial)
glmprd <- data_frame (x = seq (2, 6, length = 100)) %>% mutate (y = 8 * predict.glm(glmfit, ., type="response"))
true   <- data_frame (x = seq (2, 6, length = 100)) %>% mutate (y = 1 / (1 + exp (4 - x))) %>% mutate(y = y * 8)
p      <-
    ggplot (obs, aes(x, y)) +
    geom_jitter(width=.1, height=.1) +
    geom_line (data=glmprd, aes(x, y), size=2, colour="red") +
    geom_line (data=true, aes(x, y), size=2, linetype = "dashed", colour="blue") +
    theme_bw()
p
### fig 7.3.b
pred_dist = data_frame(x=0:8) %>% mutate (y = 20 * dbinom(x, size=8, prob=predict.glm(glmfit, data_frame(x=4), type="response")))
p <-
    obs %>%
    filter (x == 4) %>%
    group_by (y) %>%
    summarise(ctn = n()) %>%
    ggplot() +
        geom_point(aes(y, ctn), size=4, shape=21, fill="white", stroke=2) +
        geom_line(data=pred_dist, aes(x, y)) +
        theme_bw() +
        ylim (c(0, 6)) +
        xlim (c(0, 8))
p


## 7.2 過分散と個体差

# 観測データはすべて均質であるという仮定により個体差を無視して応答変数の確率分布を当てはめることで過分散が生じている.
# これはいわゆる観察データだけでなく制御された実験においても生じる問題で環境などに左右される問題を指す.
# 観測されない要因がなんなのかを特定することは不可能である


## 7.3 一般化線形混合モデル
# 個体差を考慮したロジスティック回帰モデルへ

### fig 7.3.b
logit  <- function(r) { function (x) {1 / (1 + exp (4-x+r))}}
target <- data_frame (x = seq(2, 6, length.out=10))
sims   <-
    lapply(seq (-7, 7, by = 1), function (r) {
        tmp_logit <- logit(r)
        target %>% mutate (y = tmp_logit(x), r = r)
    }) %>%
    lapply (function(d) {
        geom_line (data=d, aes(x, y, colour=r), size=1.5)
    })
p <-
    ggplot() +
        xlim(c(2, 6)) +
        ylim(c(0, 1)) +
        theme_bw()
Reduce ("+", sims, init=p)


## 7.4 一般化線形混合モデルの最尤推定
re  <- function (s) {rnorm(1, mean=0, sd=s)}
p   <- function (r) {1 / (1 + exp (-r))}
sim <- function (s) {
    N    <- 50
    SIZE <- 8
    out  <-
        data_frame (
            y = lapply (1:N, function(x) {
                    rbinom(1, SIZE, p(re(s)))
                }) %>%
                flatten_int()
        )
    mean_prob <- out$y %>% "/"(SIZE) %>% mean
    sim_out   <- out %>% group_by (y) %>% summarise(ctn = n())
    list (
        N    = N,
        SIZE = SIZE,
        mp   = mean_prob,
        so   = sim_out
    )
}
#### fig 7.6.a
rst       <- sim(.5)
binom_fit <- data_frame(x = 0:8) %>% mutate(y = rst$N * dbinom(x, rst$SIZE, rst$mp))
binom_fit %>%
    ggplot(aes(x, y)) +
    geom_point(size=2) +
    geom_line() +
    geom_point(data=rst$so, aes(y, ctn), size=4, shape=21, fill="white", stroke=2) +
    theme_bw() +
    xlim(c (0,8))
#### fig 7.6.b
rst       <- sim(3)
binom_fit <- data_frame(x = 0:8) %>% mutate(y = rst$N * dbinom(x, rst$SIZE, rst$mp))
binom_fit %>%
    ggplot(aes(x, y)) +
    geom_point(size=2) +
    geom_line() +
    geom_point(data=rst$so, aes(y, ctn), size=4, shape=21, fill="white", stroke=2) +
    theme_bw() +
    xlim(c (0,8))

## glmmMLによる推定　
glmm_fit <- glmmML::glmmML(cbind(y, N-y) ~ x, data=obs, family=binomial, cluster=id)
plot(glmm_fit)

### 無限混合分布の導出に挑戦
#### 負の対数尤度
NegativeLoglik <- (function () {
    obs <-
        readr::read_csv ("../../最新版/kubobook2012/chapter07/data6b.csv") %>%
        dplyr::select (N, x, y) %>%
        transpose()
    function (parameter) {
        beta1 <- parameter[1]
        beta2 <- parameter[2]
        s     <- parameter[3]
        ll    <-
            lapply(obs, function (o, b1, b2, s) {
                function (r) {
                    linpred <- b1 + b2 * o$x + r
                    prob    <- 1 / (1 + exp(- linpred))
                    dbinom(x=o$y, size=o$N, prob=prob) * dnorm(x=r, mean=0, sd=s)
                }
            }, b1=beta1, b2=beta2, s=s) %>%
            lapply(integrate, lower=-100, upper=100) %>%
            lapply("[[", 1) %>%
            flatten_dbl() %>%
            log %>%
            sum
        return (-ll)
    }
})()

#### 負の対数尤度の最小化
par_init <- c (1, 1, 1)
res_opt  <- optim (par_init, NegativeLoglik)

#### 演算結果を利用したシミュレーション
a_sim <- (function (op) {
    ## estimated paramter
    beta1 <- op[1]
    beta2 <- op[2]
    s     <- op[3]
    ## setting
    x      <- 4
    size   <- 8
    traial <- 100

    function () {
        rs         <- rnorm(traial, mean = 0, sd = s)
        logit_prob <- lapply (rs, function (r, b1, b2, x) {
            linpred <- b1 + b2 * x + r
            prob    <- 1 / (1 + exp (-linpred))
            prob
        }, beta1, beta2, x)
        res <-
            data_frame (
                y = lapply (logit_prob, function (p) {
                        rbinom(n=1,size=size,prob=p)
                    }) %>%
                    flatten_int
            ) %>%
            group_by (y) %>%
            summarise(ctn = n())
        res
    }
})(res_opt$par)
n_sim <- function (N) {
    lapply (1:N, function (x) {a_sim()}) %>%
        bind_rows %>%
        group_by (y) %>%
        summarise(
            cum10   = quantile (ctn, prob=.1)
            , cum50 = quantile (ctn, prob=.5)
            , cum90 = quantile (ctn, prob=.9)
        ) %>%
        ungroup()
}
res <- n_sim(100)
res %>%
    gather(cum, value, -y) %>%
    ggplot() +
    geom_line(aes(y, value, group=cum, colour=cum)) +
    geom_point(aes(y, value, group=cum), size=2) +
    theme_bw()






