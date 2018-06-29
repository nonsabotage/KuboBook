# Chapter 6
shinya suzuki  
2016年11月22日  




```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## 
## Attaching package: 'purrr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     contains, order_by
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

# Chapter6 GLMの応用範囲を広げる

***

GLMの３つの設定を再確認

- 応答変数の確率分布
    - 二項分布
    - ロジスティック分布
    - ガンマ分布
- 線形予測子(説明変数の組み合わせ)
    - 交互作用
    - オフセット項
- リンク関数(応答変数の確率分布と, 線形予測子の関係)
    - 対数関数

６章では次の３つに着目

- 二項分布ロジスティック回帰
- オフセット項
- ガンマ回帰


## 6.2 上限のあるカウントデータ

上限のあるカウントデータの場合には, 二項分布が一般的である.

まずは, データをプロットして傾向を確認する.
グラフから, xによって生存種子数は増えそうで, 肥料有りでも
生存種子数が増えそうなことがわかる.


```r
path   <- "../../最新版/kubobook2012/chapter06/binomial/data4a.csv"
obs    <- read_csv (path)
```

```
## Parsed with column specification:
## cols(
##   N = col_integer(),
##   y = col_integer(),
##   x = col_double(),
##   f = col_character()
## )
```

```r
obs %>%
    ggplot(aes(x, y)) +
    geom_jitter(aes(colour=f), width=1., height=.1, size=2)+
    scale_color_aaas() +
    theme_bw()
```

![](ch6-glm_files/figure-html/unnamed-chunk-2-1.png)<!-- -->



##6.3 二項分布で表現する「あり・なし」カウントデータ

２項分布の確率分布の式は次のとおり.


$$
p(y | N, q) = \binom{N}{y}q^y(1-q)^{N-y}
$$

$N=8$の場合(今回の問題設定)の, 二項分布の形状をいくつか見てみる.



```r
size <- 8
ps   <- seq(.1, .9, .2)
D    <- lapply (ps, function (p) {
    d <- data_frame (
        y = 0:size
        , dens = dbinom(y, size, p)
        , type = paste0("prob=", p)
    )
    d
}) %>%
    bind_rows()
D %>%
    ggplot(aes(y, dens, group=type, colour=type), size=2) +
    geom_line() +
    geom_point() +
    scale_color_aaas()+
    theme_bw()
```

![](ch6-glm_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



## ロジスティック回帰とロジットリンク関数

ここではロジットリンク関数を用いる. ロジットリンク関数は
$q$について解くと, 線形予測子を引数としたロジスティック分布関数になる.

$$
\textrm{logit}(q) = \log \frac{q}{1-q} = \sum\beta_ix_i
$$
$$
q=\frac{1}{1+\exp(-\sum\beta_ix_i)}
$$

ロジスティック関数の挙動を確認する.
まず, 単なるロジスティック関数について. 


```r
logistic <- function(x) {1 / (1 + exp (-x))}
stat_fun <- stat_function (fun=logistic, colour="blue")

data_frame(x = c(-6, 6)) %>%
ggplot() + 
    stat_fun + 
    theme_bw() + 
    xlab("線形予測子") + 
    ylab("q") + 
    xlim(c (-6, 6)) + 
    ylim(c (0, 1))
```

![](ch6-glm_files/figure-html/unnamed-chunk-4-1.png)<!-- -->




定数項の上下は関数系のシフトになる.



```r
generator_logit <- function (beta1) {
    function (x) {
        linpred <- beta1 + 2 * x
        return (1 / (1 + exp (-linpred)))
    }
}
logits <- lapply (c(-3, 0, 2), generator_logit) %>%
          lapply (function(x) stat_function(fun=x, colour="blue"))
base   <- data_frame(
    x = c(-3, 3)
) %>%
    ggplot(aes(x)) +
    theme_bw() +
    xlab("z")
Reduce("+", logits, init = base)
```

![](ch6-glm_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


$x$の係数は関数の上がり方に効果がある.


```r
generator_logit <- function (beta2) {
    function (x) {
        linpred <- beta2 * x
        return (1 / (1 + exp (-linpred)))
    }
}
logits <- lapply (c(-1, 2, 4), generator_logit) %>%
          lapply (function(x) stat_function(fun=x, colour="blue"))
base   <- data_frame(
    x = c(-3, 3)
) %>%
    ggplot(aes(x)) +
    theme_bw() +
    xlab("z")
Reduce("+", logits, init = base)
```

![](ch6-glm_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


### Rでパラメータを推定

ここまでで, 応答変数の確率分布と線形予測子, そしてリンク関数を設定・確認できたので
後はパラメータを推定すればよいだけである.

パラメータは尤度関数を設定することから始まる.
この尤度関数をみると$N$は一定でなくてもよいようだ.

$$
L(\{\beta_i\}) = \Pi_i\binom{N_i}{y_i}q_i^{y_i}(1-q_i)^{N_i-y_i}
$$

対数尤度関数は次となる.

$$
\log L(\{\beta_i\}) = \sum_i\{\log \binom{N_i}{y_i} + y_i\log q_i + (N_i-y_i)\log (1-q_i)\}
$$


パラメータ推定はあっさりとできる.


```r
fit <- glm (cbind(y, N-y) ~ x + f, data = obs, family = binomial)
```

推定結果を回帰結果に重ねたグラフを描く.


```r
d        <- data_frame(x=seq(7, 12, by=.2), f = "C")
glm_pred <- 8 * predict (
    fit
    , newdata = d
    , type="response"
)
d$y      <- glm_pred
d %>%
    ggplot(aes(x, y)) +
    geom_line() +
    geom_point(data=filter(obs, f == "C"), aes(x, y), size=2, shape=19) +
    theme_bw()
```

![](ch6-glm_files/figure-html/unnamed-chunk-8-1.png)<!-- -->



```r
d        <- data_frame(x=seq(7, 12, by=.2), f = "T")
glm_pred <- 8 * predict (
    fit
    , newdata = d
    , type="response"
)
d$y      <- glm_pred
d %>%
    ggplot(aes(x, y)) +
    geom_line() +
    geom_point(data=filter(obs, f == "T"), aes(x, y), size=2, shape=19) +
    theme_bw()
```

![](ch6-glm_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


### モデル選択

緑本では`MASS::stepAIC`で実行しているが, ここでは`broom`の練習も兼ねて
自力で実行してみる.
ここでの`deviance`として出力されるのは残差逸脱度である. 



```r
## full model以外の検討
models <- 
    tibble(
        name    = c ("fixed", "x", "f", "x+f", "x*f"), 
        formula = c (
            "cbind(y, N-y) ~ 1", 
            "cbind(y, N-y) ~ x",
            "cbind(y, N-y) ~ f",
            "cbind(y, N-y) ~ x+f",
            "cbind(y, N-y) ~ x+f+x:f"
        )
    ) %>%
    group_by (name, formula) %>%
    do (
        fit = glm(.$formula, data=obs, family=binomial)
    )
## ungroup をしてしまうと期待どおりの動作にならないことに注意
evaluation_models <-
    models %>% 
    glance (fit)

## full model
evaluation_full_model <- tibble (
    name    = "full", 
    formula = "y/N",
    null.deviance = NA,
    df.null = NA,
    deviance = NA,
    df.residual =  NA,
    logLik  = sum (with (obs, {dbinom(y, N, y/N, log=TRUE)}))
) %>%
    mutate (
        AIC = -2 * logLik + 2 * 100, 
        BIC = -2 * logLik + 100 * log(100)
    )

res <- 
    bind_rows(
        evaluation_models,
        evaluation_full_model
    )
knitr::kable(res)
```



name    formula                    null.deviance   df.null       logLik        AIC        BIC   deviance   df.residual
------  ------------------------  --------------  --------  -----------  ---------  ---------  ---------  ------------
f       cbind(y, N-y) ~ f               499.2321        99   -316.87988   637.7598   642.9701   490.5825            98
fixed   cbind(y, N-y) ~ 1               499.2321        99   -321.20467   644.4093   647.0145   499.2321            99
x       cbind(y, N-y) ~ x               499.2321        99   -180.17272   364.3454   369.5558   217.1682            98
x*f     cbind(y, N-y) ~ x+f+x:f         499.2321        99   -132.80530   273.6106   284.0313   122.4334            96
x+f     cbind(y, N-y) ~ x+f             499.2321        99   -133.10556   272.2111   280.0266   123.0339            97
full    y/N                                   NA        NA    -71.58862   343.1772   603.6943         NA            NA

## 6.5 交互作用項が入った線形予測子



交互作用項を考える. 交互作用項を考えることで, 
$x$の単位変化量が変わってくる.
交互作用項を入れることで複雑なモデルを考えることができるが, 
どの説明変数を用いるかという**組み合わせ爆発**が起きるので
むやみやたらに入れないことが重要. 

また, 適当に入れてもAICがよくなることが
多くあるので気をつけること. 


## 6.6 割算値のモデリングはやめよう


ポアソン回帰を例に考える. 

- ポアソン分布
- 対数リンク関数
- オフセット項を入れた線形予測子

考えるモデルは次. $\log(A_i)$に係数が聞いていないのがポイント. 

$$
\lambda_i = \exp(\beta_1 + \beta_2 x_i + \log(A_i))
$$

まずは, データを表示してみる. 


```r
path <- "../../最新版/kubobook2012/chapter06/binomial/data4b.csv"
obs  <- read_csv (path)
```

```
## Parsed with column specification:
## cols(
##   y = col_integer(),
##   x = col_double(),
##   A = col_double()
## )
```

```r
obs_graph <- 
    obs %>%
    mutate(x = cut(x, breaks=seq(0, 1, .2), include.lowest=TRUE)) %>%
    ggplot(aes(A, y)) +
    geom_point (aes(colour=x), size=2)+
    scale_color_aaas() +
    theme_bw() + 
    xlim (c(0, 20)) + 
    ylim (c(0, 100))
obs_graph
```

![](ch6-glm_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


Rでパラメータ推定をしてみる.


```r
fit <- glm (y ~ x, offset = log(A), family=poisson, data=obs)
```
 
推定結果をグラフに載せる. 


```r
xs     <- seq (.1, .9, .2)
slopes <- lapply (xs, function (x) {
    ests <- tidy(fit)$estimate
    exp (ests[1] + ests[2] * x)
})
ablines <- lapply(slopes, function(s) {
    geom_abline(intercept=0, slope=s)
})
Reduce ("+", ablines, init=obs_graph)
```

![](ch6-glm_files/figure-html/unnamed-chunk-13-1.png)<!-- -->



## 6.7 正規分布とその尤度


連続関数での尤度関数. 

- 確率密度で作成する場合, 確率で作成する場合がある
- 確率密度の場合には対数尤度が負の値とは限らない
- 標準偏差$\sigma$を固定した場合には最小二乗法と一致する



```r
## 正規分布の形状を確認する
## ポイントは平均値と分散が独立で, 平均値から左右対象
## 分散を1, 2, 3, 4, 5としている
parameters <- 
    data_frame(
        mean = rep(c(0), each=5), 
        sd   = sqrt(rep(1:5, each=1))
    ) %>% 
    transpose()
stat_funs <- 
    lapply(parameters, function(f) stat_function(fun=dnorm, args=f))
stat_funs2 <- 
    lapply(parameters, function(x) {x$mean <- x$mean + 10; x}) %>%
    lapply(function(f) stat_function(fun=dnorm, args=f))
graph <- ggplot(data_frame(x=c(-20,20)), aes(x)) + theme_bw()
grid.arrange(
  Reduce ("+", stat_funs,  init=graph) + xlim(c(-5,5)), 
  Reduce ("+", stat_funs2, init=graph) + xlim(c(5,15))
)
```

![](ch6-glm_files/figure-html/unnamed-chunk-14-1.png)<!-- -->




## 6.8 ガンマ分布でGLM


データを確認する


```r
path <- "../../最新版/kubobook2012/chapter06/gamma/kafun.csv"
obs  <- read_csv (path)
```

```
## Parsed with column specification:
## cols(
##   x = col_double(),
##   y = col_double()
## )
```

```r
obs_graph <- 
    ggplot() + 
    geom_point(data=obs, aes(x,y), size=2, colour="blue") + 
    theme_bw()+
    xlim(c(0, .82)) + 
    ylim(c(0, .72))
obs_graph
```

![](ch6-glm_files/figure-html/unnamed-chunk-15-1.png)<!-- -->




ガンマ分布の形状を確認する



```r
parameters <- 
    data_frame(
        mean   = rep(c(5, 10), each=5), 
        sigma2 = rep(1:5, times=2)
    ) %>% 
    mutate (
        rate  = mean / sigma2, 
        shape = mean * rate 
    ) %>%
    transpose()
generate_stat_funs <- function(m) {
    lapply(
        keep(parameters, function(x) x$mean == m), 
        function(f) {
            args <- f[c ("rate", "shape")]
            stat_function(fun=dgamma, args=args)
        })    
}
graph <- ggplot(data_frame(x=c(0,20)), aes(x)) + theme_bw()
grid.arrange(
  Reduce ("+", generate_stat_funs(5),  init=graph) + xlim(c(0,10)), 
  Reduce ("+", generate_stat_funs(10), init=graph) + xlim(c(5,15))
)
```

![](ch6-glm_files/figure-html/unnamed-chunk-16-1.png)<!-- -->



Rでなら細かいことを考えることなく, パラメータを推定することが可能となる.
... といいつつ, 平均値が決まるだけで$alhpa$や$beta$を推定しているわけではない?

予測分布を計算する**dispersion**パラメータというものから可能であるらしいが, 
詳しいことはよくわからない.... 久保先生[HP](http://hosho.ees.hokudai.ac.jp/~kubo/ce/GlmGamma.html)を参照した


```r
glmfit      <- glm (y ~ log(x), family = Gamma(link="log"), data=obs)
phi         <- summary(glmfit)$dispersion
percent_fun <- function(x, p) {
    d <- data_frame(x = x)
    v <- predict(glmfit, d, type="response")
    rate  <- 1 / v / phi
    shape <- v * rate
    qgamma(p, shape= shape, rate =rate)
}
pred_area   <- 
    data_frame(
        x = c (.01, 1:8/10)
    ) %>% 
    mutate (
        p95 = percent_fun(x, .95), 
        p75 = percent_fun(x, .75),
        p50 = percent_fun(x, .50),
        p25 = percent_fun(x, .25), 
        p05 = percent_fun(x, .05)
    ) %>%
    gather (percent, value, -x)
obs_graph + 
    geom_line(data = pred_area, aes(x, value, group = percent, colour=percent)) +
    scale_color_aaas()
```

![](ch6-glm_files/figure-html/unnamed-chunk-17-1.png)<!-- -->



