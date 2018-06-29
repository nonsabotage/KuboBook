# Chapter 4
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

# Chapter4 GLMのモデル選択-AICとモデルの予測の良さ-

***

> 欲しいのは「あてはまりが良い」モデルではなく, 
> 「予測が良い」モデルである


## 4.2 あてはまりの悪さ「逸脱度」

次式で定義されている. 最大対数尤度の-2倍である. 

$$
D = -2 \log L^*

$$

ここで様々なパターンの逸脱度を定義しておこう. 

- 逸脱度($D$)
    - $-2 \log L^*$
- 最小の逸脱度
    - フルモデルをあてはめたときの逸脱度
- 残差逸脱度
    - $D$-最小の逸脱度
- 最大の逸脱度
    - NULLモデルを当てはめたときのD
- NULL逸脱度
    - 最大の逸脱度-最小の逸脱度
    - NULLモデルのときの残差逸脱度

## なぜAICでモデル選択をしてよいか

モデル選択は次式のAICでおこなわれる. $k$はパラメータ数である. 

$$
AIC = - 2 \log L^* + 2 k
$$

AICによるモデル選択がなぜ有効なのかを理解するには
次の２つを考える必要がある.

- `平均対数尤度`
- 最大対数尤度の`バイアス補正`

観測データに対して最も当てはまりが良くなるが最尤パラメータである. 
では, 最尤パラメータの**予測の良さ**を知る方法はないのか?
ここでは「予測の良さ」を観測データがまた取得できたとして, 
どの程度の尤度になるのかで測る. 
そして, いくつもえた尤度の平均値が`平均対数尤度`となる. 

平均対数尤度を次式で書く. 

$$
E(\log L) = \log L^* -b
$$


## 平均対数尤度と最大対数尤度の差

それでは前述のものをシミュレーションで確認してみる.
モデルは次のNULLモデルを考える. 


$$
y \tilde \\Poisson(\lambda) \\
\log \lambda = \beta
$$



```r
## ポアソン回帰のNULLのモデルで試す
## 最大対数尤度と平均対数尤度のバイアス
generate_sample <- (function() {
    NSIZE  <- 50   # 観測データのサイズ
    LAMBDA <- 8    # パラメータの真の値
    function () {
        rpois(NSIZE, lambda = LAMBDA)
    }
})()
poisson_loglik <- function(y, lambda) {
    sum (dpois(y, lambda, log=TRUE))
}
NTRIAL <- 200  
a_sim  <- function (dummy) {
    sample_y    <- generate_sample()
    poisson_fit <- glm (y ~ 1, data=data_frame(y = sample_y), family=poisson)
    maxloglik   <- logLik(poisson_fit)
    ml_beta     <- coef(poisson_fit)
    logliks     <- 
        lapply (1:NTRIAL, function(i, lambda) {
            poisson_loglik (
                generate_sample(), 
                lambda
            )
        }, lambda = exp (ml_beta)) %>%
        flatten_dbl
    meanloglik <- logliks %>% mean
    return (
        tibble (
            beta       = ml_beta,
            maxloglik  = maxloglik,
            logliks    = list(logliks), 
            meanloglik = meanloglik
        )
    )
}
NSIM <- 10
res  <- 
    lapply (1:NSIM, a_sim) %>%
    bind_rows()
graph_base <- 
    res %>%
    dplyr::select (beta, logliks) %>%
    unnest %>%
    ggplot(aes(beta, logliks)) + 
    geom_point(shape=45) +
    theme_bw() + 
    ylab ("likelihood")
stat_point <- 
    res %>%
    dplyr::select (beta, maxloglik, meanloglik) %>%
    gather (type, value, -beta)
graph_base + 
    geom_point (data = stat_point, aes(beta,value, colour=type), size=3) +
    scale_color_aaas() + 
    theme_bw() + 
    ylab ("likelihood")
```

![](ch4-aic_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## 平均対数尤度と最大対数尤度の差の分布は?

パラメータ数を平均値として分布している. 


```r
## ポアソン回帰のNULLのモデルで試す
## 最大対数尤度と平均対数尤度のバイアス
generate_sample <- (function() {
    NSIZE  <- 50   # 観測データのサイズ
    LAMBDA <- 8    # パラメータの真の値
    function () {
        rpois(NSIZE, lambda = LAMBDA)
    }
})()
poisson_loglik <- function(y, lambda) {
    sum (dpois(y, lambda, log=TRUE))
}
NTRIAL <- 200  
a_sim  <- function (dummy) {
    sample_y    <- generate_sample()
    poisson_fit <- glm (y ~ 1, data=data_frame(y = sample_y), family=poisson)
    maxloglik   <- logLik(poisson_fit) %>% as.numeric
    ml_beta     <- coef(poisson_fit)
    meanloglik  <- 
        lapply (1:NTRIAL, function(i, lambda) {
            poisson_loglik (
                generate_sample(), 
                lambda
            )
        }, lambda = exp (ml_beta)) %>%
        flatten_dbl %>% 
        mean
    return (maxloglik-meanloglik)
}
NSIM  <- 300
res   <- 
    lapply (1:NSIM, a_sim) %>%
    flatten_dbl
summary (res)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -13.8300  -2.1050   0.9343   0.9858   4.3410  14.6800
```

```r
bias  <- data_frame (b = res)
bias %>%
    ggplot (aes(b)) + 
    geom_density() + 
    theme_bw() +
    geom_vline(xintercept = mean(bias$b), colour="red")
```

![](ch4-aic_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


## ネストしている場合のAICの比較

ネストしている場合のモデル式を比較することで, 
無意味な説明変数が追加された場合にAIC, 
つまり最大対数尤度と平均対数尤度がどのような挙動を示し, 
それがパラメータとどのような関係があるかを考える. 

結果としては, 応答変数と無関係な
説明変数を追加することで最大対数尤度が0.5上昇するが
平均対数尤度が0.5低下する. つまり, パラメータ数だけ上昇する. 

グラフを見ると最大対数尤度は向上しているのがわかるが, 
平均対数尤度が低下していることがわかる. 



```r
NSIZE  <- 50  
LAMBDA <- 8    
generate_sample <- (function() {
    function () {
        rpois(NSIZE, lambda = LAMBDA)
    }
})()
poisson_loglik <- function(y, lambda) {
    sum (dpois(y, lambda, log=TRUE))
}
NTRIAL   <- 200
random_x <- rnorm(NSIZE)
a_sim  <- function (dummy) {
    obs <- data_frame (
        y = generate_sample(), 
        x = random_x
    )
    fits <- 
        list (
            "fix" = "y ~ 1",
            "x"   = "y ~ x"
        ) %>%
        lapply (function(f) {
            glm (f, data = obs, family=poisson)
        })
    maxlogliks <- lapply (fits, function(x) as.numeric(logLik(x))) 
    lambda     <- 
        fits %>%
        lapply (augment) %>%
        lapply (function(x) {
            exp (x$.fitted)
        })
    meanlogliks <- 
        lapply (1:NTRIAL, function(i, lambda) {
            y <- generate_sample()
            c (
                'fix' = poisson_loglik (y, lambda$fix), 
                'x'   = poisson_loglik (y, lambda$x)
            )
        }, lambda) %>%
        transpose() %>%
        lapply (flatten_dbl) %>%
        lapply (mean)
    
    data_frame (
        diff_maxlogliks  = maxlogliks$x  - maxlogliks$fix, 
        diff_meanlogliks = meanlogliks$x - meanlogliks$fix,
        diff_bias        = 
            c(maxlogliks$x-meanlogliks$x) - c(maxlogliks$fix-meanlogliks$fix)
    )
}
NSIM <- 300
res  <- 
    lapply (1:NSIM, a_sim) %>%
    bind_rows() 
res %>%
    gather (type, value) %>%
    ggplot(aes(value)) +
    geom_histogram(aes(fill=type)) + 
    scale_color_aaas() + 
    facet_grid(type ~ .) + 
    theme_bw()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](ch4-aic_files/figure-html/unnamed-chunk-4-1.png)<!-- -->





