# Chapter 5
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

# Chapter5 GLMの尤度比検定と検定の非対称性

***

ここでは`尤度比検定`に着目する. 
尤度比検定では逸脱度の差で, ネストしたモデルを検定できる. 

検定とモデル選択の枠組みは異なるが, 
最尤法でパラメータを推定するところまでは一緒である. 


## 尤度比検定の例題：逸脱度の差を調べる


なぜ逸脱度を使うかというと, 逸脱度では対数尤度を使う, 
つまり尤度に直したときには比として計算できるため. 

$$
\Delta D_{1,2}=-2(\log L_1^* - \log L_2^*)
$$


尤度比検定では帰無仮説においたモデルから, 対象の
逸脱度をみることでその差がデータのバラつきから
生じるものなのかそれとも仮説が間違っているのかを考える. 

検定なのでタイプIエラーとタイプ２エラーがあるが, 
多くの場合には検出力検定, つまりどの程度の差を検出するかを調整しないので, 
タイプ１エラーに着目した検定となる。
このようにタイプ１しか気にしないことを指して`検定の非対称性`と呼んでいる. 



あまり, 詳しいことは書かれていないけど, 
とりあえずパラメトリックブートストラップ法による
逸脱度の差の分布を計算してみる.



```r
generate_sample <- (function() {
    LAMBDA   <- 2
    NSIZE    <- 50
    random_x <- rnorm (NSIZE)
    function () {
        data_frame (
            y = rpois (NSIZE, LAMBDA), 
            x = random_x
        )       
    }
})()
formulas <- 
    list (
        'x'   = "y ~ x",
        'fix' = "y ~ 1" 
    )
a_sim <- function (dummy) {
    obs             <- generate_sample()
    diff_deviance  <- 
        formulas %>%
        lapply(glm, data=obs, family=poisson) %>%
        lapply(deviance) %>%
        flatten_dbl %>%
        diff
    diff_deviance
}
NSIM <- 500
res  <- 
    lapply (1:NSIM, a_sim) %>%
    flatten_dbl()
data_frame (diff_d = res) %>%
    ggplot(aes(diff_d)) + 
    geom_histogram(colour = "white", fill="grey") + 
    geom_vline (xintercept=quantile(res, c (.05, .50, .95)))+
    theme_bw()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](ch5-static-test_files/figure-html/unnamed-chunk-2-1.png)<!-- -->









