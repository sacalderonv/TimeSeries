Outliers
================

## Outliers en Modelos ARIMA

Vamos a considerar el análisis de outliers para modelos ARIMA o SARIMA.

La base fundamental del análisis de outliers se basa en en análisis de
intervención. Vamos a empezar con unos ejercicios de simulación. La
librería base es tsoutliers.

``` r
library(tsoutliers)
```

    ## Registered S3 method overwritten by 'xts':
    ##   method     from
    ##   as.zoo.xts zoo

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## Registered S3 methods overwritten by 'forecast':
    ##   method             from    
    ##   fitted.fracdiff    fracdiff
    ##   residuals.fracdiff fracdiff

``` r
library(forecast)
###### Simulación Outlier aditivo

set.seed(12)
n=200
serie=arima.sim(n=n,list(ar=(0.5)))
serie2=serie
serie2[50]=serie2[50]+6

par(mfrow=c(1,2))
plot(serie,ylim=c(-3,6),main="Serie original")
plot(serie2,ylim=c(-3,6),main="Outlier aditivo")
```

![](Outliers_files/figure-gfm/simulación%20aditivo-1.png)<!-- -->

Después de simular un outlier aditivo en el tiempo \(t=50\) con impacto
de 6, basada en una serie que proviene de de un modelo AR con
\(\phi=0.5\).

En seguida vamos ajustar los modelos a las series con y sin outlier vía
el procedimiento automático.

``` r
auto.arima(serie2)###Ajuste para el modelo con outlier
```

    ## Series: serie2 
    ## ARIMA(1,0,1) with zero mean 
    ## 
    ## Coefficients:
    ##          ar1      ma1
    ##       0.6298  -0.2773
    ## s.e.  0.1234   0.1511
    ## 
    ## sigma^2 estimated as 1.119:  log likelihood=-294.12
    ## AIC=594.24   AICc=594.36   BIC=604.13

``` r
auto.arima(serie)###Ajuste para el modelo sin outlier
```

    ## Series: serie 
    ## ARIMA(1,0,0) with zero mean 
    ## 
    ## Coefficients:
    ##          ar1
    ##       0.4693
    ## s.e.  0.0622
    ## 
    ## sigma^2 estimated as 0.9065:  log likelihood=-273.6
    ## AIC=551.2   AICc=551.26   BIC=557.79

``` r
fit= arima(serie2,order=c(1,0,0),include.mean = F)
fit
```

    ## 
    ## Call:
    ## arima(x = serie2, order = c(1, 0, 0), include.mean = F)
    ## 
    ## Coefficients:
    ##          ar1
    ##       0.4002
    ## s.e.  0.0646
    ## 
    ## sigma^2 estimated as 1.122:  log likelihood = -295.37,  aic = 594.74

``` r
resi= residuals(fit)
plot(resi)
```

![](Outliers_files/figure-gfm/ajuste%20de%20dos%20modelo-1.png)<!-- -->
Note que el residual correspodiente a la observación 50 es bastante
grande.

## Detección de outliers: procedimiento automático

``` r
coef= coefs2poly(fit)
coef
```

    ## $arcoefs
    ## [1] 0.4001684
    ## 
    ## $macoefs
    ## numeric(0)
    ## 
    ## attr(,"class")
    ## [1] "ArimaPars"

``` r
outliers= tsoutliers::locate.outliers(resi,coef)
outliers###tstat se compara con C=3
```

    ##   type ind  coefhat   tstat
    ## 1   AO  50 6.073156 6.70443

``` r
?tso####Detección automática de outliers, donde el modelo que se propone es via auto.arima
tso(serie2)
```

    ## Series: serie2 
    ## Regression with ARIMA(1,0,0) errors 
    ## 
    ## Coefficients:
    ##          ar1    AO50
    ##       0.4693  6.0844
    ## s.e.  0.0622  0.8598
    ## 
    ## sigma^2 estimated as 0.9111:  log likelihood=-273.59
    ## AIC=553.19   AICc=553.31   BIC=563.08
    ## 
    ## Outliers:
    ##   type ind time coefhat tstat
    ## 1   AO  50   50   6.084 7.077

## Simulación Outlier cambio de nivel

``` r
set.seed(12)
n=500
serie=arima.sim(n=n,list(ar=0.3))
serie2=serie
serie2[100:n]=serie2[100:n]+4
par(mfrow=c(1,2))
plot(serie,ylim=c(-3,7),main="Serie original")
plot(serie2,ylim=c(-3,7),ylab="Serie")
```

![](Outliers_files/figure-gfm/Cambio%20de%20nivel-1.png)<!-- -->

``` r
plot(serie2,ylim=c(-3,7),main="Outlier cambio de nivel")
```

![](Outliers_files/figure-gfm/Cambio%20de%20nivel-2.png)<!-- -->
