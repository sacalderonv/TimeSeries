Outliers
================

Outliers en Modelos ARIMA
-------------------------

Vamos a considerar el análisis de outliers para modelos ARIMA o SARIMA.

La base fundamental del análisis de outliers se basa en en análisis de
intervención. Vamos a empezar con unos ejercicios de simulación. La
librería base es tsoutliers.

``` r
library(tsoutliers)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

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

Después de simular un outlier aditivo en el tiempo *t* = 50 con impacto
de 6, basada en una serie que proviene de de un modelo AR con *ϕ* = 0.5.

En seguida vamos ajustar dos modelos, uno vía el procedimiento
automático y el otro el verdadero modelo.

``` r
auto.arima(serie2)
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
auto.arima(serie)
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
