Descomposición
================

## Métodos de Descomposición de Series de tiempo

Vamos a hacer un análisis Inicial a la serie de pasajeros. Un análisis
similar deberá hacerse para las series ISE y ACC de la Base de datos
Base\_Accidentes.xlsx,

Las primeras tres metodologías se basarán en el supuesto que una serie
de tiempo observable puede descompuesta en una componente de tendencia y
una componente estacional, es decir, se \(\{X_{t}\}\) puede
descomponerse de la siguiente forma aditiva $
X\_{t}=m\_{t}+S\_{t}+Y\_{t}, $ donde
\(m_{t}:\text{función que cambia suavemente,}\)\\
\(S_{t}:\text{función de periodo conocido d,}\)\\
\(Y_{t}:\text{ruido aleatorio estacionario en el sentido débil.}\)\\ Un
modelo multiplicativo puede ser considerado como modelo alternativo al
aditivo, $ X\_{t}=m\_{t}S\_{t} Y\_{t}, $

``` r
data("AirPassengers")
plot(AirPassengers)
#####Transformación Box-Cox
library(FitAR)
```

    ## Loading required package: lattice

    ## Loading required package: leaps

    ## Loading required package: ltsa

    ## Loading required package: bestglm

![](Descomposicion_files/figure-gfm/importación%20y%20Gráficas-1.png)<!-- -->

``` r
library(forecast)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## 
    ## Attaching package: 'forecast'

    ## The following object is masked from 'package:FitAR':
    ## 
    ##     BoxCox

``` r
forecast::BoxCox.lambda(AirPassengers, method = "guerrero", lower = 0, upper = 2)  
```

    ## [1] 4.102259e-05

``` r
##method="loglik"
FitAR::BoxCox(AirPassengers)
```

![](Descomposicion_files/figure-gfm/importación%20y%20Gráficas-2.png)<!-- -->

``` r
air.arima<-arima(AirPassengers, c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
FitAR::BoxCox(air.arima)
```

![](Descomposicion_files/figure-gfm/importación%20y%20Gráficas-3.png)<!-- -->

``` r
lAirPass=log(AirPassengers)
```
