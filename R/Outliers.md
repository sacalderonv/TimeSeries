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

``` r
fit= Arima(serie2,order=c(1,0,0),include.mean = F)
fit
```

    ## Series: serie2 
    ## ARIMA(1,0,0) with zero mean 
    ## 
    ## Coefficients:
    ##          ar1
    ##       0.9455
    ## s.e.  0.0142
    ## 
    ## sigma^2 estimated as 1.421:  log likelihood=-797.88
    ## AIC=1599.75   AICc=1599.78   BIC=1608.18

``` r
resi= residuals(fit)
coef= coefs2poly(fit)
outliers= locate.outliers(resi,coef,cval=5)###cval=3.5 por defecto
outliers
```

    ##   type ind  coefhat    tstat
    ## 1   LS 100 4.290489 5.558564

``` r
xreg = outliers.effects(outliers, n)
xreg
```

    ##        LS100
    ##   [1,]     0
    ##   [2,]     0
    ##   [3,]     0
    ##   [4,]     0
    ##   [5,]     0
    ##   [6,]     0
    ##   [7,]     0
    ##   [8,]     0
    ##   [9,]     0
    ##  [10,]     0
    ##  [11,]     0
    ##  [12,]     0
    ##  [13,]     0
    ##  [14,]     0
    ##  [15,]     0
    ##  [16,]     0
    ##  [17,]     0
    ##  [18,]     0
    ##  [19,]     0
    ##  [20,]     0
    ##  [21,]     0
    ##  [22,]     0
    ##  [23,]     0
    ##  [24,]     0
    ##  [25,]     0
    ##  [26,]     0
    ##  [27,]     0
    ##  [28,]     0
    ##  [29,]     0
    ##  [30,]     0
    ##  [31,]     0
    ##  [32,]     0
    ##  [33,]     0
    ##  [34,]     0
    ##  [35,]     0
    ##  [36,]     0
    ##  [37,]     0
    ##  [38,]     0
    ##  [39,]     0
    ##  [40,]     0
    ##  [41,]     0
    ##  [42,]     0
    ##  [43,]     0
    ##  [44,]     0
    ##  [45,]     0
    ##  [46,]     0
    ##  [47,]     0
    ##  [48,]     0
    ##  [49,]     0
    ##  [50,]     0
    ##  [51,]     0
    ##  [52,]     0
    ##  [53,]     0
    ##  [54,]     0
    ##  [55,]     0
    ##  [56,]     0
    ##  [57,]     0
    ##  [58,]     0
    ##  [59,]     0
    ##  [60,]     0
    ##  [61,]     0
    ##  [62,]     0
    ##  [63,]     0
    ##  [64,]     0
    ##  [65,]     0
    ##  [66,]     0
    ##  [67,]     0
    ##  [68,]     0
    ##  [69,]     0
    ##  [70,]     0
    ##  [71,]     0
    ##  [72,]     0
    ##  [73,]     0
    ##  [74,]     0
    ##  [75,]     0
    ##  [76,]     0
    ##  [77,]     0
    ##  [78,]     0
    ##  [79,]     0
    ##  [80,]     0
    ##  [81,]     0
    ##  [82,]     0
    ##  [83,]     0
    ##  [84,]     0
    ##  [85,]     0
    ##  [86,]     0
    ##  [87,]     0
    ##  [88,]     0
    ##  [89,]     0
    ##  [90,]     0
    ##  [91,]     0
    ##  [92,]     0
    ##  [93,]     0
    ##  [94,]     0
    ##  [95,]     0
    ##  [96,]     0
    ##  [97,]     0
    ##  [98,]     0
    ##  [99,]     0
    ## [100,]     1
    ## [101,]     1
    ## [102,]     1
    ## [103,]     1
    ## [104,]     1
    ## [105,]     1
    ## [106,]     1
    ## [107,]     1
    ## [108,]     1
    ## [109,]     1
    ## [110,]     1
    ## [111,]     1
    ## [112,]     1
    ## [113,]     1
    ## [114,]     1
    ## [115,]     1
    ## [116,]     1
    ## [117,]     1
    ## [118,]     1
    ## [119,]     1
    ## [120,]     1
    ## [121,]     1
    ## [122,]     1
    ## [123,]     1
    ## [124,]     1
    ## [125,]     1
    ## [126,]     1
    ## [127,]     1
    ## [128,]     1
    ## [129,]     1
    ## [130,]     1
    ## [131,]     1
    ## [132,]     1
    ## [133,]     1
    ## [134,]     1
    ## [135,]     1
    ## [136,]     1
    ## [137,]     1
    ## [138,]     1
    ## [139,]     1
    ## [140,]     1
    ## [141,]     1
    ## [142,]     1
    ## [143,]     1
    ## [144,]     1
    ## [145,]     1
    ## [146,]     1
    ## [147,]     1
    ## [148,]     1
    ## [149,]     1
    ## [150,]     1
    ## [151,]     1
    ## [152,]     1
    ## [153,]     1
    ## [154,]     1
    ## [155,]     1
    ## [156,]     1
    ## [157,]     1
    ## [158,]     1
    ## [159,]     1
    ## [160,]     1
    ## [161,]     1
    ## [162,]     1
    ## [163,]     1
    ## [164,]     1
    ## [165,]     1
    ## [166,]     1
    ## [167,]     1
    ## [168,]     1
    ## [169,]     1
    ## [170,]     1
    ## [171,]     1
    ## [172,]     1
    ## [173,]     1
    ## [174,]     1
    ## [175,]     1
    ## [176,]     1
    ## [177,]     1
    ## [178,]     1
    ## [179,]     1
    ## [180,]     1
    ## [181,]     1
    ## [182,]     1
    ## [183,]     1
    ## [184,]     1
    ## [185,]     1
    ## [186,]     1
    ## [187,]     1
    ## [188,]     1
    ## [189,]     1
    ## [190,]     1
    ## [191,]     1
    ## [192,]     1
    ## [193,]     1
    ## [194,]     1
    ## [195,]     1
    ## [196,]     1
    ## [197,]     1
    ## [198,]     1
    ## [199,]     1
    ## [200,]     1
    ## [201,]     1
    ## [202,]     1
    ## [203,]     1
    ## [204,]     1
    ## [205,]     1
    ## [206,]     1
    ## [207,]     1
    ## [208,]     1
    ## [209,]     1
    ## [210,]     1
    ## [211,]     1
    ## [212,]     1
    ## [213,]     1
    ## [214,]     1
    ## [215,]     1
    ## [216,]     1
    ## [217,]     1
    ## [218,]     1
    ## [219,]     1
    ## [220,]     1
    ## [221,]     1
    ## [222,]     1
    ## [223,]     1
    ## [224,]     1
    ## [225,]     1
    ## [226,]     1
    ## [227,]     1
    ## [228,]     1
    ## [229,]     1
    ## [230,]     1
    ## [231,]     1
    ## [232,]     1
    ## [233,]     1
    ## [234,]     1
    ## [235,]     1
    ## [236,]     1
    ## [237,]     1
    ## [238,]     1
    ## [239,]     1
    ## [240,]     1
    ## [241,]     1
    ## [242,]     1
    ## [243,]     1
    ## [244,]     1
    ## [245,]     1
    ## [246,]     1
    ## [247,]     1
    ## [248,]     1
    ## [249,]     1
    ## [250,]     1
    ## [251,]     1
    ## [252,]     1
    ## [253,]     1
    ## [254,]     1
    ## [255,]     1
    ## [256,]     1
    ## [257,]     1
    ## [258,]     1
    ## [259,]     1
    ## [260,]     1
    ## [261,]     1
    ## [262,]     1
    ## [263,]     1
    ## [264,]     1
    ## [265,]     1
    ## [266,]     1
    ## [267,]     1
    ## [268,]     1
    ## [269,]     1
    ## [270,]     1
    ## [271,]     1
    ## [272,]     1
    ## [273,]     1
    ## [274,]     1
    ## [275,]     1
    ## [276,]     1
    ## [277,]     1
    ## [278,]     1
    ## [279,]     1
    ## [280,]     1
    ## [281,]     1
    ## [282,]     1
    ## [283,]     1
    ## [284,]     1
    ## [285,]     1
    ## [286,]     1
    ## [287,]     1
    ## [288,]     1
    ## [289,]     1
    ## [290,]     1
    ## [291,]     1
    ## [292,]     1
    ## [293,]     1
    ## [294,]     1
    ## [295,]     1
    ## [296,]     1
    ## [297,]     1
    ## [298,]     1
    ## [299,]     1
    ## [300,]     1
    ## [301,]     1
    ## [302,]     1
    ## [303,]     1
    ## [304,]     1
    ## [305,]     1
    ## [306,]     1
    ## [307,]     1
    ## [308,]     1
    ## [309,]     1
    ## [310,]     1
    ## [311,]     1
    ## [312,]     1
    ## [313,]     1
    ## [314,]     1
    ## [315,]     1
    ## [316,]     1
    ## [317,]     1
    ## [318,]     1
    ## [319,]     1
    ## [320,]     1
    ## [321,]     1
    ## [322,]     1
    ## [323,]     1
    ## [324,]     1
    ## [325,]     1
    ## [326,]     1
    ## [327,]     1
    ## [328,]     1
    ## [329,]     1
    ## [330,]     1
    ## [331,]     1
    ## [332,]     1
    ## [333,]     1
    ## [334,]     1
    ## [335,]     1
    ## [336,]     1
    ## [337,]     1
    ## [338,]     1
    ## [339,]     1
    ## [340,]     1
    ## [341,]     1
    ## [342,]     1
    ## [343,]     1
    ## [344,]     1
    ## [345,]     1
    ## [346,]     1
    ## [347,]     1
    ## [348,]     1
    ## [349,]     1
    ## [350,]     1
    ## [351,]     1
    ## [352,]     1
    ## [353,]     1
    ## [354,]     1
    ## [355,]     1
    ## [356,]     1
    ## [357,]     1
    ## [358,]     1
    ## [359,]     1
    ## [360,]     1
    ## [361,]     1
    ## [362,]     1
    ## [363,]     1
    ## [364,]     1
    ## [365,]     1
    ## [366,]     1
    ## [367,]     1
    ## [368,]     1
    ## [369,]     1
    ## [370,]     1
    ## [371,]     1
    ## [372,]     1
    ## [373,]     1
    ## [374,]     1
    ## [375,]     1
    ## [376,]     1
    ## [377,]     1
    ## [378,]     1
    ## [379,]     1
    ## [380,]     1
    ## [381,]     1
    ## [382,]     1
    ## [383,]     1
    ## [384,]     1
    ## [385,]     1
    ## [386,]     1
    ## [387,]     1
    ## [388,]     1
    ## [389,]     1
    ## [390,]     1
    ## [391,]     1
    ## [392,]     1
    ## [393,]     1
    ## [394,]     1
    ## [395,]     1
    ## [396,]     1
    ## [397,]     1
    ## [398,]     1
    ## [399,]     1
    ## [400,]     1
    ## [401,]     1
    ## [402,]     1
    ## [403,]     1
    ## [404,]     1
    ## [405,]     1
    ## [406,]     1
    ## [407,]     1
    ## [408,]     1
    ## [409,]     1
    ## [410,]     1
    ## [411,]     1
    ## [412,]     1
    ## [413,]     1
    ## [414,]     1
    ## [415,]     1
    ## [416,]     1
    ## [417,]     1
    ## [418,]     1
    ## [419,]     1
    ## [420,]     1
    ## [421,]     1
    ## [422,]     1
    ## [423,]     1
    ## [424,]     1
    ## [425,]     1
    ## [426,]     1
    ## [427,]     1
    ## [428,]     1
    ## [429,]     1
    ## [430,]     1
    ## [431,]     1
    ## [432,]     1
    ## [433,]     1
    ## [434,]     1
    ## [435,]     1
    ## [436,]     1
    ## [437,]     1
    ## [438,]     1
    ## [439,]     1
    ## [440,]     1
    ## [441,]     1
    ## [442,]     1
    ## [443,]     1
    ## [444,]     1
    ## [445,]     1
    ## [446,]     1
    ## [447,]     1
    ## [448,]     1
    ## [449,]     1
    ## [450,]     1
    ## [451,]     1
    ## [452,]     1
    ## [453,]     1
    ## [454,]     1
    ## [455,]     1
    ## [456,]     1
    ## [457,]     1
    ## [458,]     1
    ## [459,]     1
    ## [460,]     1
    ## [461,]     1
    ## [462,]     1
    ## [463,]     1
    ## [464,]     1
    ## [465,]     1
    ## [466,]     1
    ## [467,]     1
    ## [468,]     1
    ## [469,]     1
    ## [470,]     1
    ## [471,]     1
    ## [472,]     1
    ## [473,]     1
    ## [474,]     1
    ## [475,]     1
    ## [476,]     1
    ## [477,]     1
    ## [478,]     1
    ## [479,]     1
    ## [480,]     1
    ## [481,]     1
    ## [482,]     1
    ## [483,]     1
    ## [484,]     1
    ## [485,]     1
    ## [486,]     1
    ## [487,]     1
    ## [488,]     1
    ## [489,]     1
    ## [490,]     1
    ## [491,]     1
    ## [492,]     1
    ## [493,]     1
    ## [494,]     1
    ## [495,]     1
    ## [496,]     1
    ## [497,]     1
    ## [498,]     1
    ## [499,]     1
    ## [500,]     1

``` r
tso(serie2)
```

    ## Series: serie2 
    ## Regression with ARIMA(1,0,0) errors 
    ## 
    ## Coefficients:
    ##          ar1   LS100
    ##       0.2543  3.9591
    ## s.e.  0.0432  0.0630
    ## 
    ## sigma^2 estimated as 0.8897:  log likelihood=-679.29
    ## AIC=1364.59   AICc=1364.63   BIC=1377.23
    ## 
    ## Outliers:
    ##   type ind time coefhat tstat
    ## 1   LS 100  100   3.959 62.86

``` r
salida_tso=tso(y=serie2,types=c("LS"))
plot(salida_tso$yadj)####Esta serie es sin el efecto de los outliers
```

![](Outliers_files/figure-gfm/Ajuste%20cambio%20de%20nivel-1.png)<!-- -->

``` r
plot(tso(y=serie2,types=c("LS")))
```

![](Outliers_files/figure-gfm/Ajuste%20cambio%20de%20nivel-2.png)<!-- -->

``` r
tso(y=serie2,xreg=xreg,tsmethod="arima",args.tsmethod=list(include.mean=FALSE,order=c(1,0,0)))#####Yo especifico el modelo
```

    ## 
    ## Call:
    ## list(method = NULL)
    ## 
    ## Coefficients:
    ##          ar1   LS100
    ##       0.2543  3.9591
    ## s.e.  0.0432  0.0630
    ## 
    ## sigma^2 estimated as 0.8862:  log likelihood = -679.29,  aic = 1364.59
    ## 
    ## No outliers were detected.

``` r
Arima(serie2,order=c(1,0,0),xreg=xreg,include.mean = F)
```

    ## Series: serie2 
    ## Regression with ARIMA(1,0,0) errors 
    ## 
    ## Coefficients:
    ##          ar1   LS100
    ##       0.2543  3.9591
    ## s.e.  0.0432  0.0630
    ## 
    ## sigma^2 estimated as 0.8897:  log likelihood=-679.29
    ## AIC=1364.59   AICc=1364.63   BIC=1377.23

## Pronóstico con outliers

Para el pronóstico, debo tener en cuenta los valores futuros de la
variable de intervención.

``` r
##### Pronóstico 
fit= Arima(serie2,order=c(1,0,0),include.mean = F)
fit2= Arima(serie2,order=c(1,0,0),include.mean = F,xreg=xreg)
pronostico=  forecast(object=fit,h=15)  #4.093698                      
regresoras=c(rep(1,15))
pronostico_out=forecast(object=fit2,xreg=regresoras,h=15) 
```

    ## Warning in forecast.forecast_ARIMA(object = fit2, xreg = regresoras, h = 15):
    ## xreg contains different column names from the xreg used in training. Please
    ## check that the regressors are in the same order.

``` r
par(mfrow=c(1,2))
plot(pronostico,ylim=c(-3,7))
plot(pronostico_out,ylim=c(-3,7)) ###Note la reducción en la varianza de las predicciones 
```

![](Outliers_files/figure-gfm/ajuste%20y%20creación%20de%20la%20variablde%20intervención%20para%20pronóstico-1.png)<!-- -->

``` r
plot(pronostico$residuals,ylim=c(-3,5))
plot(pronostico_out$residuals,ylim=c(-3,5))  
```

![](Outliers_files/figure-gfm/ajuste%20y%20creación%20de%20la%20variablde%20intervención%20para%20pronóstico-2.png)<!-- -->

\#\#\#Ejemplo Serie de Pasajeros

``` r
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
data("AirPassengers")
serie <- AirPassengers
ajuste=Arima(serie,order=c(0,1,1),seasonal = list(order = c(0, 1, 1)),include.mean=F,lambda =0 )
resi= residuals(ajuste)
coef= coefs2poly(ajuste)
outliers= locate.outliers(resi,coef)
outliers
```

    ##   type ind     coefhat     tstat
    ## 1   AO  29  0.08716944  3.736147
    ## 2   AO  62 -0.08410177 -3.604320
    ## 3   AO 135 -0.10318382 -3.902051

``` r
n=length(serie)
xreg = outliers.effects(outliers,n )

###El siguiente procedimiento busca que al ajustar, en el modelo con el efecto de los outliers, se busquen si hay mas outliers.
analisis=Arima(serie,order=c(0,1,1),seasonal = list(order = c(0, 1, 1)),include.mean=F,lambda =0 ,xreg=xreg)
analisis
```

    ## Series: serie 
    ## Regression with ARIMA(0,1,1)(0,1,1)[12] errors 
    ## Box Cox transformation: lambda= 0 
    ## 
    ## Coefficients:
    ##           ma1     sma1    AO29     AO62    AO135
    ##       -0.2898  -0.5246  0.0893  -0.0804  -0.1034
    ## s.e.   0.0990   0.0746  0.0228   0.0226   0.0257
    ## 
    ## sigma^2 estimated as 0.001067:  log likelihood=263.03
    ## AIC=-514.06   AICc=-513.38   BIC=-496.81

``` r
resi_analisis= residuals(analisis)
coef_analisis= coefs2poly(analisis)
outliers_analisis= locate.outliers(resi_analisis,coef_analisis)
outliers_analisis
```

    ##   type ind     coefhat     tstat
    ## 1   LS  54 -0.09604727 -3.923821

``` r
xreg_analisis = outliers.effects(outliers_analisis,n )
####Se pone "AO" en types porque en la localización de outliers únicamente encontró aditivos. Se incluye los efectos de los outliers, así que ahora encontró un outlier cambio de nivel en el tiempo 54.  
total_outliers=cbind(xreg,xreg_analisis)
analisis_final=Arima(serie,order=c(0,1,1),seasonal = list(order = c(0, 1, 1)),include.mean=F,lambda =0 ,xreg=total_outliers)
analisis_final
```

    ## Series: serie 
    ## Regression with ARIMA(0,1,1)(0,1,1)[12] errors 
    ## Box Cox transformation: lambda= 0 
    ## 
    ## Coefficients:
    ##           ma1     sma1    AO29     AO62    AO135     LS54
    ##       -0.3320  -0.4965  0.0959  -0.0803  -0.1032  -0.0967
    ## s.e.   0.0909   0.0759  0.0218   0.0216   0.0248   0.0249
    ## 
    ## sigma^2 estimated as 0.0009703:  log likelihood=270.03
    ## AIC=-526.06   AICc=-525.15   BIC=-505.93

``` r
resi_final= residuals(analisis_final)
coef_final= coefs2poly(analisis_final)
outliers_final= locate.outliers(resi_final,coef_final)
outliers_final
```

    ## [1] type    ind     coefhat tstat  
    ## <0 rows> (or 0-length row.names)

``` r
###No se encontraron mas outliers
###Verificar los supuestos del modelo.
```

``` r
###Creación de las variable de intervención
pasos_adel=12
num_outliers=dim(total_outliers)[2]
regresoras_aditivos=matrix(c(rep(0,pasos_adel*(num_outliers-1))),pasos_adel,num_outliers-1)
regresoras_LS=matrix(c(rep(1,pasos_adel)),pasos_adel,1)
regresoras=cbind(regresoras_aditivos,regresoras_LS)
colnames(regresoras)=colnames(total_outliers)

pronostico_out=forecast(object=analisis_final,xreg=regresoras,h=pasos_adel) 
pronostico_out
```

    ##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## Jan 1961       449.8404 432.2367 468.1610 423.1985 478.1595
    ## Feb 1961       424.7048 404.7981 445.5905 394.6407 457.0593
    ## Mar 1961       500.6276 473.8772 528.8880 460.3000 544.4884
    ## Apr 1961       492.0941 462.9524 523.0702 448.2309 540.2497
    ## May 1961       508.9298 476.1319 543.9870 459.6342 563.5124
    ## Jun 1961       582.3461 542.0245 625.6673 521.8223 649.8898
    ## Jul 1961       670.5677 621.1525 723.9140 596.4852 753.8510
    ## Aug 1961       666.7261 614.8142 723.0213 588.9904 754.7215
    ## Sep 1961       556.8647 511.3170 606.4696 488.7336 634.4934
    ## Oct 1961       497.1427 454.6262 543.6352 433.6116 569.9820
    ## Nov 1961       428.9600 390.7509 470.9052 371.9219 494.7454
    ## Dec 1961       475.9017 431.8949 524.3925 410.2711 552.0311

``` r
plot(pronostico_out)
```

![](Outliers_files/figure-gfm/Pronóstico-1.png)<!-- -->
