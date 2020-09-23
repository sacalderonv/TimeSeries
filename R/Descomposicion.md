Descomposición
================

## Métodos de Descomposición de Series de tiempo

Vamos a hacer un análisis Inicial a la serie de pasajeros. Un análisis
similar deberá hacerse para las series ISE y ACC de la Base de datos
Base\_Accidentes.xlsx,

Las primeras tres metodologías se basarán en el supuesto que una serie
de tiempo observable puede ser descompuesta en una componente de
tendencia y una componente estacional, es decir, \(\{X_{t}\}\) puede
descomponerse de la siguiente forma aditiva \[
X_{t}=m_{t}+S_{t}+Y_{t},
\] donde \[m_{t}:\text{función que cambia suavemente,}\]\\
\[S_{t}:\text{función de periodo conocido d,}\]\\
\[Y_{t}:\text{ruido aleatorio estacionario en el sentido débil.}\]\\ Un
modelo multiplicativo puede ser considerado como modelo alternativo al
aditivo, \[
X_{t}=m_{t}\times S_{t} \times Y_{t}.
\] Sin embargo es necesario primero hacer una transformación de Box-Cox
para Estabilizar la varianza marginal.

\[
 f_{\lambda}(u_{t})= 
 \lambda^{-1}(u^{\lambda}_{t}-1),   si \ u_{t} \geq 0, para\  \lambda>0
 \] o \[
 f_{\lambda}(u_{t})= \ln(u_{t}), \ si\  u_{t}>0, \ para\  \lambda=0
 \]

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
par(mfrow=c(2,1))
plot(AirPassengers)
plot(lAirPass)
```

![](Descomposicion_files/figure-gfm/importación%20y%20Gráficas-4.png)<!-- -->
\# Descomposición usando promedios Móviles \#\# Para tendencia

``` r
fltr <- c(1/2, rep(1, times = 11), 1/2)/12
fltr
```

    ##  [1] 0.04166667 0.08333333 0.08333333 0.08333333 0.08333333 0.08333333
    ##  [7] 0.08333333 0.08333333 0.08333333 0.08333333 0.08333333 0.08333333
    ## [13] 0.04166667

``` r
## estimate of trend
lAirPass.trend <- filter(lAirPass, filter = fltr, method = "convo", sides = 2)
## plot the trend
#x11()
plot.ts(lAirPass.trend, ylab = "Trend", cex = 1)
```

![](Descomposicion_files/figure-gfm/PMtendencia-1.png)<!-- --> \# Efecto
estacional a través del tiempo

``` r
lAirPass.1T <- lAirPass - lAirPass.trend

## plot the monthly seasonal effects
plot.ts(lAirPass.1T, ylab = "Seasonal effect", xlab = "Month", cex = 1)
```

![](Descomposicion_files/figure-gfm/PM%20Estacional-1.png)<!-- -->

``` r
## length of ts
ll <- length(lAirPass.1T)
## frequency (ie, 12)
ff <- frequency(lAirPass.1T)
## number of periods (years); %/% is integer division
periods <- ll%/%ff
## index of cumulative month
index <- seq(1, ll, by = ff) - 1
## get mean by month
mm <- numeric(ff)
for (i in 1:ff) {
  mm[i] <- mean(lAirPass.1T[index + i], na.rm = TRUE)
}
## subtract mean to make overall mean=0
mm <- mm - mean(mm)

## plot the monthly seasonal effects
plot.ts(mm, ylab = "Seasonal effect", xlab = "Month", cex = 1)
```

![](Descomposicion_files/figure-gfm/PM%20Estacional-2.png)<!-- -->

``` r
## create ts object for season
lAirPass.seas <- ts(rep(mm, periods + 1)[seq(ll)], start = start(lAirPass.1T), 
               frequency = ff)


## random errors over time
lAirPass.err <- lAirPass - lAirPass.trend - lAirPass.seas


## plot the obs ts, trend & seasonal effect
plot(cbind(lAirPass, lAirPass.trend, lAirPass.seas, lAirPass.err), main = "", yax.flip = TRUE)
```

![](Descomposicion_files/figure-gfm/PM%20Estacional-3.png)<!-- --> \#
Dos funciones que hacen la descomposición de forma interna y automática
Una función específica para filtros de promedios móviles también puede
ser usada y es ma del paquete forecast, donde order=d, note que hay una
ligera diferencia por el 1/2 de los datos en los extremos

``` r
lAirPass.trendma=forecast::ma(lAirPass,order=13)
lAirPass.trendma-lAirPass.trend
```

    ##                Jan           Feb           Mar           Apr           May
    ## 1949            NA            NA            NA            NA            NA
    ## 1950  1.512810e-02  1.423973e-02  7.247243e-03 -5.350492e-03 -1.711213e-02
    ## 1951  1.282887e-02  1.181921e-02  4.996403e-03 -7.646584e-03 -1.900024e-02
    ## 1952  1.244482e-02  1.330987e-02  3.621721e-03 -5.675967e-03 -1.475862e-02
    ## 1953  1.071685e-02  1.300468e-02  1.290781e-03 -7.364554e-03 -1.797100e-02
    ## 1954  1.719692e-02  1.651209e-02  5.947422e-03 -3.803631e-03 -1.519825e-02
    ## 1955  1.891739e-02  1.477354e-02  4.801362e-03 -6.100417e-03 -1.738653e-02
    ## 1956  1.793440e-02  1.444231e-02  4.375753e-03 -7.100096e-03 -1.813512e-02
    ## 1957  1.846753e-02  1.704363e-02  5.530992e-03 -6.848848e-03 -1.726450e-02
    ## 1958  1.937541e-02  2.019669e-02  5.789877e-03 -4.710728e-03 -1.547808e-02
    ## 1959  2.046853e-02  2.163660e-02  5.044631e-03 -5.294726e-03 -1.634470e-02
    ## 1960  1.977664e-02  1.887399e-02  4.286370e-03 -5.102433e-03 -1.667977e-02
    ##                Jun           Jul           Aug           Sep           Oct
    ## 1949            NA -8.120335e-03 -2.894707e-03  5.321922e-03  2.408088e-03
    ## 1950 -5.195189e-03 -6.126899e-03 -2.611319e-03  6.991687e-03  5.823260e-04
    ## 1951 -7.499763e-03 -6.083321e-03 -3.919581e-03  4.501415e-03 -1.948494e-03
    ## 1952 -6.216380e-03 -5.617452e-03 -4.354918e-03  4.552475e-03  4.386815e-04
    ## 1953 -9.356792e-03 -8.634334e-03 -1.177045e-02  4.102135e-03  2.731303e-03
    ## 1954 -7.122278e-03 -5.394282e-03 -1.122871e-02  1.495602e-03 -5.027888e-04
    ## 1955 -7.732139e-03 -5.921593e-03 -9.406489e-03 -8.436885e-05 -1.321469e-03
    ## 1956 -8.063629e-03 -6.767072e-03 -1.007387e-02  9.304104e-04 -1.143789e-03
    ## 1957 -9.548256e-03 -8.522727e-03 -1.326501e-02 -2.055703e-03 -4.500425e-03
    ## 1958 -8.604726e-03 -5.803006e-03 -1.076508e-02  2.161819e-04 -3.041795e-03
    ## 1959 -9.901859e-03 -7.299754e-03 -1.264893e-02 -3.921189e-03 -1.794406e-03
    ## 1960 -8.874584e-03            NA            NA            NA            NA
    ##                Nov           Dec
    ## 1949 -3.264284e-03  7.281250e-03
    ## 1950 -1.937730e-03  4.543173e-03
    ## 1951  6.834185e-06  7.208349e-03
    ## 1952 -1.688937e-03  6.257787e-03
    ## 1953  2.946455e-03  9.533386e-03
    ## 1954 -1.947185e-04  9.348934e-03
    ## 1955 -1.579213e-03  9.513571e-03
    ## 1956 -4.607881e-04  1.168757e-02
    ## 1957 -2.182787e-03  1.125741e-02
    ## 1958 -3.722257e-05  1.068241e-02
    ## 1959  5.144059e-04  9.046882e-03
    ## 1960            NA            NA

``` r
####Uso de la función decompose
deslAirPass=decompose(lAirPass)
plot(deslAirPass)
```

![](Descomposicion_files/figure-gfm/dos%20funciones-1.png)<!-- -->

``` r
deslAirPass
```

    ## $x
    ##           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
    ## 1949 4.718499 4.770685 4.882802 4.859812 4.795791 4.905275 4.997212 4.997212
    ## 1950 4.744932 4.836282 4.948760 4.905275 4.828314 5.003946 5.135798 5.135798
    ## 1951 4.976734 5.010635 5.181784 5.093750 5.147494 5.181784 5.293305 5.293305
    ## 1952 5.141664 5.192957 5.262690 5.198497 5.209486 5.384495 5.438079 5.488938
    ## 1953 5.278115 5.278115 5.463832 5.459586 5.433722 5.493061 5.575949 5.605802
    ## 1954 5.318120 5.236442 5.459586 5.424950 5.455321 5.575949 5.710427 5.680173
    ## 1955 5.488938 5.451038 5.587249 5.594711 5.598422 5.752573 5.897154 5.849325
    ## 1956 5.648974 5.624018 5.758902 5.746203 5.762051 5.924256 6.023448 6.003887
    ## 1957 5.752573 5.707110 5.874931 5.852202 5.872118 6.045005 6.142037 6.146329
    ## 1958 5.828946 5.762051 5.891644 5.852202 5.894403 6.075346 6.196444 6.224558
    ## 1959 5.886104 5.834811 6.006353 5.981414 6.040255 6.156979 6.306275 6.326149
    ## 1960 6.033086 5.968708 6.037871 6.133398 6.156979 6.282267 6.432940 6.406880
    ##           Sep      Oct      Nov      Dec
    ## 1949 4.912655 4.779123 4.644391 4.770685
    ## 1950 5.062595 4.890349 4.736198 4.941642
    ## 1951 5.214936 5.087596 4.983607 5.111988
    ## 1952 5.342334 5.252273 5.147494 5.267858
    ## 1953 5.468060 5.351858 5.192957 5.303305
    ## 1954 5.556828 5.433722 5.313206 5.433722
    ## 1955 5.743003 5.613128 5.468060 5.627621
    ## 1956 5.872118 5.723585 5.602119 5.723585
    ## 1957 6.001415 5.849325 5.720312 5.817111
    ## 1958 6.001415 5.883322 5.736572 5.820083
    ## 1959 6.137727 6.008813 5.891644 6.003887
    ## 1960 6.230481 6.133398 5.966147 6.068426
    ## 
    ## $seasonal
    ##               Jan          Feb          Mar          Apr          May
    ## 1949 -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ## 1950 -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ## 1951 -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ## 1952 -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ## 1953 -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ## 1954 -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ## 1955 -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ## 1956 -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ## 1957 -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ## 1958 -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ## 1959 -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ## 1960 -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ##               Jun          Jul          Aug          Sep          Oct
    ## 1949  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ## 1950  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ## 1951  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ## 1952  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ## 1953  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ## 1954  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ## 1955  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ## 1956  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ## 1957  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ## 1958  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ## 1959  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ## 1960  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ##               Nov          Dec
    ## 1949 -0.215845612 -0.100315075
    ## 1950 -0.215845612 -0.100315075
    ## 1951 -0.215845612 -0.100315075
    ## 1952 -0.215845612 -0.100315075
    ## 1953 -0.215845612 -0.100315075
    ## 1954 -0.215845612 -0.100315075
    ## 1955 -0.215845612 -0.100315075
    ## 1956 -0.215845612 -0.100315075
    ## 1957 -0.215845612 -0.100315075
    ## 1958 -0.215845612 -0.100315075
    ## 1959 -0.215845612 -0.100315075
    ## 1960 -0.215845612 -0.100315075
    ## 
    ## $trend
    ##           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
    ## 1949       NA       NA       NA       NA       NA       NA 4.837280 4.841114
    ## 1950 4.869840 4.881389 4.893411 4.904293 4.912752 4.923701 4.940483 4.957406
    ## 1951 5.047776 5.060902 5.073812 5.088378 5.106906 5.124312 5.138282 5.152751
    ## 1952 5.203909 5.218093 5.231553 5.243722 5.257413 5.270736 5.282916 5.292150
    ## 1953 5.367695 5.378309 5.388417 5.397805 5.403849 5.407220 5.410364 5.410294
    ## 1954 5.419628 5.428330 5.435128 5.442237 5.450659 5.461103 5.473655 5.489713
    ## 1955 5.557864 5.572693 5.587498 5.602730 5.616658 5.631189 5.645937 5.659812
    ## 1956 5.727153 5.738856 5.750676 5.760658 5.770846 5.780430 5.788745 5.796524
    ## 1957 5.842665 5.853541 5.864863 5.875490 5.885654 5.894475 5.901555 5.907026
    ## 1958 5.917360 5.922887 5.926146 5.927563 5.929657 5.930458 5.932964 5.938377
    ## 1959 5.985269 5.994078 6.003991 6.014899 6.026589 6.040709 6.054492 6.066195
    ## 1960 6.112511 6.121153 6.128381 6.137437 6.145733 6.151526       NA       NA
    ##           Sep      Oct      Nov      Dec
    ## 1949 4.846596 4.851238 4.854488 4.859954
    ## 1950 4.974380 4.991942 5.013095 5.033804
    ## 1951 5.163718 5.171454 5.178401 5.189431
    ## 1952 5.304079 5.323338 5.343560 5.357427
    ## 1953 5.408381 5.406761 5.406218 5.410571
    ## 1954 5.503974 5.516367 5.529403 5.542725
    ## 1955 5.674172 5.687636 5.700766 5.714738
    ## 1956 5.804821 5.814072 5.823075 5.832692
    ## 1957 5.910012 5.910708 5.911637 5.913829
    ## 1958 5.946188 5.956352 5.967813 5.977291
    ## 1959 6.073088 6.080733 6.091930 6.102013
    ## 1960       NA       NA       NA       NA
    ## 
    ## $random
    ##                Jan           Feb           Mar           Apr           May
    ## 1949            NA            NA            NA            NA            NA
    ## 1950 -0.0390928761  0.0693058849  0.0372357360  0.0140276822 -0.0754725800
    ## 1951  0.0147724349  0.0641462837  0.0898580408  0.0184174933  0.0495549567
    ## 1952  0.0235692221  0.0892767048  0.0130241986 -0.0321798094 -0.0389603991
    ## 1953 -0.0037655164  0.0142183907  0.0573014076  0.0748261410  0.0388394970
    ## 1954 -0.0156931430 -0.0774753636  0.0063445490 -0.0042416399  0.0136285780
    ## 1955  0.0168884187 -0.0072413171 -0.0183626190  0.0050265167 -0.0092699200
    ## 1956  0.0076357612 -0.0004254752 -0.0098872806 -0.0014090481  0.0001714859
    ## 1957 -0.0042769756 -0.0320178538 -0.0080460604 -0.0102418776 -0.0045699327
    ## 1958 -0.0025997950 -0.0464226960 -0.0526156185 -0.0623149518 -0.0262881911
    ## 1959 -0.0133498220 -0.0448545455 -0.0157509599 -0.0204393997  0.0226314823
    ## 1960  0.0063898107 -0.0380324934 -0.1086238739  0.0090064128  0.0202125426
    ##                Jun           Jul           Aug           Sep           Oct
    ## 1949            NA -0.0508840131 -0.0484145828  0.0012226079  0.0031563172
    ## 1950 -0.0351476703 -0.0155006194 -0.0261197084  0.0233788915 -0.0263218589
    ## 1951 -0.0579214701 -0.0557934329 -0.0639582039 -0.0136190649 -0.0085864324
    ## 1952 -0.0016338513 -0.0556531046 -0.0077243559 -0.0265809253  0.0042062795
    ## 1953 -0.0295513795 -0.0452309945 -0.0090045219 -0.0051571083  0.0203685746
    ## 1954 -0.0005469630  0.0259560620 -0.0140532362 -0.0119825541 -0.0073736839
    ## 1955  0.0059902707  0.0404007439 -0.0149999562  0.0039948255  0.0007629883
    ## 1956  0.0284325199  0.0238857779  0.0028504899  0.0024605216 -0.0152157267
    ## 1957  0.0351368634  0.0296663904  0.0347908579  0.0265669175  0.0138880406
    ## 1958  0.0294945574  0.0526637918  0.0816688890 -0.0096097940  0.0022419710
    ## 1959  0.0008768183  0.0409669180  0.0554418481 -0.0001967977  0.0033510501
    ## 1960  0.0153478241            NA            NA            NA            NA
    ##                Nov           Dec
    ## 1949  0.0057486713  0.0110454132
    ## 1950 -0.0610505322  0.0081538226
    ## 1951  0.0210507630  0.0228720933
    ## 1952  0.0197798277  0.0107462135
    ## 1953  0.0025848214 -0.0069512737
    ## 1954 -0.0003512869 -0.0086876484
    ## 1955 -0.0168606818  0.0131983964
    ## 1956 -0.0051103984 -0.0087919843
    ## 1957  0.0245208403  0.0035969468
    ## 1958 -0.0153947570 -0.0568931956
    ## 1959  0.0155602521  0.0021887357
    ## 1960            NA            NA
    ## 
    ## $figure
    ##  [1] -0.085815019 -0.114412848  0.018113355 -0.013045611 -0.008966106
    ##  [6]  0.115392997  0.210816435  0.204512399  0.064836351 -0.075271265
    ## [11] -0.215845612 -0.100315075
    ## 
    ## $type
    ## [1] "additive"
    ## 
    ## attr(,"class")
    ## [1] "decomposed.ts"

# Otras ténicas de descomposición

Existe mas formas de hacer de descomposición de series de tiempo, por
ejemplo está Descomposción X11 usando el paquete seasonal, cuyo
argumento teórico se puede ver el libro Seasonal Adjustment Methods and
Real Time Trend-Cycle Estimation (Statistics for Social and Behavioral
Sciences) Estela Bee Dagum, Silvia Bianconcini

``` r
library(seasonal)
library(ggplot2)
library(fpp)
```

    ## Loading required package: fma

    ## Loading required package: expsmooth

    ## Loading required package: lmtest

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: tseries

``` r
AirPassengers %>% seas(x11="") -> fit  #%>% Operador Pipe
autoplot(fit) + ggtitle("X11 decomposition of Aipassengers")
```

![](Descomposicion_files/figure-gfm/X11-1.png)<!-- -->

``` r
data(elecequip)
autoplot(elecequip)
```

![](Descomposicion_files/figure-gfm/X11-2.png)<!-- -->

``` r
elecequip %>% seas(x11="") -> fit
autoplot(elecequip, series="Data") +
  autolayer(trendcycle(fit), series="Trend") +
  autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))
```

![](Descomposicion_files/figure-gfm/X11-3.png)<!-- --> \# Descomposición
STL

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(fpp)
###La base de datos "elecequip" está en en el paquete fpp
###Note que el operador %>% pipe permite concatenar múltiples dplyr operaciones
###Manufacture of electrical equipment: computer, electronic and optical products
elecequip %>%
  stats::stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()
```

![](Descomposicion_files/figure-gfm/STL-1.png)<!-- -->

# Pronóstico basados en descomposición

``` r
fit <- stl(elecequip, t.window=13, s.window="periodic",
           robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("Nuevo índices ordenados.") +
  ggtitle("Pronóstico Naive de la componente ajusta estacionalmente")
```

![](Descomposicion_files/figure-gfm/Pronósticos%20Basados%20en%20Descomposición-1.png)<!-- -->

``` r
###El método naive consiste en que la predicción es el último valor real de la serie(o el de una caminata aleatoria).
#Ahora se re-estacionalizan los datos añadiendo 
###Los pronósticos de la componente estacional.
fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")
```

![](Descomposicion_files/figure-gfm/Pronósticos%20Basados%20en%20Descomposición-2.png)<!-- -->

``` r
###Note que la obtención de los pronósticos  es obtenida paso a paso.
###Otra forma de hacerlo es usando la función stlf

fcast <- stlf(elecequip, method='naive')
```
