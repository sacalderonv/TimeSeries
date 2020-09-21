Arboles
================

## Métodos Basados en Árboles

Los árboles binarios son el componente básico de la mayoría de los
métodos estadísticos basados en árboles.\\ Binario quiere decir, que
cada rama del árbol puede ser dividido dentro de 2 sub-ramas Estos
árboles son comúnmente referidos a árboles de decisión.

``` r
da=read.table("/Users/sergiocalderon/Documents/GitHub/TimeSeries/Bases de Datos/GDPC1.txt",header=T) 
gdp = diff(log(da$rgdp))
tdx = da$year + (da$mon/12)
tdx = tdx[-1]
plot(tdx,gdp,xlab='año',ylab='tasa-PIB',type='l') 
```

![](Arboles_files/figure-gfm/importación%20y%20Gráficas-1.png)<!-- -->

``` r
length(gdp)
```

    ## [1] 273

## Configuración y Arbol

``` r
gdp = round(gdp*100,2)
X = cbind(gdp[4:273],gdp[3:272],gdp[2:271],gdp[1:270])
colnames(X) = c("gdp","gdp1","gdp2","gdp3")
require(tree)
```

    ## Loading required package: tree

``` r
t1 = tree(gdp~.,data=data.frame(X))
summary(t1)
```

    ## 
    ## Regression tree:
    ## tree(formula = gdp ~ ., data = data.frame(X))
    ## Number of terminal nodes:  11 
    ## Residual mean deviance:  0.6654 = 172.3 / 259 
    ## Distribution of residuals:
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.27300 -0.46200 -0.03298  0.00000  0.51020  3.25700

``` r
plot(t1)
text(t1,pretty=0)
```

![](Arboles_files/figure-gfm/Configuración%20y%20Arbol-1.png)<!-- -->

# Etapa de Pronóstico

``` r
library(NTS)
PM2.5 = scan(file="/Users/sergiocalderon/Documents/GitHub/TimeSeries/Bases de Datos/d-Shanghai-1317.txt")
tdx = c(1:length(PM2.5))/365+2013
par(mfcol=c(2,1))
plot(tdx,PM2.5,xlab='year',ylab='PM2.5',type='l') 
acf(PM2.5,lag=800)
```

![](Arboles_files/figure-gfm/Pronóstico-1.png)<!-- -->

``` r
m1 = NNsetting(PM2.5,nfore=365,lags=c(1:10,365:370))
names(m1)
```

    ## [1] "X"     "y"     "predX" "predY"

``` r
X= m1$X; y = m1$y; predX = m1$predX; predY = m1$predY
t1 = tree(y~.,data=data.frame(X))
par(mfcol=c(1,1))
plot(t1)
text(t1,pretty=0) 
```

![](Arboles_files/figure-gfm/Pronóstico-2.png)<!-- -->

``` r
pt1 = predict(t1,newdata=data.frame(predX)) 
er3 = pt1-predY
```
