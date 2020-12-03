Estructural
================

Modelos Estructurales
---------------------

En la presente sección, vamos a introducir una metodología para el
análisis de series de tiempo vía modelos estructurales a través de los
modelos de Espacio-Estado. Por ésta razón, es necesario tener el
pre-requisito de los modelos de espacio-estado y el filtro de Kalman, el
libro base será el libro de Durbin y Koopman. Consideraremos el
siguiente modelo estructural para la serie de tiempo {*Y*<sub>*t*</sub>}

*Y*<sub>*t*</sub> = *μ*<sub>*t*</sub> + *γ*<sub>*t*</sub> + *c*<sub>*t*</sub> + ∑*β*<sub>*j*</sub>*x*<sub>*j*<sub>*t*</sub></sub> + *ε*<sub>*t*</sub>

donde *μ*<sub>*t*</sub> es una componente de variación lenta llamada
tendencia, *γ*<sub>*t*</sub> es una componente periódica de periodo fijo
llamada , *c*<sub>*t*</sub> es la componente cíclica de periodo mayor a
la componente estacional, *x*<sub>*j*<sub>*t*</sub></sub> es la
*j*−ésima variable regresora o explicativa, y *ε*<sub>*t*</sub> es la
componente irregular o de error.

Ejemplo Pasajeros
-----------------

``` r
library(KFKSDS)
library(rucm)
```

    ## Loading required package: KFAS

``` r
library(readr)
library(stsm)
data("AirPassengers")
estimationm1=stats::StructTS(log(AirPassengers), type = "BSM")
plot(log(AirPassengers))
```

![](Estructural_files/figure-gfm/Ejemplo%20Pasajeros-1.png)<!-- -->

``` r
plot(cbind(fitted(estimationm1), resids=resid(estimationm1)), main = "Airpassengers")
```

![](Estructural_files/figure-gfm/Modelo%20Basico%20Estructural%20Pasajeros-1.png)<!-- -->

``` r
m1 <- StructTS(log(AirPassengers), type = "BSM")$coef[c(4,1:3)]
print(m1)
```

    ##      epsilon        level        slope         seas 
    ## 0.0000000000 0.0007718511 0.0000000000 0.0013969062

``` r
#### Con librería rucm

serie=log(AirPassengers)
model=ucm(serie~0, serie, irregular = TRUE, irregular.var = NA, level = TRUE,
    level.var = NA, slope = TRUE, slope.var = NA, season = TRUE,
    season.length = 12, season.var = NA, cycle = FALSE, cycle.period = NA,
    cycle.var = NA)
## Gráfica de las componentes de tendencia

plot(model$s.level, col = "blue")
```

![](Estructural_files/figure-gfm/Usando%20RUCM-1.png)<!-- -->

``` r
plot(model$s.slope, col = "blue")
```

![](Estructural_files/figure-gfm/Usando%20RUCM-2.png)<!-- -->

``` r
plot(model$s.season,col="blue")
```

![](Estructural_files/figure-gfm/Usando%20RUCM-3.png)<!-- -->

``` r
##Pronóstico
pron=predict(model, n.ahead = 12)
```
