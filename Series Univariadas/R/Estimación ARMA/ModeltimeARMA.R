# LIBRERÍAS ----

library(modeltime)
library(tidymodels)
library(tidyverse)
library(timetk)
library(lubridate)
library(modeltime.ensemble)



### Datos
tipos88 <- read.table("~/Documents/GitHub/TimeSeries/Series Univariadas/R/Estimación ARMA/tipos88.dat", quote="\"", comment.char="")
Intanual=tipos88$V5  #Tipo de interés Anual
x11()
plot(as.ts(Intanual))
camrelintanual=log(Intanual[2:length(Intanual)]/Intanual[1:(length(Intanual)-1)])
sercamrelint=ts(camrelintanual,start=c(1988,01),frequency=12)
sercamrelint
plot(sercamrelint)
acf(sercamrelint)
acf(sercamrelint,ci.type='ma')##Rezago máximo q=4
acf(sercamrelint,type='partial')##Rezago máximo p=3
pacf(sercamrelint)

####Flujo de trabajo en Modeltime
#1)Collect data and split into training and test sets
#2)Create & Fit Multiple Models
#3)Add fitted models to a Model Table
   #The next step is to add each of the models to a Modeltime Table using modeltime_table(). This step does some basic checking to make sure each of the models are fitted and that organizes into a scalable structure called a “Modeltime Table” that is used as part of our forecasting workflow.
#4)Calibrate the models to a testing set.
   #Calibrating adds a new column, .calibration_data, with the test predictions and residuals inside. A few notes on Calibration:
   #Calibration is how confidence intervals and accuracy metrics are determined
   #Calibration Data is simply forecasting predictions and residuals that are calculated from out-of-sample data.
   #After calibrating, the calibration data follows the data through the forecasting workflow.
#5)Perform Testing Set Forecast & Accuracy Evaluation
    #There are 2 critical parts to an evaluation.
       #* Visualizing the Forecast vs Test Data Set
       #* Evaluating the Test (Out of Sample) Accuracy

#6)Refit the models to Full Dataset & Forecast Forward
   #The final step is to refit the models to the full dataset using modeltime_refit() and forecast them forward.

###Creación de objeto Tibble Tydiverse
library(zoo)
camrela_tibble=as_tibble(data.frame(fecha=as.yearmon(time(sercamrelint)),cambrelativos=camrelintanual))
str(camrela_tibble)
camrela_tibble_ensemble=camrela_tibble
camrela_tibble_ensemble
####Entrenamiento y prueba
splits_camrela <- time_series_split(
    camrela_tibble,
    initial=150,
    assess     =20 ,
    cumulative = FALSE)
splits_camrela
splits_camrela$in_id
splits_camrela$out_id



splits_camrela %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(fecha, cambrelativos)

###Entrenamiento y Prueba ensemble
splits_camrela_ensemble <- time_series_split(
    camrela_tibble_ensemble,
    initial=150,
    assess     =20 ,
    cumulative = FALSE)
splits_camrela_ensemble


splits_camrela_ensemble %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(fecha, cambrelativos)

# Pronóstico ----
###MA(4)
model_ma_4 <- arima_reg(mode = "regression",
                         seasonal_period = "none",
                         non_seasonal_ar = 0,
                         non_seasonal_differences = NULL,
                         non_seasonal_ma = 4,
                         seasonal_ar = NULL,
                         seasonal_differences = NULL,
                         seasonal_ma = NULL) %>%
    set_engine("arima",include.mean = FALSE) %>%
    fit(cambrelativos ~ fecha, training(splits_camrela))
###arima se conecta con forecast::Arima()
model_ma_4


###AR(3)
model_ar_3 <- arima_reg(mode = "regression",
                        seasonal_period = "none",
                        non_seasonal_ar = 3,
                        non_seasonal_differences = NULL,
                        non_seasonal_ma = 0,
                        seasonal_ar = NULL,
                        seasonal_differences = NULL,
                        seasonal_ma = NULL) %>%
    set_engine("arima",include.mean = FALSE) %>%
    fit(cambrelativos ~ fecha, training(splits_camrela))
model_ar_3

### ARMA(3,4)

model_arma_34 <- arima_reg(mode = "regression",
                        seasonal_period = "none",
                        non_seasonal_ar = 3,
                        non_seasonal_differences = NULL,
                        non_seasonal_ma = 4,
                        seasonal_ar = NULL,
                        seasonal_differences = NULL,
                        seasonal_ma = NULL) %>%
    set_engine("arima",include.mean = FALSE) %>%
    fit(cambrelativos ~ fecha, training(splits_camrela))
model_arma_34

# MODELTIME COMPARARACIÓN ----

# * Modeltime Table ----
model_camrelativos_tbl <- modeltime_table(
    model_ma_4,
    model_ar_3,
    model_arma_34)

# * Calibrate ----
calib_camrelativos_tbl <- model_camrelativos_tbl %>%
    modeltime_calibrate(testing(splits_camrela))

# * Accuracy ----
calib_camrelativos_tbl %>% modeltime_accuracy(metric_set = default_forecast_accuracy_metric_set())


# * Test Set Visualization ----
calib_camrelativos_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits_camrela),
        actual_data = camrela_tibble
    ) %>%
    plot_modeltime_forecast(.interactive = FALSE)

# * Forecast Future ----
future_forecast_camrelativos_tbl <- calib_camrelativos_tbl %>%
    modeltime_refit(camrela_tibble) %>%
    modeltime_forecast(
        h           = 20,
        actual_data = camrela_tibble
    )

future_forecast_camrelativos_tbl %>%
    plot_modeltime_forecast(.interactive = FALSE)

########################
###### Hagamos combinación de pronósticos(ensemble) de una tabla de Modeltime

###Definición de los modelos
# Pronóstico ----
###MA(4)
model_ma_4_ensemble <- arima_reg(mode = "regression",
                        seasonal_period = "none",
                        non_seasonal_ar = 0,
                        non_seasonal_differences = NULL,
                        non_seasonal_ma = 4,
                        seasonal_ar = NULL,
                        seasonal_differences = NULL,
                        seasonal_ma = NULL) %>%
    set_engine("arima",include.mean = FALSE) %>%
    fit(cambrelativos ~ fecha, training(splits_camrela_ensemble))
###arima se conecta con forecast::Arima()
model_ma_4_ensemble


###AR(3)
model_ar_3_ensemble <- arima_reg(mode = "regression",
                        seasonal_period = "none",
                        non_seasonal_ar = 3,
                        non_seasonal_differences = NULL,
                        non_seasonal_ma = 0,
                        seasonal_ar = NULL,
                        seasonal_differences = NULL,
                        seasonal_ma = NULL) %>%
    set_engine("arima",include.mean = FALSE) %>%
    fit(cambrelativos ~ fecha, training(splits_camrela_ensemble))
model_ar_3_ensemble

### ARMA(3,4)

model_arma_34_ensemble <- arima_reg(mode = "regression",
                           seasonal_period = "none",
                           non_seasonal_ar = 3,
                           non_seasonal_differences = NULL,
                           non_seasonal_ma = 4,
                           seasonal_ar = NULL,
                           seasonal_differences = NULL,
                           seasonal_ma = NULL) %>%
    set_engine("arima",include.mean = FALSE) %>%
    fit(cambrelativos ~ fecha, training(splits_camrela_ensemble))
model_arma_34_ensemble
# * Modeltime Table ----
model_camrelativos_ensemble_tbl <- modeltime_table(
    model_ma_4_ensemble,
    model_ar_3_ensemble,
    model_arma_34_ensemble)

ensemble_camrelativos_fit <- model_camrelativos_ensemble_tbl %>%
    ensemble_average(type = "mean")
ensemble_camrelativos_fit


# Calibration
calibration_camrelativos_ensemble_tbl <- modeltime_table(
    ensemble_camrelativos_fit
) %>%
    modeltime_calibrate(testing(splits_camrela_ensemble), quiet = FALSE)

# Forecast vs Test Set
calibration_camrelativos_ensemble_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits_camrela_ensemble),
        actual_data = camrela_tibble_ensemble
    ) %>%
    plot_modeltime_forecast(.interactive = FALSE)

# * Forecast Future ----
future_forecast_camrelativos_ensemble_tbl <- calibration_camrelativos_ensemble_tbl %>%
    modeltime_refit(camrela_tibble_ensemble) %>%
    modeltime_forecast(
        h           = 20,
        actual_data = camrela_tibble_ensemble
    )

future_forecast_camrelativos_ensemble_tbl %>%
    plot_modeltime_forecast(.interactive = FALSE)
