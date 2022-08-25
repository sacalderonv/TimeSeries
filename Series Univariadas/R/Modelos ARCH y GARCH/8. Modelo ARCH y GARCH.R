#install.packages("fGarch")
#install.packages("aTSA")
library(fGarch)
library(aTSA)
library(forecast)

###Especificación, simulación y estimación de modelos 
#ARCH y GARCH por medio de dos librerías diferentes

##ARCH(2)
###Específicar el modelo 
###Simulación de la serie
set.seed(12)
spec = fGarch::garchSpec(model = list(alpha = c(0.2, 0.4), beta = 0))
ARCH2=fGarch::garchSim(spec, n = 500)
#x11()
plot(ARCH2)
acf(ARCH2)
pacf(ARCH2)
acf(ARCH2^2)
pacf(ARCH2^2)
Box.test(ARCH2,lag=20,type='Ljung-Box')
Box.test(ARCH2^2,lag=20,type='Ljung-Box')
salidaarima=arima(ARCH2,order = c(3,0,0),include.mean = FALSE,fixed=c(NA,NA,NA))
library(lmtest)
coeftest(salidaarima)
salidaarima
####Pruebas Portmanteau#####
##Efectos ARCH
arch.test(salidaarima)
resarma=residuals(salidaarima)^2
#x11()
pacf(resarma,lag.max = 20)
######Ajuste de la serie
m=fGarch::garchFit(garch~arma(0,0)+garch(2,0),data=ARCH2,trace=F,include.mean = FALSE)
m

sigmat=as.ts(m@sigma.t)
par(mfrow=c(2,1))
plot(ARCH2)
plot(sigmat)


###ARCH(1)
# Omega es el coeficiente constante de la ecuación de varianza
set.seed(100)
spec = garchSpec(model = list(omega=0.5,alpha = c(0.5), beta = 0))
ARCH1=garchSim(spec, n = 500,extended=F)
plot(ARCH1)
acf(ARCH1)
pacf(ARCH1)
acf(ARCH1^2)
pacf(ARCH1^2)
salidaarima=arima(ARCH1,order = c(0,0,0),include.mean = FALSE)
arch.test(salidaarima)

######Ajuste de la serie
m1=garchFit(garch~arma(1,0)+garch(1,1),data=ARCH1,trace=F,include.mean = FALSE)
m1
sigmat=as.ts(m1@sigma.t)
par(mfrow=c(2,1))
plot(ARCH1)
plot(sigmat)

prediccion=predict(n.ahead=5,m1)
predict(n.ahead=5,m1,plot=TRUE)


#####Ajuste Serie datos reales####
##Importación
###Establecer el directorio
sp5=as.ts(scan(file="SP500.txt"))

plot(sp5)
acf(sp5,ci.type='ma')
pacf(sp5) 
acf(sp5^2)
pacf(sp5^2)

coeftest(auto.arima(sp5)) ####Se podría ajustar un ARMA(2,2) con constante

modeloarima=stats::arima(sp5,order = c(2,0,2),include.mean = TRUE)
coeftest(modeloarima)
aTSA::arch.test(modeloarima)
acf(modeloarima$residual)
pacf(modeloarima$residual)
acf(modeloarima$residual^2)
pacf(modeloarima$residual^2)

modelosp5_1=garchFit(~arma(2,2)+garch(9,0),data=sp5,trace=F,include.mean=T)
summary(modelosp5_1)

plot(modelosp5_1)
prediccion=predict(modelosp5_1,n.ahead=10,plot=TRUE)

library(rugarch)
###Especificaci?n del modelo
spec1=rugarch::ugarchspec(variance.model=list(model="sGARCH",garchOrder = c(1,1)),
                 mean.model=list(armaOrder=c(2,2),include.mean = TRUE),distribution.model = "norm")
#fixed.pars = list(ar1=0,ar2=0,ar3=0,ar4=0,alpha1=0)

fit1=ugarchfit(data=sp5,spec=spec1)
show(fit1)
plot(fit1)
forc1 = ugarchforecast(fit1, n.ahead = 10)  ###Predicción de la varianza condicional
plot(forc1)



###########Modelos GARCH#########
plot(sp5,type='l')
acf(sp5)
acf(sp5^2)
modeloarima=arima(sp5,order = c(3,0,0),include.mean = TRUE)
arch.test(modeloarima)
acf(modeloarima$residual)
acf(modeloarima$residual^2)
pacf(modeloarima$residual^2)


modelosp5=garchFit(~arma(3,0)+garch(3,0),data=sp5,trace=F)
summary(modelosp5) ####Se puede verificar que el modelo para la parte autoregresiva no es significativa, adicional
                   ####a que la distribuci?n de los errores parece ser no normal
modelosp5_1=garchFit(~arma(0,0)+garch(3,0),data=sp5,trace=F)
summary(modelosp5_1)

modelosp5_2=garchFit(~arma(0,0)+garch(9,0),data=sp5,trace=F)
summary(modelosp5_2)

modelosp5_2=garchFit(~arma(0,0)+garch(9,0),data=sp5,trace=F,cond.dist ='std')
summary(modelosp5_2)


######Simulaci?n####
##GARCH(1,1)
spec = garchSpec(model = list(omega=1.5,alpha = c(0.6), beta = c(0.3)))
GARCH11=garchSim(spec, n = 500)
plot(GARCH11)
acf(GARCH11)
acf(GARCH11^2)
qqnorm(GARCH11)
qqline(GARCH11, col = 2,lwd=2,lty=2)
hist(GARCH11)
estimacion=garchFit(garch~garch(1,1),data=GARCH11,trace=F,include.mean=FALSE)
summary(estimacion)
plot(estimacion)
prediccion=predict(estimacion,n.ahead=10)
plot(prediccion)


#####Aplicaci?n con datos reales#####
####Retornar al conjunto de datos original SP5####
modelosp5_3=garchFit(~arma(0,0)+garch(1,1),data=sp5,trace=F)
summary(modelosp5_3)

modelosp5_4=garchFit(~arma(0,0)+garch(1,1),data=sp5,trace=F,cond.dist = 'std')
summary(modelosp5_4)

