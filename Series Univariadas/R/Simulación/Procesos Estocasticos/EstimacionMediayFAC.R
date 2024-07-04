# Simulaciones -----
#* Proceso IID ----
T=1000
iid=ts(rnorm(T,0,1))
plot(iid)
acf(iid)

acf(iid,ci=0.99,lag.max = sqrt(T))
acf(iid,ci=0.99,lag.max = sqrt(T),ci.type="ma")

# * Filtro Lineal invariante \sum_{j=-k}{k}a_{j}x_{t-j} ----
T=550
inic=30
k=2
x=rnorm(T,0,1) ##Proceso IID de entrada
#a=rnorm(2*k+1,0,1)
a=c(-2,-1,1,2,3) ###Están en el orden a_{-k}....a_{0}...a_{k}
## Es decir, -2X_{t+2}-1X_{t+1}+1X_{t}+2X_{t-1}+3X{t-2}
sal=na.omit(stats::filter(x,a,method = "convolution"))
y=ts(sal[inic:length(sal)])
#x11()
plot(y)
acf(y)
acf(y,plot = F)
mean(y)
####IC. para la media del proceso ----
##Para el filtro lineal invariante, la media poblacional del proceso es cero.
###Pueden tambian simular el procesos usando arima.sim como sigue:
###
#y=arima.sim(n=500, list(ar=c(0.7)),sd=sqrt(1)) ##Para simular un AR(1)
#y=arima.sim(n=500, list(ma=c(0.6)),sd=sqrt(1)) ##Para simular un MA(1)
####
###
n=length(y)
acf(y,lag.max=n-1)
xbar=mean(y)
alpha=0.05 ###Para in IC del 95%
qnormal=qnorm(1-alpha/2,0,1)
##Calculo de la autocovarianza
gamma_est_h=acf(y,plot=F,type="covariance",lag.max = sqrt(n))
raiz_n=trunc(sqrt(n))
nu=as.numeric(2*sum((1-(seq(1:raiz_n)/n))*gamma_est_h[1:raiz_n]$acf)+gamma_est_h[0]$acf)

###IC de confianza----
Lim_inf=xbar-qnormal*sqrt(nu)/sqrt(n)
Limn_suo=xbar+qnormal*sqrt(nu)/sqrt(n)
Lim_inf
Limn_suo

###IC como si fuera IID----
Lim_inf_IID=xbar-qnormal*sqrt(gamma_est_h[0]$acf)/sqrt(n)
Limn_suo_IID=xbar+qnormal*sqrt(gamma_est_h[0]$acf)/sqrt(n)
Lim_inf_IID
Limn_suo_IID

acf(y,plot = F)




#* Proceso cíclico ----
T=10000
sigma=1
mu=3
R=1
s=12
omega=2*pi/s
theta=0
at=rnorm(T,0,sigma)
t=seq(1:T)
Zt=ts(mu+R*sin(omega*t)+at)
plot(Zt)
acf(Zt,lag.max = T-1)
pacf(Zt,lag.max = 36)
