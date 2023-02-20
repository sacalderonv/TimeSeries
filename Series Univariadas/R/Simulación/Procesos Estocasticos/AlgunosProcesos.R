# Simulaciones -----
#* Proceso IID ----
T=1000
iid=ts(rnorm(T,0,1))
plot(iid)
acf(iid)
# * Filtro Lineal invariante \sum_{j=-k}{k}a_{j}x_{t-j} ----
T=550
inic=30
k=2
x=rnorm(T,0,1) ##Proceso IID
#a=rnorm(2*k+1,0,1)
a=c(-2,-1,1,2,3) ###Están en el orden a_{-k}....a_{0}...a_{k}
## Es decir, -2X_{t+2}-1X_{t+1}+1X_{t}+2X_{t-1}+3X{t-2}
sal=na.omit(stats::filter(x,a,method = "convolution"))
y=ts(sal[inic:length(sal)])
#x11()
plot(y)
acf(y)

# * Caminata Aleatoria ----

T=1000
sigma=1
x=rnorm(T,0,sigma)
St=ts(cumsum(x))
plot(St)
acf(St)

#* Proceso cíclico ----
T=1000
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
acf(Zt)
