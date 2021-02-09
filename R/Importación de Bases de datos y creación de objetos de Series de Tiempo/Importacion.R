###Creación de un objeto de Series de Tiempo###

tipos88 <- read.table("/Users/sergiocalderon/Documents/GitHub/TimeSeries/Bases de Datos/Estacionarias/tipos88.dat", quote="\"", comment.char="")
Intanual=tipos88$V5  #Tipo de interés Anual
?ts
plot(as.ts(Intanual))

camrelintanual=log(Intanual[2:length(Intanual)]/Intanual[1:(length(Intanual)-1)])
sercamrelint=ts(camrelintanual,start=c(1988,01),frequency=12)
sercamrelint
plot(sercamrelint)
acf(sercamrelint,ci.type='ma')
acf(sercamrelint,type='partial')
##Cambios relativos



library(xts)
library(readxl)
Colcap <- Datos_histo_ricos_COLCAP_3 <- read_excel("Documents/GitHub/TimeSeries/Bases de Datos/Datos históricos COLCAP-3.xlsx")
TsColCap=xts(Colcap$Ultimo, order.by = as.Date(Colcap$Fecha, "%Y-%m-%d"))
plot(TsColCap)

acf(TsColCap)

