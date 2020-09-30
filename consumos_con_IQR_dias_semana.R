library(zoo)
### CONSUMO TOTAL ###

pdf("graficas/consumo_con_IQR_dias_semana/consumo_total.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    mediaAnio <- aggregate(datosAnio$ConsumoTotal, list(Hora=datosAnio$Hora ), FUN=mean)
    medianaAnio <- aggregate(datosAnio$ConsumoTotal, list(Hora=datosAnio$Hora ), FUN=median)
    cuartil1Anio <- aggregate(datosAnio$ConsumoTotal, list(Hora=datosAnio$Hora ), FUN=function(x) quantile(x, 0.25)[[1]])
    cuartil3Anio <- aggregate(datosAnio$ConsumoTotal, list(Hora=datosAnio$Hora ), FUN=function(x) quantile(x, 0.75)[[1]])
    
    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,0)) #No margin on the right side
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(-500,2500),type = "l"))
    print(lines(mediaAnio$Hora, mediaAnio$x,type = 'l', col="blue"))
    print(lines(medianaAnio$Hora, medianaAnio$x,type = 'l', col="green"))
    print(lines(cuartil1Anio$Hora, cuartil1Anio$x,type = 'l', col="red"))
    print(lines(cuartil3Anio$Hora, cuartil3Anio$x,type = 'l', col="red"))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", c("Media", "Mediana", "Cuartiles 1 y 3"),col=c("blue", "green", "red"),cex=0.7,fill=c("blue", "green", "red")))
  }
}

dev.off()


### CONSUMO COCINA ###

pdf("graficas/consumo_con_IQR_dias_semana/consumo_cocina.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    mediaAnio <- aggregate(datosAnio$ConsumoCocina, list(Hora=datosAnio$Hora ), FUN=mean)
    medianaAnio <- aggregate(datosAnio$ConsumoCocina, list(Hora=datosAnio$Hora ), FUN=median)
    cuartil1Anio <- aggregate(datosAnio$ConsumoCocina, list(Hora=datosAnio$Hora ), FUN=function(x) quantile(x, 0.25)[[1]])
    cuartil3Anio <- aggregate(datosAnio$ConsumoCocina, list(Hora=datosAnio$Hora ), FUN=function(x) quantile(x, 0.75)[[1]])
 
    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,0)) #No margin on the right side
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(-100,100),type = "l"))
    print(lines(mediaAnio$Hora, mediaAnio$x,type = 'l', col="blue"))
    print(lines(medianaAnio$Hora, medianaAnio$x,type = 'l', col="green"))
    print(lines(cuartil1Anio$Hora, cuartil1Anio$x,type = 'l', col="red"))
    print(lines(cuartil3Anio$Hora, cuartil3Anio$x,type = 'l', col="red"))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", c("Media", "Mediana", "Cuartiles 1 y 3"),col=c("blue", "green", "red"),cex=0.7,fill=c("blue", "green", "red")))
  }
}

dev.off()


### CONSUMO AIRE Y CALEFACCION ###

pdf("graficas/consumo_con_IQR_dias_semana/consumo_aire_calefaccion.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    mediaAnio <- aggregate(datosAnio$ConsumoAireyCalefaccion, list(Hora=datosAnio$Hora ), FUN=mean)
    medianaAnio <- aggregate(datosAnio$ConsumoAireyCalefaccion, list(Hora=datosAnio$Hora ), FUN=median)
    cuartil1Anio <- aggregate(datosAnio$ConsumoAireyCalefaccion, list(Hora=datosAnio$Hora ), FUN=function(x) quantile(x, 0.25)[[1]])
    cuartil3Anio <- aggregate(datosAnio$ConsumoAireyCalefaccion, list(Hora=datosAnio$Hora ), FUN=function(x) quantile(x, 0.75)[[1]])

    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,0)) #No margin on the right side
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(-500,1500),type = "l"))
    print(lines(mediaAnio$Hora, mediaAnio$x,type = 'l', col="blue"))
    print(lines(medianaAnio$Hora, medianaAnio$x,type = 'l', col="green"))
    print(lines(cuartil1Anio$Hora, cuartil1Anio$x,type = 'l', col="red"))
    print(lines(cuartil3Anio$Hora, cuartil3Anio$x,type = 'l', col="red"))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", c("Media", "Mediana", "Cuartiles 1 y 3"),col=c("blue", "green", "red"),cex=0.7,fill=c("blue", "green", "red")))
  }
}

dev.off()

### CONSUMO LAVANDERIA ###

pdf("graficas/consumo_con_IQR_dias_semana/consumo_lavanderia.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    mediaAnio <- aggregate(datosAnio$ConsumoLavanderia, list(Hora=datosAnio$Hora ), FUN=mean)
    medianaAnio <- aggregate(datosAnio$ConsumoLavanderia, list(Hora=datosAnio$Hora ), FUN=median)
    cuartil1Anio <- aggregate(datosAnio$ConsumoLavanderia, list(Hora=datosAnio$Hora ), FUN=function(x) quantile(x, 0.25)[[1]])
    cuartil3Anio <- aggregate(datosAnio$ConsumoLavanderia, list(Hora=datosAnio$Hora ), FUN=function(x) quantile(x, 0.75)[[1]])
 
    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,0)) #No margin on the right side
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(-50,700),type = "l"))
    print(lines(mediaAnio$Hora, mediaAnio$x,type = 'l', col="blue"))
    print(lines(medianaAnio$Hora, medianaAnio$x,type = 'l', col="green"))
    print(lines(cuartil1Anio$Hora, cuartil1Anio$x,type = 'l', col="red"))
    print(lines(cuartil3Anio$Hora, cuartil3Anio$x,type = 'l', col="red"))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", c("Media", "Mediana", "Cuartiles 1 y 3"),col=c("blue", "green", "red"),cex=0.7,fill=c("blue", "green", "red"))) 
  }
}

dev.off()




