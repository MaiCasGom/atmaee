library(zoo)
library(lubridate)

### CONSUMO TOTAL ###

pdf("graficas/consumo_total/consumo_total_dias_semana.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,0)) #No margin on the right side
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(0,4500),type = "l"))
    for(m in unique(month(datosAnio$Fecha))){
      mes <- as.integer(format(as.Date(datosAnio[i, "Fecha"],format="%Y-%m-%d"), format = "%m"))
      datosMes <- datosAnio[month(datosAnio$Fecha) == m, ]
      for(d in unique(day(datosMes$Fecha))){
        dia <- as.integer(format(as.Date(datosMes[i, "Fecha"],format="%Y-%m-%d"), format = "%d"))
        datosDia <- datosMes[day(datosMes$Fecha) == d, ]
        print(lines(datosDia$Hora, datosDia$ConsumoTotal,type = 'l', col="grey"))
      }
    }
    mediasAnio <- aggregate(datosAnio$ConsumoTotal, list(Hora=datosAnio$Hora ), FUN=mean)
    print(lines(mediasAnio$Hora, mediasAnio$x,type = 'l', col="blue", lwd=3))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", c("Media"),col=c("blue"),cex=0.7,fill=c("blue"))) 
  }
}

dev.off()

### CONSUMO COCINA ###

pdf("graficas/consumo_cocina/consumo_cocina_dias_semana.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,0)) #No margin on the right side
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(0,3000),type = "l"))
    for(m in unique(month(datosAnio$Fecha))){
      mes <- as.integer(format(as.Date(datosAnio[i, "Fecha"],format="%Y-%m-%d"), format = "%m"))
      datosMes <- datosAnio[month(datosAnio$Fecha) == m, ]
      for(d in unique(day(datosMes$Fecha))){
        dia <- as.integer(format(as.Date(datosMes[i, "Fecha"],format="%Y-%m-%d"), format = "%d"))
        datosDia <- datosMes[day(datosMes$Fecha) == d, ]
        print(lines(datosDia$Hora, datosDia$ConsumoCocina,type = 'l', col="grey"))
      }
    }
    mediasAnio <- aggregate(datosAnio$ConsumoCocina, list(Hora=datosAnio$Hora ), FUN=mean)
    print(lines(mediasAnio$Hora, mediasAnio$x,type = 'l', col="blue", lwd=3))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", c("Media"),col=c("blue"),cex=0.7,fill=c("blue"))) 
  }
}

dev.off()

### CONSUMO AIRE Y CALEFACCION ###

pdf("graficas/consumo_aire_calefaccion/consumo_aireycalefaccion_dias_semana.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,0)) #No margin on the right side
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(0,1500),type = "l"))
    for(m in unique(month(datosAnio$Fecha))){
      mes <- as.integer(format(as.Date(datosAnio[i, "Fecha"],format="%Y-%m-%d"), format = "%m"))
      datosMes <- datosAnio[month(datosAnio$Fecha) == m, ]
      for(d in unique(day(datosMes$Fecha))){
        dia <- as.integer(format(as.Date(datosMes[i, "Fecha"],format="%Y-%m-%d"), format = "%d"))
        datosDia <- datosMes[day(datosMes$Fecha) == d, ]
        print(lines(datosDia$Hora, datosDia$ConsumoAireyCalefaccion,type = 'l', col="grey"))
      }
    }
    mediasAnio <- aggregate(datosAnio$ConsumoAireyCalefaccion, list(Hora=datosAnio$Hora ), FUN=mean)
    print(lines(mediasAnio$Hora, mediasAnio$x,type = 'l', col="blue", lwd=3))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", c("Media"),col=c("blue"),cex=0.7,fill=c("blue"))) 
  }
}

dev.off()

### CONSUMO LAVANDERIA ###

pdf("graficas/consumo_lavanderia/consumo_lavanderia_dias_semana.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,0)) #No margin on the right side
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(0,3000),type = "l"))
    for(m in unique(month(datosAnio$Fecha))){
      mes <- as.integer(format(as.Date(datosAnio[i, "Fecha"],format="%Y-%m-%d"), format = "%m"))
      datosMes <- datosAnio[month(datosAnio$Fecha) == m, ]
      for(d in unique(day(datosMes$Fecha))){
        dia <- as.integer(format(as.Date(datosMes[i, "Fecha"],format="%Y-%m-%d"), format = "%d"))
        datosDia <- datosMes[day(datosMes$Fecha) == d, ]
        print(lines(datosDia$Hora, datosDia$ConsumoLavanderia,type = 'l', col="grey"))
      }
    }
    mediasAnio <- aggregate(datosAnio$ConsumoLavanderia, list(Hora=datosAnio$Hora ), FUN=mean)
    print(lines(mediasAnio$Hora, mediasAnio$x,type = 'l', col="blue", lwd=3))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", c("Media"),col=c("blue"),cex=0.7,fill=c("blue"))) 
  }
}

dev.off()



