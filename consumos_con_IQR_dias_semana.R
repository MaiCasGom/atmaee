library(zoo)
library(lubridate)

### CONSUMO TOTAL ###

pdf("graficas/consumo_total/consumo_total_con_IQR_dias_semana.pdf",width=6,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(0,4000),type = "l"))
    for(m in unique(month(datosAnio$Fecha))){
      mes <- as.integer(format(as.Date(datosAnio[i, "Fecha"],format="%Y-%m-%d"), format = "%m"))
      datosMes <- datosAnio[month(datosAnio$Fecha) == m, ]
      for(d in unique(day(datosMes$Fecha))){
        dia <- as.integer(format(as.Date(datosMes[i, "Fecha"],format="%Y-%m-%d"), format = "%d"))
        datosDia <- datosMes[day(datosMes$Fecha) == d, ]
        print(lines(datosDia$Hora, datosDia$ConsumoTotal,type = 'l'))
      }
    }
    mediasAnio <- aggregate(datosAnio$ConsumoTotal, list(Hora=datosAnio$Hora ), FUN=IQR)
    print(lines(mediasAnio$Hora, mediasAnio$x,type = 'l', col="blue", lwd=3))
  }
}

dev.off()



### CONSUMO COCINA ###

pdf("graficas/consumo_cocina/consumo_cocina_con_IQR_dias_semana.pdf",width=6,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(0,2500),type = "l"))
    for(m in unique(month(datosAnio$Fecha))){
      mes <- as.integer(format(as.Date(datosAnio[i, "Fecha"],format="%Y-%m-%d"), format = "%m"))
      datosMes <- datosAnio[month(datosAnio$Fecha) == m, ]
      for(d in unique(day(datosMes$Fecha))){
        dia <- as.integer(format(as.Date(datosMes[i, "Fecha"],format="%Y-%m-%d"), format = "%d"))
        datosDia <- datosMes[day(datosMes$Fecha) == d, ]
        print(lines(datosDia$Hora, datosDia$ConsumoCocina,type = 'l'))
      }
    }
    mediasAnio <- aggregate(datosAnio$ConsumoCocina, list(Hora=datosAnio$Hora ), FUN=IQR)
    print(lines(mediasAnio$Hora, mediasAnio$x,type = 'l', col="blue", lwd=3))
  }
}

dev.off()



### CONSUMO AIRE Y CALEFACCION ###


pdf("graficas/consumo_aire_calefaccion/consumo_aireycalefaccion_con_IQR_dias_semana.pdf",width=6,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(0,2000),type = "l"))
    for(m in unique(month(datosAnio$Fecha))){
      mes <- as.integer(format(as.Date(datosAnio[i, "Fecha"],format="%Y-%m-%d"), format = "%m"))
      datosMes <- datosAnio[month(datosAnio$Fecha) == m, ]
      for(d in unique(day(datosMes$Fecha))){
        dia <- as.integer(format(as.Date(datosMes[i, "Fecha"],format="%Y-%m-%d"), format = "%d"))
        datosDia <- datosMes[day(datosMes$Fecha) == d, ]
        print(lines(datosDia$Hora, datosDia$ConsumoAireyCalefaccion,type = 'l'))
      }
    }
    mediasAnio <- aggregate(datosAnio$ConsumoAireyCalefaccion, list(Hora=datosAnio$Hora ), FUN=IQR)
    print(lines(mediasAnio$Hora, mediasAnio$x,type = 'l', col="blue", lwd=3))
  }
}

dev.off()



### CONSUMO LAVANDERIA ###



pdf("graficas/consumo_lavanderia/consumo_lavanderia_con_IQR_dias_semana.pdf",width=6,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(0,3000),type = "l"))
    for(m in unique(month(datosAnio$Fecha))){
      mes <- as.integer(format(as.Date(datosAnio[i, "Fecha"],format="%Y-%m-%d"), format = "%m"))
      datosMes <- datosAnio[month(datosAnio$Fecha) == m, ]
      for(d in unique(day(datosMes$Fecha))){
        dia <- as.integer(format(as.Date(datosMes[i, "Fecha"],format="%Y-%m-%d"), format = "%d"))
        datosDia <- datosMes[day(datosMes$Fecha) == d, ]
        print(lines(datosDia$Hora, datosDia$ConsumoLavanderia,type = 'l'))
      }
    }
    mediasAnio <- aggregate(datosAnio$ConsumoLavanderia, list(Hora=datosAnio$Hora ), FUN=IQR)
    print(lines(mediasAnio$Hora, mediasAnio$x,type = 'l', col="blue", lwd=3))
  }
}

dev.off()



