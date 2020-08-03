library(zoo)
library(lubridate)

pdf("graficas/consumo_total/precio_gasto_dias_semana.pdf",width=6,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)

  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    print(plot(0,0,xlim = c(0,23),xlab="Horas",ylab="", main = paste(w, "Año", y, sep = " "), ylim=c(0,60),type = "l"))
    for(m in unique(month(datosAnio$Fecha))){
      mes <- as.integer(format(as.Date(datosAnio[i, "Fecha"],format="%Y-%m-%d"), format = "%m"))
      datosMes <- datosAnio[month(datosAnio$Fecha) == m, ]
      for(d in unique(day(datosMes$Fecha))){
        dia <- as.integer(format(as.Date(datosMes[i, "Fecha"],format="%Y-%m-%d"), format = "%d"))
        datosDia <- datosMes[day(datosMes$Fecha) == d, ]
        #print(lines(datosDia$Hora, datosDia$Precio,type = 'l', col= "red"))
        #print(lines(datosDia$Hora, datosDia$GastoTotal,type = 'l', col="green"))
      }
    }
    mediasGastosAnio <- aggregate(datosAnio$GastoTotal, list(Hora=datosAnio$Hora ), FUN=mean)
    print(lines(mediasGastosAnio$Hora, mediasGastosAnio$x,type = 'l', col="blue", lwd=3))
    mediasPrecioAnio <- aggregate(datosAnio$Precio, list(Hora=datosAnio$Hora ), FUN=mean)
    print(lines(mediasPrecioAnio$Hora, mediasPrecioAnio$x,type = 'l', col="green", lwd=3))
    print(legend("left", legend=(c("Precio", "Gasto")),col=c("green", "blue"),cex=0.8,fill=c("green", "blue")))
    
  }
}

dev.off()