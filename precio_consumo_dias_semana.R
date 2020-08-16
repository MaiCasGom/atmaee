library(zoo)
library(lubridate)

## TOTAL ##

pdf("graficas/consumo_total/precio_consumo_dias_semana.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    mediasConsumoAnio <- aggregate(datosAnio$ConsumoTotal, list(Hora=datosAnio$Hora ), FUN=mean)
    mediasCocinaAnio <- aggregate(datosAnio$ConsumoCocina, list(Hora=datosAnio$Hora ), FUN=mean)
    mediasAireAnio <- aggregate(datosAnio$ConsumoAireyCalefaccion, list(Hora=datosAnio$Hora ), FUN=mean)
    mediasLavanderiaAnio <- aggregate(datosAnio$ConsumoLavanderia, list(Hora=datosAnio$Hora ), FUN=mean)
    mediasPrecioAnio <- aggregate(datosAnio$Precio, list(Hora=datosAnio$Hora ), FUN=mean)

    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,3.5)) #No margin on the right side
    print(plot(mediasConsumoAnio$Hora, mediasConsumoAnio$x, main = paste(w, "Año", y, sep = " "),
               type = "l",col = "blue", ylab = "Consumo (Wh)", xlab = "Horas", ylim = c(0,3000)))
    print(lines(mediasCocinaAnio$Hora, mediasCocinaAnio$x,type = 'l', col= "red"))
    print(lines(mediasAireAnio$Hora, mediasAireAnio$x,type = 'l', col= "black"))
    print(lines(mediasLavanderiaAnio$Hora, mediasLavanderiaAnio$x,type = 'l', col= "orange"))
    print(par(new=TRUE))
    print(plot(mediasPrecioAnio$Hora, mediasPrecioAnio$x,
               type = "l",col = "green", ylim = c(0,100), xaxt="n",yaxt="n",xlab="",ylab=""))
    print(axis(4))
    print(mtext("Precio (EUR/MWh)",side=4,line=3))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", legend=(c("Precio", "Consumo total", "Consumo cocina", "Consumo aire/calefaccion", "Consumo lavanderia")),col=c("green", "blue", "red", "black", "orange"),cex=0.6,fill=c("green", "blue", "red", "black", "orange")))
    
  }
}

dev.off()

