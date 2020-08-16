library(zoo)
library(lubridate)

## TOTAL ##

pdf("graficas/consumo_total/gasto_consumo_dias_semana.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    mediasConsumoAnio <- aggregate(datosAnio$ConsumoTotal, list(Hora=datosAnio$Hora ), FUN=mean)
    mediasGastoAnio <- aggregate(datosAnio$GastoTotal, list(Hora=datosAnio$Hora ), FUN=mean)
    
    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,0)) #No margin on the right side
    print(par(mar=c(5,4,4,5)+.1))
    print(plot(mediasConsumoAnio$Hora, mediasConsumoAnio$x, main = paste(w, "Año", y, sep = " "),
               type = "l",col = "blue", ylab = "Consumo (Wh)", xlab = "Horas", ylim = c(0,3000)))
    print(par(new=TRUE))
    print(plot(mediasGastoAnio$Hora, mediasGastoAnio$x,
               type = "l",col = "green", ylim = c(0,0.1), xaxt="n",yaxt="n",xlab="",ylab=""))
    print(axis(4))
    print(mtext("Gasto (EUR/h)",side=4,line=3))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", legend=(c("Gasto medio", "Consumo medio")),col=c("green", "blue"),cex=0.8,fill=c("green", "blue")))
  }
}

dev.off()

## AIRE Y CALEFACCION ##

pdf("graficas/consumo_aire_calefaccion/gasto_consumo_dias_semana.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    mediasConsumoAnio <- aggregate(datosAnio$ConsumoAireyCalefaccion, list(Hora=datosAnio$Hora ), FUN=mean)
    mediasGastoAnio <- aggregate(datosAnio$GastoAireyCalefaccion, list(Hora=datosAnio$Hora ), FUN=mean)

    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,0)) #No margin on the right side
    print(par(mar=c(5,4,4,5)+.1))
    print(plot(mediasConsumoAnio$Hora, mediasConsumoAnio$x, main = paste(w, "Año", y, sep = " "),
               type = "l",col = "blue", ylab = "Consumo (Wh)", xlab = "Horas", ylim = c(0,2500)))
    print(par(new=TRUE))
    print(plot(mediasGastoAnio$Hora, mediasGastoAnio$x,
               type = "l",col = "green", ylim = c(0,0.1), xaxt="n",yaxt="n",xlab="",ylab=""))
    print(axis(4))
    print(mtext("Gasto (EUR/h)",side=4,line=3))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", legend=(c("Gasto medio", "Consumo medio")),col=c("green", "blue"),cex=0.8,fill=c("green", "blue")))
    
  }
}

dev.off()

## COCINA ##

pdf("graficas/consumo_cocina/gasto_consumo_dias_semana.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    mediasConsumoAnio <- aggregate(datosAnio$ConsumoCocina, list(Hora=datosAnio$Hora ), FUN=mean)
    mediasGastoAnio <- aggregate(datosAnio$GastoCocina, list(Hora=datosAnio$Hora ), FUN=mean)

    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,0)) #No margin on the right side
    print(par(mar=c(5,4,4,5)+.1))
    print(plot(mediasConsumoAnio$Hora, mediasConsumoAnio$x, main = paste(w, "Año", y, sep = " "),
               type = "l",col = "blue", ylab = "Consumo (Wh)", xlab = "Horas", ylim = c(0,1000)))
    print(par(new=TRUE))
    print(plot(mediasGastoAnio$Hora, mediasGastoAnio$x,
               type = "l",col = "green", ylim = c(0,0.05), xaxt="n",yaxt="n",xlab="",ylab=""))
    print(axis(4))
    print(mtext("Gasto (EUR/h)",side=4,line=3))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", legend=(c("Gasto medio", "Consumo medio")),col=c("green", "blue"),cex=0.8,fill=c("green", "blue")))
    
  }
}

dev.off()

## TOTAL ##

pdf("graficas/consumo_lavanderia/gasto_consumo_dias_semana.pdf",width=7,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  DatosDiaSemana <- na.omit(DatosDiaSemana)
  
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]

    layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
    par(mar=c(5,5,3,0)) #No margin on the right side
    mediasConsumoAnio <- aggregate(datosAnio$ConsumoLavanderia, list(Hora=datosAnio$Hora ), FUN=mean)
    mediasGastoAnio <- aggregate(datosAnio$GastoLavanderia, list(Hora=datosAnio$Hora ), FUN=mean)
    print(par(mar=c(5,4,4,5)+.1))
    print(plot(mediasConsumoAnio$Hora, mediasConsumoAnio$x, main = paste(w, "Año", y, sep = " "),
               type = "l",col = "blue", ylab = "Consumo (Wh)", xlab = "Horas", ylim = c(0,1000)))
    print(par(new=TRUE))
    print(plot(mediasGastoAnio$Hora, mediasGastoAnio$x,
               type = "l",col = "green", ylim = c(0,0.05), xaxt="n",yaxt="n",xlab="",ylab=""))
    print(axis(4))
    print(mtext("Gasto (EUR/h)",side=4,line=3))
    par(mar=c(5,1,3,0)) #No margin on the right side
    print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
    print(legend("left", legend=(c("Gasto medio", "Consumo medio")),col=c("green", "blue"),cex=0.8,fill=c("green", "blue")))
    
  }
}

dev.off()