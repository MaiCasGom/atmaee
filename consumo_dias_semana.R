
pdf("graficas/consumo_dias_semana.pdf",width=6,height=4,paper='special')

for(w in c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")){
  DatosDiaSemana <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == w, ]
  for(y in unique(year(DatosDiaSemana$Fecha))) {
    anio <- as.integer(format(as.Date(DatosDiaSemana[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
    datosAnio <- DatosDiaSemana[year(DatosDiaSemana$Fecha) == y, ]
    print(plot(0,0,xlim = c(0,23),ylab = "Consumo (Wh)", main = paste(w, "Año", y, sep = " "), ylim=c(0,4000),type = "l"))
    for(m in unique(month(datosAnio$Fecha))){
      mes <- as.integer(format(as.Date(datosAnio[i, "Fecha"],format="%Y-%m-%d"), format = "%m"))
      datosMes <- datosAnio[month(datosAnio$Fecha) == m, ]
      for(d in unique(day(datosMes$Fecha))){
        dia <- as.integer(format(as.Date(datosMes[i, "Fecha"],format="%Y-%m-%d"), format = "%d"))
        datosDia <- datosMes[day(datosMes$Fecha) == d, ]
        print(lines(datosDia$Hora, datosDia$ConsumoTotal,type = 'l'))
      }
    }
  }
}

dev.off()

