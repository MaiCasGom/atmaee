SumaTotalPorFechaHora <- aggregate(DatosViviendaAgrupados$ConsumoTotal, list(Fecha=DatosViviendaAgrupados$Fecha,Hora=DatosViviendaAgrupados$Hora ), FUN=sum)
SumaTotalPorFechaHora$Tiempo=as.POSIXct(paste(SumaTotalPorFechaHora$Fecha, paste(SumaTotalPorFechaHora$Hora, ":00:00", sep = "")), format="%Y-%m-%d %H:%M:%S")

anios <- sort(unique(year(SumaTotalPorFechaHora$Fecha)))
pdf("graficas/consumo_mensual_por_horas.pdf",width=6,height=4,paper='special')
for(y in anios){
  datosAnio <- subset(SumaTotalPorFechaHora, year(SumaTotalPorFechaHora$Fecha) == y) 
  meses <- sort(unique(month(datosAnio$Fecha)))
  
  for(m in meses){
    datosMes <- subset(datosAnio, month(datosAnio$Fecha) == m)
    print(quickplot(datosMes$Tiempo ,datosMes$x , geom = c("line"), xlab = "", ylab = "Consumo (Wh)", ylim = c(0,4000), main = paste("Año", y, sep = " ")) +  
            geom_smooth(level = 0.90))
  }
}
dev.off()
