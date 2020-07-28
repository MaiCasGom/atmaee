SumaTotalPorFechaHora <- aggregate(DatosViviendaAgrupados$ConsumoTotal, list(Fecha=DatosViviendaAgrupados$Fecha,hora=DatosViviendaAgrupados$Hora ), FUN=sum)
SumaTotalPorFechaHora$Tiempo=as.POSIXct(paste(SumaTotalPorFechaHora$Fecha, SumaTotalPorFechaHora$Hora), format="%Y-%m-%d %H:%M:%S")

anios <- sort(unique(year(SumaTotalPorFechaHora$Fecha)))
pdf("graficas/curva_monotona_de_carga.pdf",width=6,height=4,paper='special')
for(y in anios){
  datosAnio <- subset(SumaTotalPorFechaHora, year(SumaTotalPorFechaHora$Fecha) == y) 
  datosAnio <- datosAnio[order(-datosAnio$x),]
  print(quickplot(1:nrow(datosAnio) ,datosAnio$x , geom = c("line"), xlab = "Horas", ylab = "Consumo (Wh)", ylim = c(0,3500), main = paste("Año", y, sep = " ")))
}
dev.off()
