anios <- sort(unique(year(DatosViviendaAgrupadosPorFecha$Fecha)))
pdf("graficas/consumo_mensual.pdf",width=6,height=4,paper='special')
for(y in anios){
  datosAnio <- subset(DatosViviendaAgrupadosPorFecha, year(DatosViviendaAgrupadosPorFecha$Fecha) == y) 
  meses <- sort(unique(month(datosAnio$Fecha)))
  
  for(m in meses){
    datosMes <- subset(datosAnio, month(datosAnio$Fecha) == m)
    print(quickplot(datosMes$Fecha ,datosMes$x , geom = c("line"), xlab = "", ylab = "Consumo (Wh)", ylim = c(0,35000), main = paste("Año", y, sep = " ")) +  
            geom_point()+ geom_smooth(level = 0.95))
  }
}
dev.off()