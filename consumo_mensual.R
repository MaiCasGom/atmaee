
anios <- sort(unique(year(DatosViviendaAgrupadosPorFecha$Fecha)))

## CONSUMO TOTAL ##
pdf("graficas/consumo_total/consumo_mensual.pdf",width=6,height=4,paper='special')
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

## CONSUMO COCINA ##
pdf("graficas/consumo_cocina/consumo_mensual.pdf",width=6,height=4,paper='special')
for(y in anios){
  datosAnio <- subset(DatosCocinaAgrupadosPorFecha, year(DatosCocinaAgrupadosPorFecha$Fecha) == y) 
  meses <- sort(unique(month(datosAnio$Fecha)))
  
  for(m in meses){
    datosMes <- subset(datosAnio, month(datosAnio$Fecha) == m)
    print(quickplot(datosMes$Fecha ,datosMes$x , geom = c("line"), xlab = "", ylab = "Consumo (Wh)", ylim = c(0,35000), main = paste("Año", y, sep = " ")) +  
            geom_point()+ geom_smooth(level = 0.95))
  }
}
dev.off()

## CONSUMO LAVANDERIA ##
pdf("graficas/consumo_lavanderia/consumo_mensual.pdf",width=6,height=4,paper='special')
for(y in anios){
  datosAnio <- subset(DatosLavanderiaAgrupadosPorFecha, year(DatosLavanderiaAgrupadosPorFecha$Fecha) == y) 
  meses <- sort(unique(month(datosAnio$Fecha)))
  
  for(m in meses){
    datosMes <- subset(datosAnio, month(datosAnio$Fecha) == m)
    print(quickplot(datosMes$Fecha ,datosMes$x , geom = c("line"), xlab = "", ylab = "Consumo (Wh)", ylim = c(0,15000), main = paste("Año", y, sep = " ")) +  
            geom_point()+ geom_smooth(level = 0.95))
  }
}
dev.off()

## CONSUMO AIRE Y CALEFACCION ##
pdf("graficas/consumo_aire_calefaccion/consumo_mensual.pdf",width=6,height=4,paper='special')
for(y in anios){
  datosAnio <- subset(DatosAireCalefaccionAgrupadosPorFecha, year(DatosAireCalefaccionAgrupadosPorFecha$Fecha) == y) 
  meses <- sort(unique(month(datosAnio$Fecha)))
  
  for(m in meses){
    datosMes <- subset(datosAnio, month(datosAnio$Fecha) == m)
    print(quickplot(datosMes$Fecha ,datosMes$x , geom = c("line"), xlab = "", ylab = "Consumo (Wh)", ylim = c(0,35000), main = paste("Año", y, sep = " ")) +  
            geom_point()+ geom_smooth(level = 0.95))
  }
}
dev.off()