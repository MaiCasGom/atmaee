library(lubridate)
library(ggplot2)
setwd("~/atmaee")
DatosVivienda = read.csv("household_power_consumption.txt", comment.char = "@", sep = ";", na.strings=c("?","NA"))
DatosVivienda <- na.omit(DatosVivienda)

colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
                            "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
                            "IntensidadGlobal (A)","ConsumoCocina (Wh)","ConsumoLavanderia (Wh)","ConsumoAireyCalentador (Wh)")

# Sumamos el consumo de cocina, lavanderia y aires acondicionados y si falta algun dato no lo tenemos en cuenta.
SumaTotal <- rowSums(DatosVivienda[, c(7, 8, 9)])
DatosVivienda$Fecha=as.Date(DatosVivienda$Fecha, format = "%d/%m/%Y")
DatosVivienda$Hora=paste(substr(DatosVivienda$Hora, 1,2), ":00:00", sep = "")

DatosViviendaAgrupados <- data.frame(fecha=DatosVivienda$Fecha, hora=DatosVivienda$Hora, consumo_total=SumaTotal)

SumaTotalPorFechaHora <- aggregate(DatosViviendaAgrupados$consumo_total, list(fecha=DatosViviendaAgrupados$fecha,hora=DatosViviendaAgrupados$hora ), FUN=sum)
SumaTotalPorFechaHora$tiempo=as.POSIXct(paste(SumaTotalPorFechaHora$fecha, SumaTotalPorFechaHora$hora), format="%Y-%m-%d %H:%M:%S")

anios <- sort(unique(year(SumaTotalPorFechaHora$fecha)))
pdf("graficas/consumo_mensual_por_horas.pdf",width=6,height=4,paper='special')
for(y in anios){
  datosAnio <- subset(SumaTotalPorFechaHora, year(SumaTotalPorFechaHora$fecha) == y) 
  meses <- sort(unique(month(datosAnio$fecha)))
  
  for(m in meses){
    datosMes <- subset(datosAnio, month(datosAnio$fecha) == m)
    print(quickplot(datosMes$tiempo ,datosMes$x , geom = c("line"), xlab = "", ylab = "Consumo (Wh)", ylim = c(-1000,3000), main = paste("Año", y, sep = " ")) +  
            geom_smooth(level = 0.90))
  }
}
dev.off()
