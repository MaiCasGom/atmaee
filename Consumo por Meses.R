library(lubridate)
library(ggplot2)
setwd("~/atmaee")
DatosVivienda = read.csv("household_power_consumption.txt", comment.char = "@", sep = ";", na.strings=c("?","NA"))
DatosVivienda <- na.omit(DatosVivienda)

colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
                            "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
                            "IntensidadGlobal (A)","ConsumoCocina (w/h)","ConsumoLavanderia (w/h)","ConsumoAireyCalentador (w/h)")

# Sumamos el consumo de cocina, lavanderia y aires acondicionados y si falta algun dato no lo tenemos en cuenta.
SumaTotal <- rowSums(DatosVivienda[, c(7, 8, 9)])

DatosViviendaAgrupados <- data.frame(date=DatosVivienda$Fecha, consumo_total=SumaTotal)

SumaTotalPorFecha <- aggregate(DatosViviendaAgrupados$consumo_total, list(date=DatosViviendaAgrupados$date), FUN=sum)
SumaTotalPorFecha$date=as.Date(SumaTotalPorFecha$date, format = "%d/%m/%Y")

anios <- sort(unique(year(SumaTotalPorFecha$date)))
pdf("Consumo mensual.pdf",width=6,height=4,paper='special')
for(y in anios){
  datosAnio <- subset(SumaTotalPorFecha, year(SumaTotalPorFecha$date) == y) 
  meses <- sort(unique(month(datosAnio$date)))
  
  for(m in meses){
    datosMes <- subset(datosAnio, month(datosAnio$date) == m)
    print(quickplot(datosMes$date ,datosMes$x , geom = c("line", "smooth"), xlab = "", ylab = "Consumo (w/h)", ylim = c(-10000,35000), main = paste("Año", y, sep = " ")) +  geom_point())
  }
}
dev.off()
