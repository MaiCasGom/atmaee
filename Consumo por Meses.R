library(lubridate)
library(ggplot2)
DatosVivienda= read.csv("household_power_consumption.txt", comment.char = "@", sep = ";", na.strings=c("?","NA"))
colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
                    "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
                     "IntensidadGlobal (A)","ConsumoCocina (w/h)","ConsumoLavanderia (w/h)","ConsumoAireyCalentador (w/h)")
# Sumamos el consumo de cocina, lavanderia y aires acondicionados y si falta algún dato no lo tenemos en cuenta.
Suma_Total <- rowSums(DatosVivienda[, c(7, 8, 9)], na.rm = TRUE)

DatosViviendaAgrupados <- data.frame(date=DatosVivienda$Fecha, consumo_total=Suma_Total)

Suma_Total_by_date <- aggregate(DatosViviendaAgrupados$consumo_total, list(date=DatosViviendaAgrupados$date), FUN=sum)
Suma_Total_by_date$date=as.Date(Suma_Total_by_date$date, format = "%d/%m/%Y")

years <- sort(unique(year(Suma_Total_by_date$date)))
pdf("Consumo por meses.pdf",width=6,height=4,paper='special')
for(y in years){
  year_data <- subset(Suma_Total_by_date, year(Suma_Total_by_date$date) == y) 
  months <- sort(unique(month(year_data$date)))
  
  for(m in months){
    month_data <- subset(year_data, month(year_data$date) == m)
    print(quickplot(month_data$date ,month_data$x , geom = "line", xlab = "", ylab = "Consumo (w/h)", main = paste("Año", y, sep = " ")))
  }
}
dev.off()
