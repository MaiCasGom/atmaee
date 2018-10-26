setwd("/home/mai/Escritorio/TFG")
DatosVivienda= read.csv("household_power_consumption.txt", comment.char = "@", sep = ";", na.strings=c("?","NA"))
#colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
 #                    "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
#                     "IntensidadGlobal (A)","ConsumoCocina (w/h)","ConsumoLavanderia (w/h)","ConsumoAireyCalentador (w/h)")
head(DatosVivienda)
class(DatosViviendaAgrupados$date)
#dim(DatosVivienda)
#str(DatosVivienda)
#summary(DatosVivienda)
Suma_Total <- rowSums(DatosVivienda[, c(7, 8, 9)], na.rm = TRUE)
DatosViviendaAgrupados <- data.frame(date=DatosVivienda$Date, consumo_total=Suma_Total)
Suma_Total_by_date <- aggregate(DatosViviendaAgrupados$consumo_total, list(date=DatosViviendaAgrupados$date), FUN=sum)
Suma_Total_by_date$date=as.Date(Suma_Total_by_date$date, format = "%d/%m/%Y")
