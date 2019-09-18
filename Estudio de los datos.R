DatosVivienda= read.csv("household_power_consumption.txt", comment.char = "@", sep = ";", na.strings=c("?","NA"))
colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
                    "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
                     "IntensidadGlobal (A)","ConsumoCocina (w/h)","ConsumoLavanderia (w/h)","ConsumoAireyCalentador (w/h)")
head(DatosVivienda)
dim(DatosVivienda)
str(DatosVivienda)
summary(DatosVivienda)