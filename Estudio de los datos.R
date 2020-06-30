setwd("~/atmaee")
DatosVivienda = read.csv("household_power_consumption.txt", comment.char = "@", sep = ";", na.strings=c("?","NA"))
DatosVivienda <- na.omit(DatosVivienda)

colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
                            "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
                            "IntensidadGlobal (A)","ConsumoCocina (w/h)","ConsumoLavanderia (w/h)","ConsumoAireyCalentador (w/h)")
head(DatosVivienda)
dim(DatosVivienda)
str(DatosVivienda)

rangos <- apply(DatosVivienda, 2, range)
rownames(rangos) <- c("min","max")
rangos
summary(DatosVivienda)

DatosVivienda <- subset( DatosVivienda, select = -Fecha )
DatosVivienda <- subset( DatosVivienda, select = -Hora )

Varianza<- apply(DatosVivienda, 2, var)
Desviacion <- apply(DatosVivienda, 2, sd) 
CV <- function(x) sd(x)/mean(x) 
CoeficienteVariacion <- apply(DatosVivienda, 2, CV) 
RangoIntercuartilico <- apply(DatosVivienda, 2, IQR) 
Estadisticos = rbind(Varianza,Desviacion,CoeficienteVariacion,RangoIntercuartilico)
Estadisticos