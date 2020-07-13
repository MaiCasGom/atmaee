setwd("~/atmaee")
DatosVivienda = read.csv("household_power_consumption.txt", comment.char = "@", sep = ";", na.strings=c("?","NA"))
DatosVivienda <- na.omit(DatosVivienda)

colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
                            "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
                            "IntensidadGlobal (A)","ConsumoCocina (Wh)","ConsumoLavanderia (Wh)","ConsumoAireyCalentador (Wh)")

class(DatosVivienda) # Tipo de datos del dataset
head(DatosVivienda) 
dim(DatosVivienda)
str(DatosVivienda)

rangos <- apply(DatosVivienda, 2, range)
rownames(rangos) <- c("min","max")
print(rangos)

summary(DatosVivienda)

DatosVivienda <- subset( DatosVivienda, select = -Fecha )
DatosVivienda <- subset( DatosVivienda, select = -Hora )

Varianza<- apply(DatosVivienda, 2, var)
Desviacion <- apply(DatosVivienda, 2, sd) 
CV <- function(x) sd(x)/mean(x) 
CoeficienteVariacion <- apply(DatosVivienda, 2, CV) 
RangoIntercuartilico <- apply(DatosVivienda, 2, IQR) 
Estadisticos = rbind(Varianza,Desviacion,CoeficienteVariacion,RangoIntercuartilico)
print(Estadisticos)

boxplot(DatosVivienda$Global_active_power,
        main = "Potencia Activa Global (kw)", names = c("PotenciaActivaGlobal"))
boxplot(DatosVivienda$Global_reactive_power,
        main = "Potencia Reactiva Global (kw)", names = c("PotenciaReactivaGlobal"))
boxplot(DatosVivienda$Voltage,
        main = "Voltaje (v)", names = c("Voltaje"))
boxplot(DatosVivienda$Global_intensity,
        main = "Intensidad Global (A)", names = c("IntensidadGlobal"))
boxplot(DatosVivienda$Sub_metering_1,DatosVivienda$Sub_metering_2,DatosVivienda$Sub_metering_3,
        main = "Consumos  (Wh)", names = c("Consumo Cocina","Consumo Lavandería", "Consumo Aire y Calentador"))
boxplot(DatosVivienda$Sub_metering_1,
        main = "Consumo cocina  (Wh)", names = c("Consumo Cocina"))
boxplot(DatosVivienda$Sub_metering_2,
        main = "Consumo Lavanderia  (Wh)", names = c("Consumo Lavandería"))
boxplot(DatosVivienda$Sub_metering_3,
        main = "Consumos aire y calentador (Wh)", names = c("Consumo Aire y Calentador"))