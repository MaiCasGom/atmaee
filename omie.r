setwd("~/atmaee")
library(lubridate)
library(ggplot2)

files <- list.files(path="OMIE", pattern="*", full.names=TRUE, recursive=FALSE)

DatosOmie <- lapply(files, function(x) {
  t <- read.csv(x, header= FALSE, sep = ";", comment.char = "*", skip = 1)
  t <- c(mean(t$V1), mean(t$V2), mean(t$V3), mean(t$V5))
})

DatosOmie <- do.call(rbind, DatosOmie)

colnames(DatosOmie) = c("Anio","Mes", "Dia", "PrecioES")


## Cargando datos consumo ##

DatosVivienda= read.csv("household_power_consumption.txt", comment.char = "@", sep = ";", na.strings=c("?","NA"))
colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
                            "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
                            "IntensidadGlobal (A)","ConsumoCocina (w/h)","ConsumoLavanderia (w/h)","ConsumoAireyCalentador (w/h)")

DatosVivienda <- na.omit(DatosVivienda)
# Sumamos el consumo de cocina, lavanderia y aires acondicionados y si falta algún dato no lo tenemos en cuenta.
Suma_Total <- rowSums(DatosVivienda[, c(7, 8, 9)], na.rm = TRUE)

DatosViviendaAgrupados <- data.frame(date=DatosVivienda$Fecha, consumo_total=Suma_Total)

SumaTotalPorFecha <- aggregate(DatosViviendaAgrupados$consumo_total, list(date=DatosViviendaAgrupados$date), FUN=sum)
SumaTotalPorFecha$date=as.Date(SumaTotalPorFecha$date, format = "%d/%m/%Y")

for(i in 1:nrow(SumaTotalPorFecha)) {
  dia <- as.integer(format(as.Date(SumaTotalPorFecha[i, "date"],format="%Y-%m-%d"), format = "%d"))
  mes <- as.integer(format(as.Date(SumaTotalPorFecha[i, "date"],format="%Y-%m-%d"), format = "%m"))
  anio <- as.integer(format(as.Date(SumaTotalPorFecha[i, "date"],format="%Y-%m-%d"), format = "%Y"))
  for(j in 1:nrow(DatosOmie)) {
    if (DatosOmie[j, "Anio"] == anio & DatosOmie[j, "Mes"] == mes & DatosOmie[j, "Dia"] == dia) {
      SumaTotalPorFecha[i, "Precio"] <- DatosOmie[j, "PrecioES"]
      SumaTotalPorFecha[i, "Gasto"] <- SumaTotalPorFecha[i, "x"] * DatosOmie[j, "PrecioES"] / 1000
    }
  }
}
