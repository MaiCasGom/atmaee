setwd("~/atmaee")
library(lubridate)
library(ggplot2)

files <- list.files(path="OMIE", pattern="*", full.names=TRUE, recursive=FALSE)

DatosOmie <- lapply(files, function(x) {
  t <- read.csv(x, header= FALSE, sep = ";", comment.char = "*", skip = 1)
 # t <- c(mean(t$V1), mean(t$V2), mean(t$V3), mean(t$V5))
})

DatosOmie <- do.call(rbind, DatosOmie)

colnames(DatosOmie) = c("Anio","Mes", "Dia", "Hora", "PrecioES")
DatosOmie$Hora <- sprintf("%02d", as.numeric(DatosOmie$Hora))
DatosOmie$Hora[DatosOmie$Hora=="24"] <- "00"

DatosOmie <- unique( DatosOmie[ , 1:5 ] )

## Cargando datos consumo ##

DatosVivienda= read.csv("household_power_consumption.txt", comment.char = "@", sep = ";", na.strings=c("?","NA"))
colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
                            "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
                            "IntensidadGlobal (A)","ConsumoCocina (Wh)","ConsumoLavanderia (Wh)","ConsumoAireyCalentador (Wh)")

DatosVivienda <- na.omit(DatosVivienda)
# Sumamos el consumo de cocina, lavanderia y aires acondicionados y si falta algún dato no lo tenemos en cuenta.
SumaTotal <- rowSums(DatosVivienda[, c(7, 8, 9)])
DatosVivienda$Fecha=as.Date(DatosVivienda$Fecha, format = "%d/%m/%Y")
DatosVivienda$Hora=substr(DatosVivienda$Hora, 1,2)

DatosViviendaAgrupados <- data.frame(fecha=DatosVivienda$Fecha, hora=DatosVivienda$Hora, consumo_total=SumaTotal)

SumaTotalPorFechaHora <- aggregate(DatosViviendaAgrupados$consumo_total, list(fecha=DatosViviendaAgrupados$fecha,hora=DatosViviendaAgrupados$hora ), FUN=sum)

for(i in 1:nrow(SumaTotalPorFechaHora)) {
  hora <- SumaTotalPorFechaHora[i, "hora"]
  dia <- as.integer(format(as.Date(SumaTotalPorFechaHora[i, "fecha"],format="%Y-%m-%d"), format = "%d"))
  mes <- as.integer(format(as.Date(SumaTotalPorFechaHora[i, "fecha"],format="%Y-%m-%d"), format = "%m"))
  anio <- as.integer(format(as.Date(SumaTotalPorFechaHora[i, "fecha"],format="%Y-%m-%d"), format = "%Y"))
  
  precio <- DatosOmie[DatosOmie$Anio == anio & 
                    DatosOmie$Mes == mes & 
                    DatosOmie$Dia == dia & 
                    DatosOmie$Hora == hora, "PrecioES"]

  if (length(precio) == 0) {
    precio <- NA
  }

  SumaTotalPorFechaHora[i, "Precio"] <- precio
  SumaTotalPorFechaHora[i, "Gasto"] <- SumaTotalPorFechaHora[i, "x"] * precio / 1000000
  
}

