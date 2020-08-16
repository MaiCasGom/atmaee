library(ggplot2)
library(lubridate)

setwd("~/atmaee")
DatosVivienda = read.csv("household_power_consumption.txt", comment.char = "@", sep = ";", na.strings=c("?","NA"))
DatosVivienda <- na.omit(DatosVivienda)

colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
                            "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
                            "IntensidadGlobal (A)","ConsumoCocina (Wh)","ConsumoLavanderia (Wh)","ConsumoAireyCalentador (Wh)")

DatosVivienda$Fecha=as.Date(DatosVivienda$Fecha, format = "%d/%m/%Y")
DatosVivienda$Hora=substr(DatosVivienda$Hora, 1,2)

ConsumoCocinaAgrupado <- aggregate(DatosVivienda$`ConsumoCocina (Wh)`, list(Fecha=DatosVivienda$Fecha, Hora=DatosVivienda$Hora ), FUN=sum)
ConsumoLavanderiaAgrupado <- aggregate(DatosVivienda$`ConsumoLavanderia (Wh)`, list(Fecha=DatosVivienda$Fecha, Hora=DatosVivienda$Hora ), FUN=sum)
ConsumoAireAgrupado <- aggregate(DatosVivienda$`ConsumoAireyCalentador (Wh)`, list(Fecha=DatosVivienda$Fecha, Hora=DatosVivienda$Hora ), FUN=sum)


DatosViviendaAgrupados <- data.frame(Fecha=ConsumoCocinaAgrupado$Fecha, 
                                     Hora=ConsumoCocinaAgrupado$Hora, 
                                     ConsumoCocina=ConsumoCocinaAgrupado$x,
                                     ConsumoLavanderia=ConsumoLavanderiaAgrupado$x,
                                     ConsumoAireyCalefaccion=ConsumoAireAgrupado$x)

DatosViviendaAgrupados$ConsumoTotal <- rowSums(DatosViviendaAgrupados[, c(3, 4, 5)])


files <- list.files(path="OMIE", pattern="*", full.names=TRUE, recursive=FALSE)

DatosOmie <- lapply(files, function(x) {
  t <- read.csv(x, header= FALSE, sep = ";", comment.char = "*", skip = 1)
})

DatosOmie <- do.call(rbind, DatosOmie)

colnames(DatosOmie) = c("Anio","Mes", "Dia", "Hora", "PrecioES")
DatosOmie$Hora <- sprintf("%02d", as.numeric(DatosOmie$Hora))
DatosOmie$Hora[DatosOmie$Hora=="24"] <- "00"

DatosOmie <- unique( DatosOmie[ , 1:5 ] )

for(i in 1:nrow(DatosViviendaAgrupados)) {
  hora <- DatosViviendaAgrupados[i, "Hora"]
  dia <- as.integer(format(as.Date(DatosViviendaAgrupados[i, "Fecha"],format="%Y-%m-%d"), format = "%d"))
  mes <- as.integer(format(as.Date(DatosViviendaAgrupados[i, "Fecha"],format="%Y-%m-%d"), format = "%m"))
  anio <- as.integer(format(as.Date(DatosViviendaAgrupados[i, "Fecha"],format="%Y-%m-%d"), format = "%Y"))
  
  precio <- DatosOmie[DatosOmie$Anio == anio & 
                        DatosOmie$Mes == mes & 
                        DatosOmie$Dia == dia & 
                        DatosOmie$Hora == hora, "PrecioES"]
  
  if (length(precio) == 0) {
    precio <- NA
  }
  
  if (anio < 2010) {
    precio <- precio * 10
  }
  
  if (anio != 2006 ){
    DatosViviendaAgrupados[i, "Precio"] <- precio
    DatosViviendaAgrupados[i, "GastoCocina"] <- DatosViviendaAgrupados[i, "ConsumoCocina"] * precio / 1000000
    DatosViviendaAgrupados[i, "GastoLavanderia"] <- DatosViviendaAgrupados[i, "ConsumoLavanderia"] * precio / 1000000
    DatosViviendaAgrupados[i, "GastoAireyCalefaccion"] <- DatosViviendaAgrupados[i, "ConsumoAireyCalefaccion"] * precio / 1000000
    DatosViviendaAgrupados[i, "GastoTotal"] <- DatosViviendaAgrupados[i, "ConsumoTotal"] * precio / 1000000
  }
}

DatosViviendaAgrupados <- na.omit(DatosViviendaAgrupados)

DatosViviendaAgrupadosPorFecha <- aggregate(DatosViviendaAgrupados$ConsumoTotal, list(Fecha=DatosViviendaAgrupados$Fecha), FUN=sum)
DatosCocinaAgrupadosPorFecha <- aggregate(DatosViviendaAgrupados$ConsumoCocina, list(Fecha=DatosViviendaAgrupados$Fecha), FUN=sum)
DatosLavanderiaAgrupadosPorFecha <- aggregate(DatosViviendaAgrupados$ConsumoLavanderia, list(Fecha=DatosViviendaAgrupados$Fecha), FUN=sum)
DatosAireCalefaccionAgrupadosPorFecha <- aggregate(DatosViviendaAgrupados$ConsumoAireyCalefaccion, list(Fecha=DatosViviendaAgrupados$Fecha), FUN=sum)
