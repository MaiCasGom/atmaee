DatosViviendaAgrupados <- subset( DatosViviendaAgrupados, select = -Fecha )
DatosViviendaAgrupados <- subset( DatosViviendaAgrupados, select = -Hora )
DatosViviendaRangos <-apply(DatosViviendaAgrupados,2,range)

DatosViviendaRangos <- mapply(DatosViviendaRangos, FUN=as.numeric)
DatosViviendaRangos <- matrix(data=DatosViviendaRangos, ncol=9, nrow=2)
colnames(DatosViviendaRangos) = c("ConsumoCocina","ConsumoLavanderia", "ConsumoAireyCalefaccion",
                            "ConsumoTotal", "Precio", "GastoCocina","GastoLavanderia","GastoAireyCalefaccion","GastoTotal")

Rangos <- DatosViviendaRangos[2,] - DatosViviendaRangos[1,]

# La distancia ponderada es igual a la distancia numérica al no existir variables binarias
dist_ponderada = dist(DatosViviendaAgrupados)
