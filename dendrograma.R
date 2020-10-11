DatosViviendaAgrupados <- subset( DatosViviendaAgrupados, select = -Fecha )
DatosViviendaAgrupados <- subset( DatosViviendaAgrupados, select = -Hora )
DatosViviendaRangos <-apply(DatosViviendaAgrupados,2,range)

DatosViviendaRangos <- mapply(DatosViviendaRangos, FUN=as.numeric)
DatosViviendaRangos <- matrix(data=DatosViviendaRangos, ncol=9, nrow=2)
colnames(DatosViviendaRangos) = c("ConsumoCocina","ConsumoLavanderia", "ConsumoAireyCalefaccion",
                            "ConsumoTotal", "Precio", "GastoCocina","GastoLavanderia","GastoAireyCalefaccion","GastoTotal")
## General ##
# La distancia ponderada es igual a la distancia numérica al no existir variables binarias
dist_ponderada = dist(DatosViviendaAgrupados)
hGeneral = hclust(dist_ponderada, method = "ward.D2")

## Consumo Cocina ##
DatosConsumoCocina <- DatosViviendaAgrupados$ConsumoCocina
dist_ponderada = dist(DatosConsumoCocina)
hCCocina = hclust(dist_ponderada, method = "ward.D2")

## Consumo Lavanderia ##
DatosConsumoLavanderia <- DatosViviendaAgrupados$ConsumoLavanderia
dist_ponderada = dist(DatosConsumoLavanderia)
hCLavanderia = hclust(dist_ponderada, method = "ward.D2")

## Consumo Aire y Calefaccion ##
DatosConsumoAireyCalefaccion <- DatosViviendaAgrupados$ConsumoAireyCalefaccion
dist_ponderada = dist(DatosConsumoAireyCalefaccion)
hCAireyCalefaccion = hclust(dist_ponderada, method = "ward.D2")

## Consumo Total ##
DatosConsumoTotal<- DatosViviendaAgrupados$ConsumoTotal
dist_ponderada = dist(DatosConsumoTotal)
hCTotal = hclust(dist_ponderada, method = "ward.D2")

## Consumo Cocina + Lavanderia + Aire y Calefacción ##
DatosConsumoCocinaLavanderiaAireCalefaccion <- subset( DatosViviendaAgrupados, select = c("ConsumoCocina", "ConsumoLavanderia", "ConsumoAireyCalefaccion") )
dist_ponderada = dist(DatosConsumoCocinaLavanderiaAireCalefaccion)
hCCocinaLavanderiaAireCalefaccion = hclust(dist_ponderada, method = "ward.D2")

## Precio ##
DatosPrecio<- DatosViviendaAgrupados$Precio
dist_ponderada = dist(DatosPrecio)
hCPrecio = hclust(dist_ponderada, method = "ward.D2")

## Gasto Cocina ##
DatosGastoCocina<- DatosViviendaAgrupados$GastoCocina
dist_ponderada = dist(DatosGastoCocina)
hGCocina = hclust(dist_ponderada, method = "ward.D2")

## Gasto Lavanderia ##
DatosGastoLavanderia<- DatosViviendaAgrupados$GastoLavanderia
dist_ponderada = dist(DatosGastoLavanderia)
hGLavanderia = hclust(dist_ponderada, method = "ward.D2")

## Gasto AireyCalefaccion ##
DatosGastoAireyCalefaccion<- DatosViviendaAgrupados$GastoAireyCalefaccion
dist_ponderada = dist(DatosGastoAireyCalefaccion)
hGAireyCalefaccion = hclust(dist_ponderada, method = "ward.D2")

## Gasto Total ##
DatosGastoTotal<- DatosViviendaAgrupados$GastoTotal
dist_ponderada = dist(DatosGastoTotal)
hGTotal = hclust(dist_ponderada, method = "ward.D2")

## Gasto Cocina + Lavanderia + Aire y Calefacción ##
DatosGastooCocinaLavanderiaAireCalefaccion <- subset( DatosViviendaAgrupados, select = c("GastoCocina", "GastoLavanderia", "GastoAireyCalefaccion") )
dist_ponderada = dist(DatosGastooCocinaLavanderiaAireCalefaccion)
hGCocinaLavanderiaAireCalefaccion = hclust(dist_ponderada, method = "ward.D2")

pdf("graficas/dendrogramas.pdf",width=6,height=4,paper='special')
plot(hGeneral, main="Dendrograma general")
plot(hCCocina, main="Dendrograma Consumo Cocina")
plot(hCLavanderia, main="Dendrograma Consumo Lavanderia")
plot(hCAireyCalefaccion, main="Dendrograma Consumo Aire y Calefaccion")
plot(hCTotal, main="Dendrograma Consumos totales")
plot(hCCocinaLavanderiaAireCalefaccion, main="Dendrograma general de consumos")
plot(hPrecio, main="Dendrograma Precio")
plot(hGCocina, main="Dendrograma Gasto Cocina")
plot(hGLavanderia, main="Dendrograma Gasto Lavanderia")
plot(hGAireyCalefaccion, main="Dendrograma Gasto Aire y Calefaccion")
plot(hGTotal, main="Dendrograma Gasto total")
plot(hGCocinaLavanderiaAireCalefaccion, main="Dendrograma general de gastos")
dev.off()
