###### Funcion para normalizacion ####

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

######################################

library(cluster)
Horas <- DatosViviendaAgrupados$Hora

DatosViviendaAgrupados <- subset( DatosViviendaAgrupados, select = -Fecha )
DatosViviendaAgrupados <- subset( DatosViviendaAgrupados, select = -Hora )

DatosViviendaAgrupadosNorm <- as.data.frame(lapply(DatosViviendaAgrupados[1:9], min_max_norm))
DatosViviendaAgrupadosNorm$Hora <- Horas
#DatosViviendaRangos <-apply(DatosViviendaAgrupadosNorm,2,range)

#DatosViviendaRangos <- mapply(DatosViviendaRangos, FUN=as.numeric)
#DatosViviendaRangos <- matrix(data=DatosViviendaRangos, ncol=9, nrow=2)
#colnames(DatosViviendaRangos) = c("ConsumoCocina","ConsumoLavanderia", "ConsumoAireyCalefaccion",
#                            "ConsumoTotal", "Precio", "GastoCocina","GastoLavanderia","GastoAireyCalefaccion","GastoTotal")
## General ##
# La distancia ponderada es igual a la distancia numérica al no existir variables binarias
dist_ponderada = dist(DatosViviendaAgrupadosNorm)
hGeneral = hclust(dist_ponderada, method = "ward.D2")

## Consumo Cocina ##
DatosConsumoCocina <- subset( DatosViviendaAgrupadosNorm, select = c(ConsumoCocina,Hora) )
dist_ponderada = dist(DatosConsumoCocina)
hCCocina = hclust(dist_ponderada, method = "ward.D2")

## Consumo Lavanderia ##
DatosConsumoLavanderia <- subset( DatosViviendaAgrupadosNorm, select = c(ConsumoLavanderia,Hora) )
dist_ponderada = dist(DatosConsumoLavanderia)
hCLavanderia = hclust(dist_ponderada, method = "ward.D2")

## Consumo Aire y Calefaccion ##
DatosConsumoAireyCalefaccion <- subset( DatosViviendaAgrupadosNorm, select = c(ConsumoAireyCalefaccion,Hora) )
dist_ponderada = dist(DatosConsumoAireyCalefaccion)
hCAireyCalefaccion = hclust(dist_ponderada, method = "ward.D2")

## Consumo Total ##
DatosConsumoTotal<- subset( DatosViviendaAgrupadosNorm, select = c(ConsumoTotal,Hora) )
dist_ponderada = dist(DatosConsumoTotal)
hCTotal = hclust(dist_ponderada, method = "ward.D2")

## Consumo Cocina + Lavanderia + Aire y Calefacción ##
DatosConsumoCocinaLavanderiaAireCalefaccion <- subset( DatosViviendaAgrupadosNorm, select = c("ConsumoCocina", "ConsumoLavanderia", "ConsumoAireyCalefaccion", "Hora") )
dist_ponderada = dist(DatosConsumoCocinaLavanderiaAireCalefaccion)
hCCocinaLavanderiaAireCalefaccion = hclust(dist_ponderada, method = "ward.D2")

## Precio ##
DatosPrecio<- subset( DatosViviendaAgrupadosNorm, select = c(Precio,Hora) )
dist_ponderada = dist(DatosPrecio)
hPrecio = hclust(dist_ponderada, method = "ward.D2")

# ## Gasto Cocina ##
# DatosGastoCocina<- DatosViviendaAgrupadosNorm$GastoCocina
# dist_ponderada = dist(DatosGastoCocina)
# hGCocina = hclust(dist_ponderada, method = "ward.D2")
# 
# ## Gasto Lavanderia ##
# DatosGastoLavanderia<- DatosViviendaAgrupadosNorm$GastoLavanderia
# dist_ponderada = dist(DatosGastoLavanderia)
# hGLavanderia = hclust(dist_ponderada, method = "ward.D2")
# 
# ## Gasto AireyCalefaccion ##
# DatosGastoAireyCalefaccion<- DatosViviendaAgrupadosNorm$GastoAireyCalefaccion
# dist_ponderada = dist(DatosGastoAireyCalefaccion)
# hGAireyCalefaccion = hclust(dist_ponderada, method = "ward.D2")
# 
# ## Gasto Total ##
# DatosGastoTotal<- DatosViviendaAgrupadosNorm$GastoTotal
# dist_ponderada = dist(DatosGastoTotal)
# hGTotal = hclust(dist_ponderada, method = "ward.D2")
# 
# ## Gasto Cocina + Lavanderia + Aire y Calefacción ##
# DatosGastooCocinaLavanderiaAireCalefaccion <- subset( DatosViviendaAgrupadosNorm, select = c("GastoCocina", "GastoLavanderia", "GastoAireyCalefaccion") )
# dist_ponderada = dist(DatosGastooCocinaLavanderiaAireCalefaccion)
# hGCocinaLavanderiaAireCalefaccion = hclust(dist_ponderada, method = "ward.D2")

jpeg("graficas/dendrogramas_normalizados/general.jpg",width=1080,height=720)
plot(hGeneral, main="Dendrograma general")
rect.hclust(hGeneral, k = 3) 
dev.off()

jpeg("graficas/dendrogramas_normalizados/consumo_cocina.jpg",width=1080,height=720)
plot(hCCocina, main="Dendrograma Consumo Cocina")
rect.hclust(hCCocina, k = 3) 
dev.off()

jpeg("graficas/dendrogramas_normalizados/consumo_lavanderia.jpg",width=1080,height=720)
plot(hCLavanderia, main="Dendrograma Consumo Lavanderia")
rect.hclust(hCLavanderia, k = 2) 
dev.off()

jpeg("graficas/dendrogramas_normalizados/consumo_aire_calefaccion.jpg",width=1080,height=720)
plot(hCAireyCalefaccion, main="Dendrograma Consumo Aire y Calefaccion")
rect.hclust(hCAireyCalefaccion, k = 2) 
dev.off()

jpeg("graficas/dendrogramas_normalizados/consumo_total.jpg",width=1080,height=720)
plot(hCTotal, main="Dendrograma Consumos totales")
rect.hclust(hCTotal, k = 5) 
dev.off()

jpeg("graficas/dendrogramas_normalizados/general_consumos.jpg",width=1080,height=720)
plot(hCCocinaLavanderiaAireCalefaccion, main="Dendrograma general de consumos")
rect.hclust(hCCocinaLavanderiaAireCalefaccion, k = 3) 
dev.off()

jpeg("graficas/dendrogramas_normalizados/precios.jpg",width=1080,height=720)
plot(hPrecio, main="Dendrograma Precio")
rect.hclust(hPrecio, k = 2) 
dev.off()

# jpeg("graficas/dendrogramas_normalizados/gasto_cocina.jpg",width=1080,height=720)
# plot(hGCocina, main="Dendrograma Gasto Cocina")
# rect.hclust(hGCocina, k = 2) 
# dev.off()
# 
# jpeg("graficas/dendrogramas_normalizados/gasto_lavanderia.jpg",width=1080,height=720)
# plot(hGLavanderia, main="Dendrograma Gasto Lavanderia")
# rect.hclust(hGLavanderia, k = 2) 
# dev.off()
# 
# jpeg("graficas/dendrogramas_normalizados/gasto_aire_calefaccion.jpg",width=1080,height=720)
# plot(hGAireyCalefaccion, main="Dendrograma Gasto Aire y Calefaccion")
# rect.hclust(hGAireyCalefaccion, k = 4) 
# dev.off()
# 
# jpeg("graficas/dendrogramas_normalizados/gasto_total.jpg",width=1080,height=720)
# plot(hGTotal, main="Dendrograma Gasto total")
# rect.hclust(hGTotal, k = 2) 
# dev.off()
# 
# jpeg("graficas/dendrogramas_normalizados/general_gastos.jpg",width=1080,height=720)
# plot(hGCocinaLavanderiaAireCalefaccion, main="Dendrograma general de gastos")
# rect.hclust(hGCocinaLavanderiaAireCalefaccion, k = 5) 
# dev.off()
