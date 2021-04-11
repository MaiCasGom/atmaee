###### Funcion para normalizacion ####

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

######################################

library(cluster)

DatosViviendaAgrupados <- subset( DatosViviendaAgrupados, select = -Fecha )
horas <- DatosViviendaAgrupados$Hora
#DatosViviendaAgrupados <- subset( DatosViviendaAgrupados, select = -Hora )

#DatosViviendaAgrupadosNorm <- as.data.frame(lapply(DatosViviendaAgrupados[1:9], min_max_norm))

#DatosViviendaRangos <-apply(DatosViviendaAgrupadosNorm,2,range)
#
#DatosViviendaRangos <- mapply(DatosViviendaRangos, FUN=as.numeric)
#DatosViviendaRangos <- matrix(data=DatosViviendaRangos, ncol=9, nrow=2)
#colnames(DatosViviendaRangos) = c("ConsumoCocina","ConsumoLavanderia", "ConsumoAireyCalefaccion",
#                                  "ConsumoTotal", "Precio", "GastoCocina","GastoLavanderia","GastoAireyCalefaccion","GastoTotal")

k = 7

DatosConsumoCocina <- subset( DatosViviendaAgrupados, select = c(ConsumoCocina,Hora) )

## Consumo Cocina ##
#DatosConsumoCocina <- DatosViviendaAgrupados$ConsumoCocina
dist_ponderada = dist(DatosConsumoCocina)
#jpeg("graficas/silhouette/consumo_cocina_kmedioides.jpg",width=1080,height=720)
#plot(1, type="n", xlab="", ylab="", xlim=c(1, k+1), ylim=c(0.92, 0.96), main="Cálculo factor K - Consumo Cocina")
#for (i in 2:k){ 
  km <- pam(DatosConsumoCocina, 7)
  ss <- silhouette(km$cluster, dist_ponderada)
  coeficiente_silhouette = mean(ss[, 3])
#  points(i,mean(ss[, 3]))
 # text(i, mean(ss[, 3]), round(mean(ss[, 3]), 4), cex=0.8, pos = 3)
#}
#dev.off()

  k=7
DatosConsumoLavanderia <- subset(DatosViviendaAgrupados, select = c(ConsumoLavanderia,Hora) )
## Consumo Lavanderia ##
#DatosConsumoLavanderia <- DatosViviendaAgrupadosNorm$ConsumoLavanderia
dist_ponderada = dist(DatosConsumoLavanderia)
#jpeg("graficas/silhouette/consumo_lavanderia_kmedioides.jpg",width=1080,height=720)
#plot(1, type="n", xlab="", ylab="", xlim=c(1, k+1), ylim=c(0.6, 0.96), main="Cálculo factor K - Consumo Lavandería")
#for (i in 2:k){ 
  km <- pam(DatosConsumoLavanderia, 7)
  ss <- silhouette(km$cluster, dist_ponderada)
  coeficiente_silhouette = mean(ss[, 3])
#  points(i,mean(ss[, 3]))
#  text(i, mean(ss[, 3]), round(mean(ss[, 3]), 4), cex=0.8, pos = 3)
#}
#dev.off()

## Consumo Aire y Calefaccion ##
DatosConsumoAireyCalefaccion <- subset(DatosViviendaAgrupados, select = c(ConsumoAireyCalefaccion,Hora) )
dist_ponderada = dist(DatosConsumoAireyCalefaccion)
#jpeg("graficas/silhouette/consumo_aireycalefaccion_kmedioides.jpg",width=1080,height=720)
#plot(1, type="n", xlab="", ylab="", xlim=c(1, k+1), ylim=c(0.60, 0.85), main="Cálculo factor K - Consumo Aire y Calefaccion")
#for (i in 2:k){ 
  km <- pam(DatosConsumoAireyCalefaccion, 7)
  ss <- silhouette(km$cluster, dist_ponderada)
  coeficiente_silhouette = mean(ss[, 3])
#  points(i,mean(ss[, 3]))
#  text(i, mean(ss[, 3]), round(mean(ss[, 3]), 4), cex=0.8, pos = 3)
#}
#dev.off()

## Consumo Total ##
DatosConsumoTotal<- subset(DatosViviendaAgrupados, select = c(ConsumoTotal,Hora) )
dist_ponderada = dist(DatosConsumoTotal)
#jpeg("graficas/silhouette/consumo_total_kmedioides.jpg",width=1080,height=720)
#plot(1, type="n", xlab="", ylab="", xlim=c(1, k+1), ylim=c(0.60, 0.85), main="Cálculo factor K - Consumo Total")
#for (i in 2:k){ 
  km <- pam(DatosConsumoTotal, 7)
  ss <- silhouette(km$cluster, dist_ponderada)
  coeficiente_silhouette = mean(ss[, 3])
#  points(i,mean(ss[, 3]))
#  text(i, mean(ss[, 3]), round(mean(ss[, 3]), 4), cex=0.8, pos = 3)
#}
#dev.off()

## Consumo Cocina + Lavanderia + Aire y Calefacción ##
DatosConsumoCocinaLavanderiaAireCalefaccion <- subset( DatosViviendaAgrupados, select = c("ConsumoCocina", "ConsumoLavanderia", "ConsumoAireyCalefaccion", "Hora") )
dist_ponderada = dist(DatosConsumoCocinaLavanderiaAireCalefaccion)
#jpeg("graficas/silhouette/consumo_general_kmedioides.jpg",width=1080,height=720)
#plot(1, type="n", xlab="", ylab="", xlim=c(1, k+1), ylim=c(0.40, 0.80), main="Cálculo factor K - Consumo General")
#for (i in 2:k){ 
  km <- pam(DatosConsumoCocinaLavanderiaAireCalefaccion, 6)
  ss <- silhouette(km$cluster, dist_ponderada)
  coeficiente_silhouette = mean(ss[, 3])
#  points(i,mean(ss[, 3]))
#  text(i, mean(ss[, 3]), round(mean(ss[, 3]), 4), cex=0.8, pos = 3)
#}
#dev.off()

## Precio ##
DatosPrecio<- subset(DatosViviendaAgrupados, select = c(Precio,Hora) )
dist_ponderada = dist(DatosPrecio)
#jpeg("graficas/silhouette/precio_kmedioides.jpg",width=1080,height=720)
#plot(1, type="n", xlab="", ylab="", xlim=c(1, k+1), ylim=c(0, 1), main="Cálculo factor K - Precio")
#for (i in 2:k){ 
  km <- pam(DatosPrecio, 7)
  ss <- silhouette(km$cluster, dist_ponderada)
  coeficiente_silhouette = mean(ss[, 3])
#  points(i,mean(ss[, 3]))
#  text(i, mean(ss[, 3]), round(mean(ss[, 3]), 4), cex=0.8, pos = 3)
#}
#dev.off()