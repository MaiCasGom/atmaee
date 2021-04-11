library(cluster)
library(dplyr)
library(ggplot2)

DatosViviendaAgrupados <- subset( DatosViviendaAgrupados, select = -Fecha )
DatosViviendaAgrupados <- subset( DatosViviendaAgrupados, select = -Hora )

## Consumo cocina ##
k = 3
DatosConsumoCocina <- subset( DatosViviendaAgrupados, select = c(ConsumoCocina) )
dist_ponderada = dist(DatosConsumoCocina)
km <- kmeans(DatosConsumoCocina, centers = k)

DatosConsumoCocina <- data.frame(DatosConsumoCocina) %>% mutate(cluster = km$cluster)

DatosConsumoCocina <- DatosConsumoCocina %>% mutate(cluster = as.factor(cluster),
                                                    grupo   = as.factor(k))

png("graficas/clasificaciones_sin_horas/consumo_cocina.png",width=1080,height=720)
ggplot(data = DatosConsumoCocina, aes(x = ConsumoCocina, y=cluster, color=cluster)) +
  geom_text(aes(label = cluster), size = 5) +
  theme_bw() +
  theme(legend.position = "none")
dev.off()


## Consumo lavandería ##
k = 2
DatosConsumoLavanderia <- subset( DatosViviendaAgrupados, select = c(ConsumoLavanderia) )
dist_ponderada = dist(DatosConsumoLavanderia)
km <- kmeans(DatosConsumoLavanderia, centers = k)

DatosConsumoLavanderia <- data.frame(DatosConsumoLavanderia) %>% mutate(cluster = km$cluster)

DatosConsumoLavanderia <- DatosConsumoLavanderia %>% mutate(cluster = as.factor(cluster),
                                                            grupo   = as.factor(k))

png("graficas/clasificaciones_sin_horas/consumo_lavanderia.png",width=1080,height=720)
ggplot(data = DatosConsumoLavanderia, aes(x = ConsumoLavanderia, y = cluster, color=cluster)) +
  geom_text(aes(label = cluster), size = 5) +
  theme_bw() +
  theme(legend.position = "none")
dev.off()


## Consumo Aire y calefacción ##
k = 2
DatosConsumoAireyCalefaccion <- subset( DatosViviendaAgrupados, select = c(ConsumoAireyCalefaccion) )
dist_ponderada = dist(DatosConsumoAireyCalefaccion)
km <- kmeans(DatosConsumoAireyCalefaccion, centers = k)

DatosConsumoAireyCalefaccion <- data.frame(DatosConsumoAireyCalefaccion) %>% mutate(cluster = km$cluster)

DatosConsumoAireyCalefaccion <- DatosConsumoAireyCalefaccion %>% mutate(cluster = as.factor(cluster),
                                                                        grupo   = as.factor(k))

png("graficas/clasificaciones_sin_horas/consumo_aireycalefaccion.png",width=1080,height=720)
ggplot(data = DatosConsumoAireyCalefaccion, aes(x = ConsumoAireyCalefaccion, y = cluster, color=cluster)) +
  geom_text(aes(label = cluster), size = 5) +
  theme_bw() +
  theme(legend.position = "none")
dev.off()


## Consumo Consumo Total ##
k = 5
DatosConsumoTotal <- subset( DatosViviendaAgrupados, select = c(ConsumoTotal) )
dist_ponderada = dist(DatosConsumoTotal)
km <- kmeans(DatosConsumoTotal, centers = k)

DatosConsumoTotal <- data.frame(DatosConsumoTotal) %>% mutate(cluster = km$cluster)

DatosConsumoTotal <- DatosConsumoTotal %>% mutate(cluster = as.factor(cluster),
                                                  grupo   = as.factor(k))

png("graficas/clasificaciones_sin_horas/consumo_total.png",width=1080,height=720)
ggplot(data = DatosConsumoTotal, aes(x = ConsumoTotal, y = cluster, color=cluster)) +
  geom_text(aes(label = cluster), size = 5) +
  theme_bw() +
  theme(legend.position = "none")
dev.off()

## Consumo Consumo General ##
# k = 2
# DatosConsumoGeneral <- subset( DatosViviendaAgrupados, select = c("ConsumoCocina", "ConsumoLavanderia", "ConsumoAireyCalefaccion") )
# km <- kmeans(DatosConsumoGeneral, centers = k)
# 
# DatosConsumoGeneral <- data.frame(DatosConsumoGeneral) %>% mutate(cluster = km$cluster)
# 
# DatosConsumoGeneral <- DatosConsumoGeneral %>% mutate(cluster = as.factor(cluster),
#                                                   grupo   = as.factor(k))

#ggplot(data = DatosConsumoGeneral, aes(x = DatosConsumoGeneral, y = cluster)) +
#  geom_text(aes(label = cluster), size = 5) +
#  theme_bw() +
#  theme(legend.position = "none")

##### NO SE PUEDE REPRESENTAR PORQUE IMPLICA 3 VARIABLES, 3 DIMENSIONES #####

## Precio ##
k = 2
DatosPrecio <- subset( DatosViviendaAgrupados, select = c(Precio) )
dist_ponderada = dist(DatosPrecio)
km <- kmeans(DatosPrecio, centers = k)

DatosPrecio <- data.frame(DatosPrecio) %>% mutate(cluster = km$cluster)

DatosPrecio <- DatosPrecio %>% mutate(cluster = as.factor(cluster),
                                      grupo   = as.factor(k))

png("graficas/clasificaciones_sin_horas/precio.png",width=1080,height=720)
ggplot(data = DatosPrecio, aes(x = Precio, y = cluster, color=cluster)) +
  geom_text(aes(label = cluster), size = 5) +
  theme_bw() +
  theme(legend.position = "none")
dev.off()



k = 3
DatosConsumoCocinaHoras <- subset( DatosViviendaAgrupados, select = c("ConsumoCocina", "Hora") )
km <- kmeans(DatosConsumoCocinaHoras, centers = k)

DatosConsumoCocinaHoras <- data.frame(DatosConsumoCocinaHoras) %>% mutate(cluster = km$cluster)

DatosConsumoCocinaHoras <- DatosConsumoCocinaHoras %>% mutate(cluster = as.factor(cluster),
                                                              grupo   = as.factor(k))

ggplot(data = DatosConsumoCocinaHoras, aes(x = DatosConsumoCocinaHoras$ConsumoCocina, y = DatosConsumoCocinaHoras$Hora, color=cluster)) +
  geom_text(aes(label = cluster), size = 5) +
  theme_bw() +
  theme(legend.position = "none")
