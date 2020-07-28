lunes <- DatosViviendaAgrupados[weekdays(as.Date(DatosViviendaAgrupados$Fecha)) == "lunes", ]


for unique(year(SumaTotalPorFechaHora$fecha)
quickplot(1:nrow(datosAnio) ,datosAnio$x , geom = c("line"), xlab = "Horas", ylab = "Consumo (Wh)", ylim = c(0,3500), main = paste("Año", y, sep = " "))