
## CONSUMO TOTAL ##

anios <- sort(unique(year(DatosViviendaAgrupadosPorFecha$Fecha)))
meses <- sort(unique(month(DatosViviendaAgrupadosPorFecha$Fecha)))

pdf("graficas/consumo_total/consumo_trimestre.pdf",width=6,height=4,paper='special')
for(m in c(1,4,7,10)){
  datosTrimestre <- subset(DatosViviendaAgrupadosPorFecha, (month(DatosViviendaAgrupadosPorFecha$Fecha) == m | month(DatosViviendaAgrupadosPorFecha$Fecha) == m+1 |  month(DatosViviendaAgrupadosPorFecha$Fecha) == m+2 ))
  matrizDatosTrimestre <- matrix(nrow=93, ncol=length(anios))
  colnames(matrizDatosTrimestre) <- c(anios)
  #matrizDatosTrimestre[] <- NA
  for(row in 1:nrow(datosTrimestre)) {
    dia <- strtoi(format(as.Date(datosTrimestre[row, "Fecha"],format="%Y-%m-%d"), format = "%d"),base = 10L)
    mes <- strtoi(format(as.Date(datosTrimestre[row, "Fecha"],format="%Y-%m-%d"), format = "%m"),base = 10L)
    dia <- dia + (((mes - 1) %% 3) * 31)
    anio <- format(as.Date(datosTrimestre[row, "Fecha"],format="%Y-%m-%d"), format = "%Y")
    matrizDatosTrimestre[dia, anio] <- datosTrimestre[row, "x"]
  }
  nn <- ncol(matrizDatosTrimestre)
  layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
  par(mar=c(4,3,3,0)) #No margin on the right side
  print(matplot(na.approx(matrizDatosTrimestre), type="l", lty = "solid", col = 1:6, xlab=paste(month.abb[m], month.abb[m+2], sep=" - "), ylab="Consumo (Wh)", ylim = c(0,35000)))
  print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
  print(legend("left", colnames(matrizDatosTrimestre),col=seq_len(nn),cex=0.8,fill=seq_len(nn)))
}
dev.off()

## CONSUMO AIRE Y CALEFACCION ##

anios <- sort(unique(year(DatosAireCalefaccionAgrupadosPorFecha$Fecha)))
meses <- sort(unique(month(DatosAireCalefaccionAgrupadosPorFecha$Fecha)))

pdf("graficas/consumo_aire_calefaccion/consumo_trimestre.pdf",width=6,height=4,paper='special')
for(m in c(1,4,7,10)){
  datosTrimestre <- subset(DatosAireCalefaccionAgrupadosPorFecha, (month(DatosAireCalefaccionAgrupadosPorFecha$Fecha) == m | month(DatosAireCalefaccionAgrupadosPorFecha$Fecha) == m+1 |  month(DatosAireCalefaccionAgrupadosPorFecha$Fecha) == m+2 ))
  matrizDatosTrimestre <- matrix(nrow=93, ncol=length(anios))
  colnames(matrizDatosTrimestre) <- c(anios)
  #matrizDatosTrimestre[] <- NA
  for(row in 1:nrow(datosTrimestre)) {
    dia <- strtoi(format(as.Date(datosTrimestre[row, "Fecha"],format="%Y-%m-%d"), format = "%d"),base = 10L)
    mes <- strtoi(format(as.Date(datosTrimestre[row, "Fecha"],format="%Y-%m-%d"), format = "%m"),base = 10L)
    dia <- dia + (((mes - 1) %% 3) * 31)
    anio <- format(as.Date(datosTrimestre[row, "Fecha"],format="%Y-%m-%d"), format = "%Y")
    matrizDatosTrimestre[dia, anio] <- datosTrimestre[row, "x"]
  }
  nn <- ncol(matrizDatosTrimestre)
  layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
  par(mar=c(4,3,3,0)) #No margin on the right side
  print(matplot(na.approx(matrizDatosTrimestre), type="l", lty = "solid", col = 1:6, xlab=paste(month.abb[m], month.abb[m+2], sep=" - "), ylab="Consumo (Wh)", ylim = c(0,25000)))
  print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
  print(legend("left", colnames(matrizDatosTrimestre),col=seq_len(nn),cex=0.8,fill=seq_len(nn)))
}
dev.off()

## CONSUMO COCINA ##

anios <- sort(unique(year(DatosCocinaAgrupadosPorFecha$Fecha)))
meses <- sort(unique(month(DatosCocinaAgrupadosPorFecha$Fecha)))

pdf("graficas/consumo_cocina/consumo_trimestre.pdf",width=6,height=4,paper='special')
for(m in c(1,4,7,10)){
  datosTrimestre <- subset(DatosCocinaAgrupadosPorFecha, (month(DatosCocinaAgrupadosPorFecha$Fecha) == m | month(DatosCocinaAgrupadosPorFecha$Fecha) == m+1 |  month(DatosCocinaAgrupadosPorFecha$Fecha) == m+2 ))
  matrizDatosTrimestre <- matrix(nrow=93, ncol=length(anios))
  colnames(matrizDatosTrimestre) <- c(anios)
  #matrizDatosTrimestre[] <- NA
  for(row in 1:nrow(datosTrimestre)) {
    dia <- strtoi(format(as.Date(datosTrimestre[row, "Fecha"],format="%Y-%m-%d"), format = "%d"),base = 10L)
    mes <- strtoi(format(as.Date(datosTrimestre[row, "Fecha"],format="%Y-%m-%d"), format = "%m"),base = 10L)
    dia <- dia + (((mes - 1) %% 3) * 31)
    anio <- format(as.Date(datosTrimestre[row, "Fecha"],format="%Y-%m-%d"), format = "%Y")
    matrizDatosTrimestre[dia, anio] <- datosTrimestre[row, "x"]
  }
  nn <- ncol(matrizDatosTrimestre)
  layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
  par(mar=c(4,3,3,0)) #No margin on the right side
  print(matplot(na.approx(matrizDatosTrimestre), type="l", lty = "solid", col = 1:6, xlab=paste(month.abb[m], month.abb[m+2], sep=" - "), ylab="Consumo (Wh)", ylim = c(0,15000)))
  print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
  print(legend("left", colnames(matrizDatosTrimestre),col=seq_len(nn),cex=0.8,fill=seq_len(nn)))
}
dev.off()

## CONSUMO LAVANDERIA ##

anios <- sort(unique(year(DatosLavanderiaAgrupadosPorFecha$Fecha)))
meses <- sort(unique(month(DatosLavanderiaAgrupadosPorFecha$Fecha)))

pdf("graficas/consumo_lavanderia/consumo_trimestre.pdf",width=6,height=4,paper='special')
for(m in c(1,4,7,10)){
  datosTrimestre <- subset(DatosLavanderiaAgrupadosPorFecha, (month(DatosLavanderiaAgrupadosPorFecha$Fecha) == m | month(DatosLavanderiaAgrupadosPorFecha$Fecha) == m+1 |  month(DatosLavanderiaAgrupadosPorFecha$Fecha) == m+2 ))
  matrizDatosTrimestre <- matrix(nrow=93, ncol=length(anios))
  colnames(matrizDatosTrimestre) <- c(anios)
  #matrizDatosTrimestre[] <- NA
  for(row in 1:nrow(datosTrimestre)) {
    dia <- strtoi(format(as.Date(datosTrimestre[row, "Fecha"],format="%Y-%m-%d"), format = "%d"),base = 10L)
    mes <- strtoi(format(as.Date(datosTrimestre[row, "Fecha"],format="%Y-%m-%d"), format = "%m"),base = 10L)
    dia <- dia + (((mes - 1) %% 3) * 31)
    anio <- format(as.Date(datosTrimestre[row, "Fecha"],format="%Y-%m-%d"), format = "%Y")
    matrizDatosTrimestre[dia, anio] <- datosTrimestre[row, "x"]
  }
  nn <- ncol(matrizDatosTrimestre)
  layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
  par(mar=c(4,3,3,0)) #No margin on the right side
  print(matplot(na.approx(matrizDatosTrimestre), type="l", lty = "solid", col = 1:6, xlab=paste(month.abb[m], month.abb[m+2], sep=" - "), ylab="Consumo (Wh)", ylim = c(0,15000)))
  print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
  print(legend("left", colnames(matrizDatosTrimestre),col=seq_len(nn),cex=0.8,fill=seq_len(nn)))
}
dev.off()