library(zoo)

## CONSUMO TOTAL ##

anios <- sort(unique(year(DatosViviendaAgrupadosPorFecha$Fecha)))
meses <- sort(unique(month(DatosViviendaAgrupadosPorFecha$Fecha)))

pdf("graficas/consumo_total/consumo_mes_a_mes.pdf",width=6,height=4,paper='special')
for(m in meses){
  datosMes <- subset(DatosViviendaAgrupadosPorFecha, month(DatosViviendaAgrupadosPorFecha$Fecha) == m)
  matrizDatosMes <- matrix(nrow=31, ncol=length(anios))
  colnames(matrizDatosMes) <- c(anios)
  for(row in 1:nrow(datosMes)) {
    dia <- format(as.Date(datosMes[row, "Fecha"],format="%Y-%m-%d"), format = "%d")
    anio <- format(as.Date(datosMes[row, "Fecha"],format="%Y-%m-%d"), format = "%Y")
    matrizDatosMes[strtoi(dia, base = 10L), anio] <- datosMes[row, "x"]
  }
  
  nn <- ncol(matrizDatosMes)
  layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
  par(mar=c(4,3,3,0)) #No margin on the right side
  print(matplot(na.approx(matrizDatosMes), type="l", lty = "solid", col = 1:6, xlab=month.abb[m], ylab="Consumo (Wh)", ylim = c(0,35000)))
  print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
  print(legend("left", colnames(matrizDatosMes),col=seq_len(nn),cex=0.8,fill=seq_len(nn)))
}
dev.off()

## CONSUMO AIRE Y CALEFACCION ##

anios <- sort(unique(year(DatosAireCalefaccionAgrupadosPorFecha$Fecha)))
meses <- sort(unique(month(DatosAireCalefaccionAgrupadosPorFecha$Fecha)))

pdf("graficas/consumo_aire_calefaccion/consumo_mes_a_mes.pdf",width=6,height=4,paper='special')
for(m in meses){
  datosMes <- subset(DatosAireCalefaccionAgrupadosPorFecha, month(DatosAireCalefaccionAgrupadosPorFecha$Fecha) == m)
  matrizDatosMes <- matrix(nrow=31, ncol=length(anios))
  colnames(matrizDatosMes) <- c(anios)
  for(row in 1:nrow(datosMes)) {
    dia <- format(as.Date(datosMes[row, "Fecha"],format="%Y-%m-%d"), format = "%d")
    anio <- format(as.Date(datosMes[row, "Fecha"],format="%Y-%m-%d"), format = "%Y")
    matrizDatosMes[strtoi(dia, base = 10L), anio] <- datosMes[row, "x"]
  }
  
  nn <- ncol(matrizDatosMes)
  layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
  par(mar=c(4,3,3,0)) #No margin on the right side
  print(matplot(na.approx(matrizDatosMes), type="l", lty = "solid", col = 1:6, xlab=month.abb[m], ylab="Consumo (Wh)", ylim = c(0,25000)))
  print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
  print(legend("left", colnames(matrizDatosMes),col=seq_len(nn),cex=0.8,fill=seq_len(nn)))
}
dev.off()
z
## CONSUMO COCINA ##

anios <- sort(unique(year(DatosCocinaAgrupadosPorFecha$Fecha)))
meses <- sort(unique(month(DatosCocinaAgrupadosPorFecha$Fecha)))

pdf("graficas/consumo_cocina/consumo_mes_a_mes.pdf",width=6,height=4,paper='special')
for(m in meses){
  datosMes <- subset(DatosCocinaAgrupadosPorFecha, month(DatosCocinaAgrupadosPorFecha$Fecha) == m)
  matrizDatosMes <- matrix(nrow=31, ncol=length(anios))
  colnames(matrizDatosMes) <- c(anios)
  for(row in 1:nrow(datosMes)) {
    dia <- format(as.Date(datosMes[row, "Fecha"],format="%Y-%m-%d"), format = "%d")
    anio <- format(as.Date(datosMes[row, "Fecha"],format="%Y-%m-%d"), format = "%Y")
    matrizDatosMes[strtoi(dia, base = 10L), anio] <- datosMes[row, "x"]
  }
  
  nn <- ncol(matrizDatosMes)
  layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
  par(mar=c(4,3,3,0)) #No margin on the right side
  print(matplot(na.approx(matrizDatosMes), type="l", lty = "solid", col = 1:6, xlab=month.abb[m], ylab="Consumo (Wh)", ylim = c(0,15000)))
  print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
  print(legend("left", colnames(matrizDatosMes),col=seq_len(nn),cex=0.8,fill=seq_len(nn)))
}
dev.off()

## CONSUMO LAVANDERIA ##

anios <- sort(unique(year(DatosLavanderiaAgrupadosPorFecha$Fecha)))
meses <- sort(unique(month(DatosLavanderiaAgrupadosPorFecha$Fecha)))

pdf("graficas/consumo_lavanderia/consumo_mes_a_mes.pdf",width=6,height=4,paper='special')
for(m in meses){
  datosMes <- subset(DatosLavanderiaAgrupadosPorFecha, month(DatosLavanderiaAgrupadosPorFecha$Fecha) == m)
  matrizDatosMes <- matrix(nrow=31, ncol=length(anios))
  colnames(matrizDatosMes) <- c(anios)
  for(row in 1:nrow(datosMes)) {
    dia <- format(as.Date(datosMes[row, "Fecha"],format="%Y-%m-%d"), format = "%d")
    anio <- format(as.Date(datosMes[row, "Fecha"],format="%Y-%m-%d"), format = "%Y")
    matrizDatosMes[strtoi(dia, base = 10L), anio] <- datosMes[row, "x"]
  }
  
  nn <- ncol(matrizDatosMes)
  layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
  par(mar=c(4,3,3,0)) #No margin on the right side
  print(matplot(na.approx(matrizDatosMes), type="l", lty = "solid", col = 1:6, xlab=month.abb[m], ylab="Consumo (Wh)", ylim = c(0,15000)))
  print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
  print(legend("left", colnames(matrizDatosMes),col=seq_len(nn),cex=0.8,fill=seq_len(nn)))
}
dev.off()
