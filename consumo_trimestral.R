anios <- sort(unique(year(DatosViviendaAgrupadosPorFecha$Fecha)))
meses <- sort(unique(month(DatosViviendaAgrupadosPorFecha$Fecha)))

pdf("graficas/consumo_trimestre.pdf",width=6,height=4,paper='special')
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