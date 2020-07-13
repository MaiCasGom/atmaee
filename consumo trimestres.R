setwd("~/atmaee")
library(lubridate)
library(ggplot2)
library(zoo)
DatosVivienda= read.csv("household_power_consumption.txt", comment.char = "@", sep = ";")
colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
                            "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
                            "IntensidadGlobal (A)","ConsumoCocina (w/h)","ConsumoLavanderia (w/h)","ConsumoAireyCalentador (w/h)")

DatosVivienda <- na.omit(DatosVivienda)
# Sumamos el consumo de cocina, lavanderia y aires acondicionados y si falta algún dato no lo tenemos en cuenta.
Suma_Total <- rowSums(DatosVivienda[, c(7, 8, 9)])

DatosViviendaAgrupados <- data.frame(date=DatosVivienda$Fecha, consumo_total=Suma_Total)

SumaTotalPorFecha <- aggregate(DatosViviendaAgrupados$consumo_total, list(date=DatosViviendaAgrupados$date), FUN=sum)
SumaTotalPorFecha$date=as.Date(SumaTotalPorFecha$date, format = "%d/%m/%Y")

anios <- sort(unique(year(SumaTotalPorFecha$date)))
meses <- sort(unique(month(SumaTotalPorFecha$date)))

pdf("Consumo trimestres.pdf",width=6,height=4,paper='special')
for(m in c(1,4,7,10)){
  datosTrimestre <- subset(SumaTotalPorFecha, (month(SumaTotalPorFecha$date) == m | month(SumaTotalPorFecha$date) == m+1 |  month(SumaTotalPorFecha$date) == m+2 ))
  matrizDatosTrimestre <- matrix(nrow=93, ncol=length(anios))
  colnames(matrizDatosTrimestre) <- c(anios)
  #matrizDatosTrimestre[] <- NA
  for(row in 1:nrow(datosTrimestre)) {
    dia <- strtoi(format(as.Date(datosTrimestre[row, "date"],format="%Y-%m-%d"), format = "%d"),base = 10L)
    mes <- strtoi(format(as.Date(datosTrimestre[row, "date"],format="%Y-%m-%d"), format = "%m"),base = 10L)
    dia <- dia + (((mes - 1) %% 3) * 31)
    anio <- format(as.Date(datosTrimestre[row, "date"],format="%Y-%m-%d"), format = "%Y")
    matrizDatosTrimestre[dia, anio] <- datosTrimestre[row, "x"]
  }
  nn <- ncol(matrizDatosTrimestre)
  layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
  par(mar=c(4,3,3,0)) #No margin on the right side
  print(matplot(na.approx(matrizDatosTrimestre), type="l", lty = "solid", col = 1:6, xlab=paste(month.abb[m], month.abb[m+2], sep=" - "), ylab="Consumo (w/h)", ylim = c(0,35000)))
  print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
  print(legend("left", colnames(matrizDatosTrimestre),col=seq_len(nn),cex=0.8,fill=seq_len(nn)))
}
dev.off()