setwd("~/atmaee")
library(lubridate)
library(ggplot2)
library(zoo)

DatosVivienda= read.csv("household_power_consumption.txt", comment.char = "@", sep = ";", na.strings=c("?","NA"))
colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
                            "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
                            "IntensidadGlobal (A)","ConsumoCocina (Wh)","ConsumoLavanderia (Wh)","ConsumoAireyCalentador (Wh)")

DatosVivienda <- na.omit(DatosVivienda)

# Sumamos el consumo de cocina, lavanderia y aires acondicionados y si falta algún dato no lo tenemos en cuenta.
Suma_Total <- rowSums(DatosVivienda[, c(7, 8, 9)])

DatosViviendaAgrupados <- data.frame(date=DatosVivienda$Fecha, consumo_total=Suma_Total)

SumaTotalPorFecha <- aggregate(DatosViviendaAgrupados$consumo_total, list(date=DatosViviendaAgrupados$date), FUN=sum)
SumaTotalPorFecha$date=as.Date(SumaTotalPorFecha$date, format = "%d/%m/%Y")

anios <- sort(unique(year(SumaTotalPorFecha$date)))
meses <- sort(unique(month(SumaTotalPorFecha$date)))

pdf("graficas/consumo_mes_a_mes.pdf",width=6,height=4,paper='special')
for(m in meses){
  datosMes <- subset(SumaTotalPorFecha, month(SumaTotalPorFecha$date) == m)
  matrizDatosMes <- matrix(nrow=31, ncol=length(anios))
  colnames(matrizDatosMes) <- c(anios)
  matrizDatosMes[] <- NA
  for(row in 1:nrow(datosMes)) {
    dia <- format(as.Date(datosMes[row, "date"],format="%Y-%m-%d"), format = "%d")
    anio <- format(as.Date(datosMes[row, "date"],format="%Y-%m-%d"), format = "%Y")
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
