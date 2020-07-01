setwd("~/atmaee")
library(lubridate)
library(ggplot2)
DatosVivienda= read.csv("household_power_consumption.txt", comment.char = "@", sep = ";", na.strings=c("?","NA"))
colnames(DatosVivienda) = c("Fecha","Hora", "PotenciaActivaGlobal (kW)",
                            "PotenciaReactivaGlobal (kW)", "Voltaje (V)",
                            "IntensidadGlobal (A)","ConsumoCocina (w/h)","ConsumoLavanderia (w/h)","ConsumoAireyCalentador (w/h)")

DatosVivienda <- na.omit(DatosVivienda)
# Sumamos el consumo de cocina, lavanderia y aires acondicionados y si falta algún dato no lo tenemos en cuenta.
Suma_Total <- rowSums(DatosVivienda[, c(7, 8, 9)], na.rm = TRUE)

DatosViviendaAgrupados <- data.frame(date=DatosVivienda$Fecha, consumo_total=Suma_Total)

Suma_Total_by_date <- aggregate(DatosViviendaAgrupados$consumo_total, list(date=DatosViviendaAgrupados$date), FUN=sum)
Suma_Total_by_date$date=as.Date(Suma_Total_by_date$date, format = "%d/%m/%Y")

years <- sort(unique(year(Suma_Total_by_date$date)))
months <- sort(unique(month(Suma_Total_by_date$date)))

pdf("Consumo mes a mes.pdf",width=6,height=4,paper='special')
for(m in months){
  month_data <- subset(Suma_Total_by_date, month(Suma_Total_by_date$date) == m)
  data_matrix_month = matrix(nrow=31, ncol=length(years))
  colnames(data_matrix_month) = c(years)
  data_matrix_month[] <- NA
  
  for(row in 1:nrow(month_data)) {
    day = format(as.Date(month_data[row, "date"],format="%Y-%m-%d"), format = "%d")
    year = format(as.Date(month_data[row, "date"],format="%Y-%m-%d"), format = "%Y")
    data_matrix_month[strtoi(day, base = 0L),year] = month_data[row, "x"]
  }
  
  nn <- ncol(data_matrix_month)
  layout(matrix(c(1,2),nrow=1), width=c(3,1)) 
  par(mar=c(4,3,3,0)) #No margin on the right side
  print(matplot(data_matrix_month, type="l",pch=1,col = 1:6, xlab=month.abb[m], ylab="Consumo (w/h)"))
  print(plot(c(0,1),type="n", axes=F, xlab="", ylab=""))
  print(legend("center", colnames(data_matrix_month),col=seq_len(nn),cex=0.8,fill=seq_len(nn)))
}
dev.off()
