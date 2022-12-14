#######################################################

## Analisis RFM-Kmeans ##

#######################################################

## DISCLAIMER: los comandos y c?gido desarrollados en este Script fueron tomados a partir de los siguientes documentos
## Documentos originales: Kmeans_Clustering II 2021 e hierarchical_mcdonalds.

## ---------------------------------------------------------
## Instalar paquetes a usar

install.packages("cluster")
install.packages("factoextra")
install.packages("vegan")
install.packages("NbClust")
install.packages("flexclust")
install.packages("ISLR")
install.packages("readxl")
install.packages("ggplot2")
install.packages("factoextra")
install.packages("plotly")
install.packages("skimr")
install.packages("tidyverse")
install.packages("readr")

library(cluster)
library(factoextra)
library(vegan)
library(flexclust)
library(ISLR)
library(readxl)
library(dplyr)
library(ggplot2)
library(factoextra)
library(plotly)
library(skimr)
library(readr)
library(tidyverse)
##----------------------------------------------------------

rm(list = ls())

## Directorio de trabajo
getwd()
setwd("C:/Users/olcay/Desktop/R studio/Archivo data")

# Funci?n que permite calcular valor promedio de variables de segmentaci?n para cada segmento.

seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

## Cargar base de datos
datosfarmacia.df <- read.csv("R-datos_farmacia.csv")
view(datosfarmacia.df)
str(datosfarmacia.df)

## Vemos que las variables RFM son num?ricas y n?meros enteros, pero faltar?a estandarizar y categorizar en quintiles
## Nombre de las variables: Recencia, Frecuencia, Monto, Sexo, GSE, Edad e ?..Cliente
## Las variables RFM ya est?n calculadas

## Estandarizaci?n de variables RFM
datosfarmacia.df$Rec_estand <- round(scale(-datosfarmacia.df$Recencia, center = TRUE , scale = TRUE), 9)
datosfarmacia.df$Frec_estand <- round(scale(datosfarmacia.df$Frecuencia, center = TRUE , scale = TRUE), 9)
datosfarmacia.df$Monto_estand <- round(scale(datosfarmacia.df$Monto, center = TRUE , scale = TRUE), 9)

{## Vamos a trabajar con las variables estandarizadas, no las originales
datosfarmacia.df$Recencia <- NULL
datosfarmacia.df$Frecuencia <- NULL
datosfarmacia.df$Monto <- NULL
}
## Realizar quintiles por cada variable RFM

## Recencia
## Quintil 1
LimRec_Q1 = quantile(datosfarmacia.df$Rec_estand,probs = 0.20) 
## Quintil 2
LimRec_Q2 = quantile(datosfarmacia.df$Rec_estand,probs = 0.40) 
## Vemos que el Quntil 1 y 2 se traslapan debido a los datos que tenemos en el campo Rec_estand a ra?z de estandarizar el campo Recencia

## Quintil 3
LimRec_Q3 = quantile(datosfarmacia.df$Rec_estand,probs = 0.60)
## Quintil 4
LimRec_Q4 = quantile(datosfarmacia.df$Rec_estand,probs = 0.80) 

datosfarmacia.df$Rec_estand_quintil = case_when(datosfarmacia.df$Rec_estand <= LimRec_Q1 ~ 1,
                                          datosfarmacia.df$Rec_estand > LimRec_Q1 & datosfarmacia.df$Rec_estand <= LimRec_Q2 ~ 2,
                                          datosfarmacia.df$Rec_estand > LimRec_Q2 & datosfarmacia.df$Rec_estand <= LimRec_Q3 ~ 3,
                                          datosfarmacia.df$Rec_estand > LimRec_Q3 & datosfarmacia.df$Rec_estand <= LimRec_Q4 ~ 4,
                                          datosfarmacia.df$Rec_estand > LimRec_Q4 ~ 5)


hist(datosfarmacia.df$Rec_estand_quintil, breaks=6, border="black", col="lightblue",xlab="Quintiles",ylab="Cantidad clientes", main="Recencia de los clientes", xlim=c(0,5),ylim=c(0,7000))
## Debido a la estandarizaci?n el quintil 3 reune a todos los quintiles 3, 4 y 5, que son aquellos clientes que ha pasado muy poco tiempo respecto al promedio para comprar en la farmacia

## Frecuencia
## Quintil 1
LimFrec_Q1 = quantile(datosfarmacia.df$Frec_estand,probs = 0.20) 
## Quintil 2
LimFrec_Q2 = quantile(datosfarmacia.df$Frec_estand,probs = 0.40) 
## Quintil 3
LimFrec_Q3 = quantile(datosfarmacia.df$Frec_estand,probs = 0.60)
## Quintil 4
LimFrec_Q4 = quantile(datosfarmacia.df$Frec_estand,probs = 0.80) 

datosfarmacia.df$Frec_estand_quintil = case_when(datosfarmacia.df$Frec_estand <= LimFrec_Q1 ~ 1,
                                                datosfarmacia.df$Frec_estand > LimFrec_Q1 & datosfarmacia.df$Frec_estand <= LimFrec_Q2 ~ 2,
                                                datosfarmacia.df$Frec_estand > LimFrec_Q2 & datosfarmacia.df$Frec_estand <= LimFrec_Q3 ~ 3,
                                                datosfarmacia.df$Frec_estand > LimFrec_Q3 & datosfarmacia.df$Frec_estand <= LimFrec_Q4 ~ 4,
                                                datosfarmacia.df$Frec_estand > LimFrec_Q4 ~ 5)


hist(datosfarmacia.df$Frec_estand_quintil, breaks=6, border="black", col="aquamarine",xlab="Quintiles",ylab="Cantidad clientes", main="Frecuencia de los clientes", xlim=c(0,5),ylim=c(0,3000))
## Se aprecia que hay m?s clientes en el primer y cuarto quintil

## Monto
## Quintil 1
LimMonto_Q1 = quantile(datosfarmacia.df$Monto_estand,probs = 0.20) 
## Quintil 2
LimMonto_Q2 = quantile(datosfarmacia.df$Monto_estand,probs = 0.40) 
## Quintil 3
LimMonto_Q3 = quantile(datosfarmacia.df$Monto_estand,probs = 0.60)
## Quintil 4
LimMonto_Q4 = quantile(datosfarmacia.df$Monto_estand,probs = 0.80) 

datosfarmacia.df$Monto_estand_quintil = case_when(datosfarmacia.df$Monto_estand <= LimMonto_Q1 ~ 1,
                                                 datosfarmacia.df$Monto_estand > LimMonto_Q1 & datosfarmacia.df$Monto_estand <= LimMonto_Q2 ~ 2,
                                                 datosfarmacia.df$Monto_estand > LimMonto_Q2 & datosfarmacia.df$Monto_estand <= LimMonto_Q3 ~ 3,
                                                 datosfarmacia.df$Monto_estand > LimMonto_Q3 & datosfarmacia.df$Monto_estand <= LimMonto_Q4 ~ 4,
                                                 datosfarmacia.df$Monto_estand > LimMonto_Q4 ~ 5)

hist(datosfarmacia.df$Monto_estand_quintil, breaks=6, border="black", col="darkgoldenrod2",xlab="Quintiles",ylab="Cantidad clientes", main="Monto promedio de los clientes", xlim=c(0,5),ylim=c(0,2500))
## Al estandarizar los clientes gastan en promedio muy similar  

fig1 <- plot_ly(datosfarmacia.df$Monto_estand_quintil, y = ~ Monto, type = "box", name = "Monto", boxpoints = "all")

## Concatenar los ptjes RFM estandarizados de acuerdo a los quintiles

datosfarmacia.df$RFM <- 100*datosfarmacia.df$Rec_estand_quintil + 10*datosfarmacia.df$Frec_estand_quintil + datosfarmacia.df$Monto_estand_quintil


## An?lisis de k-medias
RFM_estand.df <- datosfarmacia.df[,8:10]

RFM.km27 <- stepFlexclust(RFM_estand.df, 2:7, nrep = 10, verbose = FALSE)
plot(RFM.km27, xlab="n?mero de segmentos")
## (a)
## Parecer?a que con 3 segmentos por k-medias porque cae mucho la distancia. Luego, seg?n el gr?fico, desde 4 segmentos la distancia cae poco

RFM.km3 <- cclust(RFM_estand.df, k=3)
RFM.km3
summary(RFM.km3)

clusters(RFM.km3)[1:20]

RFM_estand.df$cluster <- clusters(RFM.km3)
head(RFM_estand.df)

seg.summ(RFM_estand.df, RFM_estand.df$cluster)


## An?lisis de k-medias extendido con las otras variables

RFM.km27_ext <- stepFlexclust(datosfarmacia.df[,8:10], 2:7, nrep = 10, verbose = FALSE)
plot(RFM.km27_ext, xlab="n?mero de segmentos")


## Parecer?a que, al igual que antes, hay que segmentar con 3 segmentos por k-medias porque cae mucho la distancia. Luego, seg?n el gr?fico, desde 4 segmentos la distancia cae poco

RFM.km3_ext <- cclust(datosfarmacia.df[,8:10], k=3)
RFM.km3_ext
summary(RFM.km3_ext)

clusters(RFM.km3_ext)[1:20]

datosfarmacia.df$cluster <- clusters(RFM.km3_ext)
head(datosfarmacia.df)

seg.summ(datosfarmacia.df, datosfarmacia.df$cluster)





