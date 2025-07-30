library(tidyverse)
library(doBy)
enigh <- read_csv("conjunto_de_datos_concentradohogar_enigh2024_ns.csv")

#deciles general----
Conc <- orderBy (~+ing_cor+folioviv+foliohog, data= enigh)

Numded <- c("Total", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")

Conc$Nhog <- 1

tot_hogares <- Conc |> 
  summarise(sum(factor)) |> 
  pull()

tam_dec<-trunc(tot_hogares/10)

Conc$tam_dec=tam_dec

BD1 <- Conc

BD1$MAXT <- BD1$ing_cor

BD1 <- BD1[with(BD1, order(rank(MAXT))),]

BD1$ACUMULA <- cumsum(BD1$factor)


for(i in 1:9)
{
  a1<-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1,]$factor
  BD1<-rbind(BD1[1:(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),],
             BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1):dim(BD1[1])[1],])
  b1<-tam_dec*i-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}
BD1$ACUMULA2<-cumsum(BD1$factor)
BD1$DECIL <- 0
BD1[(BD1$ACUMULA2<=tam_dec),]$DECIL <- 1
for(i in 1:9)
{
  BD1[((BD1$ACUMULA2>tam_dec*i)&(BD1$ACUMULA2<=tam_dec*(i+1))),]$DECIL <- (i+1)
}
BD1[BD1$DECIL%in%"0",]$DECIL <- 10
## Total de hogares
x <- tapply(BD1$factor,BD1$Nhog,sum)

## Deciles
y <- tapply(BD1$factor,BD1$DECIL,sum)
## Se calcula el promedio de ingreso para el total y para cada uno de los deciles
ing_cormed_t <- tapply(BD1$factor*BD1$ing_cor,BD1$Nhog,sum)/x
ing_cormed_d <- tapply(BD1$factor*BD1$ing_cor,BD1$DECIL,sum)/y
## Cuadros
## Guarda los resultados en un data frame
prom_rub <- data.frame (c(ing_cormed_t,ing_cormed_d))
## Agrega el nombre a las filas
row.names(prom_rub) <- Numded

x <- tapply(BD1$factor,BD1$Nhog,sum)
y <- tapply(BD1$factor,BD1$DECIL,sum)

ing_cormed_t <- tapply(BD1$factor*BD1$ing_cor,BD1$Nhog,sum)/x
ing_cormed_d <- tapply(BD1$factor*BD1$ing_cor,BD1$DECIL,sum)/y

prom_rub <- data.frame (c(ing_cormed_t,ing_cormed_d))
row.names(prom_rub) <- Numded

#para hombres----

Conch <- enigh |> 
  filter(sexo_jefe == 1)

Conch <- orderBy (~+ing_cor+folioviv+foliohog, data= Conch)

Conch$Nhog <- 1

tot_hogaresh <- Conch |> 
  summarise(sum(factor)) |> 
  pull()

tam_dech<-trunc(tot_hogaresh/10)

Conch$tam_dech=tam_dech

BD1h <- Conch

BD1h$MAXT <- BD1h$ing_cor

BD1h <- BD1h[with(BD1h, order(rank(MAXT))),]

BD1h$ACUMULA <- cumsum(BD1h$factor)


for(i in 1:9)
{
  a1h<-BD1h[dim(BD1h[BD1h$ACUMULA<tam_dech*i,])[1]+1,]$factor
  BD1h<-rbind(BD1h[1:(dim(BD1h[BD1h$ACUMULA<tam_dech*i,])[1]+1),],
             BD1h[(dim(BD1h[BD1h$ACUMULA<tam_dech*i,])[1]+1):dim(BD1h[1])[1],])
  b1h<-tam_dech*i-BD1h[dim(BD1h[BD1h$ACUMULA<tam_dech*i,])[1],]$ACUMULA
  BD1h[(dim(BD1h[BD1h$ACUMULA<tam_dech*i,])[1]+1),]$factor<-b1
  BD1h[(dim(BD1h[BD1h$ACUMULA<tam_dech*i,])[1]+2),]$factor<-(a1h-b1h)
}
BD1h$ACUMULA2<-cumsum(BD1h$factor)
BD1h$DECIL <- 0
BD1h[(BD1h$ACUMULA2<=tam_dech),]$DECIL <- 1
for(i in 1:9)
{
  BD1h[((BD1h$ACUMULA2>tam_dech*i)&(BD1h$ACUMULA2<=tam_dech*(i+1))),]$DECIL <- (i+1)
}
BD1h[BD1h$DECIL%in%"0",]$DECIL <- 10
## Total de hogares
xh <- tapply(BD1h$factor,BD1h$Nhog,sum)

## Deciles
yh <- tapply(BD1h$factor,BD1h$DECIL,sum)
## Se calcula el promedio de ingreso para el total y para cada uno de los deciles
ing_cormed_th <- tapply(BD1h$factor*BD1h$ing_cor,BD1h$Nhog,sum)/xh
ing_cormed_dh <- tapply(BD1h$factor*BD1h$ing_cor,BD1h$DECIL,sum)/yh
## Cuadros
## Guarda los resultados en un data frame
prom_rubh <- data.frame (c(ing_cormed_th,ing_cormed_dh))
## Agrega el nombre a las filas
row.names(prom_rubh) <- Numded

xh <- tapply(BD1h$factor,BD1h$Nhog,sum)
yh <- tapply(BD1h$factor,BD1h$DECIL,sum)

ing_cormed_th <- tapply(BD1h$factor*BD1h$ing_cor,BD1h$Nhog,sum)/xh
ing_cormed_dh <- tapply(BD1h$factor*BD1h$ing_cor,BD1h$DECIL,sum)/yh

prom_rubh <- data.frame (c(ing_cormed_th,ing_cormed_dh))
row.names(prom_rubh) <- Numded

#para mujeres----
Concm <- enigh |> 
  filter(sexo_jefe == 2)

Concm <- orderBy (~+ing_cor+folioviv+foliohog, data= Concm)

Concm$Nhog <- 1

tot_hogaresm <- Concm |> 
  summarise(sum(factor)) |> 
  pull()

tam_decm<-trunc(tot_hogaresm/10)

Concm$tam_decm=tam_decm

BD1m <- Concm

BD1m$MAXT <- BD1m$ing_cor

BD1m <- BD1m[with(BD1m, order(rank(MAXT))),]

BD1m$ACUMULA <- cumsum(BD1m$factor)


for(i in 1:9)
{
  a1m<-BD1m[dim(BD1m[BD1m$ACUMULA<tam_decm*i,])[1]+1,]$factor
  BD1m<-rbind(BD1m[1:(dim(BD1m[BD1m$ACUMULA<tam_decm*i,])[1]+1),],
              BD1m[(dim(BD1m[BD1m$ACUMULA<tam_decm*i,])[1]+1):dim(BD1m[1])[1],])
  b1m<-tam_decm*i-BD1m[dim(BD1m[BD1m$ACUMULA<tam_decm*i,])[1],]$ACUMULA
  BD1m[(dim(BD1m[BD1m$ACUMULA<tam_decm*i,])[1]+1),]$factor<-b1
  BD1m[(dim(BD1m[BD1m$ACUMULA<tam_decm*i,])[1]+2),]$factor<-(a1m-b1m)
}
BD1m$ACUMULA2<-cumsum(BD1m$factor)
BD1m$DECIL <- 0
BD1m[(BD1m$ACUMULA2<=tam_decm),]$DECIL <- 1
for(i in 1:9)
{
  BD1m[((BD1m$ACUMULA2>tam_decm*i)&(BD1m$ACUMULA2<=tam_decm*(i+1))),]$DECIL <- (i+1)
}
BD1m[BD1m$DECIL%in%"0",]$DECIL <- 10
## Total de hogares
xm <- tapply(BD1m$factor,BD1m$Nhog,sum)

## Deciles
ym <- tapply(BD1m$factor,BD1m$DECIL,sum)
## Se calcula el promedio de ingreso para el total y para cada uno de los deciles
ing_cormed_tm <- tapply(BD1m$factor*BD1m$ing_cor,BD1m$Nhog,sum)/xm
ing_cormed_dm <- tapply(BD1m$factor*BD1m$ing_cor,BD1m$DECIL,sum)/ym

## Guarda los resultados en un data frame
prom_rubm <- data.frame (c(ing_cormed_tm,ing_cormed_dm))
## Agrega el nombre a las filas
row.names(prom_rubm) <- Numded

xm <- tapply(BD1m$factor,BD1m$Nhog,sum)
ym <- tapply(BD1m$factor,BD1m$DECIL,sum)

ing_cormed_tm <- tapply(BD1m$factor*BD1m$ing_cor,BD1m$Nhog,sum)/xm
ing_cormed_dm <- tapply(BD1m$factor*BD1m$ing_cor,BD1m$DECIL,sum)/ym

prom_rubm <- data.frame (c(ing_cormed_tm,ing_cormed_dm))
row.names(prom_rubm) <- Numded
