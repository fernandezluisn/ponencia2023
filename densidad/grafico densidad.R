remove(list=ls())
gc()

library(dplyr)
library(here)

library(ltm)
library(car)


library(tidyverse)
library(tidymodels)
library(gridExtra)

library(robustbase)

library(randomForest)


here()
setwd(here())

source("../funciones/funciones.r")

calculoP21Final<-function(base, modelo){
  
  error_standard <- sigma(modelo)/10
  base$error <- rnorm(nrow(base),0,error_standard) 
  base$error <- ifelse(base$error>3*error_standard, 3*error_standard,base$error)
  base$error <- ifelse(base$error< -3*error_standard, -3*error_standard,base$error)
  base$P21_con_error<-base$P21_predicho+base$error
  base$P21_final<-exp(as.numeric(base$P21_con_error))
  return(base)
}



usu_individual_T422.txt <- read.csv2("../EPH_usu_4to_Trim_2022_txt/usu_individual_T422.txt.txt", sep=";", 
                                     colClasses = c(PP04D_COD = "character"))



ocupados2<-usu_individual_T422.txt %>% filter(ESTADO==1)
auxiliares(ocupados2)->ocupados2

set.seed(12345)


ponerNulos(ocupados2)->baseEjercicio
baseEjercicio[is.na(baseEjercicio$JERARQUIA),]$JERARQUIA<-9
baseEjercicio[is.na(baseEjercicio$PP3E_TOT),]$PP3E_TOT<-mean(baseEjercicio$PP3E_TOT, na.rm = T)
baseEjercicio[baseEjercicio$INF=="CP formal",]$INF<-"CP otros"
  
  
baseTrain<-baseEjercicio %>% filter(!is.na(P21_i)) 
basePredict<-baseEjercicio %>% filter(is.na(P21_i))
  
##AA
baseTrain<-baseTrain[,c("P21_i","INF", "CH06","CH04","rol", "rama.eph", "NIVEL_ED", "car1","car2", 
                         "AGLOMERADO", "PP07H", "PP07A", "PP07E", "CH03", "CH15", "PP03D","TECNO", 
                         "PP3E_TOT", "CH11", "CH08", "CH09", "PP04C", "CH16", "PP04B1",
                         "PP05C_1", "PP05C_2", "PP05C_3", "REGION", "JERARQUIA", "CAT_OCUP")]
  
  
  
  
m1 <- randomForest(
    formula = P21_i ~ .,
    data    = baseTrain,
    mtry=23, min.node.size=10
  )
  

  
predict(m1,basePredict)->basePredict$P21_AA
  




  
  ##RB
baseTrain<-baseEjercicio %>% filter(!is.na(P21_i)) 
baseTrain$Y<-log(baseTrain$P21_i)
m1 <- lmrob( Y ~ AGLOMERADO + INF + CH06+car1 + PP3E_TOT + PP04C+rama.eph, data=baseTrain,
               weights = PONDERA, fast.s.large.n = Inf )
predict(m1,basePredict)->basePredict$P21_predicho
calculoP21Final(basePredict,m1)->basePredict



##HD
impHD_secuencial(baseEjercicio, "P21_i",dominio = c("AGLOMERADO","INF", "rama.eph","car1", "INTENSI", "PP04C"))->baseEjercicio
baseEjercicio %>% filter(P21_i_impHD)->base2

plot(density(log(basePredict$P21_AA)), col="red", main="densidad log P21 imputada", lwd=3)
lines(density(log(basePredict$P21)), col="black", lwd=5)
lines(density(log(basePredict$P21_final)), col="blue", lwd=3)
lines(density(log(base2$P21_i)), col="green", lwd=3)

legend("topleft",legend = c("Declarado", "AA", "HD","RLMR"), fill = c("black", "red", "green","blue"))
  
  

  

  
  
  
  
  
  
  
  
  
