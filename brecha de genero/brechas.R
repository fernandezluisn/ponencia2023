remove(list=ls())
gc()
options(scipen=999)
library(dplyr)
library(here)

library(ltm)
library(car)


library(tidyverse)
library(tidymodels)
library(gridExtra)

library(robustbase)

library(randomForest)
library(stats)
here()
setwd(here())

source("../funciones/funciones.r")

calculoP21Final<-function(base, modelo){
  
  error_standard <- sigma(modelo)/10
  base$error <- rnorm(nrow(base),0,error_standard) 
  
  base$P21_con_error<-base$P21_predicho+base$error
  base$P21_final<-exp(as.numeric(base$P21_con_error))
  return(base)
}

calcularBrecha<-function(base, variable="P21"){
  
  varones<-base %>% filter(CH04==1)
  mujeres<-base %>% filter(CH04==2)
  
  brecha<-weighted.mean(varones[,variable]/100, varones$PONDIIO)/weighted.mean(mujeres[,variable]/100, mujeres$PONDIIO)
  
  return(brecha)
}

usu_individual_T422.txt <- read.csv2("../EPH_usu_4to_Trim_2022_txt/usu_individual_T422.txt.txt", sep=";", 
                                     colClasses = c(PP04D_COD = "character"))

table(usu_individual_T422.txt$CAT_OCUP)
#### experimento ####
ocupados2<-usu_individual_T422.txt %>% filter(ESTADO==1)
usu_individual_T422.txt %>% filter(ESTADO==1 & P21>0)->ocupados



varones<-ocupados %>% filter(CH04==1)
mujeres<-ocupados %>% filter(CH04==2)

brechaOrig<-weighted.mean(varones$P21/100, varones$PONDIIO)/weighted.mean(mujeres$P21/100, mujeres$PONDIIO)


auxiliares(ocupados2)->ocupados2
remove(usu_individual_T422.txt, ocupados)
gc()

if(file.exists("resumen.csv")){
  resumen <- read.csv("resumen.csv")
  
  
  ni<-nrow(resumen)+1
}else{
  ni=1
}


for(n in ni:30){
  print(n)
  i <- proc.time()
  
  ponerNulos(ocupados2)->baseEjercicio
  baseEjercicio[is.na(baseEjercicio$JERARQUIA),]$JERARQUIA<-9
  baseEjercicio[is.na(baseEjercicio$PP3E_TOT),]$PP3E_TOT<-mean(baseEjercicio$PP3E_TOT, na.rm = T)
  baseEjercicio[baseEjercicio$INF=="CP formal",]$INF<-"CP otros"
  
  baseEjercicio2<-baseEjercicio
  baseTrain<-baseEjercicio %>% filter(!is.na(P21_i)) 
  basePredict<-baseEjercicio %>% filter(is.na(P21_i))
  basePredict1<-baseEjercicio %>% filter(is.na(P21_i))
  
  ##AA
  baseTrain1<-baseTrain[,c("P21_i","INF", "CH06","CH04","rol", "rama.eph", "NIVEL_ED", "car1","car2", 
                           "AGLOMERADO", "PP07H", "PP07A", "PP07E", "CH03", "CH15", "PP03D","TECNO", 
                           "PP3E_TOT", "CH11", "CH08", "CH09", "PP04C", "CH16", "PP04B1",
                           "PP05C_1", "PP05C_2", "PP05C_3", "REGION", "JERARQUIA", "CAT_OCUP")]
  
  
  
  
  
  print("AA")
  # AA
  m1 <- randomForest(
    formula = P21_i ~ .,
    data    = baseTrain1,
    mtry=23, min.node.size=10
  )
  
  
  
  
  predict(m1,basePredict1)->basePredict1$P21_AA  
  
  anti_join(baseEjercicio, basePredict1)->difer
  
  difer$P21_AA<-difer$P21_i
  rbind(difer, basePredict1)->baseAA
  brechaAA<-calcularBrecha(baseAA, "P21_AA")
  
  ##RB
  print("RL")
  baseTrain$Y <- log(baseTrain$P21_i)
  m1 <- lmrob( Y ~ AGLOMERADO + INF + CH06+car1 + PP3E_TOT + PP04C+rama.eph, data=baseTrain,
               weights = PONDERA, fast.s.large.n = Inf )
  predict(m1,basePredict)->basePredict$P21_predicho
  calculoP21Final(basePredict,m1)->basePredict

  
  difer$P21_final<-difer$P21_i
  basePredict$P21_con_error<-NULL
  basePredict$P21_predicho<-NULL
  
  basePredict$error<-NULL
  difer$P21_AA<-NULL
  rbind(difer, basePredict)->baseRL
  
  brechaRL<-calcularBrecha(baseRL, "P21_final")
  
  #HD
  
  print("HD")
  impHD_secuencial(baseEjercicio, "P21_i",dominio = c("CH04","AGLOMERADO","INF", "rama.eph","car1", "INTENSI", "PP04C"))->baseEjercicio
  impHD_secuencial(baseEjercicio2, "P21_i",dominio = c("AGLOMERADO","INF", "rama.eph","car1", "INTENSI", "PP04C"))->baseEjercicio2
  
  brechaHD<-calcularBrecha(baseEjercicio, "P21_i")
  brechaHD2<-calcularBrecha(baseEjercicio2, "P21_i")
  
  
  
  if(n==1){
    
    resumen<-as.data.frame(rbind(c(brechaAA,brechaRL,brechaHD,brechaHD2)))
    
    
    
  }else{
    
    
    as.data.frame(rbind(resumen, c(brechaAA,brechaRL, brechaHD, brechaHD2)))->resumen
    
    write.csv(resumen, "resumen.csv", row.names = F)
    
    
  }
  
  
  
  
  
  
  
}

#### experimento 2####

ocupados2<-usu_individual_T422.txt %>% filter(ESTADO==1)
usu_individual_T422.txt %>% filter(ESTADO==1 & P21>0)->ocupados



auxiliares(ocupados2)->ocupados2
remove(usu_individual_T422.txt, ocupados)
gc()

if(file.exists("resumen2.csv")){
  resumen <- read.csv("resumen2.csv")
  
  
  ni<-nrow(resumen)+1
}else{
  ni=1
}


for(n in ni:31){
  print(n)
  i <- proc.time()
  
  ponerNulos(ocupados2)->baseEjercicio
  baseEjercicio[is.na(baseEjercicio$JERARQUIA),]$JERARQUIA<-9
  baseEjercicio[is.na(baseEjercicio$PP3E_TOT),]$PP3E_TOT<-mean(baseEjercicio$PP3E_TOT, na.rm = T)
  baseEjercicio[baseEjercicio$INF=="CP formal",]$INF<-"CP otros"
  
  baseEjercicio2<-baseEjercicio
  baseTrain<-baseEjercicio %>% filter(!is.na(P21_i)) 
  basePredict<-baseEjercicio %>% filter(is.na(P21_i))
  
  ##RB
  print("RL")
  baseTrain$Y <- log(baseTrain$P21_i)
  m1 <- lmrob( Y ~ AGLOMERADO + INF + CH06+car1 + PP3E_TOT + PP04C+rama.eph+CH04, data=baseTrain,
               weights = PONDERA, fast.s.large.n = Inf )
  predict(m1,basePredict)->basePredict$P21_predicho
  calculoP21Final(basePredict,m1)->basePredict
  
  
  baseTrain$P21_final<-baseTrain$P21_i
  basePredict$P21_con_error<-NULL
  basePredict$P21_predicho<-NULL
  baseTrain$Y<-NULL
  basePredict$error<-NULL
  
  rbind(baseTrain, basePredict)->baseRL
  
  brechaRL2<-calcularBrecha(baseRL, "P21_final")
  
 
  
  
  
  if(n==1){
    
    resumen<-as.data.frame(rbind(c(brechaRL2)))
    
    
    
  }else{
    
    
    as.data.frame(rbind(resumen, c(brechaRL2)))->resumen
    
    write.csv(resumen, "resumen2.csv", row.names = F)
    
    
  }
  
  
  
  
  
  
  
}
