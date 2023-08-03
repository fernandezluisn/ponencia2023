remove(list=ls())
gc()
options(scipen=999)
library(dplyr)
library(here)

library(ltm)
library(robustbase)

library(tidyverse)
library(tidymodels)
library(gridExtra)

library(Metrics)
library(randomForest)
library(reldist)
library(parallel)

here()
setwd(here())

source("../funciones/funciones.r")

hacerTabla<-function(base, tabla_final=NULL){
  sesgo<-c("Bias", "Standard",Metrics::bias(base$P21,base$P21_final))
  varianza<-c("Varianza", "Muestral", var(base$P21)-var(base$P21_final))
  
  if(is.null(tabla_final)){
    
    tabla_final<- rbind(metrics(base,truth=P21, estimate=P21_final),sesgo,varianza)
    
  }else{
    
    tablaActual<- rbind(metrics(base,truth=P21, estimate=P21_final),sesgo,varianza)
    
    tablaActual[, paste0("prueba_",n)]<-tablaActual[,".estimate"]
    tablaActual[,".estimate"]<-NULL
    tabla_final<- left_join(tabla_final,tablaActual, by=c(".metric",".estimator"))
    
  }
  
  return(tabla_final)
}

calculoP21Final<-function(base, modelo){
  
  error_standard <- sigma(modelo)/10
  base$error <- rnorm(nrow(base),0,error_standard) 
  
  base$P21_con_error<-base$P21_predicho+base$error
  base$P21_final<-exp(as.numeric(base$P21_con_error))
  return(base)
}

usu_individual_T422.txt <- read.csv2("../EPH_usu_4to_Trim_2022_txt/usu_individual_T422.txt.txt", sep=";", 
                                    colClasses = c(PP04D_COD = "character"))

table(usu_individual_T422.txt$CAT_OCUP)
#### experimento ####
ocupados2<-usu_individual_T422.txt %>% filter(ESTADO==1)
auxiliares(ocupados2)->ocupados2
remove(usu_individual_T422.txt)
gc()

if(file.exists("AA_final.csv")){
  AA_final<-read.csv("AA_final.csv")
  RL_final<-read.csv("RL_final.csv")
  HD_final<-read.csv("HD_final.csv")
  conteo<- read.csv("conteo_v2.csv")
  
  ni<-ncol(HD_final)-1
}else{
  ni=1
}



for (n in ni:30) {
  print(n)
  i <- proc.time()
  
  ponerNulos(ocupados2)->baseEjercicio
  baseEjercicio[is.na(baseEjercicio$JERARQUIA),]$JERARQUIA<-9
  baseEjercicio[is.na(baseEjercicio$PP3E_TOT),]$PP3E_TOT<-mean(baseEjercicio$PP3E_TOT, na.rm = T)
  baseEjercicio[baseEjercicio$INF=="CP formal",]$INF<-"CP otros"
  
  if(n==1){
    conteo<-baseEjercicio[,c("CODUSU", "NRO_HOGAR", "COMPONENTE", "imp")]
  }else{
    conteo$imp<-conteo$imp+baseEjercicio$imp
  }
  
  baseTrain<-baseEjercicio %>% filter(!is.na(P21_i)) 
  basePredict<-baseEjercicio %>% filter(is.na(P21_i))
  basePredict1<-baseEjercicio %>% filter(is.na(P21_i))
  
  table(ocupados2$INF)
  
  
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
  
 
  
  predict(m1,basePredict1)->basePredict1$P21_final
  
  
  if(n==1){
    hacerTabla(basePredict1)->AA_final
    
    
  }else{
    
    hacerTabla(basePredict1,AA_final)->AA_final
    
  }
  
  
  
  
  remove(basePredict1)
  write.csv(AA_final, "AA_final.csv", row.names = F)
  
  write.csv(conteo, "conteo_v2.csv", row.names = F)
  
  
  
  # RL
  
  print("RL")
  
  baseTrain$Y <- log(baseTrain$P21_i)
  
  m1 <- lmrob( Y ~ AGLOMERADO + INF + CH06+car1 + PP3E_TOT + PP04C+rama.eph, data=baseTrain,
               weights = PONDERA, fast.s.large.n = Inf )
  
  predict(m1,basePredict)->basePredict$P21_predicho
  
  calculoP21Final(basePredict,m1)->basePredict
  
  if(n==1){
    hacerTabla(basePredict)->RL_final
    
    
  }else{
    
    hacerTabla(basePredict,RL_final)->RL_final
    
  }
  
  #HD
  
  print("HD")
  impHD_secuencial(baseEjercicio, "P21_i",dominio = c("AGLOMERADO","INF", "rama.eph","car1", "INTENSI", "PP04C"))->baseEjercicio
  
  baseEjercicio %>% filter(P21_i_impHD)->baseI
  baseI$P21_final<-baseI$P21_i
  if(n==1){
    hacerTabla(baseI)->HD_final
    
    
  }else{
    
    hacerTabla(baseI,HD_final)->HD_final
    
  }
  
  write.csv(HD_final, "HD_final.csv", row.names = F)
  write.csv(RL_final, "RL_final.csv", row.names = F)
  
  f <- proc.time()
  
  print(i-f)
}





total<-as.numeric(t(patrones_Tot[1,3:10]))
parcial<-as.numeric(t(patrones_parc[1,3:10]))
summary(total)
summary(parcial)

boxp2<-data.frame(cbind(total,
                        parcial))

boxplot(boxp2)
