remove(list=ls())
gc()

library(here)
library(dplyr)
library(reldist)

setwd(here())

# grafico resumen
usu_individual_T422.txt <- read.csv2("../EPH_usu_4to_Trim_2022_txt/usu_individual_T422.txt.txt", sep=";", 
                                     colClasses = c(PP04D_COD = "character"))
usu_individual_T422.txt %>% filter(ESTADO==1 & P21>0)->ocupados
resumen <- read.csv("resumen.csv")
names(resumen)<-c("AA", "RLMR", "HD")
giniTot2<-gini(ocupados$P21, ocupados$PONDERA)


boxplot(resumen, col = c("red", "blue", "violet"), main= "resultados coeficiente de gini")
abline(h=giniTot2,col="green", lwd = 2,
       lty = 2)
