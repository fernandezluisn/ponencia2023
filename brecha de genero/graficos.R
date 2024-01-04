remove(list=ls())
gc()

library(here)
library(dplyr)

setwd(here())

# grafico resumen
usu_individual_T422.txt <- read.csv2("../EPH_usu_4to_Trim_2022_txt/usu_individual_T422.txt.txt", sep=";", 
                                     colClasses = c(PP04D_COD = "character"))
usu_individual_T422.txt %>% filter(ESTADO==1 & P21>0)->ocupados
resumen <- read.csv("resumen.csv")
resumen2 <- read.csv("resumen2.csv")

resumen$V5<-resumen2$V1


names(resumen)<-c("AA", "RLMR1", "HD2", "HD1", "RLMR2")
resumen[,c("AA", "RLMR1", "RLMR2", "HD1", "HD2")]->resumen
varones<-ocupados %>% filter(CH04==1)
mujeres<-ocupados %>% filter(CH04==2)

brechaOrig<-weighted.mean(varones$P21/100, varones$PONDIIO)/weighted.mean(mujeres$P21/100, mujeres$PONDIIO)




boxplot(resumen, col = c("red", "blue4", "blue","violet", "pink"), main= "Brecha de género")
abline(h=brechaOrig,col="green", lwd = 2,
       lty = 2)
