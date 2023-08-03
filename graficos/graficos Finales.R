remove(list=ls())
gc()

library(here)

setwd(here())

AA_final <- read.csv("../experimento/AA_final.csv")
HD_final <- read.csv("../experimento/HD_final.csv")
RL_final <- read.csv("../experimento/RL_final.csv")



RL<-as.numeric(t(RL_final[1,3:32]))
AA<-as.numeric(t(AA_final[1,3:32]))
HD<-as.numeric(t(HD_final[1,3:32]))



boxp2<-data.frame(cbind(RL,
                        AA,
                        HD))

par(mfrow = c(1, 2))
boxplot(boxp2, main="ECM", col = c("red", "blue", "violet"),outline=FALSE)


# EMA
RL<-as.numeric(t(RL_final[3,3:32]))
AA<-as.numeric(t(AA_final[3,3:32]))
HD<-as.numeric(t(HD_final[3,3:32]))


boxp2<-data.frame(cbind(RL,
                        AA,
                        HD))

boxplot(boxp2, main="EMA", col = c("red", "blue", "violet"),outline=FALSE)

#bias  (actual - predicted).

RL<-as.numeric(t(RL_final[4,3:32]))
AA<-as.numeric(t(AA_final[4,3:32]))
HD<-as.numeric(t(HD_final[4,3:32]))

boxp2<-data.frame(cbind(RL,
                        AA,
                        HD))

boxplot(boxp2, main="Sesgo", col = c("red", "blue", "violet"),outline=FALSE)

abline(h=0,col="green", lwd = 2,
       lty = 2)

#varianza
# var(base$P21_final)-var(base$P21)
RL<-as.numeric(t(RL_final[5,3:32]))
AA<-as.numeric(t(AA_final[5,3:32]))
HD<-as.numeric(t(HD_final[5,3:32]))

boxp2<-data.frame(cbind(RL,
                        AA,
                        HD))

boxplot(boxp2, main="varianza (declarada-predicha)", col = c("red", "blue", "violet"),outline=FALSE)


abline(h=0,col="green", lwd = 2,
       lty = 2)

