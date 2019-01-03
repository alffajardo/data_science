## Un ejemplo práctico de ancova

library(dplyr)
Data <- read.csv("crickets.csv")%>%
         select(2:4)
attach(Data)
plot(Temp,Pulse, col=factor(Species),pch=20,xlim=c(16,32),
     ylim=c(0,120), xlab="Temperture",ylab="calling rate(pulses per second)")
text(23,100,"O.exclamationis",cex=0.7)
text(23,50,"O.Nievus",cex=0.7)

lm_ex <- lm(Pulse[Species=="ex"]~ Temp[Species=="ex"])
y0_ex <- lm_ex$coefficients[2]*20 + lm_ex$coefficients[1]
y1_ex <- lm_ex$coefficients[2]* 31 + lm_ex$coefficients[1]
segments(x0=20,x1=31,y0=y0_ex,y1 = y1_ex,lty=2)

lm_niv <- lm(Pulse[Species=="niv"]~ Temp[Species=="niv"])
y0_niv <- lm_niv$coefficients[2]*17 + lm_niv$coefficients[1]
y1_niv <- lm_niv$coefficients[2]* 29 + lm_niv$coefficients[1]
segments(x0=17,x1=29,y0=y0_niv,y1 = y1_niv,lty=1,col="red")



