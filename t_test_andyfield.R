#### learning to do t test with R. Andy Field data
library(tidyverse)
library(WRS2)
spiderWide <- read.csv("SpiderWide.csv",sep = "\t") %>%
mutate(pMean = (picture +real)/2 ) %>%
mutate(grandMean = mean(c(picture,real))) %>%
mutate(adj = grandMean - pMean)%>%
mutate (picture_adj = picture + adj)%>%
mutate(real_adj  = real+adj) %>%
  select(picture_adj,real_adj)

attach(spiderWide)

spiderlong <- read.csv("spiderlong.csv",sep="\t")

summary(lm(Anxiety ~ Group, data = spiderlong))

### Calculte t test manually

x1 <- mean(spiderlong$Anxiety [spiderlong$Group=="Real Spider"])
x2 <- mean(spiderlong$Anxiety[spiderlong$Group=="Picture"])
sd1 <- sd(spiderlong$Anxiety [spiderlong$Group=="Real Spider"])
sd2 <- sd(spiderlong$Anxiety[spiderlong$Group=="Picture"])
n1 <- length(spiderlong$Group[spiderlong$Group =="Real Spider"])
n2 <- length(spiderlong$Group[spiderlong$Group =="Picture"])


ttestfromMeans<-function(x1, x2, sd1, sd2, n1, n2)
{
  df<-n1 + n2 - 2
  poolvar<-(((n1-1)*sd1^2)+((n2-1)*sd2^2))/df
  t<-(x1-x2)/sqrt(poolvar*((1/n1)+(1/n2)))
  sig<-2*(1-(pt(abs(t),df)))
  paste("t(df = ", df, ") = ", t, ", p = ", sig, sep = "")
}

# Exploring T test through robust methods (bootstapping)
detach(spiderWide)
spiderWide <- read.csv("SpiderWide.csv",sep = "\t") 
yuen(spiderWide$real, spiderWide$picture)
