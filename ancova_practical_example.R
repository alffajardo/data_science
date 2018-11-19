## Un ejemplo práctico de ancova
library(dplyr)
Data <- read.csv("crickets.csv")%>%
         select(2:4)
