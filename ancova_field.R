## ANCOVA --- Andy Field Book
library(multcomp)
library(compute.es)
library(effects)
library(ggplot2)
library(pastecs)
library(WRS2)

viagraData <- read.csv("ViagraCovariate.csv",sep="\t")
