library(emmeans)
library(tidyverse)
install.packages("multcompView")
library(multcompView)
setwd("C:/Users/bwl42/Desktop/Pred EBF/ebf1")
ebfp1 = read.csv("ebfp1.csv", header=TRUE, sep=",")
head(ebfp1,6)


model.glm <- glm(dCT ~ treat + Time + treat*Time, data=ebfp1)
summary(model.glm)

DFebfp1 <- as.data.frame(cld(emmeans(model.glm, ~ treat*Time, type= "response")))

View(DFebfp1)
