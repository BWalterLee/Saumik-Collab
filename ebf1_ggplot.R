library(tidyverse)

install.packages("ggthemes") # Install 
library(ggthemes) # Load

setwd("C:/Users/bwl42/Desktop/Pred EBF/ebf1")
ebf <- read.csv("ebfpemv1FINALT.csv", sep=",", header=TRUE)

ebf$Treatment <- factor(ebf$treat, levels = c("Sham", "No Cue", "50ng", "200ng"))

ebf$Acquisition_Access_Period <- factor(ebf$Time, levels = c("1h", "3h", "6h", "12h", "24h"))

ggplot(data= ebf, mapping = aes(fill= Treatment, x = Acquisition_Access_Period, y = EFC)) +
  geom_bar(position= "dodge", stat = "identity") +theme_classic() +  theme(panel.grid = element_blank())+
  geom_errorbar(aes(ymin = LowerSE, ymax= UpperSE), width = .1, position = position_dodge(.9)) +
  labs(title = "PEMV 1 Titer", x="Acquisition Access Period", y="Fold Change in Expression" )

#boxplot attempt for log scale SIKE THIS WONT WORK BECAUASE ITS ALL ONE VALUE
ggplot(data= ebf, aes(x = Acquisition_Access_Period, y = EFC)) +
  geom_boxplot(aes(fill = Treatment)) + theme_bw()+
  labs(title = "PEMV 1 Titer", x="Acquisition Access Period", y="Fold Change in Expression" ) + scale_y_log10()

ggsave("EBF_Infinite_Sadness.png")
ggsave("EBF_PEMV1.png")

#YO FUTURE BEN!!!!

  #The files for plotting have a "T" at the end now, Sham values were removed, replaced with zeros

    #Changes axes scaling, but no trends, did it for all 6 plots (pemv 1/2 for ebf, acq, trans)