# SEM Lesson R Group 12/4 

library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("piecewiseSEM")

# Set WD and load data
setwd("C:/Users/bwl42/Desktop/sem") 
sem <- read.csv("sem_data.csv", sep = ",", header=TRUE)