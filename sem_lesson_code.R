# SEM Lesson R Group 12/4 

library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("piecewiseSEM")
library("grid")
library("gridExtra")
### Set WD and load data
setwd("C:/Users/bwl42/Desktop/sem") 
sem <- read.csv("sem_data.csv", sep = ",", header=TRUE)

#### Check headers
str(sem)

### Important: Create a full hypothetical model prior to any further path analysis
    ### How do Predator treatments affect aphids?
ggplot(sem, aes(x=Treat, y = totad)) + geom_boxplot()
ggplot(sem, aes(x=Treat, y = totnym)) + geom_boxplot()
ggplot(sem, aes(x=Treat, y = addist)) + geom_boxplot()
ggplot(sem, aes(x=Treat, y = inf.ratio)) + geom_boxplot()

    ### How do aphid responses affect the number of plants infected? 
ggplot(sem,aes(x = totnym, y = inf.ratio)) + geom_smooth(method='lm')
ggplot(sem,aes(x = totad, y = inf.ratio)) + geom_smooth(method='lm')
ggplot(sem,aes(x = addist, y = inf.ratio)) + geom_smooth(method='lm')

    ##### Predictions based on these plots are what inform the a priori model we create
    ##### Draw a priori model on the whiteboard

### Run Primary GLMs for each hypothesized path
  ####Adult Population
ad.pop1 <- glm(totad ~ lethal, data = sem)
####Nymph Population
nym.pop1 <- glm(totnym ~ lethal, data = sem)

ad.dist1 <- glm(addist ~ lethal + risk, data = sem)

inf.rat1 <- glm(inf.ratio ~ totad + totnym + addist, family="binomial", weights= inf.weight, dat=sem)


#### Pop our glms into the sem 
sem.1 <- psem(ad.pop1, nym.pop1, ad.dist1, inf.rat1)

###### Is our model Valid? 
summary(sem.1, standardize = "none", conserve = TRUE)
###### Add our correlated error
summary(update(sem.1, totad %~~% totnym), standardize="none")


##### Can we refine this model to strengthen our case?
#For our infection ratio model, lets add a direct effect of "lethal"

inf.rat2 <- glm(inf.ratio ~ totad + totnym + addist + lethal, family="binomial", weights= inf.weight, dat=sem)

# Create a new SEM
sem.2 <- psem(ad.pop1, nym.pop1, ad.dist1, inf.rat2)
summary(update(sem.2, totad %~~% totnym), standardize= "none")

# Let's see if adding Risk's effect on nymph populations and inf. ratio will improve the model further

nym.pop2 <- glm(totnym ~ lethal + risk, data = sem)
inf.rat3 <- glm(inf.ratio ~ totad + totnym + addist + lethal + risk, family="binomial", weights= inf.weight, dat=sem)

sem.3 <- psem(ad.pop1, nym.pop2, ad.dist1, inf.rat3)
summary(update(sem.3, totad %~~% totnym), standardize= "none")

# Need to report a standardized estimate for effect size for each interaction
summary(update(sem.3, totad %~~% totnym), standardize= "scale")

