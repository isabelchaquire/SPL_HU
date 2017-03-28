library(data.table)
#require(data.table)
getwd()
####Importing source file:
seattleData<-fread("C:/Users/isabe/Documents/isabel/R_HU_Statistik/Course_StatisticalProgramming/Projects_SPL/
    bikeRental/Rawdata_bikeRental/Seattle.csv")
#
###1) POISSON REGRESSION##################
str(seattleData)
library(MASS)
library(iterpc)
library(glm.predict)
#
#converting Niederschlag to numeric
seattleData$Niederschlagmm = as.numeric(seattleData$Niederschlagmm)
seattleData$Niederschlagmm[is.na(seattleData$Niederschlagmm)] = 0
str(seattleData)
#converting Ereignisse:
typeof(seattleData$Ereignisse)
seattleData$Ereignisse[which(seattleData$Ereignisse == "Regen")] = 2
seattleData$Ereignisse[which(seattleData$Ereignisse == "Schneefall")] = 3
seattleData$Ereignisse[which(seattleData$Ereignisse == "Regen-Gewitter")] = 5
seattleData$Ereignisse[which(seattleData$Ereignisse == "Nebel-Regen")] = 4
seattleData$Ereignisse[which(seattleData$Ereignisse == "Nebel")] = 1
#
#Converting Ereignisse chr to int  and NA to 0:
seattleData$Ereignisse=as.integer(seattleData$Ereignisse)
seattleData$Ereignisse[is.na(seattleData$Ereignisse)] = 0
str(seattleData) #checking Ereignisse is now num
#
#changing variables contents:
#usertype char to factor:
seattleData$usertype = factor(seattleData$usertype)
levels(seattleData$usertype)<-list(subscriber="subscriber", casual="casual")
#gender char to int:
seattleData$gender = as.integer(seattleData$gender) #shown as num
seattleData$gender[is.na(seattleData$gender)] = 2  #NA converted to num 2
str(seattleData) #checking
#
##1) REGRESSION POISSON:
#3 variables: 1 dependent variable Y and 2 independent X variables:
poi.mod = glm(seattleData$tripduration ~ seattleData$usertype + seattleData$Ereignisse, family=poisson, data=seattleData)
exp(poi.mod$coef)
summary.glm(poi.mod)
#4 variables: 1 dependent Y var. and 3 ind. variables:
poisson.mod = glm(seattleData$tripduration ~ seattleData$usertype+seattleData$gender+seattleData$Ereignisse, 
    family=poisson, data=seattleData)
exp(poisson.mod$coef)
summary.glm(poisson.mod)
#
###2) REGRESSION QUASIPOISSON.
#3 variables: 2 X ind.variables
quasipoi.mod = glm(seattleData$tripduration~seattleData$usertype+seattleData$Ereignisse, family=quasipoisson, data=seattleData)
#
exp(quasipoi.mod$coef)
summary.glm(quasipoi.mod)
#
# 4variables: 3 X ind. var.
quasipoisson.mod = glm(seattleData$tripduration~seattleData$usertype+seattleData$gender+seattleData$Ereignisse, 
    family=quasipoisson, data=seattleData)
exp(quasipoisson.mod$coef)
summary.glm(quasipoisson.mod)
#
################
###3) NEGATIVE BINOMIAL REGRESSION.
# 4variables:  3 X ind. var.
negbinom.mod = glm.nb(seattleData$tripduration~seattleData$usertype+seattleData$gender+seattleData$Ereignisse, 
    data=seattleData, link=log)
exp(negbinom.mod$coef)
summary.glm(negbinom.mod)
#
str(seattleData)
#############################################################
### 4) Linear regression with transformation#################
#transforming Y as logY##
seattleData$tripduration = log(seattleData$tripduration)
lin.mod = lm(seattleData$tripduration~seattleData$usertype+seattleData$gender+seattleData$Ereignisse, data=seattleData)
c(adjusted.R.squared=summary(lin.mod)$adj.r.squared)
plot(lin.mod)
#
##transformation Of x in log(x):
seattleData$usertype = as.numeric(seattleData$usertype)
#seattleData$gender = as.numeric(seattleData$gender)
#seattleData$Ereignisse = as.numeric(seattleData$Ereignisse)
seattleData$usertype = log(seattleData$usertype)
str(seattleData$usertype)
#Now regression with x transformierte variable/s:
xtransf_lin.mod = lm(seattleData$tripduration~seattleData$usertype+seattleData$gender+seattleData$Ereignisse, data=seattleData)
c(adjusted.R.squared=summary(xtransf_lin.mod)$adj.r.squared)
plot(xtransf_lin.mod)
##continuing with backward regression (***)
#
###ANOVA für beide linear transnsformed regression:
anova(lin.mod, xtransf_lin.mod, test="Chisq")
#
#########################################################
###5) SHRINKAGE METHODS##################################
library("glmnet")
#
#A) METHOD BACKWARD SELECTION with logY:
lin.mod_back = step(lin.mod, direction = "backward")
summary(lin.mod_back)
# qqplot & fitted
plot(lin.mod_back)
#
################################################################
#B) METHOD BACKWARD WITH logX, logY transformed lineal
##Taking data from (***) or 4):
xtransf_lin.mod_back = step(xtransf_lin.mod, direction = "backward")
summary(xtransf_lin.mod_back)
# qqplot & fitted
plot(xtransf_lin.mod_back)
