#BASIC DESCRPTIVES:
#Read INPUT DATA
seattle_merged_reduced= fread("C:/Users/isabe/Documents/isabel/R_HU_Statistik/Course_StatisticalProgramming/Projects_SPL/
    bikeRental/Rawdata_bikeRental/Seattle.csv")#seattleMerged_reduced.csv") 
#
seattle_data = seattle_merged_reduced  #working with seattle_data
#########################################
#some needed format conversions:
seattle_data$Niederschlagmm = as.numeric(seattle_data$Niederschlagmm)
seattle_data$Niederschlagmm[is.na(seattle_data$Niederschlagmm)] = 0
str(seattle_data)
#seattle_data1$tripduration = as.numeric(seattle_data1$tripduration)#do not needed!it is already
seattle_data$mittlereTemperaturC = as.numeric(seattle_data$mittlereTemperaturC)
#
#Niederschlagmm chr to num and NA to 0:
seattle_data$Niederschlagmm = as.numeric(seattle_data$Niederschlagmm)
seattle_data$Niederschlagmm[is.na(seattle_data$Niederschlagmm)] = 0
#Temperatur as numeric:
seattle_data$mittlereTemperaturC = as.numeric(seattle_data$mittlereTemperaturC)
str(seattle_data)

##New values for Ereignisse (weather)
typeof(seattle_data$Ereignisse)
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Regen")] = 2
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Schneefall")] = 3
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Regen-Gewitter")] = 5
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Nebel-Regen")] = 4
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Nebel")] = 1
#
#Converting Ereignisse chr to int  and NA to 0:
seattle_data$Ereignisse=as.integer(seattle_data$Ereignisse)
#
seattle_data$Ereignisse[is.na(seattle_data$Ereignisse)] = 0
str(seattle_data) #Ereignisse is num
##
seattle_data$gender=as.integer(seattle_data$gender)
seattle_data$gender[is.na(seattle_data$gender)] = 2 #NA to 2
#
#usertype to factor:
seattle_data$gender<-factor(seattle_data$gender)
levels(seattle_data$gender)<-list(male="0", female="1", nodata="2")

#######################################################
#BASIC DESCRIPTIVE:
library("MASS")
#Summary(Median, Mean,Quantiles)
summary(seattle_data$tripduration)
sd(seattle_data$tripduration) #standard deviation of tripduration
#Histograms:
hist(seattle_data$birthyear)
hist(seattle_data$mittlereTemperaturC)
density(seattle_data$tripduration) #density of tripduration
#Plots:
plot(seattle_data$birthyear, seattle_data$tripduration)
plot(seattle_data$gender, seattle_data$tripduration)

