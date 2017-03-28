###################################
#####DATA PREPARATION##############
getwd()
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
#require(data.table)
#
##Loading data frame:
#
weatherSeattle14_16=fread("C:/Users/isabe/Documents/isabel/R_HU_Statistik/Course_StatisticalProgramming/Projects_SPL/
    bikeRental/Rawdata_bikeRental/weather_Seattle2014_2016.csv") 
#
seattle_trip= fread("C:/Users/isabe/Documents/isabel/R_HU_Statistik/Course_StatisticalProgramming/Projects_SPL/bikeRental/
    Rawdata_bikeRental/2016-12_trip_data_1.csv") 
str(seattle_trip)
seattle_trip$Stadtschluessel = "Seattle Pronto"
##cutting and considering only date:
seattle_trip$starttime = substr(seattle_trip$starttime, 1, 10)
str(seattle_trip)
###converting to string with "." as separation
seattle_trip$starttime = paste(substr(seattle_trip$starttime, 4, 5),substr(seattle_trip$starttime, 1, 2),
    substr(seattle_trip$starttime, 7, 10))
seattle_trip$starttime=gsub(" ", ".", seattle_trip$starttime)
#converting chr to date:
seattle_trip$starttime = as.Date(seattle_trip$starttime, "%d.%m.%Y")
weatherSeattle14_16$PST = as.Date(weatherSeattle14_16$PST, "%d.%m.%Y")
######
names(seattle_trip)<-c("trip_id", "startdate", "stoptime", "bikeid", "tripduration", "from_station_name", "to_station_name", 
    "from_station_id", "to_station_id", "usertype", "gender", "birthyear", "Stadtschluessel")
colnames(seattle_trip)
names(weatherSeattle14_16) = c("PST", "Max.TemperaturC", "mittlereTemperaturC",  "Min.TemperaturC", "TaupunktC", 
    "MeanDew PointC", "Min.DewpointC", "Max.Feuchtigkeit", "Mean.Feuchtigkeit", "Min.Feuchtigkeit", 
    "Max.Luftdruck_in_MeereshoehehPa", "Mean.Luftdruck_in_MeereshoehehPa", "Min.Luftdruck_in_MeereshoehehPa", 
    "Max.SichtweiteKm", "Mean.SichtweiteKm", "Min.SichtweiteKm", "Max.WindgeschwindigkeitKm/h", 
    "Mean.WindgeschwindigkeitKm/h", "Max.BoeengeschwindigkeitKm/h", "Niederschlagmm", "CloudCover", "Ereignisse", 
    "WindDirDegrees", "Stadt")                          
colnames(weatherSeattle14_16)
#
#######MERGING TRIP * WEATHER:
seattle_merged<-merge(seattle_trip, weatherSeattle14_16, by.x=c("startdate", "Stadtschluessel"), by.y=c("PST", "Stadt"))
#exporting_writing
fwrite(seattle_merged, "C:/Users/isabe/Documents/isabel/R_HU_Statistik/Course_StatisticalProgramming/Projects_SPL/
bikeRental/Rawdata_bikeRental/seattleMerged.csv") 
####creating other complete dataframe non reduced of 35 columns seattle_merged:
seattle_data1 = seattle_merged
######deleting non-necessary columns)
seattle_merged$stoptime = NULL
seattle_merged$from_station_name = NULL
seattle_merged$to_station_name = NULL
seattle_merged$from_station_id = NULL
seattle_merged$to_station_id = NULL
seattle_merged$Min.DewpointC = NULL
seattle_merged$Max.Feuchtigkeit = NULL
seattle_merged$Min.Feuchtigkeit = NULL
seattle_merged$Max.Luftdruck_in_MeereshoehehPa = NULL
seattle_merged$Min.Luftdruck_in_MeereshoehehPa = NULL
seattle_merged$Max.SichtweiteKm = NULL
seattle_merged$Min.SichtweiteKm = NULL
seattle_merged$`Max.WindgeschwindigkeitKm/h` = NULL
seattle_merged$`Max.BoeengeschwindigkeitKm/h` = NULL
str(seattle_merged)
###Exporting reduced seattle merged as: seattleMerged_reduced
fwrite(seattle_merged, "C:/Users/isabe/Documents/isabel/R_HU_Statistik/Course_StatisticalProgramming/Projects_SPL/
    bikeRental/Rawdata_bikeRental/seattleMerged_reduced.csv") 

###########DATA ANALYSIS#######################################################
###############################################################################
###1stMethod:considering no data transformation
##Importing seattle merged reduced data for Data Analysis#######
seattle_merged_reduced= fread("C:/Users/isabe/Documents/isabel/R_HU_Statistik/Course_StatisticalProgramming/
    Projects_SPL/bikeRental/Rawdata_bikeRental/seattleMerged_reduced.csv") 
#########################################
####saving reduced seattle_merged_reduced with other name:
seattle_data<-seattle_merged_reduced
#seattle_trip$starttime<-as.Date(seattle_trip$starttime, "%d.%m.%Y") 
##3D Scatterplots########################
library ("scatterplot3d") 
#Plot 1: just in black##3 Variables(tripduration, precipitation and mean T°C)
scatterplot3d(seattle_data$Niederschlagmm, seattle_data$mittlereTemperaturC, seattle_data$tripduration)
#
#Plot 2: in colours  #3 Variables + 1 colkey (Ereignisse)
dev.off()
seattle_data$Niederschlagmm = as.numeric(seattle_data$Niederschlagmm)
seattle_data$Niederschlagmm[is.na(seattle_data$Niederschlagmm)] = 0
str(seattle_data)
#seattle_data1$tripduration = as.numeric(seattle_data1$tripduration)#do not needed!it is already
seattle_data$mittlereTemperaturC = as.numeric(seattle_data$mittlereTemperaturC)
##
##Scatter3D coloured plot for 3 variables, mean T° is coloured
layout(cbind(1:2, 1:2), heights = c(2, 1))
temp = hsv((temp = 0.7*seattle_data$mittlereTemperaturC/diff(range(seattle_data$mittlereTemperaturC)))-min(temp) + 0.3)
s3d = scatterplot3d(seattle_data$Niederschlagmm, seattle_data$mittlereTemperaturC, seattle_data$tripduration,
    pch=5, color=temp,
    main="Influence of precipitation and temperature on tripduration",
    xlab="precipitation, mm",
    ylab="mean temperature, °C",
    zlab="tripduration, min")
par(mar=c(5, 3, 0, 3))
plot(seq(min(seattle_data$mittlereTemperaturC), max(seattle_data$mittlereTemperaturC), length = 10), rep(0, 10), pch = 2,
    axes = FALSE, xlab = "color code of variable \"mean T°C\"", ylab = "",
    col = hsv(seq(0.3, 1, length = 10)))
axis(1, at = seq(-20, 25, 5))
###################################################################################
###2ndMethod: scatter3D with transformation Log(tripdata):#########################
##Importing seattle merged reduced data for Data Analysis
seattle_merged_reduced= fread("C:/Users/isabe/Documents/isabel/R_HU_Statistik/Course_StatisticalProgramming/
    Projects_SPL/bikeRental/Rawdata_bikeRental/seattleMerged_reduced.csv") 
#
####saving dataframe seattle_merged_reduced with other name:
seattleData = seattle_merged_reduced  
#
###########
dev.off()
seattleData$Niederschlagmm = as.numeric(seattleData$Niederschlagmm)
seattleData$Niederschlagmm[is.na(seattleData$Niederschlagmm)] = 0
str(seattleData)
seattleData$mittlereTemperaturC = as.numeric(seattleData$mittlereTemperaturC)
#
#log (tripduration):
seattleData$tripduration = log(seattleData$tripduration) ##log(tripduration)
#
layout(cbind(1:2, 1:2), heights = c(2, 1))
temp = hsv((temp = 0.7*seattleData$mittlereTemperaturC/diff(range(seattleData$mittlereTemperaturC)))-min(temp) + 0.3)
s3d = scatterplot3d(seattleData$Niederschlagmm, seattleData$mittlereTemperaturC, seattleData$tripduration,
    pch=5, color=temp,
    main="Influence of precipitation and temperature on tripduration",
    xlab="precipitation, mm",
    ylab="mean temperature, °C",
    zlab="log tripduration, min")
par(mar=c(5, 3, 0, 3))
plot(seq(min(seattleData$mittlereTemperaturC), max(seattleData$mittlereTemperaturC), length = 10), rep(0, 10), pch = 2,
    axes = FALSE, xlab = "color code of variable \"mean T°C\"", ylab = "",
    col = hsv(seq(0.3, 1, length = 10)))
axis(1, at = seq(-20, 25, 5))

################################################################################
##Plot3D Package################################################################
##1st method: considering whole Data Set
###3 Variables + 1 colkey (Ereignisse=weather)
#
##Importing seattle merged reduced data for Data Anaylisis#######
seattle_merged_reduced= fread("C:/Users/isabe/Documents/isabel/R_HU_Statistik/Course_StatisticalProgramming/
    Projects_SPL/bikeRental/Rawdata_bikeRental/seattleMerged_reduced.csv") 
##saving seattle_merged reduced with other name
seattle_data = seattle_merged_reduced 
#############################################
###converting variables formats:
dev.off()
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
#
######Taking directly seattle_data from 3dscatterplot is auch OK
##New values for Ereignisse (weather)
typeof(seattle_data$Ereignisse)
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Regen")] = 2
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Schneefall")] = 3
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Regen-Gewitter")] = 5
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Nebel-Regen")] = 4
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Nebel")] = 1
#
#Converting Ereignisse chr to int  and NA to 0:
seattle_data$Ereignisse = as.integer(seattle_data$Ereignisse)
#
seattle_data$Ereignisse[is.na(seattle_data$Ereignisse)] = 0
str(seattle_data) #Ereignisse is num
#
##################################
## Exporting cleaned seattle_data to file:eattle.csv for obtaining distribution modells 
#fwrite(seattle_data, "C:/Users/isabe/Documents/isabel/R_HU_Statistik/Course_StatisticalProgramming/
    Projects_SPL/bikeRental/Rawdata_bikeRental/Seattle_1.csv") 
##################################
##################################
##Importing cleaned seattle_data:
#seattle_data = fread("C:/Users/isabe/Documents/isabel/R_HU_Statistik/Course_StatisticalProgramming/Projects_SPL/
    bikeRental/Rawdata_bikeRental/Seattle_1.csv") 
#
##############
library(plot3D)
par(mfrow = c(1.0, 1.0))
panelfirst = function(pmat) {
    zmin = min(seattle_data$tripduration)    
    XY = trans3D(seattle_data$Niederschlagmm, seattle_data$mittlereTemperaturC,  
        z = rep(zmin, nrow(seattle_data)), pmat = pmat)
    scatter2D(XY$x, XY$y, 
        colvar = seattle_data$Ereignisse, 
        pch = ".",
        cex = 2, add = TRUE, colkey = FALSE)
    xmin = min(seattle_data$Niederschlagmm)
    XY = trans3D(x =rep(xmin, nrow(seattle_data)), y=seattle_data$mittlereTemperaturC,
        z = seattle_data$tripduration, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = seattle_data$Ereignisse,
        pch = ".",
        cex = 2, add = TRUE, colkey = FALSE)
}
with(seattle_data, scatter3D(x=seattle_data$Niederschlagmm, y=seattle_data$mittlereTemperaturC, z=seattle_data$tripduration,
        colvar=seattle_data$Ereignisse,
        pch=8, cex=1.0, xlab="°Precipitation mm", ylab ="Mean T °C",
        zlab="Tripduration, min", clab=c("Weather"),
        main="People rent bikes more in raining than by clouding between 10-20°C\n City: 
            Seattle\n Weather values: 1=cloud, 2=rain, 3=snow, 4=cloudy-rain, 5=rain-storm\n Note: 
            0=NA \n",
        ticktype="detailed",
        panel.first=panelfirst, theta=15, d=2.0,
        colkey=list(length=0.5, width=0.5, cex.clab=0.75))
)
##################################################################################
#2nd method: TRUNCATED Data Ereignisse :just to get thermometer values from 1 to 5:
#################################################################################
#Result: no much optical difference respect to the whole data above!
#
##truncating NA Ereignisse:
sep2 = seattle_data$Ereignisse==0
seattle_data$subtable = cumsum(sep2)+1
seattle_data_sep = seattle_data[!sep2,]
###
library(plot3D)
par(mfrow = c(1.8, 1.8))
panelfirst <- function(pmat) {
    zmin = min(seattle_data_sep$tripduration)    
    XY = trans3D(seattle_data_sep$Niederschlagmm, seattle_data_sep$mittlereTemperaturC,  
        z = rep(zmin, nrow(seattle_data_sep)), pmat = pmat)
    scatter2D(XY$x, XY$y, 
        colvar = seattle_data_sep$Ereignisse, 
        pch = ".",
        cex = 2, add = TRUE, colkey = FALSE)
    xmin = min(seattle_data_sep$Niederschlagmm)
    XY = trans3D(x =rep(xmin, nrow(seattle_data_sep)), y=seattle_data_sep$mittlereTemperaturC,
        z = seattle_data_sep$tripduration, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = seattle_data_sep$Ereignisse,
        pch = ".",
        cex = 2, add = TRUE, colkey = FALSE)
}
with(seattle_data_sep, scatter3D(x=seattle_data_sep$Niederschlagmm, y=seattle_data_sep$mittlereTemperaturC, 
    z=seattle_data_sep$tripduration,
    colvar=seattle_data_sep$Ereignisse,
    pch=8, cex=1.0, xlab="°Precipitation mm", ylab ="Mean T °C",
    zlab="tripduration, min", clab=c("Weather"),
    main="People rent bikes more in raining than by clouding between 10-20°C \n City: Seattle\n Weather values: 1=cloud, 
        2=rain, 3=snow, 4=cloudy-rain, 5=rain-storm \n",
    ticktype="detailed",
    panel.first=panelfirst, theta=15, d=2.0,
    colkey=list(length=0.4, width=0.5, cex.clab=0.75))
)
#############################################################
#############################################################
##3rd Method: 3DPlot  with Log (tripduration):
#############################################################
##Importing seattle merged reduced data for Data Analysis#######
seattle_merged_reduced= fread("C:/Users/isabe/Documents/isabel/R_HU_Statistik/Course_StatisticalProgramming/Projects_SPL/
    bikeRental/Rawdata_bikeRental/seattleMerged_reduced.csv") 
####saving reduced seattle_merged with other names
seattle_data = seattle_merged_reduced # to be used for normal data
#
###3 Variables + 1 colkey (Ereignisse)
#Niederschlagmm chr to num and NA to 0:
seattle_data$Niederschlagmm = as.numeric(seattle_data$Niederschlagmm)
seattle_data$Niederschlagmm[is.na(seattle_data$Niederschlagmm)] = 0
#
seattle_data$mittlereTemperaturC = as.numeric(seattle_data$mittlereTemperaturC)
str(seattle_data)
##New value for Ereignisse (weather)
typeof(seattle_data$Ereignisse)
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Regen")] = 2
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Schneefall")] = 3
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Regen-Gewitter")] = 5
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Nebel-Regen")] = 4
seattle_data$Ereignisse[which(seattle_data$Ereignisse == "Nebel")] = 1
#
#Converting Ereignisse chr to int  and NA to 0:
seattle_data$Ereignisse=as.integer(seattle_data$Ereignisse)
seattle_data$Ereignisse[is.na(seattle_data$Ereignisse)] = 0
str(seattle_data) #Ereignisse is num
################
#transforming as log of tripduration:
seattle_data$tripduration = log(seattle_data$tripduration) 
###########
library(plot3D)
par(mfrow = c(1.0, 1.0))
panelfirst <- function(pmat) {
    zmin = min(seattle_data$tripduration)    
    XY = trans3D(seattle_data$Niederschlagmm, seattle_data$mittlereTemperaturC,  
        z = rep(zmin, nrow(seattle_data)), pmat = pmat)
    scatter2D(XY$x, XY$y, 
        colvar = seattle_data$Ereignisse, 
        pch = ".",
        cex = 2, add = TRUE, colkey = FALSE)
    xmin = min(seattle_data$Niederschlagmm)
    XY = trans3D(x =rep(xmin, nrow(seattle_data)), y=seattle_data$mittlereTemperaturC,
        z = seattle_data$tripduration, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = seattle_data$Ereignisse,
        pch = ".",
        cex = 2, add = TRUE, colkey = FALSE)
}
with(seattle_data, scatter3D(x=seattle_data$Niederschlagmm, y=seattle_data$mittlereTemperaturC, z=seattle_data$tripduration,
    colvar=seattle_data$Ereignisse,
    pch=8, cex=1.0, xlab="°Precipitation mm", ylab ="Mean T °C",
    zlab="log Tripduration, min", clab=c("Weather"),
    main="People rent bikes more in raining than by clouding between 10-20°C\n City: Seattle\n Weather values: 1=cloud, 
        2=rain, 3=snow, 4=cloudy-rain, 5=rain-storm\n Note: 0=NA \n",
    ticktype="detailed",
    panel.first=panelfirst, theta=15, d=2.0,
    colkey=list(length=0.5, width=0.5, cex.clab=0.75))
)
##################################################################################
#4th Method: truncated Data Ereignisse :just to get thermometer values from 1 to 5: 
##################################################################################
#Result: no much optical difference respect to the whole data above!
##truncating NA Ereignisse:
sep2 = seattle_data$Ereignisse==0
seattle_data$subtable =  cumsum(sep2)+1
seattle_data_sep = seattle_data[!sep2,]
###
library(plot3D)
par(mfrow = c(1.0, 1.0))
panelfirst = function(pmat) {
    zmin = min(seattle_data_sep$tripduration)    
    XY = trans3D(seattle_data_sep$Niederschlagmm, seattle_data_sep$mittlereTemperaturC,  
        z = rep(zmin, nrow(seattle_data_sep)), pmat = pmat)
    scatter2D(XY$x, XY$y, 
        colvar = seattle_data_sep$Ereignisse, 
        pch = ".",
        cex = 2, add = TRUE, colkey = FALSE)
    xmin = min(seattle_data_sep$Niederschlagmm)
    XY = trans3D(x =rep(xmin, nrow(seattle_data_sep)), y=seattle_data_sep$mittlereTemperaturC,
        z = seattle_data_sep$tripduration, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = seattle_data_sep$Ereignisse,
        pch = ".",
        cex = 2, add = TRUE, colkey = FALSE)
}
with(seattle_data_sep, scatter3D(x=seattle_data_sep$Niederschlagmm, y=seattle_data_sep$mittlereTemperaturC, 
    z=seattle_data_sep$tripduration,
    colvar=seattle_data_sep$Ereignisse,
    pch=8, cex=1.0, xlab="°Precipitation mm", ylab ="Mean T °C",
    zlab="Log tripduration, min", clab=c("Weather"),
    main="People rent bikes more in raining than by clouding between 10-20°C \n City: Seattle\n Weather values: 1=cloud, 
        2=rain, 3=snow, 4=cloudy-rain, 5=rain-storm \n",
    ticktype="detailed",
    panel.first=panelfirst, theta=15, d=2.0,
    colkey=list(length=0.4, width=0.5, cex.clab=0.75))
)
