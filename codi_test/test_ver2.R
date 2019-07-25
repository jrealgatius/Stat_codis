library(tibble)

#------------#
#iris
#airquality
#ChickWeight
#mtcars
#------------#

#iris

#iris2<-as_tibble(iris)
#iris2
#############################################
#i
#airquality
# Un marc de dades amb 153 observacions sobre 6 variables.
# [, 1] Ozó numèric d'ozó (ppb)
# [, 2] Solar.R numèrica R (lang)
# [, 3] Vent numèric vent (mph)
# [, 4] Temperatura de temperatura numèrica (graus F)
# [, 5] Mes del mes numèric (1--12)
# [, 6] Dia del mes numèric (1--31)
#Detalls
# Lectures diaris dels següents valors de qualitat de l'aire per a l'1 de maig de 1973 (un dimarts) al 30 de setembre de 1973.
#Ozona: significa l'ozó en parts per mil milions de 1300 a 1500 hores a Roosevelt Island
#Solar.R: Radiació solar a Langleys a la banda de freqüències 4000-7700 Angstroms de 0800 a 1200 hores a Central Park
#Wind: velocitat mitjana del vent en milles per hora a les 07.00 i les 1000 hores a l'aeroport de LaGuardia
#Temp: temperatura màxima diària en graus Fahrenheit a l'aeroport de La Guardia.

#boxplot(airquality$Ozone)
#boxplot(airquality$Solar.R)
#boxplot(airquality$Wind)
#boxplot(airquality)

#############################################
#attenu
#beaver1
#beaver2

#ChickWeight

#Els pesos corporals dels pollets es van mesurar en néixer i 
#després cada dos dies fins al dia 20. 
#També es van mesurar el dia 21. 
#Hi va haver quatre grups de pollets en diferents dietes de proteïnes.







mtcars
#Description
#The data was extracted from the 1974 Motor Trend US magazine, 
#and comprises fuel consumption and 10 aspects of automobile design and 
#performance for 32 automobiles (1973-74 models).

#A data frame with 32 observations on 11 (numeric) variables.
#[, 1]	mpg	Miles/(US) gallon
#[, 2]	cyl	Number of cylinders
#[, 3]	disp	Displacement (cu.in.)
#[, 4]	hp	Gross horsepower
#[, 5]	drat	Rear axle ratio
#[, 6]	wt	Weight (1000 lbs)
#[, 7]	qsec	1/4 mile time
#[, 8]	vs	Engine (0 = V-shaped, 1 = straight)
#[, 9]	am	Transmission (0 = automatic, 1 = manual)
#[,10]	gear	Number of forward gears
#[,11]	carb	Number of carburetors




#També popdem mirar  MIRAR...............................:[]
#############################################
#Seatbelts
#Puromycin
#Orange
#CO2
#DNase
#esoph
#Indometh
#infert
#morley
#npk
#OrchardSprays
#sleep
#ToothGrowth
#############################################


mtcars2<-as_tibble(mtcars)
#mtcars2
variable.names(mtcars2)
#C:\Users\38122893W\Desktop\TEST

setwd("C:/Users/38122893W/Desktop/TEST")


# 2. FASE PREPARCIÓ   ####

#---------------#
conductor_variables2<-"taulavariables.xls"
#---------------#


#---------------#
#DT_VISITA_HISTORIC_MES00_lab<-convertir_dates(d=DT_VISITA_HISTORIC_MES00,taulavariables=conductor_variables2)
#---------------#
mtcars2_lab<-etiquetar(d=mtcars2,taulavariables=conductor_variables2)
#---------------#
mtcars2_lab<-etiquetar_valors(dt=mtcars2_lab,variables_factors=conductor_variables2,fulla="etiquetes0",camp_etiqueta="etiqueta1")
#---------------#

summary(mtcars2_lab)

#etiquetar_valors<-function(dt=dades,variables_factors=conductor_variables,fulla="etiquetes",
#                           camp_etiqueta="etiqueta")
#DT_VISITA_HISTORIC_MES00_lab<-LAB_ETIQ_v2(dt=DT_VISITA_HISTORIC_MES00_lab,variables_factors=conductor_variables2,fulla="etiquetes0",idioma="etiqueta2")
#---------------#
#DT_VISITA_HISTORIC_MES00_lab




#taulavariables


