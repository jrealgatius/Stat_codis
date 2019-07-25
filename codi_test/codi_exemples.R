# Codi exemple per avaluar les funcions propies del Biostat DAP-CAP  ------


# Carrega de funcions 

source("funcions_propies.R")

# etiquetar() mtcars -------

# Funció que assigna label segons una taula externa ("conductor.xls")

mtcars

summary(mtcars)

descrTable(~.,data=mtcars)

# Aplicar etiquetatje 
mtcars_lab<-etiquetar(d=mtcars,taulavariables="./dades_test/taulavariables.xls",camp_descripcio="descripcio")


descrTable(~.,data=mtcars_lab)


library(expss)

#------------------------------#
#i)
expss::cro(mtcars)
descrTable(~.,data=mtcars)
#ii)
expss::cro(mtcars_lab)
descrTable(~.,data=mtcars_lab)
#------------------------------#