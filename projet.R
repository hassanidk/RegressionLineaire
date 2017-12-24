#Import et installation des librairies utiles
#install.packages("readxl")
rm (list=ls())
library("readxl")
setwd("C:/Users/LokiS/Documents/Master 2/Mod�le de Regression/Projet/RegressionLineaire")
fw_groupe <- read_excel("data/FW_groupe2.xls")
fw_groupe_obs <- read_excel("data/FW_groupe2_obs.xls")

#Premiere �tape : Suppression les observations contenant des valeurs manquantes
##Apparament aucune valeur vide ? Pas sur fais gaffe

### TODO ?

#Deuxieme �tape : Suppression des descripteurs inutiles
#On affiche quels sont les descripteurs qui sont li�es
listDescripteurs <- c()
for (i in names(fw_groupe)){
  if (i != "X__1"){
    message("Pour le ", i)
    for (j in names(fw_groupe)){
      if (i != j && j != "X__1"){
        res = lm(as.matrix(fw_groupe[i]) ~ as.matrix(fw_groupe[j]), data=fw_groupe)
        if (summary(res)$r.squared > 0.95 ){
            print(j)
          listDescripteurs <- c(listDescripteurs, j)
        }
      }
    }

  }
}
listDescripteurs <- unique(listDescripteurs)
fw_groupe_1 <- fw_groupe
# On supprime les variables li�es 
for (i in listDescripteurs){
  fw_groupe_1[i] <-NULL
}
# Puis recommencer l'algo avec des relations � 2 variables, puis�  3
## Apres cette �tape on aura 10, 15 variables �limin�es

###ZONE DE TEST