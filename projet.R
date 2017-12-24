#Import et installation des librairies utiles
#install.packages("readxl")
rm (list=ls())
library("readxl")
setwd("C:/Users/LokiS/Documents/Master 2/Modéle de Regression/Projet/RegressionLineaire")
train_set <- read_excel("data/FW_groupe2.xls")
test_set <- read_excel("data/FW_groupe2_obs.xls")

#Premiere étape : Suppression les observations contenant des valeurs manquantes
##Apparament aucune valeur vide ? Pas sur fais gaffe
### CF réponse Mail

## Deuxieme étape, on cherche les relations linéaire à 1 variable qui lient un regresseur
## à un autre. On élimine ceux qui peuvent être prédit avec un rsquared > 0.95
nbDescripteurs = length(train_set) 


listDescripteurs <- c()
for (i in 2:nbDescripteurs){
  for (j in i:nbDescripteurs){
    if (i !=j){
      res = lm(as.matrix(train_set[i]) ~ as.matrix(train_set[j]), data=train_set)
      if (summary(res)$r.squared > 0.95 ){
        if (!(i %in% listDescripteurs)){
          listDescripteurs <- c(listDescripteurs, i)
        }
      }
    }
  }  
}

## Troisieme étape, on cherche les relations linéaire à 2 variables. On effectue comme
## l'étape précédente
### On modifie notre dataSet
train_set_iteration_1 = train_set[, -listDescripteurs]
test_set_iteration_1 = test_set[, -listDescripteurs]
nbDescripteurs = length(train_set_iteration_1)
listDescripteurs <- c()
for (i in 2:nbDescripteurs){
  for (j in i:nbDescripteurs){
    for (k in j:nbDescripteurs){
      if (i!=j && i!=k && j!=k){
        res = lm(as.matrix(train_set_iteration_1[i]) ~ as.matrix(train_set_iteration_1[j]) + as.matrix(train_set_iteration_1[k]), data=train_set_iteration_1)
        if (summary(res)$r.squared > 0.95 ){
          if (!i %in% listDescripteurs){
            listDescripteurs <- c(listDescripteurs, i)
          }
        }
      }
    }
    
  }  
}
## Troisieme étape, on cherche les relations linéaire à 3 variables. On effectue comme
## l'étape précédente
### On modifie notre dataSet
train_set_iteration_2 = train_set_iteration_1[, -listDescripteurs]
test_set_iteration_2 = test_set_iteration_1[, -listDescripteurs]

nbDescripteurs = length(train_set_iteration_2)
listDescripteurs <- c()
for (i in 2:nbDescripteurs){
  for (j in i:nbDescripteurs){
    for (k in j:nbDescripteurs){
      for (l in k:nbDescripteurs){
        if (i!=j && i!=k && i!=l && j!=k && j!=l && k!=l){
          res = lm(as.matrix(train_set_iteration_2[i]) ~ as.matrix(train_set_iteration_2[j]) + as.matrix(train_set_iteration_2[k]), data=train_set_iteration_2)
          if (summary(res)$r.squared > 0.95 ){
            if (!i %in% listDescripteurs){
              listDescripteurs <- c(listDescripteurs, i)
            }
          }
        }
      }
    }
  }  
}
#### Utilisé avant
#Deuxieme étape : Suppression des descripteurs inutiles
#On affiche quels sont les descripteurs qui sont liées
listDescripteurs <- c()
for (i in names(train_set)){
  if (i != "X__1"){
    message("Pour le ", i)
    for (j in names(train_set)){
      if (i != j && j != "X__1"){
        res = lm(as.matrix(train_set[i]) ~ as.matrix(train_set[j]), data=train_set)
        if (summary(res)$r.squared > 0.95 ){
            print(j)
          listDescripteurs <- c(listDescripteurs, j)
        }
      }
    }

  }
}
listDescripteurs <- unique(listDescripteurs)
train_set_1 <- train_set
# On supprime les variables liées 
for (i in listDescripteurs){
  train_set_1[i] <-NULL
}
# Puis recommencer l'algo avec des relations à 2 variables, puisà  3
## Apres cette étape on aura 10, 15 variables éliminées

###ZONE DE TEST
