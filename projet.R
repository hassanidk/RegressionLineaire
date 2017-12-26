#Import et installation des librairies utiles
#install.packages("readxl")
rm (list=ls())
library("readxl")
setwd("C:/Users/LokiS/Documents/Master 2/Modéle de Regression/Projet/RegressionLineaire")
train_set <- read_excel("data/FW_groupe2.xls")
test_set <- read_excel("data/FW_groupe2_obs.xls")

#Premiere étape : Suppression les observations contenant des valeurs manquantes
## Aucune valeur vide, mais vérifier | Vérifier mail

# Deuxieme étape, on cherche les relations linéaire à 1 variable qui lient un regresseur
# à un autre. On élimine ceux qui peuvent être prédit avec un rsquared > 0.95

nbDescripteurs = length(train_set) 
listDescripteurs <- c()
for (i in 2:nbDescripteurs){
    for (j in (i+1):nbDescripteurs){
      if (j < nbDescripteurs){
        res = lm(as.matrix(train_set[i]) ~ as.matrix(train_set[j]), data=train_set)
        if (summary(res)$r.squared > 0.95 ){
          if (!(i %in% listDescripteurs)){
            listDescripteurs <- c(listDescripteurs, i)
          }
        }
    }  
  }
}
listDescripteursTest = listDescripteurs+1
### On modifie notre dataSet
train_set_iteration_1 = train_set[, -listDescripteurs]
test_set_iteration_1 = test_set[, -listDescripteursTest]
nbDescripteurs = length(train_set_iteration_1)
listDescripteurs <- c()
listDescripteursTest <- c()

## Avec une variable on arrive à supprimmer 9 variables
## Troisieme étape, on cherche les relations linéaire à 2 variables. On effectue comme
## l'étape précédente

for (i in 2:nbDescripteurs){
  for (j in (i+1):nbDescripteurs){
    for (k in (j+1):nbDescripteurs){
      if (j < nbDescripteurs && k< nbDescripteurs){
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

listDescripteursTest = listDescripteurs+1
### On modifie notre dataSet
train_set_iteration_2 = train_set_iteration_1[, -listDescripteurs]
test_set_iteration_2 = test_set_iteration_1[, -listDescripteursTest]

nbDescripteurs = length(train_set_iteration_2)
listDescripteurs <- c()
listDescripteursTest <- c()

##Avec deux variables, on arrive à supprimer 11 variables
## Troisieme étape, on cherche les relations linéaire à 3 variables. On effectue comme
## l'étape précédente

for (i in 2:nbDescripteurs){
  for (j in (i+1):nbDescripteurs){
    for (k in (j+1):nbDescripteurs){
      for (l in (k+1):nbDescripteurs){
        if (j<nbDescripteurs && k<nbDescripteurs && l < nbDescripteurs){
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

## Avec 3 variables on ne supprime aucune variable
## Notre jeu de donnée est donc le suivant
train_set_final = train_set_iteration_2
test_set_final = test_set_iteration_2

# On supprime toutes les variables temporaires
rm(test_set_iteration_1)
rm(test_set_iteration_2)
rm(train_set_iteration_1)
rm(train_set_iteration_2)
rm(listDescripteurs)
rm(listDescripteursTest)

#### NOTE : On utilise pas factor car toutes les variables sont continues
#### TODO : IL FAUT DETECTER LES OUTLIERS

# On utilise la regression stepwise pour determiner un modéle par selection de variable
library("MASS")
##TODO
X <- as.matrix(train_set_final[,-1])
X[1:100, 2]
lm(as.matrix(train_set_final[3]) ~ , data=train_set_final)
modAIC = stepAIC(fit, direction="both")
nbDescripteurs = length(train_set_final)
for (i in 2:nbDescripteurs){
  fit <- lm(as.matrix(train_set_final[i]) ~ ., data=train_set_final)
  modAIC = stepAIC(fit, direction="both")
  modBIC = stepAIC(fit, direction="both", k=log(100)) #Car 100 observation
  modAIC$anova
}
# modAIC = stepAIC(X ,~., trace=TRUE, direction=c("both))
# modBIC = stepAIC(X ,~., trace=TRUE, direction=c("both), k=log(tailleDeLaMatriceAevaluer))
# Faire un summary de chaque modele , et trouver celui qui est le plus interessant
# Faire le predict puis ???