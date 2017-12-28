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
#install.packages('leaps')
library("MASS")
library("qpcR")
library(leaps)
# Explication de PRESS : https://www.rdocumentation.org/packages/qpcR/versions/1.4-0/topics/PRESS

##TODO
X <- as.matrix(test_set_final[,-1:-2])
Y <- as.matrix(test_set_final[2])
# On fixe le minPress à la valeur maximal d'un int
minPress = 2147483647
# Le nombre de descripteurs
nbDescripteurs = length(test_set_final) - 2
# On conserve uniquement les numéro de colonne au lieu d'un vecteur en entier
# Permet d'optimiser l'utilisation des ressources
variablesChoisies = c(0,0,0)

for (i in 3:nbDescripteurs){
  for (j in (i+1):nbDescripteurs){
    for (k in (j+1):nbDescripteurs){
      if (i<nbDescripteurs && j<nbDescripteurs && k<nbDescripteurs){
          X1 = X[1:25, i]
          X2 = X[1:25, j]
          X3 = X[1:25, k]
          #AIC pour descriptif, BIC pour predictif 
          fit = lm(Y ~ X1 + X2 + X3 + X1 * X2 + X1 * X3 + X2 * X3)
          ladReg = lad(Y ~ X1 + X2 + X3 + X1 * X2 + X1 * X3 + X2 * X3)
          #modAIC = stepAIC(fit, direction="forward", trace=FALSE)
          modBIC = stepAIC(fit, direction="both", trace=FALSE, k=log(25))    
          psquareModel = PRESS(modAIC, verbose=FALSE)$P.square
          # On conserve le modele qui a le PRESS le plus faible ainsi que les descripteurs
          if(psquareModel < minPress){
            minPress = psquareModel
            fitFinal = fit
            ladFinal = ladReg
            summary(testFinal)
            modeleChoisie = modBIC
            variablesChoisies = c(i, j, k)
          }
      }
    }
  }
}
library(Blossom)
## Regreession Least absolute
install.packages("Blossom")

## TEST 1: Comparaison de correlation Y et Ypredit des deux modeles

cor(Y, predict(fitFinal))
cor(Y, predict(ladFinal))

## TEST 2 : Selection de variable via la fonction regsubsets
## permet de donner le meilleur subseet pour n = 3
# On remarque qu'on a des correlation à 80% contre 59% avec le truc du prof
test = regsubsets(Y ~ as.matrix(test_set_final[,-1:-2]), data=test_set_final, nvmax=3)
summary(test)$outmat

X1 = X[1:25, 1]
X2 = X[1:25, 5]
X3 = X[1:25, 8]
fit = lm(Y ~ X1 + X2 + X3 + X1 * X2 + X1 * X3 + X2 * X3)
modBIC = stepAIC(fit, direction="both", trace=FALSE, k=log(25))
fitted(modBIC)
cor(Y, fitted(modBIC))
## On obtient un score bien meilleur que les précédents

cor(Y, fitted(modeleChoisie))
plot(modeleChoisie, scale="r2")
plot(Y, predict(modeleChoisie))
plot(Y, predict(modBIC))


# modAIC = stepAIC(X ,~., trace=TRUE, direction=c("both))
# modBIC = stepAIC(X ,~., trace=TRUE, direction=c("both), k=log(tailleDeLaMatriceAevaluer))
# Faire un summary de chaque modele , et trouver celui qui est le plus interessant
# Faire le predict puis ???