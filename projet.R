#Import et installation des librairies utiles
#install.packages("readxl")
rm (list=ls())
library("readxl")
setwd("C:/Users/LokiS/Documents/Master 2/Modéle de Regression/Projet/RegressionLineaire")
test_set <- read_excel("data/FW_groupe2.xls")
train_set <- read_excel("data/FW_groupe2_obs.xls")

#Premiere étape : Suppression les observations contenant des valeurs manquantes
## Aucune valeur vide, mais vérifier | Vérifier mail

# Deuxieme étape, on cherche les relations linéaire à 1 variable qui lient un regresseur
# à un autre. On élimine ceux qui peuvent être prédit avec un rsquared > 0.95

nbDescripteurs = length(test_set) 
listDescripteurs <- c()
for (i in 2:nbDescripteurs){
    for (j in (i+1):nbDescripteurs){
      if (i<=nbDescripteurs && j <=nbDescripteurs){
        res = lm(as.matrix(test_set[i]) ~ as.matrix(test_set[j]), data=test_set)
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
test_set_iteration_1 = test_set[, -listDescripteurs]
train_set_iteration_1 = train_set[, -listDescripteursTest]
nbDescripteurs = length(test_set_iteration_1)
listDescripteurs <- c()
listDescripteursTest <- c()

## Avec une variable on arrive à supprimmer 9 variables UPDATE : 10
## Troisieme étape, on cherche les relations linéaire à 2 variables. On effectue comme
## l'étape précédente

for (i in 2:nbDescripteurs){
  for (j in (i+1):nbDescripteurs){
    for (k in (j+1):nbDescripteurs){
      if (i <= nbDescripteurs && j <= nbDescripteurs && k<= nbDescripteurs){
        res = lm(as.matrix(test_set_iteration_1[i]) ~ as.matrix(test_set_iteration_1[j]) + as.matrix(test_set_iteration_1[k]), data=test_set_iteration_1)
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
test_set_iteration_2 = test_set_iteration_1[, -listDescripteurs]
train_set_iteration_2 = train_set_iteration_1[, -listDescripteursTest]

nbDescripteurs = length(test_set_iteration_2)
listDescripteurs <- c()
listDescripteursTest <- c()

##Avec deux variables, on arrive à supprimer 11 variables UPDATE 12
## Troisieme étape, on cherche les relations linéaire à 3 variables. On effectue comme
## l'étape précédente

for (i in 2:nbDescripteurs){
  for (j in (i+1):nbDescripteurs){
    for (k in (j+1):nbDescripteurs){
      for (l in (k+1):nbDescripteurs){
        if (i <= nbDescripteurs &&j <= nbDescripteurs && k<= nbDescripteurs && l <= nbDescripteurs){
          res = lm(as.matrix(test_set_iteration_2[i]) ~ as.matrix(test_set_iteration_2[j]) + as.matrix(test_set_iteration_2[k]) +  as.matrix(test_set_iteration_2[l]), data=test_set_iteration_2)
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
#test_set_final = test_set_iteration_2
#train_set_final = train_set_iteration_2

##UPDATE En mettant <= on retire en tout 25 variables
listDescripteursTest = listDescripteurs+1
### On modifie notre dataSet
test_set_final = test_set_iteration_2[, -listDescripteurs]
train_set_final = train_set_iteration_2[, -listDescripteursTest]
# On supprime toutes les variables temporaires
rm(train_set_iteration_1, train_set_iteration_2)
rm(test_set_iteration_1, test_set_iteration_2)
rm(listDescripteurs, listDescripteursTest)
rm(i,j,k,l,nbDescripteurs, res)

#### NOTE : On utilise pas factor car toutes les variables sont continues
#### TODO : IL FAUT DETECTER LES OUTLIERS

# On utilise la regression stepwise pour determiner un modéle par selection de variable
library("MASS")
library("qpcR")

# Explication de PRESS : https://www.rdocumentation.org/packages/qpcR/versions/1.4-0/topics/PRESS

##TODO
X <- as.matrix(train_set_final[,-1:-2])
Y <- as.matrix(train_set_final[2])
# On fixe le minPress à la valeur maximal d'un int
minPress = 2147483647
# Le nombre de descripteurs
nbDescripteurs = length(train_set_final) - 2
# On conserve uniquement les numéro de colonne au lieu d'un vecteur en entier
# Permet d'optimiser l'utilisation des ressources
variablesChoisies = c(0,0,0)

## Regreession Least absolute
#install.packages("Blossom")
library(Blossom)
bestScoreLad = 0
varLad = c(0,0,0)
for (i in 1:nbDescripteurs){
  for (j in (i+1):nbDescripteurs){
    for (k in (j+1):nbDescripteurs){
      if (i<=nbDescripteurs && j<=nbDescripteurs && k<=nbDescripteurs){
          X1 = X[1:25, i]
          X2 = X[1:25, j]
          X3 = X[1:25, k]
          #AIC pour descriptif, BIC pour predictif 
          
          fit = lm(Y ~ X1 + X2 + X3 + X1 * X2 + X1 * X3 + X2 * X3)
          
          ladReg = lad(Y ~ X1 + X2 + X3 + X1 * X2 + X1 * X3 + X2 * X3)
          scoreLad = cor(Y, predict(ladReg))
          #TODO ; Verifier qu'il n'y a pas surapprentissage
          if (scoreLad > bestScoreLad){
            bestScoreLad = scoreLad
            ladFinal = ladReg
            varLad = c(i, j, k)
          }
          #modAIC = stepAIC(fit, direction="both", trace=FALSE)
          modBIC = stepAIC(fit, direction="both", trace=FALSE, k=log(25))    
          if(length(modBIC$coefficients) > 1){
            
            psquareModel = PRESS(modBIC, verbose=FALSE)$P.square
            # On conserve le modele qui a le PRESS le plus faible ainsi que les descripteurs
            if(psquareModel < minPress){
              minPress = psquareModel
              modeleChoisie = modBIC
              variablesChoisies = c(i, j, k)
            }
          }
      }
    }
  }
}




## TEST 1 : Selection de variable via la fonction regsubsets
## permet de donner le meilleur subseet pour n = 3. Evite les diffférentes boucles
# On utilise la librairie leaps
#install.packages('leaps')
## TODO : Pour le rapport expliquer ce que fait cette fonction (Petit bonus)
library(leaps)
test = regsubsets(Y ~ as.matrix(train_set_final[,-1:-2]), data=train_set_final, nvmax=3)
summary(test)$outmat

#On remarque que nous avons pas les mêmes variables.
# En effet cette fonction utilise les descripteurs 1, 14 et 19
# Nous utilisons les descripteurs 1, 8, et 22 si on utilise l'AIC
# Nous utilisons les descripteurs 1 4 9 si on utilise le BIC


#TEST 2: Comparaison modele (Stepwise vs LAD)

cor(Y, fitted(modeleChoisie))
cor(Y, predict(ladFinal))
# Nous obtenons un score de 76% que ce soit avec l'AIC ou le BIC
# LAD 72% lorsqu'on faisait par rappart à PRESS, maintenant à 91% quand on cherche tout simplement
# à gonfler notre score. TODO est ce une bonne méthode ?
# BONUS : On remarque que via la fonction regsubset, on obtient un score de 82%

# Stepwise est sensible aux outliers contrairement à least absolute
# L'idée est donc de supprimer manuellement les outliers pour essayé d'améliorer le score

### TODO
X1 = X[1:25, 1]
X2 = X[1:25, 4]
X3 = X[1:25, 9]
plot(Y, predict(modeleChoisie))
boxplot(predict(modeleChoisie))$out
#On supprime la 24 observation du jeu de donnée de train
X1 = X1[-24]
X2 = X2[-24]
X3 = X3[-24]
Y = Y[-24]

fit = lm(Y ~ X1 + X2 + X3 + X1 * X2 + X1 * X3 + X2 * X3)
modeleChoisie= stepAIC(fit, direction="both", trace=FALSE, k=log(25))
cor(Y, predict(modeleChoisie))
boxplot(predict(modeleChoisie))
#Score inchangé, mais apparition de deux nouveaux outliers
boxplot(predict(modeleChoisie))$out
# On supprime la 10 et 16 observation du de donnée de train
X1 = X1[-c(10, 16)]
X2 = X2[-c(10, 16)]
X3 = X3[-c(10, 16)]
Y = Y[-c(10, 16)]
fit = lm(Y ~ X1 + X2 + X3 + X1 * X2 + X1 * X3 + X2 * X3)
modeleChoisie= stepAIC(fit, direction="both", trace=FALSE, k=log(25))
cor(Y, predict(modeleChoisie))
boxplot(predict(modeleChoisie))
#Le score augmente de 1 point, mais apparation encore d'un outlier
# On pense que ça ne sert à rien de continuer, le modèle est trop sensible
# car on supprime à chaque itération des variables. De plus, notre score augmente
# trop peu.

# TODO : Peut être essayer de supprimer les variables pour essayé d'améliorer LAD
# Test sur le jeu de test et effectuer un boxplot pour voir ?
