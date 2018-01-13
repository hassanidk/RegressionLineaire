#Import et installation des librairies utiles
#install.packages("readxl")
rm (list=ls())
library("readxl")
setwd("C:/Users/LokiS/Documents/Master 2/Modéle de Regression/Projet/RegressionLineaire")
test_set <- read_excel("data/FW_groupe2.xls")
train_set <- read_excel("data/FW_groupe2_obs.xls")

#Premiere étape : Suppression les observations contenant des valeurs manquantes
missing_value_row = sort(which(test_set==0, TRUE)[,1])
# On remarque que le lignes 10, 11, 13, 25, 26, 48, 70 et 71 ont plusieurs valeurs manquantes
# On supprime ces lignes du dataset
test_set = test_set[-missing_value_row,]
rm(missing_value_row)
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
## On modifie notre dataSet
# On utilise deux vecteurs de descripteur, car dans le jeu de donnée Test, il y a une colonne supplémentaire
# Cela provoque un décalage
listDescripteursTest = listDescripteurs+1
test_set_iteration_1 = test_set[, -listDescripteurs]
train_set_iteration_1 = train_set[, -listDescripteursTest]
nbDescripteurs = length(test_set_iteration_1)
listDescripteurs <- c()
listDescripteursTest <- c()

## Avec une variable on arrive à supprimmer 10 variables
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
## On modifie notre dataSet

listDescripteursTest = listDescripteurs+1
test_set_iteration_2 = test_set_iteration_1[, -listDescripteurs]
train_set_iteration_2 = train_set_iteration_1[, -listDescripteursTest]
nbDescripteurs = length(test_set_iteration_2)
listDescripteurs <- c()
listDescripteursTest <- c()

##Avec deux variables, on arrive à supprimer 13 variables 
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

## Avec 3 variables on ne supprime 3 variables
## On retire en tout 26 variables

### On modifie notre dataSet
listDescripteursTest = listDescripteurs+1
test_set_final = test_set_iteration_2[, -listDescripteurs]
train_set_final = train_set_iteration_2[, -listDescripteursTest]

# On supprime toutes les variables temporaires
rm(train_set_iteration_1, train_set_iteration_2)
rm(test_set_iteration_1, test_set_iteration_2)
rm(listDescripteurs, listDescripteursTest)
rm(i,j,k,l,nbDescripteurs, res)

#### NOTE : On utilise pas factor car toutes les variables sont continues

# On utilise la regression stepwise pour determiner un modéle par selection de variable
library("MASS")
library("qpcR")

# Explication de PRESS : https://www.rdocumentation.org/packages/qpcR/versions/1.4-0/topics/PRESS


X <- as.matrix(train_set_final[,-1:-2])
Y <- as.matrix(train_set_final[2])
# On fixe le minPress à la valeur maximal d'un int
minPress = 2147483647
# Le nombre de descripteurs
nbDescripteurs = length(train_set_final) - 2
# On conserve uniquement les numéro de colonne au lieu d'un vecteur en entier
# Permet d'optimiser l'utilisation des ressources
variablesChoisies = c(0,0,0)

## Regreession Least absolute : On  ne sait pas sur quel modèle se baser pour l'effectuer
## Se baser comme sur la question 2 par rapport au PRESS ?
## Se baser sur un autre critère ?
## Nous avons décider de selectionner le modéle qui prédit au mieux ce jeu de donné train
## Cela ammène à quelques problèmes (Sur apprentissage)
## install.packages("Blossom")

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
          # Méthode qui se base sur un score de correlation
          # Pas très optimisé
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


# Qu'on supprime les lignes contenant les valeurs nulles au début de l'analyse ou pas, nous obtenons les mêmes variables
# Cependant, on supprime une variable en plus lorsqu'on s'occupe des valeurs nulles

# Nous utilisons les descripteurs 1 4 9 si on utilise le critère BIC
# Notre modèle choisie ce base donc sur ces descripteurs


#TEST 2: Comparaison modele (Stepwise vs LAD)

cor(Y, fitted(modeleChoisie))
cor(Y, predict(ladFinal))
# Nous obtenons un score de 76% que ce soit avec l'AIC ou le BIC
# LAD 72% lorsqu'on faisait par rappart à PRESS, maintenant à 91% 
# si on se se base sur les descripteurs qui prédisent au mieux les données du jeu d'entrainement

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
# Le score augmente de 1 point, mais apparation encore d'un outlier
# On pense que ça ne sert à rien de continuer, le modèle est trop sensible
# car on supprime à chaque itération des variables. De plus, notre score augmente
# trop peu.


## TEST BONUS : Selection de variable via la fonction regsubsets
## permet de donner le meilleur subseet pour n = 3. Evite les diffférentes boucles
## On utilise la librairie leaps. Expliquer en détail ce que fait ce bonus
## sur quel test, critère il se base pour detecter les variables
## install.packages('leaps')
## TODO : Pour le rapport expliquer ce que fait cette fonction (Petit bonus)
library(leaps)
test = regsubsets(Y ~ as.matrix(train_set_final[,-1:-2]), data=train_set_final, nvmax=3)
summary(test)$outmat

