#Import et installation des librairies utiles
#install.packages("readxl")
rm (list=ls())
library("readxl")
setwd("C:/Users/LokiS/Documents/Master 2/Mod�le de Regression/Projet/RegressionLineaire")
test_set <- read_excel("data/FW_groupe2.xls")
train_set <- read_excel("data/FW_groupe2_obs.xls")

#Premiere �tape : Suppression les observations contenant des valeurs manquantes
missing_value_row = sort(which(test_set==0, TRUE)[,1])
# On remarque que le lignes 10, 11, 13, 25, 26, 48, 70 et 71 ont plusieurs valeurs manquantes
# On supprime ces lignes du dataset
test_set = test_set[-missing_value_row,]
rm(missing_value_row)
# Deuxieme �tape, on cherche les relations lin�aire � 1 variable qui lient un regresseur
# � un autre. On �limine ceux qui peuvent �tre pr�dit avec un rsquared > 0.95
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
# On utilise deux vecteurs de descripteur, car dans le jeu de donn�e Test, il y a une colonne suppl�mentaire
# Cela provoque un d�calage
listDescripteursTest = listDescripteurs+1
test_set_iteration_1 = test_set[, -listDescripteurs]
train_set_iteration_1 = train_set[, -listDescripteursTest]
nbDescripteurs = length(test_set_iteration_1)
listDescripteurs <- c()
listDescripteursTest <- c()

## Avec une variable on arrive � supprimmer 10 variables
## Troisieme �tape, on cherche les relations lin�aire � 2 variables. On effectue comme
## l'�tape pr�c�dente

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

##Avec deux variables, on arrive � supprimer 13 variables 
## Troisieme �tape, on cherche les relations lin�aire � 3 variables. On effectue comme
## l'�tape pr�c�dente

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

# On utilise la regression stepwise pour determiner un mod�le par selection de variable
library("MASS")
library("qpcR")

# Explication de PRESS : https://www.rdocumentation.org/packages/qpcR/versions/1.4-0/topics/PRESS


X <- as.matrix(train_set_final[,-1:-2])
Y <- as.matrix(train_set_final[2])
# On fixe le minPress � la valeur maximal d'un int
minPress = 2147483647
# Le nombre de descripteurs
nbDescripteurs = length(train_set_final) - 2
# On conserve uniquement les num�ro de colonne au lieu d'un vecteur en entier
# Permet d'optimiser l'utilisation des ressources
variablesChoisies = c(0,0,0)

## Regreession Least absolute : On  ne sait pas sur quel mod�le se baser pour l'effectuer
## Se baser comme sur la question 2 par rapport au PRESS ?
## Se baser sur un autre crit�re ?
## UPDATE : Maximum de vraissemblance, cf statistiques inf�rentielles
## install.packages("Blossom")

library(Blossom)
minScoreLad = 2147483647
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
          #Reg L1
          ladReg = lad(fit)
          # On cherche � avoir le modele qui minimise la valeur absolue de la somme des erreurs
          # r�siduels
          scoreLad = sum(abs(residuals(ladReg)))
          if (scoreLad < minScoreLad){
            minScoreLad = scoreLad
            ladFinal = ladReg
            varLad = c(i, j, k)
          }
          #modAIC = stepAIC(fit, direction="both", trace=FALSE)
          modBIC = stepAIC(fit, direction="both", trace=FALSE, k=log(25))    
          if(length(modBIC$coefficients) > 1){
            
            pressValue = PRESS(modBIC, verbose=FALSE)$stat
            # On conserve le modele qui a le PRESS le plus faible ainsi que les descripteurs
            if(pressValue < minPress){
              minPress = pressValue
              modeleChoisie = modBIC
              regChoisie = fit
              variablesChoisies = c(i, j, k)
            }
          }
      }
    }
  }
}

# Qu'on supprime les lignes contenant les valeurs nulles au d�but de l'analyse ou pas, nous obtenons les m�mes variables
# Cependant, on supprime une variable en plus lorsqu'on s'occupe des valeurs nulles

# Nous utilisons les descripteurs 1 4 8 si on utilise le crit�re BIC
# Notre mod�le choisie ce base donc sur ces descripteurs

#TEST 2: Comparaison modele (Stepwise vs LAD)

cor(Y, fitted(modeleChoisie))
cor(Y, predict(ladFinal))

plot(Y, predict(ladFinal))


plot(Y, predict(modeleChoisie) )
title("Stepwise Regression")
abline(lm(X1 ~ X2))

plot(Y, predict(ladFinal) )
title("LAD Regression")
abline(0,1)


scatter.smooth(Y, predict(ladFinal))
# Nous obtenons un score de 91.4% que ce soit avec l'AIC ou le BIC
# LAD 72% lorsqu'on faisait par rappart � PRESS, maintenant � 91.1% 
# si on se se base sur les descripteurs qui pr�disent au mieux les donn�es du jeu d'entrainement

# Stepwise est sensible aux outliers contrairement � least absolute
# L'id�e est donc de supprimer manuellement les outliers pour essay� d'am�liorer le score

### UPDATE : Il es possible qu'on ne supprime plus les bons outliers
### En effet, nous avons corrig� notre mod�le et l'on utilise pour X3 la 8eme colonne du jeu de
### donn�e au lieu de la 9eme
X1 = X[1:25, 1]
X2 = X[1:25, 4]
X3 = X[1:25, 8]
plot(Y, predict(modeleChoisie))
boxplot(predict(modeleChoisie))$out
title("Boxplot des valeurs pr�dites")
#On supprime la 24 observation du jeu de donn�e de train
X1 = X1[-24]
X2 = X2[-24]
X3 = X3[-24]
Y = Y[-24]

fit = lm(Y ~ X1 + X2 + X3 + X1 * X2 + X1 * X3 + X2 * X3)
modeleChoisie= stepAIC(fit, direction="both", trace=FALSE, k=log(25))
cor(Y, predict(modeleChoisie))
boxplot(predict(modeleChoisie))
#Score inchang�, mais apparition de deux nouveaux outliers
boxplot(predict(modeleChoisie))$out
# On supprime la 10 et 16 observation du de donn�e de train
X1 = X1[-c(10, 16)]
X2 = X2[-c(10, 16)]
X3 = X3[-c(10, 16)]
Y = Y[-c(10, 16)]
fit = lm(Y ~ X1 + X2 + X3 + X1 * X2 + X1 * X3 + X2 * X3)
modeleChoisie= stepAIC(fit, direction="both", trace=FALSE, k=log(25))
cor(Y, predict(modeleChoisie))
boxplot(predict(modeleChoisie))
# Le score augmente de 1 point, mais apparation encore d'un outlier
# On pense que �a ne sert � rien de continuer, le mod�le est trop sensible
# car on supprime � chaque it�ration des variables. De plus, notre score augmente
# trop peu.


## TEST BONUS : Selection de variable via la fonction regsubsets
## permet de donner le meilleur subseet pour n = 3. Evite les difff�rentes boucles
## On utilise la librairie leaps. Expliquer en d�tail ce que fait ce bonus
## sur quel test, crit�re il se base pour detecter les variables
## install.packages('leaps')
## TODO : Pour le rapport expliquer ce que fait cette fonction (Petit bonus si le temps le 
## permet)

library(leaps)
test = regsubsets(Y ~ as.matrix(train_set_final[,-1:-2]), data=train_set_final, nvmax=3)
summary(test)$outmat

