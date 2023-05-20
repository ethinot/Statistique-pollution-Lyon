##################################################
#             TP not� num�ro 97177               #
# Fares Sioni p1907037 & Edouard Thinot p1909945 #
##################################################

##############
# Exercice 1 #
#  Partie A  #
##############

Air<-read.delim2("http://tinyurl.com/y39an7ef/DATA97177.csv",na.strings="-") # Chargement des donn�es n�cessaires

# Question 1

# Voir la doc pdf.

# Question 2

j = NROW(Air) # Calcul le nombre de lignes sur le dataset pour en d�duire le nombre de jours observ�s
cat("Il y a", j, "jours observ�s")

# Compte le nombre de jours o� toutes les stations ont bien observ�es le Monoxyde d'azote
CountMonoxyteAzoteDays = function(){
  daysObservated = 0
  for(i in 1:j){
    s = 0
    s = !is.na(Air$Lyon.Gerland.Monoxyde.d.azote[i])
    s = s + !is.na(Air$Lyon.Centre.Monoxyde.d.azote[i])
    s = s + !is.na(Air$Villeurbanne.Place.Grandcl�.ment.Monoxyde.d.azote[i])
    s = s + !is.na(Air$Est.lyonnais...Saint.Exup�.ry.Monoxyde.d.azote[i])
    s = s + !is.na(Air$Lyon...Tunnel.Croix.Rousse...Sortie.Rh�.ne.Monoxyde.d.azote[i])
    s = s + !is.na(Air$Lyon.P�.riph�.rique.Monoxyde.d.azote[i])
    if(s == 6){
      daysObservated = daysObservated + 1
    }
  }
  cat("Le monoxyde d'azote a �t� observ� dans toutes les stations", daysObservated, "/", j, "jours.")
}
  
CountMonoxyteAzoteDays() # appelle la fonction.

# Question 3

# Compte le nombre de NA par colonne
CountNAByColumns = function(){
  counter <- vector()
  for (i in 2:24) {
    counter = c(counter, sum(is.na(Air[i]))) 
  }
  print(counter)
  return(counter)
}

counter = CountNAByColumns()

# R�cup�re les indexes des 6 colonnes ayant moins de 25 NA
Get6Less = function(){
  counterTF = counter<25
  indexes <- vector()
  for(i in 1:23){
    if(counterTF[i] == TRUE){
      indexes = c(indexes, (i+1))
    }
  }
  return(indexes)
}

c = c(1, Get6Less()) # c est la colonne date + les 6 colonnes ayant moins de 25 NA

#install.packages("dplyr") # r�cup�re la biblioth�ques permettant la selection de colonnes 
library("dplyr")
below25NA = select(Air, c)# ne garde que les 6 colonnes des caract�res ayant moins de 25 NA

 
# Question 4

varEB<-function(x){n<-length(x);var(x)*(n-1)/n} # d�clare la fonction de la variance biais�e

cat("Monoxyde de Carbone sur le p�riph�rique lyonnais : ", 
    "\nMoyenne :", round(mean(na.omit(Air$Lyon.P�.riph�.rique.Monoxyde.carbone)), digits=3),   
    "\nVariance biais�e", varEB(na.omit(Air$Lyon.P�.riph�.rique.Monoxyde.carbone)),
    "\nVariance non-biais�e :", var(na.omit(Air$Lyon.P�.riph�.rique.Monoxyde.carbone)),
    "\n3�me quartile :", quantile(na.omit(Air$Lyon.P�.riph�.rique.Monoxyde.carbone),type=1)[4]
    )
 
# Question 5

LyonPM10 = select(Air, c(5, 8, 12, 16, 20, 24)) # selectionne toutes les colonnes PM10
PM10Moyen = round(rowMeans(LyonPM10, na.rm = TRUE), digits=3) # effectue la moyenne pour chaque lignes de PM10 en omettant les NA

cat("Le seuil de 50mg/m cube est d�pass� ", round(1-ecdf(PM10Moyen)(50), digits=3), "% du temps sur les",j , "jours observ�es")
cat("Le seuil de 80mg/m cube est d�pass� ", round(1-ecdf(PM10Moyen)(80), digits=3), "% du temps sur les",j ,"jours observ�es")

# Question 6

PM10Moyen = cbind(PM10Moyen, date = select(Air, c(1)))# r�cup�re le PM10Moyen et ajoute la colonne date

PM10SeuilInfo = filter(PM10Moyen, PM10Moyen > 50)
PM10SeuilAlerte = filter(PM10Moyen, PM10Moyen > 80)


LyonOzone = select(Air, c(4, 11, 15))
OzoneMoyen = round(rowMeans(LyonOzone, na.rm = TRUE), digits=3)

OzoneMoyen = cbind(OzoneMoyen, date = select(Air, c(1)))

OzoneSeuilInfo = filter(OzoneMoyen, OzoneMoyen > 180)
OzoneSeuilAlerte = filter(OzoneMoyen, OzoneMoyen > 240) # il n'y a aucun jour ou la moyenne d'ozone d�passe les seuils.


##############
# Exercice 1 #
#  Partie B  #  
##############

dfg = na.omit(Air[, c(2,3,4,5)]) # selectionne seulement les mesures de Gerland
dfg2 = na.omit(Air[, c(4,5)])

# Question 1

round(cov(dfg), digits=3) #calcul de la covariance de toutes les donn�es de dfg
round(cor(dfg), digits=3) #calcul de la corr�lation de toutes les donn�es de dfg

round(cov(dfg2), digits=3) #calcul de la covariance de toutes les donn�es de dfg2
round(cor(dfg2), digits=3) #calcul de la corr�lation de toutes les donn�es de dfg2

# Les r�sultats des calculs entre l'ozone et les particules PM10 sont diff�rentes selon les deux data frame 
# car na.omit supprime une ligne compl�te pour que chaque colonne ai le m�me nombre de ligne.

# Question 2

# Pour utiliser un test de Pearson il faut supposer que les deux �chantillons suivent une distribution
# de loi normale.

x = dfg2$Lyon.Gerland.Ozone
y = dfg2$Lyon.Gerland.Particules.PM10

res = cor.test(x,y) # test de corr�lation entre l'ozone et les particules PM10
res
# Le coefficient de corr�lation qu'on appellera r, est �gale � -0.358 et la p-value < 0
# on en conclut donc que x et y sont n�gativement corr�l�s et qu'on peut rejeter H0 l'hypoth�se que nos �chantillons suivent une loi normal.

# Question 3

lmll = lm(y~x)
summary(lmll)
# Les residus ne sont pas sym�triques/il y a un Residual standard error �lev�. Il ne vaut mieux pas l'utiliser.
# On peut constater que l'erreur du mod�le de r�gression lin�aire (Residual standard error, proche de l'�cart-type) est de 12.52 sur un degr� de libert� �gal � 636
# Le Multiple R-squared est un pourcentage (12.810) de la variance de y apr�s la soustraction de l'erreur du mod�le. 
# Idem au Multiple R-squared, l'Adjusted R-squared (12.680) prend en compte le nombre d'�chantillons et les variables qu'on utilise.
# F-statistic est un test global permettant de voir si notre mod�le a au moins une variable significative.

d = lmll$coefficients 
a = d[2]
b= d[1]
plot(x, y, cex=0.3,main="Nuage de point x y avec droite de r�gression lin�raire")
abline(b,a,col="red")

# Question 4

# install.packages("EnvStats")
library(EnvStats)

gofTest(y, test = "chisq", distribution = "norm") #p-value = 0
# On en conclut que y ne suit pas une loi log normale
gofTest(x, test = "chisq", distribution = "norm") #p-value = 0.255
# On en conclut que x suit une loi log normale

y2 = log(y)   
gofTest(y2, test = "chisq", distribution = "norm") #p-value = 0.498
# On en conclut que y suit une loi log normale

# Question 5

lmll2 = lm(y2~x)
summary(lmll2) 
# Le Residual standard error est faible, on peut utiliser ce mod�le.
# Le Multiple R-squared a un pourcentage de 7.556 et le Adjustes R-squares 7.411. 

#install.packages("HH")
library("HH")

ci.plot(lmll2, conf.level = 0.9, pch = ".", cex=3,lty=c(1,1,0,1))


# Question 6 

plot(x, y, cex=0.3)

d2 = lmll2$coefficients 
d3 = exp(d2)
abline(d3,col="red",lwd=2)

##############
# Exercice 2 #
##############

# Question 1

U<- runif(1000,min=0,max=1)
S <- tan(pi*U)

#install.packages("qboxplot")
library("qboxplot")

boxplot(S,horizontal=TRUE,qtype=1,main="Diagramme � moustache de S")

S2 = S[S<10 & S>-10] # r�duction de l'intervalle
boxplot(S2,horizontal=TRUE,qtype=1,main="Diagramme � moustache de S2")

# Question 2

MS = mean(S)
ETS = sqrt(var(S))
# rnorm simule un �chantillon de loi normale
T = rnorm(1000, mean = MS, sd = ETS) # loi normale de param�tres (MS, ETS)

boxplot(T,S,horizontal=TRUE,qtype=1,main="Diagramme � moustache de S et T") # compare T & S

T2 = T[T<10 & T>-10] # sous-�chantillon de T
boxplot(T2,S2,horizontal=TRUE,qtype=1,main="Diagramme � moustache de S2 et T2") # compare T2 & S2 sur le m�me �chentillion (-10,10).
# S2 semble �tre un �chantillon de loi normale.

# Question 3

gofTest(S, test = "chisq", distribution = "norm") #p-value = 0
# On en conclut que S ne suit pas une loi normale.

# Question 4

U = runif(10000 ,min=0,max=1)
V = runif(10000 ,min=0,max=1)
S = tan(pi*U)
T = V/(1+S^2)

plot(S,T, pch=".", xlim=c(-10,10)) # (S,T) est uniform�ment distribu� sur [-10,10]� et [0,1]�.

# Question 5 

S2 = S[S<10 & S>-10]

hist(S2,breaks=200, main = "Histogramme de S2")

L = rnorm(1000, 0, 1.5)
boxplot(S2,L,horizontal=TRUE,qtype=1,main="Diagramme � moustache de S2 et L",ylim=c(-10,10))
# S semble suivre une loi normale (0, 1,5).

gofTest(S2, test = "chisq", distribution = "norm") #p-value = 0
# S2 ne suit pas du tout une loi normale.

