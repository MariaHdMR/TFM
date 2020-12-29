#Last stept in the analysis, the SEM multigroup. 
library(corrgram)#SEM
library(sem)#SEM
library(lavaan)#SEM
library(scales)
library(tidyverse)
library(semPlot)
#>CHFU. ----
#First I'm going to do the SEM that have the Neighbors intra and inter separate (2 columns) at Plot, 3m,1m and 7.5cm
#The group argument in the model is the distance level: plot (letter a), 3m (letter c), 1m (letter e), 7.5cm (letter g). 
#reminder: neigbors.intra.inter.split is the dataframe with in distance_total have the levels of plot, 3m,2m, 7.5cm
# and the dataframe neigbors.intra.inter is the datafreme that in distance have: plot_inter, plot_intra, 3m_intra, 3m_inter
# 1m_inter, 1m_intra, 7.5_inter, 7.5_intra.

data <- read_csv2("Analisis_BES/data/Data_neighbors.intra.inter.split.csv") #this is the data That I have the neigbors split
data <- as.data.frame(data) 
head(data)


modelo.chfu <- ' #This model is for the dataframe neigbors.intra.inter.split
#regresions
#Plant_fitness = 
seed ~ Bee + n_neighbors_inter
seed ~ n_neighbors_inter
seed ~ n_neighbors_intra
Bee ~  n_neighbors_intra
Bee ~ n_neighbors_inter

#intercept
'


modelo.chfu.fl <- ' #This model is for the dataframe neigbors.intra.inter.split #◘works with the no contrained
#regresions
#Plant_fitness = 

seed ~ Bee
seed ~ n_neighbors_inter 
seed ~ n_neighbors_intra
Bee ~ flowers2
flowers2 ~ n_neighbors_inter + n_neighbors_intra

seed ~~flowers2

#intercept
'


modelo.chfu.1 <- ' #This model is for the dataframe neigbors.intra.inter.split #◘works with the no contrained
#regresions
#Plant_fitness = 

seed ~ Bee
seed ~ inter 
seed ~ intra
Bee ~ flowers2
Bee ~inter
flowers2 ~ inter + intra

seed ~~flowers2

#intercept
'



modelo.chfu.2 <- ' #This model is for the dataframe neigbors.intra.inter.split. works for the no constrained
#regresions
#Plant_fitness = 
seed ~ Bee
seed ~ n_neighbors_inter 
Bee ~n_neighbors_inter
Bee ~n_neighbors_intra
flowers2 ~ n_neighbors_inter
flowers2 ~ n_neighbors_intra

seed ~~flowers2

#intercept
'





modelo.chfu.3 <- ' #This model is for the dataframe neigbors.intra.inter.split.  works with the no constrained
#regresions
#Plant_fitness = 
seed ~ Bee
seed ~ n_neighbors_inter 
Bee ~n_neighbors_inter
flowers2 ~ n_neighbors_inter
flowers2 ~ n_neighbors_intra

seed ~~flowers2

#intercept
'

modelo.chfu.4 <- ' #This model is for the dataframe neigbors.intra.inter.split.  works with the no constrained
#regresions
#Plant_fitness = 
seed ~ Bee
seed ~ n_neighbors_inter 
Bee ~n_neighbors_inter
flowers2 ~ n_neighbors_inter

seed ~~flowers2

#intercept
'


modelo.chfu.5 <- ' #This model is for the dataframe neigbors.intra.inter.split. 
#regresions
#Plant_fitness = 
seed ~ Bee
seed ~ n_neighbors_inter 
Bee ~n_neighbors_inter
flowers2 ~ n_neighbors_inter

seed ~~flowers2
seed~seed
#intercept
'




modelo.chfu.latent <-  '#This model is for the dataframe neigbors.intra.inter.split
#regresions
#Plant_fitness = ~ seed 
#define the latent variables: V= floral visitors, n= Neighbors
V = ~ Bee
N = ~ n_neighbors_inter + n_neighbors_intra 
#structural relations
seed ~ V+N
Bee ~n_neighbors_inter
V~N
#intercept
'
modelo1.chfu <- ' #This model is for the dataframe neigbors.intra.inter
#regresions
#Plant_fitness = ~ seed 
seed ~ Fly
seed ~ Beetle
seed ~ n_neighbors
Fly ~  n_neighbors
Beetle ~ n_neighbors
#intercept
'
#IMPORTANTE: 
# >>CFI: The Comparative Fit Index is a revised form of NFI. Not very sensitive to sample size (Fan, Thompson, & Wang, 1999). 
#           Compares the fit of a target model to the fit of an independent, or null, model. It should be > .90.
#>>RMSEA: The Root Mean Square Error of Approximation is a parsimony-adjusted index. Values closer to 0 represent a good fit. It should be < .08 or < .05. 
#           The p-value printed with it tests the hypothesis that RMSEA is less than or equal to .05 (a cutoff sometimes used for good fit), and thus should be not significant.
#>>SRMR: the (Standardized) Root Mean Square Residual represents the square-root of the difference between the residuals of the sample covariance matrix and the hypothesized model. 
#           As the RMR can be sometimes hard to interpret, better to use SRMR. Should be < .08.
#>>p.value: not significant. Upper 0.05



CHFU.sem <- subset(data, Plant== "CHFU")
CHFU.sem1 <- CHFU.sem[,c( "seed", "fruit","seed.indv" ,"x_coor2", "y_coor2", "Bee","Beetle",
                          "Fly", "flowers2","distance_total","n_neighbors_intra", "n_neighbors_inter")] #there are no visits of 
#                                        butterflies, so I just need to eliminate it to can do the corregram


CHFU.sem1$seed <- rescale(CHFU.sem1$seed) #this is ok, but it's a decision how to scale... I am curious why this one.
CHFU.sem1$n_neighbors_inter <- rescale(CHFU.sem1$n_neighbors_inter)
CHFU.sem1$n_neighbors_intra <- rescale(CHFU.sem1$n_neighbors_intra)
CHFU.sem1$flowers2 <- rescale(CHFU.sem1$flowers2)
CHFU.sem1$Bee <- rescale(CHFU.sem1$Bee)
CHFU.sem1$seed.indv <- rescale (CHFU.sem1$seed.indv)

CHFU.sem1$inter <- CHFU.sem1$n_neighbors_inter
CHFU.sem1$intra <- CHFU.sem1$n_neighbors_intra

corrgram(CHFU.sem1, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="CHFU correlations") #to check the correlation between the variables
multigroup.8 <- sem(modelo.chfu.1, CHFU.sem1, group = "distance_total") #the best model is the model.chfu.1 
summary(multigroup.8, standardize=T)
varTable(multigroup.8)
fitMeasures(multigroup.8, c("cfi","rmsea","srmr", "pvalue")) 
print(modindices(multigroup.8))


par(mfrow=c(1,1))
semPaths(multigroup.8)
#IB: Mola, podrias añadir en power point o similar los coeficientes de los paths, 
# y esto sera tu resultado principal de la charla de la BES.


#total seed per individual
modelo.chfu.1.indv <- ' #This model is for the dataframe neigbors.intra.inter.split #◘works with the no contrained
#regresions
#Plant_fitness = 

seed.indv ~ Bee
seed.indv ~ inter 
seed.indv ~ intra
Bee ~ flowers2
Bee ~inter
flowers2 ~ inter + intra

seed.indv ~~flowers2

#intercept
'
multigroup.8.indv <- sem(modelo.chfu.1.indv, CHFU.sem1, group = "distance_total") #the best model is the model.chfu.1 
summary(multigroup.8.indv, standardize=T)
varTable(multigroup.8.indv)
fitMeasures(multigroup.8.indv, c("cfi","rmsea","srmr", "pvalue")) #fittea
print(modindices(multigroup.8.indv))





#LEMA----

modelo.lema.1 <- ' #This model is for the dataframe neigbors.intra.inter.split #works with the no contrained
#regresions
#Plant_fitness = 
seed ~ Beetle
seed ~ inter 
seed ~ intra
Beetle ~flowers2
Fly ~intra
flowers2 ~inter+intra
seed ~~ flowers2
Beetle ~~ Fly
Beetle  ~  inter
#intercept
'

LEMA.sem <- subset(data, Plant== "LEMA")
LEMA.sem1 <- LEMA.sem[,c( "seed", "fruit","seed.indv", "x_coor2", "y_coor2", "Butterfly","Beetle",
                          "Fly", "flowers2","distance_total","n_neighbors_intra", "n_neighbors_inter")] #there are no visits of 
#                                        butterflies, so I just need to eliminate it to can do the corregram


LEMA.sem1$seed <- rescale(LEMA.sem1$seed)
LEMA.sem1$n_neighbors_inter <- rescale(LEMA.sem1$n_neighbors_inter)
LEMA.sem1$n_neighbors_intra <- rescale(LEMA.sem1$n_neighbors_intra)
LEMA.sem1$flowers2 <- rescale(LEMA.sem1$flowers2)
LEMA.sem1$Butterfly <- rescale(LEMA.sem1$Butterfly)
LEMA.sem1$seed.indv <- rescale(LEMA.sem1$seed.indv)
LEMA.sem1$flowers2 <- rescale(LEMA.sem1$flowers2)
LEMA.sem1$Beetle <- rescale(LEMA.sem1$Beetle)
LEMA.sem1$Fly <- rescale(LEMA.sem1$Fly)




LEMA.sem1$inter <- LEMA.sem1$n_neighbors_inter
LEMA.sem1$intra <- LEMA.sem1$n_neighbors_intra

corrgram(LEMA.sem1, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="CHFU correlations") #to check the correlation between the variables
multigroup.lema <- sem(modelo.lema.1, LEMA.sem1, group = "distance_total") #the best model is the model.chfu.1 
summary(multigroup.lema, standardize=T)
varTable(multigroup.lema)
fitMeasures(multigroup.lema, c("cfi","rmsea","srmr", "pvalue")) #fitea
print(modindices(multigroup.lema))

par(mfrow=c(1,1))
semPaths(multigroup.lema)




#seed per indv
modelo.lema.1.indv <- ' #This model is for the dataframe neigbors.intra.inter.split #works with the no contrained
#regresions
#Plant_fitness = 
seed.indv ~ Beetle
seed.indv ~ inter 
seed.indv ~ intra
Beetle ~flowers2
Fly ~intra
flowers2 ~inter+intra
seed.indv ~~ flowers2
Beetle ~~ Fly
Beetle  ~  inter
#intercept
'

multigroup.lema.indv <- sem(modelo.lema.1.indv, LEMA.sem1, group = "distance_total") #the best model is the model.chfu.1 
summary(multigroup.lema.indv, standardize=T)
varTable(multigroup.lema.indv)
fitMeasures(multigroup.lema.indv, c("cfi","rmsea","srmr", "pvalue")) #fitea
print(modindices(multigroup.lema.indv))

par(mfrow=c(1,1))
semPaths(multigroup.lema.indv)





#PUPA ----
modelo.pupa.1 <- ' #This model is for the dataframe neigbors.intra.inter.split #works with the no contrained
#regresions
#Plant_fitness = 
seed ~ Beetle
seed ~ inter 
seed ~ intra
Beetle ~flowers2
Fly ~intra
flowers2 ~inter+intra
seed ~~ flowers2
Beetle ~~ Fly
Beetle  ~  inter
#intercept
'

modelo.pupa.2 <- ' #This model is for the dataframe neigbors.intra.inter.split #wnot bad, but can be good
#regresions
#Plant_fitness = 
seed ~ Bee
seed ~Fly
seed ~ inter 
seed ~ intra
Bee ~inter + flowers2
Fly ~intra

#intercept
'

modelo.pupa.3 <- ' #This model is for the dataframe neigbors.intra.inter.split #wnot bad, but can be good
#regresions
#Plant_fitness = 
seed ~ Bee
seed ~ inter 
seed ~ intra
Bee ~inter + flowers2 + intra
Fly ~intra 

#intercept
'

PUPA.sem <- subset(data, Plant== "PUPA")
PUPA.sem1 <- PUPA.sem[,c( "seed", "fruit", "x_coor2", "y_coor2", "Bee","Beetle",
                          "Fly", "flowers2","seed.indv", "distance_total","n_neighbors_intra", "n_neighbors_inter")] #there are no visits of 
#                                        butterflies, so I just need to eliminate it to can do the corregram


PUPA.sem1$seed <- rescale(PUPA.sem1$seed)
PUPA.sem1$n_neighbors_inter <- rescale(PUPA.sem1$n_neighbors_inter)
PUPA.sem1$n_neighbors_intra <- rescale(PUPA.sem1$n_neighbors_intra)
PUPA.sem1$flowers2 <- rescale(PUPA.sem1$flowers2)
PUPA.sem1$Butterfly <- rescale(PUPA.sem1$Butterfly)
PUPA.sem1$seed.indv <- rescale(PUPA.sem1$seed.indv)

PUPA.sem1$inter <- PUPA.sem1$n_neighbors_inter
PUPA.sem1$intra <- PUPA.sem1$n_neighbors_intra

corrgram(PUPA.sem1, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="CHFU correlations") #to check the correlation between the variables
multigroup.pupa <- sem(modelo.pupa.3, PUPA.sem1, group = "distance_total") #the best model is the model.chfu.1 
summary(multigroup.pupa, standardize=T)
varTable(multigroup.pupa)
fitMeasures(multigroup.pupa, c("cfi","rmsea","srmr", "pvalue")) #fitea
print(modindices(multigroup.pupa))

par(mfrow=c(1,1))
semPaths(multigroup.pupa)

#seed per individuals
modelo.pupa.3.indv <- ' #This model is for the dataframe neigbors.intra.inter.split #wnot bad, but can be good
#regresions
#Plant_fitness = 
seed.indv ~ Bee
seed.indv ~ inter 
seed.indv ~ intra
Bee ~inter + flowers2 + intra
Fly ~intra 

#intercept
'

multigroup.pupa.indv <- sem(modelo.pupa.3.indv, PUPA.sem1, group = "distance_total") #the best model is the model.chfu.1 
summary(multigroup.pupa.indv, standardize=T)
varTable(multigroup.pupa.indv)
fitMeasures(multigroup.pupa.indv, c("cfi","rmsea","srmr", "pvalue")) #fitea
print(modindices(multigroup.pupa.indv))

