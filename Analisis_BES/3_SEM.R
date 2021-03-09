#Last stept in the analysis, the SEM multigroup. 
library(corrgram)#SEM
#library(sem)#SEM # tener cuidado porque el paquese sem y lavaan crean problemas si estan los dos. 
library(lavaan)#SEM
library(scales)
library(tidyverse)
library(semPlot)
#detach("package:sem",unload=TRUE)


#IMPORTANTE: 
# >>CFI: The Comparative Fit Index is a revised form of NFI. Not very sensitive to sample size (Fan, Thompson, & Wang, 1999). 
#           Compares the fit of a target model to the fit of an independent, or null, model. It should be > .90.
#>>RMSEA: The Root Mean Square Error of Approximation is a parsimony-adjusted index. Values closer to 0 represent a good fit. It should be < .08 or < .05. 
#           The p-value printed with it tests the hypothesis that RMSEA is less than or equal to .05 (a cutoff sometimes used for good fit), and thus should be not significant.
#>>SRMR: the (Standardized) Root Mean Square Residual represents the square-root of the difference between the residuals of the sample covariance matrix and the hypothesized model. 
#           As the RMR can be sometimes hard to interpret, better to use SRMR. Should be < .08.
#>>p.value: not significant. Upper 0.05


#>CHFU. ----
#First I'm going to do the SEM that have the Neighbors intra and inter separate (2 columns) at Plot, 3m,1m and 7.5cm
#The group argument in the model is the distance level: plot (letter a), 3m (letter c), 1m (letter e), 7.5cm (letter g). 
#reminder: neigbors.intra.inter.split is the dataframe with in distance_total have the levels of plot, 3m,2m, 7.5cm
# and the dataframe neigbors.intra.inter is the datafreme that in distance have: plot_inter, plot_intra, 3m_intra, 3m_inter
# 1m_inter, 1m_intra, 7.5_inter, 7.5_intra.

data <- read_csv2("Analisis_BES/data/Data_neighbors.intra.inter.split.check1.csv") #this is the data That I have the neigbors split
data <- as.data.frame(data) 
head(data)
str(data)
summary(data) #WHY THERE ARE between 444 and 1856 NA's per variable?!
data$fruit * data$seed == data$seed.indv #WHY some values are FALSE?
plot(data$fruit, data$seed) #THIS IS HIGHLY SUSPICIOUS.
boxplot(data$n_neighbors_inter ~ data$distance)
boxplot(data$n_neighbors_intra ~ data$distance)
boxplot(data$seed ~ data$distance)
boxplot(data$flowers ~ data$distance) #wierd to think on this across scales.
#I suggest removing it
boxplot(data$Bee ~ data$distance) #wierd to think on this across scales.
boxplot(data$Fly ~ data$distance) #wierd to think on this across scales.
table(data$Month, data$plot) #So, we are using visits per sampling day, BUT n_nei and seeds per season?
table(data$Month, data$seed) ## I don't understand the data.



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

modelo.chfu.fl <- ' #This model is for the dataframe neigbors.intra.inter.split #◘works with the no constrained
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



#modelo.chufu.1 es el mejor modelo si unicamente quiero meter las semillas por fruto. Saltar el resto de modelos salvo modelo.chfu.new
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

#this next model includes the seed/fruit that is going to be affected by the floral visitors, and the seed/indv that it is going to be affected by the neighbors
#       (inter and intra neighbors). 
modelo.chfu.new <- ' #This model is for the dataframe neigbors.intra.inter.split #◘works with the no contrained
#regresions
#Plant_fitness = 

seed ~ Bee
seed.indv ~ intra
Bee ~inter 
flowers2 ~ inter + intra

seed~~flowers2
seed.indv~~flowers2

#intercept
'

modelo.chfu.nacho <- ' #This model is for the dataframe neigbors.intra.inter.split #◘works with the no contrained
#regresions
#Plant_fitness = 

seed ~ Bee + Fly
fruit ~ inter + intra
Bee ~ inter + intra + flowers2
Fly ~ inter + intra + flowers2
flowers2 ~ inter + intra
seed.indv ~ seed + fruit

Bee ~~ Fly
fruit ~~ seed

#intercept
'


CHFU.sem <- subset(data, Plant== "CHFU")
CHFU.sem1 <- CHFU.sem[,c( "seed", "fruit","seed.indv" ,"x_coor2", "y_coor2", "Bee","Beetle",
                          "Fly", "flowers2","distance_total","n_neighbors_intra", "n_neighbors_inter")] #there are no visits of 
#                                        butterflies, so I just need to eliminate it to can do the corregram


#I need to eliminate the letter g, because we are not going to consider the 7.5cm more. Update: I'm going to use 7.5cm, so I block this next line
#chfu.sem2 <- subset(CHFU.sem1, distance_total != "g")
chfu.sem2 <- CHFU.sem1
head(chfu.sem2)

chfu.sem2$seed <- scale(chfu.sem2$seed) 
chfu.sem2$seed.indv <- scale(chfu.sem2$seed.indv)
chfu.sem2$n_neighbors_inter <- scale(chfu.sem2$n_neighbors_inter)
chfu.sem2$n_neighbors_intra <- scale(chfu.sem2$n_neighbors_intra)
chfu.sem2$flowers2 <- scale(chfu.sem2$flowers2)
chfu.sem2$Bee <- scale(chfu.sem2$Bee)
#chfu.sem2$seed.indv <- scale (chfu.sem2$seed.indv)
chfu.sem2$Fly <- scale (chfu.sem2$Fly)
chfu.sem2$fruit <- scale(chfu.sem2$fruit)

chfu.sem2$inter <- chfu.sem2$n_neighbors_inter
chfu.sem2$intra <- chfu.sem2$n_neighbors_intra
chfu.corr <- chfu.sem2[,c( "seed", "fruit","seed.indv" ,
                           "flowers2","distance_total")]
corrgram(chfu.corr, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="CHFU correlations simplified")

corrgram(chfu.sem2, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="CHFU correlations") #to check the correlation between the variables
multigroup.8 <- sem(modelo.chfu.new, chfu.sem2, group = "distance_total") #the best model is the model.chfu.1  fot the seed/fruit
summary(multigroup.8, standardize=T)
varTable(multigroup.8)
#model indices CHFU ----
fitMeasures(multigroup.8, c("cfi","rmsea","srmr", "pvalue")) #2 of the 4 indices don't fit, rmesea and srmr.
print(modindices(multigroup.8))

#nacho path
multigroup.9 <- sem(modelo.chfu.nacho, chfu.sem2, group = "distance_total") #the best model is the model.chfu.1  fot the seed/fruit
summary(multigroup.9, standardize=T)
varTable(multigroup.9)
#model indices CHFU ----
fitMeasures(multigroup.9, c("cfi","rmsea","srmr", "pvalue")) #2 of the 4 indices don't fit, rmesea and srmr.
print(modindices(multigroup.9))

#Draw the path
par(mfrow=c(1,1))
semPaths(multigroup.8,whatLabels = "std",  residuals = F, exoCov = F, edge.label.cex=1.00, reorder = FALSE)#los coeff de los paths son la std

#IB: Mola, podrias añadir en power point o similar los coeficientes de los paths, 
# y esto sera tu resultado principal de la charla de la BES.



#now I'm going to creat the same model but constrained. This means that we are not considerating the different scales. 
multigroup2.constrained <- sem(modelo.chfu.new, chfu.sem2, group = "distance_total", group.equal = c("intercepts", "regressions"))
summary(multigroup2.constrained)
fitMeasures(multigroup2.constrained,c("cfi","rmsea","srmr", "pvalue") )
#now I compare both models to see if there is a difference between them

anova(multigroup.8, multigroup2.constrained) # Yes, there is a difference between the models,
#       the best model is the model no constrained one because the cfi, rmsea...indices.


#nacho model
multigroup2.constrained <- sem(modelo.chfu.nacho, chfu.sem2, group = "distance_total", group.equal = c("intercepts", "regressions"))
summary(multigroup2.constrained)
fitMeasures(multigroup2.constrained,c("cfi","rmsea","srmr", "pvalue") )
#now I compare both models to see if there is a difference between them
anova(multigroup.9, multigroup2.constrained) # Yes, there is a difference between the models,
#       the best model is the model no constrained one because the cfi, rmsea...indices.


#LEMA----
#el siguiente modelo es el bueno si lo que quiero es hacer semillas por fruto
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

#this next model includes the seed/fruit and the seed/indv.
#stranger things: the interaction seed ~beetles hace que el modelo fittee peor. 
modelo.lema.new <- ' #This model is for the dataframe neigbors.intra.inter.split #works with the no contrained
#regresions
#Plant_fitness = 

seed ~ Fly
seed~ Beetle
seed.indv ~ inter 
seed.indv ~intra
Beetle ~ inter+intra 
Fly ~intra
flowers2 ~intra + inter


Fly~~flowers2
Beetle ~~Fly
seed.indv~~Beetle
seed~~Fly
Beetle~~flowers2
#intercept
'

#nacho
modelo.lema.nacho <- ' #This model is for the dataframe neigbors.intra.inter.split #works with the no contrained
#regresions
#Plant_fitness = 

seed ~ Fly + Beetle
fruit ~ inter + intra
Beetle ~ inter + intra + flowers2
Fly ~ inter + intra + flowers2
flowers2 ~ intra + inter
seed.indv ~ seed + fruit

Beetle ~~Fly
fruit ~~ seed
#intercept
'

LEMA.sem <- subset(data, Plant== "LEMA")
LEMA.sem1 <- LEMA.sem[,c( "seed", "fruit","seed.indv", "x_coor2", "y_coor2", "Butterfly","Beetle", "Bee",
                          "Fly", "flowers2","distance_total","n_neighbors_intra", "n_neighbors_inter")] 

lema.sem2 <- LEMA.sem1
#update: I will use the 7.5cm scale, so I blocked the next line.
#lema.sem2 <- subset(LEMA.sem1, distance_total != "g")
lema.sem2$seed <- scale(lema.sem2$seed)
lema.sem2$seed.indv <- scale(lema.sem2$seed.indv)
lema.sem2$n_neighbors_inter <- scale(lema.sem2$n_neighbors_inter)
lema.sem2$n_neighbors_intra <- scale(lema.sem2$n_neighbors_intra)
lema.sem2$flowers2 <- scale(lema.sem2$flowers2)
lema.sem2$Butterfly <- scale(lema.sem2$Butterfly)
#lema.sem2$seed.indv <- scale(lema.sem2$seed.indv)
#lema.sem2$flowers2 <- scale(lema.sem2$flowers2)
lema.sem2$Beetle <- scale(lema.sem2$Beetle)
lema.sem2$Fly <- scale(lema.sem2$Fly)
lema.sem2$Bee <- scale(lema.sem2$Bee)
lema.sem2$fruit <- scale(lema.sem2$fruit)

lema.sem2$inter <- lema.sem2$n_neighbors_inter
lema.sem2$intra <- lema.sem2$n_neighbors_intra


lema.corr <- lema.sem2[,c( "seed", "fruit","seed.indv" ,
                           "flowers2","distance_total")]
corrgram(lema.corr, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="LEMA correlations simplified")

corrgram(lema.sem2, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="LEMA correlations") #to check the correlation between the variables
multigroup.lema <- sem(modelo.lema.new, lema.sem2, group = "distance_total") #the best model is the modelo.lema.1 for the seed/fruit
summary(multigroup.lema, standardize=T)
varTable(multigroup.lema)
#model indeces LEMA ----
fitMeasures(multigroup.lema, c("cfi","rmsea","srmr", "pvalue")) #fittea todo 
print(modindices(multigroup.lema))

#nacho
multigroup.lema.2 <- sem(modelo.lema.nacho, lema.sem2, group = "distance_total") #the best model is the modelo.lema.1 for the seed/fruit
summary(multigroup.lema.2, standardize=T)
varTable(multigroup.lema.2)
#model indeces LEMA ----
fitMeasures(multigroup.lema.2, c("cfi","rmsea","srmr", "pvalue")) #fittea todo 
print(modindices(multigroup.lema.2))


#now I'm going to creat the same model but constrained. This means that we are not considerating the different scales. 
multigroup.lema.constrained <- sem(modelo.lema.new, lema.sem2, group = "distance_total", group.equal = c("intercepts", "regressions"))
summary(multigroup.lema.constrained)
fitMeasures(multigroup.lema.constrained,c("cfi","rmsea","srmr", "pvalue") )

#now I compare both models to see if there is a difference between them
anova(multigroup.lema, multigroup.lema.constrained) # Yes, there is a difference between the models,
#       the best model is the model no constrained one because the cfi, rmsea...indices.


#nacho
multigroup.lema.constrained.2 <- sem(modelo.lema.nacho, lema.sem2, group = "distance_total", group.equal = c("intercepts", "regressions"))
summary(multigroup.lema.constrained.2)
fitMeasures(multigroup.lema.constrained.2,c("cfi","rmsea","srmr", "pvalue") )
#now I compare both models to see if there is a difference between them
anova(multigroup.lema.2, multigroup.lema.constrained.2) # Yes, there is a difference between the models,
#       the best model is the model no constrained one because the cfi, rmsea...indices.

#draw the path
par(mfrow=c(1,1))
semPaths(multigroup.lema,whatLabels = "std",  residuals = F, exoCov = F, edge.label.cex=1.00, reorder = FALSE)



#PUPA ----
modelo.pupa.1 <- ' #This model is for the dataframe neigbors.intra.inter.split #works with the no contrained
#regresions
#Plant_fitness = 
seed ~ Bee
seed ~ inter 
seed ~ intra
Bee ~flowers2
Fly ~intra
flowers2 ~inter+intra
seed ~~ flowers2
Bee ~~ Fly
Bee  ~  inter
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
#this next model is the good one for the seeds/fruit
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
#Este modelo es el mas cercano que he conseguido para PUPA. This model includes the seed/fruit and the seed/indv. 
#strange things: in the output of the lme of this species, the Bees seems to be important for the fitness of this species, but in the SEM 
# if we put the interaction between seed ~Bee the model didn't get a good fit; the fit of the model fit greatly when the seed~Bee interaction disappear.
modelo.pupa.new <- ' #This model is for the dataframe neigbors.intra.inter.split 
#regresions
#Plant_fitness = 
seed ~Fly
seed.indv ~ inter 
seed.indv ~ intra
Fly ~intra 
seed.indv ~~Beetle #el lme dice que podría ser importante para las semillas totales
Beetle ~inter + intra
#intercept
'


modelo.pupa.nacho <- ' #This model is for the dataframe neigbors.intra.inter.split 
#regresions
#Plant_fitness = 
seed ~ Fly + Bee + Beetle
fruit ~ inter + intra
Fly ~ intra + inter #+ flowers2
Bee ~ inter + intra #+ flowers2
Beetle ~ inter + intra #+ flowers2
#flowers2 ~ intra + inter
seed.indv ~ seed + fruit

#Beetle ~~ Fly
#Beetle ~~ Bee
#Bee ~~ Fly
#fruit ~~ seed
#intercept
'


PUPA.sem <- subset(data, Plant== "PUPA")
PUPA.sem2 <- PUPA.sem %>% filter(!(seed==3244.5))#este es el outlayer que nos encontramos en el lme
PUPA.sem1 <- PUPA.sem2[,c( "seed", "fruit", "x_coor2", "y_coor2", "Bee","Beetle",
                          "Fly","Butterfly", "flowers2","seed.indv", "distance_total","n_neighbors_intra", "n_neighbors_inter")] 

PUPA.sem1$seed <- scale(PUPA.sem1$seed)
PUPA.sem1$n_neighbors_inter <- scale(PUPA.sem1$n_neighbors_inter)
PUPA.sem1$n_neighbors_intra <- scale(PUPA.sem1$n_neighbors_intra)
PUPA.sem1$flowers2 <- scale(PUPA.sem1$flowers2)
PUPA.sem1$Butterfly <- scale(PUPA.sem1$Butterfly)
PUPA.sem1$seed.indv <- scale(PUPA.sem1$seed.indv)
PUPA.sem1$fruit <- scale(PUPA.sem1$fruit)

PUPA.sem1$inter <- PUPA.sem1$n_neighbors_inter
PUPA.sem1$intra <- PUPA.sem1$n_neighbors_intra


pupa.corr <- PUPA.sem1[,c( "seed", "fruit","seed.indv" ,
                           "flowers2","distance_total")]
corrgram(pupa.corr, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="PUPA correlations simplified")

corrgram(PUPA.sem1, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="PUPA correlations") #to check the correlation between the variables
multigroup.pupa <- sem(modelo.pupa.new, PUPA.sem1, group = "distance_total") #the best model is the modelo.pupa.3 for seed/fruit
summary(multigroup.pupa, standardize=T)
varTable(multigroup.pupa)
#model indices PUPA ----
fitMeasures(multigroup.pupa, c("cfi","rmsea","srmr", "pvalue")) #fitea 2 de 4. No fitea: pvalue y rmsea. 
print(modindices(multigroup.pupa))

#nacho
multigroup.pupa.nacho <- sem(modelo.pupa.nacho, PUPA.sem1, group = "distance_total") #the best model is the modelo.pupa.3 for seed/fruit
summary(multigroup.pupa.nacho, standardize=T)
varTable(multigroup.pupa.nacho)
#model indices PUPA ----
fitMeasures(multigroup.pupa.nacho, c("cfi","rmsea","srmr", "pvalue")) #fitea 2 de 4. No fitea: pvalue y rmsea. 
print(modindices(multigroup.pupa.nacho))


#now I'm going to creat the same model but constrained. This means that we are not considerating the different scales. 
multigroup.pupa.constrained <- sem(modelo.pupa.new, PUPA.sem1, group = "distance_total", group.equal = c("intercepts", "regressions"))
summary(multigroup.pupa.constrained, standardize=T)
fitMeasures(multigroup.pupa.constrained,c("cfi","rmsea","srmr", "pvalue") )

#now I compare both models to see if there is a difference between them
anova(multigroup.pupa, multigroup.pupa.constrained) # NO! There is not a difference between the two models. So in Pupa
#                   the different abudances of the neighbors do not have an effect on the fitness. The model that 
#                   explains better ou data is the model constrained. Better indices. 


#nacho
#now I'm going to creat the same model but constrained. This means that we are not considerating the different scales. 
multigroup.pupa.constrained.nacho <- sem(modelo.pupa.nacho, PUPA.sem1, group = "distance_total", group.equal = c("intercepts", "regressions"))
summary(multigroup.pupa.constrained.nacho, standardize=T)
fitMeasures(multigroup.pupa.constrained.nacho,c("cfi","rmsea","srmr", "pvalue") )

#now I compare both models to see if there is a difference between them
anova(multigroup.pupa.nacho, multigroup.pupa.constrained.nacho) # NO! There is not a difference between the two models. So in Pupa
#                   the different abudances of the neighbors do not have an effect on the fitness. The model that 
#                   explains better ou data is the model constrained. Better indices. 


#draw the path
par(mfrow=c(1,1))
semPaths(multigroup.pupa.constrained,whatLabels = "std",  residuals = F, exoCov = F, edge.label.cex=1.00, reorder = FALSE)
