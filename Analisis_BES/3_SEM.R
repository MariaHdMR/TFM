#Last stept in the analysis, the SEM multigroup. 
library(corrgram)#SEM
library(sem)#SEM
library(lavaan)#SEM
library(scales)
library(tidyverse)
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


modelo.chfu.fl <- ' #This model is for the dataframe neigbors.intra.inter.split
#regresions
#Plant_fitness = 
seed ~ Bee
seed ~ n_neighbors_inter
seed ~ n_neighbors_intra
Bee ~  n_neighbors_intra
Bee ~ n_neighbors_inter
Bee ~ flowers2
n_neighbors_inter ~~n_neighbors_intra
n_neighbors_inter ~~ flowers2
n_neighbors_intra ~~ flowers2
#intercept
'


modelo.chfu.latent <- ' #This model is for the dataframe neigbors.intra.inter.split
#regresions
#Plant_fitness = ~ seed 
#define the latent variables: V= floral visitors, n= Neighbors
V = ~ Bee
N = ~ n_neighbors_inter + n_neighbors_intra 

#structural relations
seed ~ V+N
V~~N
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
CHFU.sem1 <- CHFU.sem[,c( "seed", "fruit", "x_coor2", "y_coor2", "Bee","Beetle",
                          "Fly", "flowers2","distance_total","n_neighbors_intra", "n_neighbors_inter")] #there are no visits of 
#                                        butterflies, so I just need to eliminate it to can do the corregram

CHFU.sem1$seed <- rescale(CHFU.sem1$seed)
CHFU.sem1$n_neighbors_inter <- rescale(CHFU.sem1$n_neighbors_inter)
CHFU.sem1$n_neighbors_intra <- rescale(CHFU.sem1$n_neighbors_intra)
CHFU.sem1$flowers2 <- rescale(CHFU.sem1$flowers2)
CHFU.sem1$Bee <- rescale(CHFU.sem1$Bee)

corrgram(CHFU.sem1, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="CHFU correlations") #to check the correlation between the variables
multigroup.8 <- sem(modelo.chfu.fl, CHFU.sem1, group = "distance_total") 
summary(multigroup.8, standardize=T)
varTable(multigroup.8)
fitMeasures(multigroup.8, c("cfi","rmsea","srmr", "pvalue")) 
print(modindices(multigroup.8))


#no funciona
multigroup2.constrained.t <- sem(modelo.chfu, CHFU.sem1, group = "distance_total", group.equal = c("intercepts", "regressions"))
summary(multigroup2.constrained.t)
anova(multigroup.1, multigroup2.constrained.t)#better the constrained
par(mfrow=c(1,1))
semPaths(multigroup2.constrained.t)
