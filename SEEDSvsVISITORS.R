#load library
library(tidyverse)
library(MuMIn)
library(nlme)
library(lme4)
library(DHARMa)
library(reshape2)
#cargar datos
SEEDS <- read.table("data/simplex_competencia_SEEDS_2019.csv", header= T, sep= ";")
head(SEEDS)
str(SEEDS)
ABUNDANCES.pol <- read.table("data/FV_16_19.csv", header= T, sep= ";")#data de VISITS de polinizadores
#preparar datos
abun.pol.19 <- subset(ABUNDANCES.pol, Year=='2019')
abun.pol.19 <- subset(abun.pol.19, Plot != "OUT")
seeds1<- subset(SEEDS, Plant_Simple %in% c('CHFU', 'LEMA', 'ME', 'PUPA'))


P <- abun.pol.19 %>% group_by(Plot, Subplot, Group,Plant_Simple) %>% summarise (total.visits = sum(Visits))
simple<- subset(P, Plant_Simple %in% c('CHFU', 'LEMA', 'ME', 'PUPA') & Group %in% c('Bee','Beetle','Butterfly','Fly'))
simple <-simple[which(complete.cases(simple)),]
seeds1$Plot <- as.numeric(seeds1$Plot)
simple$Plot <- as.numeric(simple$Plot)
todo <- dplyr::left_join(seeds1,simple)
head(todo)
#str(juntos)
#juntos$Group <- as.character(juntos$Group)
todo$total.visits[is.na(todo$total.visits)] <- 0
head(todo)
#todo1 <- subset(todo, Plant_Simple %in% c('CHFU', 'LEMA', 'ME', 'PUPA'))
todo$Seed[which(todo$Seed == 0)] <- NA
todo$Fruit[which(todo$Fruit == 0)] <- NA
todo$total.visits[is.na(todo$total.visits)] <- 0

#Analysis
#globales ----
x_scale3 <- scale_x_continuous(limits = c(0,8))
global <- ggplot(todo, aes(x = total.visits, y = Seed, group = Group))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    x_scale3+ 
    ylab("Number of seeds")+
    xlab("Number of visits")+
    ggtitle("Seeds vs pollinator visits")
    NULL
global
x_scale4 <- scale_x_continuous(limits = c(0,7))
y_scale4 <- scale_y_continuous(limits = c(-100,1200))
dos <- ggplot(todo, aes(x = jitter(todo$total.visits), y= Seed))+ #GRAFICO GLOBAL PERO SEPARADO POR GUILD Y PLANTA
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    facet_wrap(Plant_Simple~Group)+
    ggtitle('Relation between the number of visits per plant species and their seeds')+
    ylab("Number of seeds")+
    xlab("Number of visits")+
    x_scale4+
    y_scale4+
    NULL
dos #este gráfico muestra que en LEMA los coleopteros hacen que haya menos semillas! 
#ahora graficos separados por especies de planta ----



##chfu ----

Seed.chfu <- subset(todo, Plant_Simple == "CHFU")
x_scale5 <- scale_x_continuous(limits = c(0,6))
S.chfu <- ggplot(Seed.chfu, aes(x = total.visits, y = Seed))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Relation between the number of CHFU visits and the seeds')+
    ylab("Number of CHFU's seeds")+
    xlab("Number of visits")+
    x_scale5+
    NULL
S.chfu

#glm por spp de visitor para comprobar la relacion

#Nacho redoes the model.
Seed.chfu2 <- dcast(Seed.chfu, Plot + Subplot + Plant_Simple + Fruit + Seed ~ Group, 
                    fun.aggregate = mean, value.var = "total.visits")
Seed.chfu2[is.na(Seed.chfu2)] <- 0
head(Seed.chfu2)
colSums(Seed.chfu2[6:9])
#Los cero fruits, en realidad es que no hay planta, los quitamos
Seed.chfu2$Seed[which(Seed.chfu2$Seed == 0)] <- NA
Seed.chfu2$Fruit[which(Seed.chfu2$Fruit == 0)] <- NA

#GLM.chfu <- lm(Seed.chfu2$Seed ~ Seed.chfu2$Fly + Seed.chfu2$Beetle + Seed.chfu2$Bee)
#plot(GLM.chfu) #bad

#GLM.chfu <- glm(Seed.chfu2$Seed ~ Seed.chfu2$Fly + Seed.chfu2$Beetle + Seed.chfu2$Bee, 
  #              family = "poisson")
vignette("DHARMa", package="DHARMa")
simulationOutput <- simulateResiduals(fittedModel = GLM.chfu, n = 250)
plot(simulationOutput)

GLM.chfu <- glm(Seed.chfu2$Seed ~ Seed.chfu2$Fly + Seed.chfu2$Beetle + Seed.chfu2$Bee, 
                family = "quasipoisson") #bueno
simulationOutput <- simulateResiduals(fittedModel = GLM.chfu, n = 250)
plot(simulationOutput)
summary(GLM.chfu)
r.squaredGLMM(GLM.chfu)


plot(Seed.chfu2$Seed ~ jitter(Seed.chfu2$Fly), las = 1)
abline(a = exp(GLM.chfu$coefficients[1]),
       b = exp(GLM.chfu$coefficients[2]), col = "red")

flies.chfu <- Seed.chfu2 [,c("Seed", "Fly")]
x_scale.1 <- scale_x_continuous(limits = c(0,8))
S.chfu.fly <- ggplot(flies.chfu,aes(x =jitter(Seed.chfu2$Fly), y = Seed))+ 
  geom_point(shape= "0", size = 2)+
  geom_smooth(method= "lm", col= "red")+
  ggtitle('Relation between the number of fly visits to CHFU and the seeds of CHFU')+
  ylab("Number of CHFU's seeds")+
  xlab("Number of fly visits")+
  x_scale.1+
  NULL
S.chfu.fly

beetles.chfu <- Seed.chfu2 [,c("Seed", "Beetle")]
x_scale.2 <- scale_x_continuous(limits = c(0,3))
S.chfu.beetles <- ggplot(beetles.chfu,aes(x =jitter(beetles.chfu$Beetle), y = Seed))+ 
  geom_point( shape= "0", size = 2)+
  geom_smooth(method= "lm")+
  ggtitle('Relation between the number of CHFU visits and the seeds')+
  ylab("Number of CHFU's seeds")+
  xlab("Number of visits")+
  x_scale.2+
  NULL
S.chfu.beetles

#No estoy seguro de que lalinea este bien. 
plot(Seed.chfu2$Seed ~ jitter(Seed.chfu2$Beetle), las = 1)
abline(a = exp(GLM.chfu$coefficients[1])/1+exp(GLM.chfu$coefficients[1]),
       b = exp(GLM.chfu$coefficients[2])/1+exp(GLM.chfu$coefficients[2]), col = "red")
#No estoy seguro de que lalinea este bien. 
plot(Seed.chfu2$Seed ~ jitter(Seed.chfu2$Bee), las = 1)
abline(a = exp(GLM.chfu$coefficients[1])/1+exp(GLM.chfu$coefficients[1]),
       b = exp(GLM.chfu$coefficients[2])/1+exp(GLM.chfu$coefficients[2]), col = "red")
#No estoy seguro de que lalinea este bien. 



#Maria's models.
#GLM.chfu.flies<- glm(Seed.chfu$Seed ~ Seed.chfu$total.visits, family = "quasipoisson", 
 #             subset = Seed.chfu$Group == "Fly") 
#plot(GLM.chfu.flies)
#summary(GLM.chfu.flies) #tendencia positva, pero ns para flies. 
#r.squaredGLMM(GLM.chfu.flies)
#GLM.chfu.bees<- glm(Seed.chfu$Seed ~ Seed.chfu$total.visits, family = "quasipoisson", 
 #                    subset = Seed.chfu$Group == "Bee")
#plot(GLM.chfu.bees)
#summary(GLM.chfu.bees) #tendencia positiva pero no para bees
#r.squaredGLMM(GLM.chfu.bees)
#GLM.chfu.beetles<- glm(Seed.chfu$Seed ~ Seed.chfu$total.visits, family = "quasipoisson", 
 #                   subset = Seed.chfu$Group == "Beetle")
#plot(GLM.chfu.beetles)
#summary(GLM.chfu.beetles) #tendencia positiva pero no para beetles
#r.squaredGLMM(GLM.chfu.beetles)

#chfu.t <- lme(Seed~ total.visits*Group, random=~1|Plot, 
 #         data=Seed.chfu,
  #        method="REML")
#plot(chfu.t)
#summary(chfu.t)
#r.squaredGLMM(chfu.t)
####
#lema ----
Seed.LEMA <- subset(todo, Plant_Simple=="LEMA")
x_scale6 <- scale_x_continuous(limits = c(0,8))
S.LEMA <- ggplot(Seed.LEMA, aes(x = jitter(Seed.LEMA$total.visits), y = Seed))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "glm", aes(color = Group))+
    ggtitle('Reltaion between the LEMA visits and seeds')+
    ylab("Number of LEMA's seeds")+
    xlab("Number of visits")+
    x_scale6+
    NULL
S.LEMA

Seed.lema2 <- dcast(Seed.LEMA, Plot + Subplot + Plant_Simple + Fruit + Seed ~ Group, 
                   fun.aggregate = mean, value.var = "total.visits")
Seed.lema2[is.na(Seed.lema2)] <- 0
head(Seed.lema2)
colSums(Seed.lema2[6:9])
Seed.lema2$Seed[which(Seed.lema2$Seed == 0)] <- NA
Seed.lema2$Fruit[which(Seed.lema2$Fruit == 0)] <- NA
GLM.lema <- glm(Seed.lema2$Seed ~ Seed.lema2$Fly + Seed.lema2$Beetle + Seed.lema2$Bee, 
                family = "quasipoisson")
summary(GLM.lema)
r.squaredGLMM(GLM.lema)

bee.le <- Seed.lema2[,c("Seed", "Bee")]
x_scale6 <- scale_x_continuous(limits = c(0,5))
S.LEMA.3 <- ggplot(bee.le, aes(x = jitter(bee.le$Bee), y = Seed))+ 
  geom_point(shape= "0", size = 2)+
  geom_smooth(method = "glm", col= "red")+
  ggtitle('Relation between the number of bee visits to LEMA and the seeds of LEMA')+
  ylab("Number of LEMA's seeds")+
  xlab("Number of bee visits")+
  x_scale6+
  NULL
S.LEMA.3

fly.le <- Seed.lema2[,c("Seed", "Fly")]
x_scale6 <- scale_x_continuous(limits = c(0,5))
S.fly.3 <- ggplot(fly.le, aes(x = jitter(fly.le$Fly), y = Seed))+ 
  geom_point(shape= "0", size = 2)+
  geom_smooth(method = "glm", col= "red")+
  ggtitle('Relation between the number of fly visits to LEMA and the seeds of LEMA')+
  ylab("Number of LEMA's seeds")+
  xlab("Number of fly visits")+
  x_scale6+
  NULL
S.fly.3


plot(Seed.lema2$Seed ~ jitter(Seed.lema2$Fly), las = 1)
abline(a = exp(GLM.lema$coefficients[1]),
       b = exp(GLM.lema$coefficients[2]), col = "red")
#No estoy seguro de que lalinea este bien. 
plot(Seed.lema2$Seed ~ jitter(Seed.lema2$Beetle), las = 1)
abline(a = exp(GLM.lema$coefficients[1])/1+exp(GLM.lema$coefficients[1]),
       b = exp(GLM.lema$coefficients[2])/1+exp(GLM.lema$coefficients[2]), col = "red")
#No estoy seguro de que lalinea este bien. 
plot(Seed.lema2$Seed ~ jitter(Seed.lema2$Bee), las = 1)
abline(a = exp(GLM.lema$coefficients[1])/1+exp(GLM.lema$coefficients[1]),
       b = exp(GLM.lema$coefficients[2])/1+exp(GLM.lema$coefficients[2]), col = "red")
#No estoy seguro de que lalinea este bien. 


#pupa----


Seed.PUPA <- subset(todo, Plant_Simple=="PUPA")

x_scale7 <- scale_x_continuous(limits = c(0,4.5))
y_scale7 <- scale_y_continuous(limits = c(-1,800))
S.PUPA <- ggplot(Seed.PUPA, aes(x = total.visits, y = Seed))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Relation betwwen PUPA visits and the seeds')+
    ylab("Number of PUPA's seeds")+
    xlab("Number of visits")+
    x_scale7+
    y_scale7+
    NULL
S.PUPA #especie de planta donde mas error hay en las regresiones lineales 
Seed.pupa2 <- dcast(Seed.PUPA, Plot + Subplot + Plant_Simple + Fruit + Seed ~ Group, 
                    fun.aggregate = mean, value.var = "total.visits")
Seed.pupa2[is.na(Seed.pupa2)] <- 0
head(Seed.pupa2)
colSums(Seed.pupa2[6:9])
Seed.pupa2$Seed[which(Seed.pupa2$Seed == 0)] <- NA
Seed.pupa2$Fruit[which(Seed.pupa2$Fruit == 0)] <- NA
GLM.pupa <- glm(Seed.pupa2$Seed ~ Seed.pupa2$Fly + Seed.pupa2$Beetle + Seed.pupa2$Bee+ Seed.pupa2$Butterfly, 
                family = "quasipoisson")
summary(GLM.pupa)
r.squaredGLMM(GLM.pupa)
plot(Seed.lema2$Seed ~ jitter(Seed.lema2$Fly), las = 1)
abline(a = exp(GLM.lema$coefficients[1])/1+exp(GLM.lema$coefficients[1]),
       b = exp(GLM.lema$coefficients[2])/1+exp(GLM.lema$coefficients[2]), col = "red")

plot(Seed.lema2$Seed ~ jitter(Seed.lema2$Beetle), las = 1)
abline(a = exp(GLM.lema$coefficients[1])/1+exp(GLM.lema$coefficients[1]),
       b = exp(GLM.lema$coefficients[2])/1+exp(GLM.lema$coefficients[2]), col = "red")

plot(Seed.lema2$Seed ~ jitter(Seed.lema2$Bee), las = 1)
abline(a = exp(GLM.lema$coefficients[1])/1+exp(GLM.lema$coefficients[1]),
       b = exp(GLM.lema$coefficients[2])/1+exp(GLM.lema$coefficients[2]), col = "red")

plot(Seed.lema2$Seed ~ jitter(Seed.lema2$Butterfly), las = 1)
abline(a = exp(GLM.lema$coefficients[1])/1+exp(GLM.lema$coefficients[1]),
       b = exp(GLM.lema$coefficients[2])/1+exp(GLM.lema$coefficients[2]), col = "red")



##me ----

Seed.ME<- subset(todo, Plant_Simple=="ME")
x_scale9 <- scale_x_continuous(limits = c(0,5))
S.ME <- ggplot(Seed.ME, aes(x = total.visits, y = Seed))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Relation between the number of visits of ME and the seeds')+
    ylab("Number of ME's seeds")+
    xlab("Number of visits")+
    x_scale9+
    NULL
S.ME


Seed.me2 <- dcast(Seed.ME, Plot + Subplot + Plant_Simple + Fruit + Seed ~ Group, 
                    fun.aggregate = mean, value.var = "total.visits")
Seed.me2[is.na(Seed.me2)] <- 0
head(Seed.me2)
colSums(Seed.me2[6:9])
Seed.me2$Seed[which(Seed.me2$Seed == 0)] <- NA
Seed.me2$Fruit[which(Seed.me2$Fruit == 0)] <- NA
GLM.me <- glm(Seed.me2$Seed ~ Seed.me2$Fly + Seed.me2$Beetle + Seed.me2$Bee, 
                family = "quasipoisson")
summary(GLM.me)
r.squaredGLMM(GLM.me)
plot(Seed.me2$Seed ~ jitter(Seed.me2$Fly), las = 1)
abline(a = exp(GLM.me$coefficients[1])/1+exp(GLM.me$coefficients[1]),
       b = exp(GLM.me$coefficients[2])/1+exp(GLM.me$coefficients[2]), col = "red")

plot(Seed.me2$Seed ~ jitter(Seed.me2$Beetle), las = 1)
abline(a = exp(GLM.me$coefficients[1])/1+exp(GLM.me$coefficients[1]),
       b = exp(GLM.me$coefficients[2])/1+exp(GLM.me$coefficients[2]), col = "red")

plot(Seed.me2$Seed ~ jitter(Seed.me2$Bee), las = 1)
abline(a = exp(GLM.me$coefficients[1])/1+exp(GLM.me$coefficients[1]),
       b = exp(GLM.me$coefficients[2])/1+exp(GLM.me$coefficients[2]), col = "red")
