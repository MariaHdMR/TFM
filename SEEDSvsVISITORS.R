#load library
library(tidyverse)
library(MuMIn)
library(nlme)
library(lme4)
library(DHARMa)

#cargar datos
SEEDS <- read.table("data/simplex_competencia_SEEDS_2019.csv", header= T, sep= ";")
head(SEEDS)
str(SEEDS)
ABUNDANCES.pol <- read.table("data/FV_16_19.csv", header= T, sep= ";")#data de VISITS de polinizadores
#preparar datos
abun.pol.19 <- subset(ABUNDANCES.pol, Year=='2019')
abun.pol.19 <- subset(abun.pol.19, Plot != "OUT")


P <- abun.pol.19 %>% group_by(Plot, Subplot, Group,Plant_Simple) %>% summarise (total.visits = sum(Visits))
P1<- subset(P, Plant_Simple %in% c('CHFU', 'LEMA', 'ME', 'PUPA') & Group %in% c('Bee','Beetle','Butterfly','Fly'))
P <-P[which(complete.cases(P)),]
SEEDS$Plot <- as.numeric(SEEDS$Plot)
P$Plot <- as.numeric(P$Plot)
juntos <- dplyr::left_join(SEEDS,P)
head(juntos)
#str(juntos)
#juntos$Group <- as.character(juntos$Group)
juntos[is.na(juntos)] <- 0
head(juntos)
juntos.b <- subset(juntos, Plant_Simple %in% c('CHFU', 'LEMA', 'ME', 'PUPA'))

#Analysis
#globales ----
x_scale3 <- scale_x_continuous(limits = c(0,8))
global <- ggplot(juntos.b, aes(x = total.visits, y = Seed, group = Group))+
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
dos <- ggplot(juntos.b, aes(x = total.visits, y= Seed))+ #GRAFICO GLOBAL PERO SEPARADO POR GUILD Y PLANTA
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

Seed.chfu <- subset(juntos.b, Plant_Simple == "CHFU")
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
colSums(Seed.chfu2[6:10])
#Los cero fruits, en realidad es que no hay planta, los quitamos
Seed.chfu2$Seed[which(Seed.chfu2$Seed == 0)] <- NA

GLM.chfu <- lm(Seed.chfu2$Seed ~ Seed.chfu2$Fly + Seed.chfu2$Beetle + Seed.chfu2$Bee)
#plot(GLM.chfu) #bad

GLM.chfu <- glm(Seed.chfu2$Seed ~ Seed.chfu2$Fly + Seed.chfu2$Beetle + Seed.chfu2$Bee, 
                family = "poisson")
vignette("DHARMa", package="DHARMa")
simulationOutput <- simulateResiduals(fittedModel = GLM.chfu, n = 250)
plot(simulationOutput)

GLM.chfu <- glm(Seed.chfu2$Seed ~ Seed.chfu2$Fly + Seed.chfu2$Beetle + Seed.chfu2$Bee, 
                family = "quasipoisson")
simulationOutput <- simulateResiduals(fittedModel = GLM.chfu, n = 250)
plot(simulationOutput)
summary(GLM.chfu)

plot(Seed.chfu2$Seed ~ jitter(Seed.chfu2$Fly), las = 1)
abline(a = exp(GLM.chfu$coefficients[1])/1+exp(GLM.chfu$coefficients[1]),
       b = exp(GLM.chfu$coefficients[2])/1+exp(GLM.chfu$coefficients[2]), col = "red")
#No estoy seguro de que lalinea este bien. 

#Maria's models.
GLM.chfu.flies<- glm(Seed.chfu$Seed ~ Seed.chfu$total.visits, family = "quasipoisson", 
              subset = Seed.chfu$Group == "Fly") 
plot(GLM.chfu.flies)
summary(GLM.chfu.flies) #tendencia positva, pero ns para flies. 
r.squaredGLMM(GLM.chfu.flies)
GLM.chfu.bees<- glm(Seed.chfu$Seed ~ Seed.chfu$total.visits, family = "quasipoisson", 
                     subset = Seed.chfu$Group == "Bee")
plot(GLM.chfu.bees)
summary(GLM.chfu.bees) #tendencia positiva pero no para bees
r.squaredGLMM(GLM.chfu.bees)
GLM.chfu.beetles<- glm(Seed.chfu$Seed ~ Seed.chfu$total.visits, family = "quasipoisson", 
                    subset = Seed.chfu$Group == "Beetle")
plot(GLM.chfu.beetles)
summary(GLM.chfu.beetles) #tendencia positiva pero no para beetles
r.squaredGLMM(GLM.chfu.beetles)

chfu.t <- lme(Seed~ total.visits*Group, random=~1|Plot, 
          data=Seed.chfu,
          method="REML")
plot(chfu.t)
summary(chfu.t)
r.squaredGLMM(chfu.t)
#lema ----
Seed.LEMA <- subset(juntos.b, Plant_Simple=="LEMA")
x_scale6 <- scale_x_continuous(limits = c(0,8))
S.LEMA <- ggplot(Seed.LEMA, aes(x = total.visits, y = Seed))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Reltaion between the LEMA visits and seeds')+
    ylab("Number of LEMA's seeds")+
    xlab("Number of visits")+
    x_scale6+
    NULL
S.LEMA
GLM.LEMA.flies<- glm(Seed.LEMA$Seed ~ Seed.LEMA$total.visits, family = "quasipoisson", 
               subset = Seed.LEMA$Group == "Fly")
plot(GLM.LEMA.flies)
summary(GLM.LEMA.flies) #tendencia positiva pero no para flies
r.squaredGLMM(GLM.LEMA.flies)

GLM.LEMA.beetles<- glm(Seed.LEMA$Seed ~ Seed.LEMA$total.visits, family = "quasipoisson", 
                     subset = Seed.LEMA$Group == "Beetle")
plot(GLM.LEMA.beetles)
summary(GLM.LEMA.beetles) #tendencia positiva pero no para beetles
r.squaredGLMM(GLM.LEMA.beetles)
GLM.LEMA.bees<- glm(Seed.LEMA$Seed ~ Seed.LEMA$total.visits, family = "quasipoisson", 
                       subset = Seed.LEMA$Group == "Bee")
plot(GLM.LEMA.bees)
summary(GLM.LEMA.bees) #tendencia positiva pero no para bees
r.squaredGLMM(GLM.LEMA.bees)
lema.t <- lme(Seed~ total.visits*Group, random=~1|Plot, 
              data=Seed.LEMA,
              method="REML")
summary(lema.t)
r.squaredGLMM(lema.t)


#pupa----


Seed.PUPA <- subset(juntos.b, Plant_Simple=="PUPA")
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


GLM.PUPA.flies<- glm(Seed.PUPA$Seed ~ Seed.PUPA$total.visits, family = "quasipoisson", 
                     subset = Seed.PUPA$Group == "Fly")
plot(GLM.PUPA.flies)
summary(GLM.PUPA.flies)#tendencia positiva pero no para flies
r.squaredGLMM(GLM.PUPA.flies)

GLM.PUPA.bee<- glm(Seed.PUPA$Seed ~ Seed.PUPA$total.visits, family = "quasipoisson", 
                     subset = Seed.PUPA$Group == "Bee")

plot(GLM.PUPA.bee)
summary(GLM.PUPA.bee) #tendencia positiva pero no para bee
r.squaredGLMM(GLM.PUPA.bee)
GLM.PUPA.beetle<- glm(Seed.PUPA$Seed ~ Seed.PUPA$total.visits, family = "quasipoisson", 
                   subset = Seed.PUPA$Group == "Beetle") #solo hay 3 datos de pupa con beetle 

plot(GLM.PUPA.beetle)
summary(GLM.PUPA.beetle)
r.squaredGLMM(GLM.PUPA.beetle)

GLM.PUPA.butterfly<- glm(Seed.PUPA$Seed ~ Seed.PUPA$total.visits, family = "quasipoisson", 
                      subset = Seed.PUPA$Group == "Butterfly")

plot(GLM.PUPA.butterfly)
summary(GLM.PUPA.butterfly)#tendencia positiva pero no para butterflies
r.squaredGLMM(GLM.PUPA.butterfly)

pupa.t <- lme(Seed~ total.visits*Group, random=~1|Plot, 
              data=Seed.PUPA,
              method="REML")
summary(pupa.t)
r.squaredGLMM(pupa.t)
##me ----

Seed.ME<- subset(juntos.b, Plant_Simple=="ME")
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

GLM.ME.bee <- glm(Seed.ME$Seed ~ Seed.ME$total.visits, family = "quasipoisson", 
                         subset = Seed.ME$Group == "Bee")

plot(GLM.ME.bee )
summary(GLM.ME.bee) #!!! Something strange! NAs appeared in the analysis ----
r.squaredGLMM(GLM.ME.bee)
GLM.ME.beetle <- glm(Seed.ME$Seed ~ Seed.ME$total.visits, family = "quasipoisson", 
                  subset = Seed.ME$Group == "Beetle")

plot(GLM.ME.beetle )
summary(GLM.ME.beetle ) #!!! Something strange! NAs appeared in the analysis


GLM.ME.fly <- glm(Seed.ME$Seed ~ Seed.ME$total.visits, family = "quasipoisson", 
                     subset = Seed.ME$Group == "Fly")

plot(GLM.ME.fly )
summary(GLM.ME.fly ) #tendencia positiva salvo para flies
r.squaredGLMM(GLM.ME.fly)
me.t <- lme(Seed~ total.visits*Group, random=~1|Plot, 
              data=Seed.ME,
              method="REML")#no works ----
summary(me.t)
r.squaredGLMM(me.t)
####################### Glm globales por especie de planta -> Nuevo----

juntos.b$Plot <- as.numeric(juntos.b$Plot)
mod1 <- glm (data = juntos.b, Seed ~ Plant_Simple + total.visits + Group + 
                 (1|Plot)) # Error, no pilla las abejas ni a CHFU----
summary(mod1)#este es un glm global.

#mod2 <- glm (data = juntos.b, Seed ~ total.visits +
 #                (1|Plot)) # Error,plot como NA
#summary(mod2) #este es un glm global.
#mod3 <- lm (data = Seed.ME, Seed ~  total.visits + Group + 
#         (1|Plot))
#summary(mod3)
#mod4 <- glm (data = Seed.PUPA, Seed ~  total.visits + Group + 
#                 (1|Plot))
#summary(mod4)
#r.squaredGLMM(mod4)
#mod5 <- lmer(data = Seed.chfu, Seed ~  total.visits + Group+
#                 (1|Plot))
#summary(mod5)
#r.squaredGLMM(mod5)


t1 <- lme(Seed~ total.visits + Group, random=~1|Plot, 
    data=Seed.chfu,
    method="REML") #me siguen sin salir los datos de bees al añadir los grupos----
summary(t1)
r.squaredGLMM(t1)
plot(t1)
t2 <- lme(Seed~ total.visits*Group, random=~1|Plot, 
          data=Seed.PUPA,
          method="REML") #me siguen sin salir los datos de bees al añadir los grupos----
summary(t2)
r.squaredGLMM(t2)#este se ajusta mejor que el siguiente
plot(t2)
anova(t2)
t3 <- lme(Seed~ total.visits + Group, random=~1|Plot, 
          data=Seed.PUPA,
          method="REML")
summary(t3)
r.squaredGLMM(t3)
anova(t3)
##dharma package ----
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = GLM.PUPA.flies, n = 250)
simulationOutput$scaledResiduals
plot(simulationOutput)
testResiduals(simulationOutput)
testUniformity(simulationOutput = simulationOutput)
###