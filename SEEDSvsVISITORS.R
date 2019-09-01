#load library
library(tidyverse)
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
#juntos[is.na(juntos)] <- 0
head(juntos)
juntos.b <- subset(juntos, Plant_Simple %in% c('CHFU', 'LEMA', 'ME', 'PUPA') & Group  %in% c('Bee','Beetle','Butterfly','Fly'))

#Analysis
#globales ----
x_scale3 <- scale_x_continuous(limits = c(0,1000))
global <- ggplot(juntos.b, aes(x = Seed, y = total.visits, group = Group))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    x_scale3+ 
    ylab("Number of visits")+
    xlab("Number of seeds")+
    ggtitle("Seeds vs pollinator visits")
    NULL
global
x_scale4 <- scale_x_continuous(limits = c(0,600))
dos <- ggplot(juntos.b, aes(x = Seed, y= total.visits))+ #GRAFICO GLOBAL PERO SEPARADO POR GUILD Y PLANTA
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    facet_wrap(Plant_Simple~Group)+
    ggtitle('Relation between the number of visits per plant species and their seeds')+
    ylab("Number of visits")+
    xlab("Number of seeds")+
    x_scale4+
    NULL
dos #este gráfico muestra que en LEMA los coleopteros hacen que haya menos semillas! 
#ahora graficos separados por especies de planta ----

##chfu ----

Seed.chfu <- subset(juntos.b, Plant_Simple == "CHFU")
x_scale5 <- scale_x_continuous(limits = c(0,900))
S.chfu <- ggplot(Seed.chfu, aes(x = Seed, y = total.visits))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Relation between the number of CHFU visits and the seeds')+
    ylab("Number of visits")+
    xlab("Number of CHFU's seeds")+
    x_scale5+
    NULL
S.chfu

#glm por spp de visitor para comprobar la relacion


GLM.chfu.flies<- glm(Seed.chfu$Seed ~ Seed.chfu$total.visits, family = "quasipoisson", 
              subset = Seed.chfu$Group == "Fly")
plot(GLM.chfu.flies)
summary(GLM.chfu.flies) #tendencia positva, pero ns para flies. 
GLM.chfu.bees<- glm(Seed.chfu$Seed ~ Seed.chfu$total.visits, family = "quasipoisson", 
                     subset = Seed.chfu$Group == "Bee")
plot(GLM.chfu.bees)
summary(GLM.chfu.bees) #tendencia positiva pero no para bees

GLM.chfu.beetles<- glm(Seed.chfu$Seed ~ Seed.chfu$total.visits, family = "quasipoisson", 
                    subset = Seed.chfu$Group == "Beetle")
plot(GLM.chfu.beetles)
summary(GLM.chfu.beetles) #tendencia positiva pero no para beetles

#lema ----
Seed.LEMA <- subset(juntos.b, Plant_Simple=="LEMA")
x_scale6 <- scale_x_continuous(limits = c(0,900))
S.LEMA <- ggplot(Seed.LEMA, aes(x = Seed, y = total.visits))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Reltaion between the LEMA visits and seeds')+
    ylab("Number of visits")+
    xlab("Number of LEMA's seeds")+
    x_scale6+
    NULL
S.LEMA
GLM.LEMA.flies<- glm(Seed.LEMA$Seed ~ Seed.LEMA$total.visits, family = "quasipoisson", 
               subset = Seed.LEMA$Group == "Fly")
plot(GLM.LEMA.flies)
summary(GLM.LEMA.flies) #tendencia positiva pero no para flies

GLM.LEMA.beetles<- glm(Seed.LEMA$Seed ~ Seed.LEMA$total.visits, family = "quasipoisson", 
                     subset = Seed.LEMA$Group == "Beetle")
plot(GLM.LEMA.beetles)
summary(GLM.LEMA.beetles) #tendencia positiva pero no para beetles

GLM.LEMA.bees<- glm(Seed.LEMA$Seed ~ Seed.LEMA$total.visits, family = "quasipoisson", 
                       subset = Seed.LEMA$Group == "Bee")
plot(GLM.LEMA.bees)
summary(GLM.LEMA.bees) #tendencia positiva pero no para bees



#pupa----


Seed.PUPA <- subset(juntos.b, Plant_Simple=="PUPA")
x_scale7 <- scale_x_continuous(limits = c(0,600))
S.PUPA <- ggplot(Seed.PUPA, aes(x = Seed, y = total.visits))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Relation betwwen PUPA visits and the seeds')+
    ylab("Number of visits")+
    xlab("Number of PUPA's seeds")+
    x_scale7+
    NULL
S.PUPA #especie de planta donde mas error hay en las regresiones lineales 


GLM.PUPA.flies<- glm(Seed.PUPA$Seed ~ Seed.PUPA$total.visits, family = "quasipoisson", 
                     subset = Seed.PUPA$Group == "Fly")

plot(GLM.PUPA.flies)
summary(GLM.PUPA.flies)#tendencia positiva pero no para flies


GLM.PUPA.bee<- glm(Seed.PUPA$Seed ~ Seed.PUPA$total.visits, family = "quasipoisson", 
                     subset = Seed.PUPA$Group == "Bee")

plot(GLM.PUPA.bee)
summary(GLM.PUPA.bee) #tendencia positiva pero no para bee

GLM.PUPA.beetle<- glm(Seed.PUPA$Seed ~ Seed.PUPA$total.visits, family = "quasipoisson", 
                   subset = Seed.PUPA$Group == "Beetle") #solo hay 3 datos de pupa con beetle 

plot(GLM.PUPA.beetle)
summary(GLM.PUPA.beetle)


GLM.PUPA.butterfly<- glm(Seed.PUPA$Seed ~ Seed.PUPA$total.visits, family = "quasipoisson", 
                      subset = Seed.PUPA$Group == "Butterfly")

plot(GLM.PUPA.butterfly)
summary(GLM.PUPA.butterfly)#tendencia positiva pero no para butterflies

##me ----

Seed.ME<- subset(juntos.b, Plant_Simple=="ME")
x_scale9 <- scale_x_continuous(limits = c(0,400))
S.ME <- ggplot(Seed.ME, aes(x = Seed, y = total.visits))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Relation between the number of visits of ME and the seeds')+
    ylab("Number of visits")+
    xlab("Number of ME's seeds")+
    x_scale9+
    NULL
S.ME

GLM.ME.bee <- glm(Seed.ME$Seed ~ Seed.ME$total.visits, family = "quasipoisson", 
                         subset = Seed.ME$Group == "Bee")

plot(GLM.ME.bee )
summary(GLM.ME.bee) #!!! Something strange! NAs appeared in the analysis

GLM.ME.beetle <- glm(Seed.ME$Seed ~ Seed.ME$total.visits, family = "quasipoisson", 
                  subset = Seed.ME$Group == "Beetle")

plot(GLM.ME.beetle )
summary(GLM.ME.beetle ) #!!! Something strange! NAs appeared in the analysis


GLM.ME.fly <- glm(Seed.ME$Seed ~ Seed.ME$total.visits, family = "quasipoisson", 
                     subset = Seed.ME$Group == "Fly")

plot(GLM.ME.fly )
summary(GLM.ME.fly ) #tendencia positiva salvo para flies


