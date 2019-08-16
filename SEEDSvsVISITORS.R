#load library
library(tidyverse)
#cargar datos
SEEDS <- read.table("data/simplex_competencia_SEEDS_2019.csv", header= T, sep= ";")
head(SEEDS)
str(SEEDS)
ABUNDANCES.pol <- read.table("V_a_16_19.csv", header= T, sep= ";")
#preparar datos
abun.pol.19 <- subset(ABUNDANCES.pol, Year=='2019')
abun.pol.19 <- subset(abun.pol.19, Plot != "OUT")


P <- abun.pol.19 %>% group_by(Plot, Subplot, Group,Plant_Simple) %>% summarise (abun.pol = sum(Abundances))
P<- subset(P, Plant_Simple %in% c('CHFU', 'HOMA', 'LEMA', 'ME', 'PUPA', 'POMA','CHMI') & Group %in% c('Bees','Beetles','Butterflies','Flies'))
P <-P[which(complete.cases(P)),]
juntos <- dplyr::left_join(SEEDS,P)
str(juntos)
juntos$Group <- as.character(juntos$Group)
#juntos[is.na(juntos)] <- 0
head(juntos)
juntos.b <- subset(juntos, Plant_Simple %in% c('CHFU', 'HOMA', 'LEMA', 'ME', 'PUPA', 'POMA','CHMI') & Group  %in% c('Bees','Beetles','Butterflies','Flies'))

#Analysis
#globales ----
x_scale3 <- scale_x_continuous(limits = c(0,700))
global <- ggplot(juntos.b, aes(x = Seed, y = abun.pol, group = Group))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    x_scale3+ 
    ylab("Number of visitors")+
    xlab("Number of seeds")+
    ggtitle("Seeds vs abun pol")
    NULL
global
x_scale4 <- scale_x_continuous(limits = c(0,600))
dos <- ggplot(juntos.b, aes(x = Seed, y= abun.pol))+ #GRAFICO GLOBAL PERO SEPARADO POR GUILD Y PLANTA
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    facet_wrap(Plant_Simple~Group)+
    ggtitle('Relacion del numero de semillas y la abundancia de polinizadores 2019')+
    ylab("Number of visitors")+
    xlab("Number of seeds")+
    x_scale4+
    NULL
dos #este gráfico muestra que en LEMA los coleopteros hacen que haya menos semillas! Confirmado!
#ahora graficos separados por especies de planta ----
Seed.chfu <- subset(juntos.b, Plant_Simple == "CHFU")
x_scale5 <- scale_x_continuous(limits = c(0,900))
S.chfu <- ggplot(Seed.chfu, aes(x = Seed, y = abun.pol))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Relacion del numero de semillas  de CHFU y la abundancia de polinizadores 2019')+
    ylab("Number of visitors")+
    xlab("Number of CHFU's seeds")+
    x_scale5+
    NULL
S.chfu

#glm por spp de visitor para comprobar la relacion
GLM.chfu<- glm(cbind(Seed.chfu$Seed,Seed.chfu$abun.pol) ~ Seed.chfu$Group, 
          family = "binomial")
summary(GLM.chfu) # las moscas y las abejas afectan a la cantidad de semillas en CHFU



Seed.LEMA <- subset(juntos.b, Plant_Simple=="LEMA")
x_scale6 <- scale_x_continuous(limits = c(0,900))
S.LEMA <- ggplot(Seed.LEMA, aes(x = Seed, y = abun.pol))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Relacion del numero de semillas  de LEMA y la abundancia de polinizadores 2019')+
    ylab("Number of visitors")+
    xlab("Number of LEMA's seeds")+
    x_scale6+
    NULL
S.LEMA
GLM.LEMA<- glm(cbind(Seed.LEMA$Seed,Seed.LEMA$abun.pol) ~ Seed.LEMA$Group, 
                     family = "binomial")
summary(GLM.LEMA) #todas las especies afectan a la cantidad de semillas!!!

Seed.PUPA <- subset(juntos.b, Plant_Simple=="PUPA")
x_scale7 <- scale_x_continuous(limits = c(0,600))
S.PUPA <- ggplot(Seed.PUPA, aes(x = Seed, y = abun.pol))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Relacion del numero de semillas de PUPA y la abundancia de polinizadores 2019')+
    ylab("Number of visitors")+
    xlab("Number of PUPA's seeds")+
    x_scale7+
    NULL
S.PUPA

GLM.PUPA<- glm(cbind(Seed.PUPA$Seed,Seed.PUPA$abun.pol) ~ Seed.PUPA$Group, 
               family = "binomial")
summary(GLM.PUPA) #las abejas son las únicas que afectan

Seed.Homa <- subset(juntos.b, Plant_Simple=="HOMA")
x_scale8 <- scale_x_continuous(limits = c(0,40))
S.Homa <- ggplot(Seed.Homa, aes(x = Seed, y = abun.pol))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Relacion del numero de semillas de HOMA y la abundancia de polinizadores 2019')+
    ylab("Number of visitors")+
    xlab("Number of HOMA's seeds")+
    x_scale8+
    NULL
S.Homa

GLM.HOMA<- glm(cbind(Seed.Homa$Seed,Seed.Homa$abun.pol) ~ Seed.Homa$Group, 
               family = "binomial")
summary(GLM.HOMA) #le afectan los coleopteros, pero hay muy pocos datos de visitas de polinizadores a HOMA

Seed.ME<- subset(juntos.b, Plant_Simple=="ME")
x_scale9 <- scale_x_continuous(limits = c(0,400))
S.ME <- ggplot(Seed.ME, aes(x = Seed, y = abun.pol))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Relacion del numero de semillas de ME y la abundancia de polinizadores 2019')+
    ylab("Number of visitors")+
    xlab("Number of ME's seeds")+
    x_scale9+
    NULL
S.ME

GLM.ME<- glm(cbind(Seed.ME$Seed,Seed.ME$abun.pol) ~ Seed.ME$Group, 
               family = "binomial")
summary(GLM.ME) #aparece que le afectan las abejas, pero en la gráfica parece que es la menos determinantem ya que se queda estable, Hmmm...extraño

Seed.CHMI<- subset(juntos.b, Plant_Simple=="CHMI")
x_scale10 <- scale_x_continuous(limits = c(0,400))
S.CHMI <- ggplot(Seed.CHMI, aes(x = Seed, y = abun.pol))+ 
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ggtitle('Relacion del numero de semillas de CHMI y la abundancia de polinizadores 2019')+
    ylab("Number of visitors")+
    xlab("Number of CHMI's seeds")+
    x_scale9+
    NULL
S.CHMI

GLM.CHMI<- glm(cbind(Seed.CHMI$Seed,Seed.CHMI$abun.pol) ~ Seed.CHMI$Group, 
             family = "binomial")
summary(GLM.CHMI)#no tiene sentido tener en cuenta CHMI por las dos visitas que tiene en las que no hay semillas registradas

#resultados: 
    # SEGUN LOS GRAFICOS :en las especies CHFU,LEMA,PUPA,ME,HOMA se ve que los coleopteros afectan negativamente a la producción de semillas. 
    #parece que a estas mismas especies(salvo PUPA), las moscas afectan positivamente (de manera ligera) a la producción de semillas
    #segun los glm solo en LEMA afectan todos los visitantes florales a la produccion de semillas.  