### aqui creamos la tabla compelta de abundancias mas visitas ----
FV_16_19 <- read.csv("data/FV_16_19.csv", sep = ";")
head(FV_16_19)
Abun_19 <- read.csv("data/Abun_19.csv", sep = ";")
head(Abun_19)

#load librraies
library(tidyverse)


FV_19 <- subset(FV_16_19, Year == 2019) 
FV_19
Abun_19
Abun_19$Plot <- Abun_19$plot
Abun_19$Subplot <- Abun_19$subplot
Abun_19$Plant_Simple <- Abun_19$Sp.Focal
ab.19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Plantas))
FV.19<- FV_19 %>% group_by(Plot, Subplot, Plant_Simple, Group, Order, Family, Species) %>% summarise(num.visits= sum(Visits))

FINAL <- dplyr::left_join(FV.19, ab.19) #IB: not working in my computer.

## datos de visitors (numero de visitas por subplot a una planta) + abundancias plantas
V_a_16_19 <- read.csv("data/V_a_16_19.csv", sep = ";") #imagino que estos son los datos.
head(V_a_16_19)

V.19 <- subset(V_a_16_19, Year == 2019)
V.19$Plot <- as.numeric(as.character(V.19$Plot))
V <-V.19 %>% group_by(Plot, Subplot,Plant_Simple, Group, Order, Family, Species)%>% summarise (abun =sum(Abundances))
abun.F <- dplyr::left_join(V, ab.19)


#visitas de polinizadores en las diferentes plantas ----

FINAL1 <- subset(FINAL,Plant_Simple %in% c("LEMA","CHFU","RAPE","ME", "HOMA","PUPA", "CHMI") & Group %in% c("Beetle", "Fly", "Butterfly","Bee"))
pollinator <- ggplot(V) + 
    geom_boxplot(aes(x = Plant_Simple, y = abun)) +
    ggtitle("visitor abundance per Plant")
# facet_grid(plant~.)+
NULL
pollinator
#abundancia de pol segun los Grupos ----
boxplot(V$abun~V$Group,xlab= 'Guild', ylab= 'num.pol', main= 'Num de pol en los grupos 2019' )
#abundancia de pol segun los plots ----
boxplot(V$abun~V$Plot,xlab='Plots', ylab= 'num.pol', main= 'Abundancia de pol en los plots 2019')

#relacion entre el numero de abundancias de plantas y el numero de visitantes que reciben, GLOBAL----
#abun.F = base de visitors_abundances
visitas.19 <- ggplot(abun.F, aes(x = num.plantas, y = abun))+
    geom_point()+
    geom_smooth(method = "lm")+
    xlab("Abundancia de plantas")+
    ylab("Numero de visitas de polinizadores")+
    ggtitle ("relacion entre el numero de abundancias de plantas y la abun de visitantes")
NULL
visitas.19
#num de visitors segun las abundancias de plantas 2019 ----
visitors <- ggplot(abun.F, aes(x = num.plantas, y = abun, group = Group))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    x_scale1+ #not found
    ggtitle("num de visitors segun las abundancias de plantas 2019")
NULL
visitors

#visitas de pol segun la abundancia por spp de Planta ----
abun.F$Plant_Simple <- as.factor(abun.F$Plant_Simple)
abun.F$Group <- as.factor(abun.F$Group)
#lema -----
tabla.LEMA <- subset(abun.F, Plant_Simple == 'LEMA')
t.LEMA <-subset(tabla.LEMA, Group %in% c("Bees","Butterflies","Flies","Beetles"))
visitas.LEMA <- ggplot(t.LEMA, aes(x = num.plantas, y = abun, group = Group))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ylab("num visitors")+
    xlab("abundancia LEMA")+
    ggtitle("Relacion de la abundancia de LEMA y numero de visitors que recibe 2019")+
    NULL
visitas.LEMA

#chfu----
library(tidyverse) #ya cargada
tabla.CHFU<- subset(abun.F, Plant_Simple == 'CHFU')
visitas.CHFU <- ggplot(tabla.CHFU, aes(x = num.plantas, y = abun, group = Group))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ylab("num visitors")+
    xlab("abundancia CHFU")+
    ggtitle("Relacion de la abundancia de CHFU y numero de visitors que recibe 2019")+
    NULL
visitas.CHFU
#PUPA ----
tabla.PUPA<- subset(abun.F, Plant_Simple == 'PUPA')
visitas.PUPA <- ggplot(tabla.PUPA, aes(x = num.plantas, y = abun, group = Group))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ylab("num visitors")+
    xlab("abundancia PUPA")+
    ggtitle("Relacion de la abundancia de PUPA y numero de visitors que recibe 2019")+
    NULL
visitas.PUPA

#HOMA ----
tabla.HOMA<- subset(abun.F, Plant_Simple == 'HOMA')
visitas.HOMA <- ggplot(tabla.HOMA, aes(x = num.plantas, y = abun, group = Group))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ylab("num visitors")+
    xlab("abundancia HOMA")+
    ggtitle("Relacion de la abundancia de HOMA y numero de visitors que recibe 2019")+
    NULL
visitas.HOMA # solo le afectan los coleopteros
#POMA----
#tabla.POMA<- subset(abun.F, Plant_Simple == 'POMA')
#visitas.POMA <- ggplot(tabla.POMA, aes(x = num.plantas, y = abun, group = Group))+
 #   geom_point(aes(color = Group))+
  #  geom_smooth(method = "lm", aes(color = Group))+
  #  ylab("num visitors")+
  #  xlab("abundancia POMA")+
  #  ggtitle("Relacion de la abundancia de POMA y numero de visitors que recibe 2019")+
  # NULL
#visitas.POMA #solo tiene visitas de coleopteros, y y TIENE SOLO 4 VISITAS --> no interesa

#ME ----
tabla.ME<- subset(abun.F, Plant_Simple == 'ME')
visitas.ME <- ggplot(tabla.ME, aes(x = num.plantas, y = abun, group = Group))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    ylab("num visitors")+
    xlab("abundancia ME")+
    ggtitle("Relacion de la abundancia de ME y numero de visitors que recibe 2019")+
    NULL
visitas.ME

#RAPE ----
#tabla.RAPE<- subset(abun.F, Plant_Simple == 'RAPE')
#visitas.RAPE <- ggplot(tabla.RAPE, aes(x = num.plantas, y = abun, group = Group))+
  #  geom_point(aes(color = Group))+
  #  geom_smooth(method = "lm", aes(color = Group))+
  #  ylab("num visitors")+
  #  xlab("abundancia RAPE")+
  #  ggtitle("Relacion de la abundancia de RAPE y numero de visitors que recibe 2019")+
  #  NULL
#visitas.RAPE #no dice nada,SOLO 6 REGISTROS

#chmi
#tabla.CHMI<- subset(abun.F, Plant_Simple == 'CHMI')
#visitas.CHMI <- ggplot(tabla.CHMI, aes(x = num.plantas, y = abun, group = Group))+
 #   geom_point(aes(color = Group))+
  #  geom_smooth(method = "lm", aes(color = Group))+
   # ylab("num visitors")+
   # xlab("abundancia CHMI")+
  #  ggtitle("Relacion de la abundancia de CHMI y numero de visitors que recibe 2019")+
   # NULL
# visitas.CHMI #solo una visita, no interesa


#global polinizadores y plantas por casos -> OCURRE ALGO RARO PORQUE HOMA Y BEETLES APENAS DATOS----
x_scale2 <- scale_x_continuous(limits = c(0,100))#para cortar la escala
m <-subset(abun.F, Plant_Simple %in% c('CHFU', 'HOMA', 'LEMA', 'ME', 'PUPA', 'POMA') & Group %in% c('Bees','Beetles','Butterflies','Flies'))
vis <- ggplot(m, aes(x = num.plantas, y = abun))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    facet_wrap(Plant_Simple~Group)+
    ggtitle('Relacion de las abundancias de plantas y la abundancia de visitantes Global 2019')+
    x_scale2+
    NULL
vis

#phenology ----
V.19.1 <- V.19[which(!is.na(V.19$Month)),] 
V.19.1$date <- paste(V.19.1$Year,"-",V.19.1$Month,"-",V.19.1$Day,sep="")
V.19.1$week <- strftime(V.19.1$date,format = "%V")
V.19.1$week <- strftime(V.19.1$date,format = "%V")

library(tidyverse) #idem
phenology.color. <- ggplot(V.19.1, aes(x= week, y = Plant_Simple))+
    geom_point(aes(color = week))+
    ggtitle ("Phenology 2019")+
    xlab ("weeks")+
    ylab ("spp_Plants")
NULL
phenology.color.
#fases: 
#   1) RAPE, CHFU
#   2) ME,LEMA, HOMA, CHFU,POMA,SOAS
#   3) PUPA, LEMA, HOMA, CHMI


fase1<- subset(abun.F, Plant_Simple %in% c('RAPE', 'CHFU'))
fase2<- subset(abun.F, Plant_Simple %in% c('ME', 'CHFU','LEMA','HOMA','POMA','SOAS'))
fase3<- subset(abun.F, Plant_Simple %in% c('CHMI','LEMA','HOMA','PUPA'))

fenologia.fase1 <- ggplot(fase1, aes(x = num.plantas, y = abun))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    facet_wrap(Plant_Simple~Group)+
    ggtitle('relacion entre spp de plantas de la fase 1 y los polinizadores 2019')+
    xlab('abundancia de las plantas')+
    ylab('numero de visitas de los pol')+
    NULL
fenologia.fase1

fenologia.fase2 <- ggplot(fase2, aes(x = num.plantas, y = abun))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    facet_wrap(Plant_Simple~Group)+
    ggtitle('relacion entre spp de plantas de la fase 2 y los polinizadores 2019')+
    xlab('abundancia de las plantas')+
    ylab('numero de visitas de los pol')+
    x_scale2+
    NULL
fenologia.fase2


fenologia.fase3 <- ggplot(fase3, aes(x = num.plantas, y = abun))+
    geom_point(aes(color = Group))+
    geom_smooth(method = "lm", aes(color = Group))+
    facet_wrap(Plant_Simple~Group)+
    ggtitle('relacion entre spp de plantas de la fase 3 y los polinizadores 2019')+
    xlab('abundancia de las plantas')+
    ylab('numero de visitas de los pol')+
    x_scale2
    NULL
fenologia.fase3


### random factors ----
library(MuMIn)
library(nlme)
library(tidyverse)
#PUPA ----
m.pupa = lme(num.plantas~ abun, random=~1|Plot, #no es significativo
                 data=tabla.PUPA,
                 method="REML")   
summary(m.pupa)
plot(m.pupa) #este modelo no cumple las asumpciones. No cambiara el resultado, 
 #pero si lo usamos habria que corregir esto, e.g. hacer log's
r.squaredGLMM(m.pupa) #fijate que plot lo explica casi todo. 

#ahora solo con un grupo
tabla.P.Co <- subset(tabla.PUPA, Group == 'Beetles')
tabla.P.Bee <- subset(tabla.PUPA, Group == 'Bees')
tabla.P.Bi <- subset(tabla.PUPA, Group == 'Butterflies')
tabla.P.F <- subset(tabla.PUPA, Group == 'Flies')
#m.P.Co = lme(num.plantas~ abun, random=~1|Plot, #no es significativo
 #            data=tabla.P.Co,
  #           method="REML")   
#summary(m.P.Co) #solo 3 visitas de coleopteros, no funciona
m.P.Bee = lme(num.plantas~ abun, random=~1|Plot, 
             data=tabla.P.Bee,
             method="REML")   
summary(m.P.Bee)
r.squaredGLMM(m.P.Bee) 
plot(m.P.Bee)

m.P.Bi = lme(num.plantas~ abun, random=~1|Plot, 
              data=tabla.P.Bi,
              method="REML")   
summary(m.P.Bi)
r.squaredGLMM(m.P.Bi) 

m.P.F = lme(log(num.plantas+1) ~ abun, random=~1|Plot, 
             data=tabla.P.F,
             method="REML")   
summary(m.P.F) #SIGNIFICATIVOOOOO!!!!!! PARA FLIES
plot(m.P.F) #modelo no se ajusta a las asunciones. Añado lo por ahora.
plot(log(tabla.P.F$num.plantas+1) ~ tabla.P.F$abun)
abline(m.P.F$coefficients$fixed) 
#Es significativo, pero solo hay tres puntos con abundance = 2... no me creeria yo este modelo mucho.

#chfu----
tabla.CHFU.1 <- tabla.CHFU[which(!is.na(tabla.CHFU$num.plantas)),] 
m.chfu = lme(num.plantas~ abun, random=~1|Plot, #no es significativo
             data=tabla.CHFU.1,
             method="REML")   
summary(m.chfu)
plot(m.chfu) #tambien hay un poco de dispersion de la variancia con la media...
r.squaredGLMM(m.chfu)

tabla.C.Co <- subset(tabla.CHFU.1, Group == 'Beetles')
tabla.C.Bee <- subset(tabla.CHFU.1, Group == 'Bees')
tabla.C.F <- subset(tabla.CHFU.1, Group == 'Flies')
m.C.Co = lme(num.plantas~ abun, random=~1|Plot, 
             data=tabla.C.Co,
             method="REML")   
summary(m.C.Co) 
tabla.C.CMely <- subset(tabla.C.Co, Family == 'Melyridae')
m.C.melyridae = lme(num.plantas~ abun, random=~1|Plot, 
             data=tabla.C.CMely,
             method="REML")   
summary(m.C.melyridae)



m.C.Bee = lme(num.plantas~ abun, random=~1|Plot, 
             data=tabla.C.Bee,
             method="REML")   
summary(m.C.Bee) # casi sig
plot(m.C.Bee) #buff
m.C.Bee = lme(log(num.plantas+1) ~ abun, random=~1|Plot, 
              data=tabla.C.Bee,
              method="REML")   
summary(m.C.Bee) # casi sig
plot(m.C.Bee) #buff, no se soluciona
tabla.C.Bee$abun #No hay varianza con esas abundancias...

m.C.F = lme(num.plantas~ abun, random=~1|Plot, 
              data=tabla.C.F,
              method="REML")   
summary(m.C.F) 

tabla.C.syr <- subset(tabla.C.F, Family == 'Syrphidae')
m.C.syr = lme(num.plantas~ abun, random=~1|Plot, 
            data=tabla.C.syr,
            method="REML")   
summary(m.C.syr) 


#lema----
library(lme4)
m.LEMA = lme(num.plantas~ abun, random=~1|Plot, 
             data=t.LEMA,
             method="REML")   
summary(m.LEMA)
plot(m.LEMA) #clasioco mal modelo, habria que corregir.
r.squaredGLMM(m.LEMA)

tabla.L.Co <- subset(t.LEMA, Group == 'Beetles')
tabla.L.Bee <- subset(t.LEMA, Group == 'Bees')
tabla.L.F <- subset(t.LEMA, Group == 'Flies')


m.L.Co = lme(num.plantas~ abun, random=~1|Plot,
             data=tabla.L.Co,
             method="REML")   
summary(m.L.Co) 

m.L.Bee = lme(num.plantas~ abun, random=~1|Plot, 
             data=tabla.L.Bee,
             method="REML")   
summary(m.L.Bee)

m.L.F = lme(num.plantas~ abun, random=~1|Plot, 
              data=tabla.L.F,
              method="REML")   
summary(m.L.F) 

#ME----
m.ME = lme(num.plantas~ abun, random=~1|Plot, 
             data=tabla.ME,
             method="REML")   
summary(m.ME)
plot(m.ME) #mal modelo
r.squaredGLMM(m.ME)


tabla.ME.Co <- subset(tabla.ME, Group == 'Beetles')
tabla.ME.Bee <- subset(tabla.ME, Group == 'Bees')
tabla.ME.F <- subset(tabla.ME, Group == 'Flies')
m.ME.Co = lme(num.plantas~ abun, random=~1|Plot, 
           data=tabla.ME.Co,
           method="REML")   
summary(m.ME.Co)

#m.ME.Bee = lme(num.plantas~ abun, random=~1|Plot, 
 #             data=tabla.ME.Bee,
  #            method="REML")   
#summary(m.ME.Bee) #solo 5 datos de bees, no funciona
m.ME.F = lme(num.plantas~ abun, random=~1|Plot, 
              data=tabla.ME.F,
              method="REML")   
summary(m.ME.F) 



#HOMA
m.homa = lme(num.plantas~ abun, random=~1|Plot, 
           data=tabla.HOMA,
           method="REML")   
summary(m.homa)
plot(m.homa) #mal modelo
r.squaredGLMM(m.homa)

tabla.HOMA.Co <- subset(tabla.HOMA, Group == 'Beetles')
m.homa.co = lme(num.plantas~ abun, random=~1|Plot, 
             data=tabla.HOMA.Co,
             method="REML")   
summary(m.homa.co)


