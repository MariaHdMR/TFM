library(tidyverse)
library(bipartite)
library(igraph)
library(reshape2)
library(dplyr)
library(lme4)
library(DHARMa)
library(GGally)#reescalar variables
library(MuMIn) #dredge function 
#install.packages("openxlsx")
library(openxlsx)
library(usdm)
#install.packages("lavaan")
library(lavaan)
#cargar las bases de datos
FV <- read.table("data/Metadata_Pollinators_2019_2016.csv", header=T, sep=";")
competencia <- read.table("data/simplex_competencia_SEEDS_2019_new.csv", header=T, sep=";")
total <- read.table("C:/Users/Cisco/Documents/TFM/data/FV_2019_FINAL_visits_abundances_seeds_copia.csv", header=T, sep=";")
c.l.r <- read.table("C:/Users/Cisco/Documents/TFM/focal_neighbours_chfu_lema_RAPE.csv", header=T, sep=";")
vecinos <- read.table("C:/Users/Cisco/Documents/TFM/data/focal_neighbours.csv", header=T, sep=";")


#preparar y limpiar datos
#FV <- read.table("data/Metadata_Pollinators_2019_2016.csv", header=T, sep=";")
FV_19 <- subset(FV, Year == 2019) 
#competencia <- read.table("data/simplex_competencia_SEEDS_2019_new.csv", header=T, sep=";")
Abun_19 <-read.table("data/Abun_19.csv", header=T, sep=";")
Abun_19$Plot <- Abun_19$plot
Abun_19$Subplot <- Abun_19$subplot
Abun_19$Plant_Simple <- Abun_19$Sp.Focal
ab.19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Plantas))
suma.sp.plantas <- ab.19 %>% group_by(Plant_Simple) %>% summarise (plantas = sum(num.plantas))
suma <- subset(suma.sp.plantas, Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
FV_19$Plot<- as.factor(FV_19$Plot)
ab.19$Plot<- as.factor(ab.19$Plot)
FINAL <- dplyr::full_join(FV_19, ab.19) #polinizadores+abun plantas
FINAL$Plot <- as.numeric(FINAL$Plot)
competencia$Plot <- as.numeric(competencia$Plot)
competencia1 <- competencia[,c("Plot","Subplot","Plant_Simple","Fruit","Seed")]
FINAL.seeds <- dplyr::full_join(FINAL, competencia, by= c("Plot", "Subplot", "Plant_Simple"))
#del FINAL ahora tengo que seleccionar los grupos de especies que me interesan, son 9: 
#   butterflies, flower_beetles, house_flies, Humbleflies, small_beetles, small_flies, social_bees, solitary_bees, hoverflies
G.funcinales <-subset(FINAL.seeds,G_F %in% c("Butterflies","Flower_beetles","House_flies","Humbleflies", "Small_beetles","Small_flies",
                                      "Bees" ,"Hoverflies"))
a <-G.funcinales[,c("Plot","Subplot","Plant_Simple","G_F","Visits","num.plantas", "Fruit","Seed")] #selecciono las columnas 
a2 <- subset(a, Plot != "OUT") #mas limpieza
head(a2)
a3 <-subset(a2,Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
semi.total <- a3 #base de datos final, pero sin convertir las abundancias 0 de plantas en 1
#write.table(semi.total,file = "C:/Users/Cisco/Documents/TFM/results/dataFV_2019_22.csv",sep=";",row.names = F)
#lo he descargado para cambiar las abundancias de 0 a 1 de las plantas, y multiplicar las semillas por el num
#   de plantas
#total <- read.table("C:/Users/Cisco/Documents/TFM/data/FV_2019_FINAL_visits_abundances_seeds_copia.csv", header=T, sep=";")
s<- subset(total,Plant_Simple %in% c("LEMA", "CHFU", "PUPA"))
#para ver como se distribuyen los polinizadores con las plantas
ntw <- dcast(total, Plant_Simple ~ G_F, fun.aggregate = sum, value.var = "Visits") 

#Analisis visulaes
visitas_plantas <- ggplot(total, aes(x = Visits, y = log(Seed_t)))+
    geom_point()+
    geom_smooth(method = "lm")+
    xlab("Numero de visitas de polinizadores")+
    ylab("Numero de visitas de semillas de plantas")+
    ggtitle ("relacion entre el numero de abundancias de semillas y la abun de visitantes")+
    NULL
visitas_plantas
#x_scale1 <- scale_x_continuous(limits = c(0,500))
y_scale1 <- scale_y_continuous(limits = c(4,7))
visitors <- ggplot(total, aes(x = Visits, y = log(Seed_t), group = G_F))+
    geom_point(aes(color = G_F))+
    geom_smooth(method = "lm", aes(color = G_F))+
    ggtitle("num de visitors segun las abundancias de semillas 2019")+
   # x_scale1+
    y_scale1+
    NULL
visitors #con semillas
y_scale3 <- scale_y_continuous(limits = c(0,80))
#x_scale3 <- scale_x_continuous(limits = c(0,80))
visitors1 <- ggplot(total, aes(x = Visits, y = num.plantas, group = G_F))+
    geom_point(aes(color = G_F))+
    geom_smooth(method = "lm", aes(color = G_F))+
    #x_scale3+
    y_scale3+
    ggtitle("num de visitors segun las abundancias de plantas 2019")+
    NULL
visitors1#salen mucho mas acusadas las rectas con la abundancia de plantas
#x_scale <- scale_x_continuous(limits = c(0,500))
total.see.visitas <- ggplot(total, aes(x = Visits, y = log(Seed_t), group = G_F))+
    geom_point(aes(color = G_F))+
    geom_smooth(method = "lm", aes(color = G_F))+
    facet_wrap(Plant_Simple~G_F)+
    ylab("abundancia seed")+
    xlab("num visits")+
    #x_scale+
    ggtitle("Relacion de la abundancia de seeds y numero de visitors que recibe 2019")+
    NULL
total.see.visitas
prueba2 <-ggplot(total, aes(x = num.plantas, y = Visits, group = G_F))+
    geom_point(aes(color = G_F))+
    geom_smooth(method = "lm", aes(color = G_F))+
    facet_wrap(Plant_Simple~G_F)+
    ylab("num visits")+
    xlab("abundancia seed")+
     ggtitle("Relacion de la abundancia de plantas y numero de visitors que recibe 2019")+
    NULL
prueba2
#voy a mirarlo ahora por sp.focal de plantas, eligiendo los grupos que me interesen
t.chfu <- subset(total, Plant_Simple =="CHFU")
g.chfu.1 <- ggplot(t.chfu, aes(x = Visits, y = log(Seed_t), group = G_F))+
    geom_point(aes(color = G_F))+
    geom_smooth(method = "lm", aes(color = G_F))+
    #x_scale3+
   # y_scale3+
    ggtitle("numero de semillas en funcion del numero de visitas 2019:CHFU")+
    NULL
g.chfu.1
t.lema <- subset(total, Plant_Simple =="LEMA")
#vt.lema$G_F == "Hoverflies"<- "Small_Flies"
#y_scale4 <- scale_y_continuous(limits = c(0, 600))

g.lema <- ggplot(t.lema, aes(x = Visits, y = log(Seed), group = G_F))+
    geom_point(aes(color = G_F))+
    geom_smooth(method = "lm", aes(color = G_F))+
    #x_scale3+
  #  y_scale4+
    ggtitle("numero de semillas en funcion del numero de visitas 2019:LEMA")+
    NULL
g.lema #aqui seria interesante agrupar hoverflies y small_flies. no tiene sentido, pero me salen resultados diferentes con 
#           logaritmo que con los datos reales.

t.pupa <- subset(total, Plant_Simple =="PUPA")
#y_scale5 <- scale_y_continuous(limits = c(0, 750))
#x_scale5 <- scale_x_continuous(limits = c(1, 3))
g.pupa <- ggplot(t.pupa, aes(x = Visits, y = log(Seed_t), group = G_F))+
    geom_point(aes(color = G_F))+
    geom_smooth(method = "lm", aes(color = G_F))+
  #  x_scale5+
   #y_scale5+
    ggtitle("numero de semillas en funcion del numero de visitas 2019:PUPA")+
    NULL
g.pupa
#voy a mirar como se distribuyen las plantas a lo largo de los datos
#fenologia----
plantas.feno <-subset(FINAL,Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
abundancee <- read.table("data/abundances.csv", header=T, sep= ";")
V.19.2 <- FINAL[which(!is.na(FINAL$Month)),] 

V.19.2$date <- paste(V.19.2$Year,"-",V.19.2$Month,"-",V.19.2$Day,sep="")
V.19.2$week <- strftime(V.19.2$date,format = "%V")
V.19.2$week <- strftime(V.19.2$date,format = "%V")
phenology.color. <- ggplot(V.19.2, aes(x= week, y = Plant_Simple))+
    geom_point(aes(color = week))+
    ggtitle ("Phenology 2019")+
    xlab ("weeks")+
    ylab ("spp_Plants")+
    NULL
phenology.color. # segun esto tenemos dos grupos: CHFU,LEMA,ME; y PUPA (si eso con LEMA y CHMI, con las que coincide 1 semana DE 4 )

v.1<- abundancee[which(!is.na(abundancee$month)),] 
v.2 <- subset(abundancee, year== 2019)
v.2$date <- paste(v.2$year,"-",v.2$month,"-",v.2$day,sep="")
v.2$week <- strftime(v.2$date,format = "%V")
v.2$week <- strftime(v.2$date,format = "%V")
phenology.color1 <- ggplot(v.2, aes(x= week, y = species))+
  geom_point(aes(color = week))+
  ggtitle ("Phenology 2019")+
  xlab ("weeks")+
  ylab ("spp_Plants")+
  NULL
phenology.color1


#vecinos----
#utilizo el scrit de David para sacar las primeras especies que coinciden: LEMA, RAPE, CHFU
#c.l.r <- read.table("C:/Users/Cisco/Documents/TFM/focal_neighbours_chfu_lema_RAPE.csv", header=T, sep=";")
c1 <- subset(c.l.r, year == 2019) 
c1.sinedge <- subset(c1,edge %in% c("FALSE"))

c1.sinedge$Plant_Simple<- c1.sinedge$focal 
c1.sinedge$Subplot <- c1.sinedge$subplot
c1.sinedge$Plot <- c1.sinedge$plot
c2 <- dplyr::left_join(total, c1.sinedge, by= c("Plot", "Subplot", "Plant_Simple"))
c2.clean <- subset(c2, Plant_Simple %in% c("LEMA", "CHFU"))

c2.7.5 <- subset(c2.clean, distance %in% c("d1")) 
c2.7.5$distance7.5 <- c2.7.5$distance
c2.7.5$neigh_intra.7.5 <- c2.7.5$neigh_intra
c2.7.5$neigh_inter.7.5<- c2.7.5$neigh_inter

c2.1m <- subset(c2.clean, distance %in% c("d2")) 
c2.1m$distances.1m <- c2.1m$distance
c2.1m$neigh_intra.1m <- c2.1m$neigh_intra
c2.1m$neigh_inter.1m<- c2.1m$neigh_inter
c2.3m <- subset(c2.clean, distance %in% c("d3")) 
c2.3m$distances.3m <- c2.3m$distance
c2.3m$neigh_intra.3m <- c2.3m$neigh_intra
c2.3m$neigh_inter.3m<- c2.3m$neigh_inter
c2.plot <- subset(c2.clean, distance %in% c("d4")) 
c2.plot$distances.plot <- c2.plot$distance
c2.plot$neigh_intra.plot <- c2.plot$neigh_intra
c2.plot$neigh_inter.plot <- c2.plot$neigh_inter

c2.1 <- dplyr::full_join(c2.7.5, c2.1m, by= c("Plot", "Seed_t","num.plantas", "Subplot", "Visits", "Plant_Simple", "edge","focal" ,"Seed", "Fruit",  "G_F"))
c2.2 <- dplyr::full_join(c2.3m, c2.plot, by= c("Plot", "Seed_t","num.plantas", "Subplot", "Visits", "Plant_Simple", "edge","focal" ,"Seed", "Fruit",  "G_F"))

c2.total <- dplyr::full_join(c2.1, c2.2, by= c("Plot", "Seed_t","num.plantas", "Subplot", "Visits", "Plant_Simple", "edge","focal" ,"Seed", "Fruit",  "G_F"))

head(c2.total)

vecinos.chfu.lema <- c2.total[,c("Plot","Subplot","Plant_Simple","G_F","Visits","num.plantas", "Fruit","Seed_t", "neigh_inter.plot","neigh_intra.plot",
                          "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")]


#utilizo la base de datos creada por David llamada "focal_neighbours". De esta base de datos total sacaré las abundancias de 
#     PUPA, al ser la especie que fenologicamente aparece la última se ve afectada por todo el resto. 
#vecinos <- read.table("C:/Users/Cisco/Documents/TFM/data/focal_neighbours.csv", header=T, sep=";")
sp.vecinos <- subset(vecinos,focal %in% c("PUPA"))
sp.vecinos.19 <- subset(sp.vecinos, year == 2019) 
sp.vecinos.19.sinedge <- subset(sp.vecinos.19,edge %in% c("FALSE"))
sp.vecinos.19.sinedge$Plant_Simple<- sp.vecinos.19.sinedge$focal 
sp.vecinos.19.sinedge$Subplot <- sp.vecinos.19.sinedge$subplot
sp.vecinos.19.sinedge$Plot <- sp.vecinos.19.sinedge$plot
total.vecinos1 <- dplyr::left_join(total, sp.vecinos.19.sinedge, by= c("Plot", "Subplot", "Plant_Simple"))
total.clean.1 <- subset(total.vecinos1, Plant_Simple %in% c("PUPA"))

vecinos.7.5.1 <- subset(total.clean.1, distance %in% c("d1")) 
vecinos.7.5.1$distance7.5 <- vecinos.7.5.1$distance
vecinos.7.5.1$neigh_intra.7.5 <- vecinos.7.5.1$neigh_intra
vecinos.7.5.1$neigh_inter.7.5<- vecinos.7.5.1$neigh_inter

vecinos.1m.1 <- subset(total.clean.1, distance %in% c("d2")) 
vecinos.1m.1$distances.1m <- vecinos.1m.1$distance
vecinos.1m.1$neigh_intra.1m <- vecinos.1m.1$neigh_intra
vecinos.1m.1$neigh_inter.1m<- vecinos.1m.1$neigh_inter
vecinos.3m.1 <- subset(total.clean.1, distance %in% c("d3")) 
vecinos.3m.1$distances.3m <- vecinos.3m.1$distance
vecinos.3m.1$neigh_intra.3m <- vecinos.3m.1$neigh_intra
vecinos.3m.1$neigh_inter.3m<- vecinos.3m.1$neigh_inter
vecinos.plot.1 <- subset(total.clean.1, distance %in% c("d4")) 
vecinos.plot.1$distances.plot <- vecinos.plot.1$distance
vecinos.plot.1$neigh_intra.plot <- vecinos.plot.1$neigh_intra
vecinos.plot.1$neigh_inter.plot <- vecinos.plot.1$neigh_inter

v.1.1 <- dplyr::full_join(vecinos.7.5.1, vecinos.1m.1, by= c("Plot", "Seed_t","num.plantas", "Subplot", "Visits", "Plant_Simple", "edge","focal" ,"Seed", "Fruit",  "G_F"))
v.2.1 <- dplyr::full_join(vecinos.3m.1, vecinos.plot.1, by= c("Plot", "Seed_t","num.plantas", "Subplot", "Visits", "Plant_Simple", "edge","focal" ,"Seed", "Fruit",  "G_F"))

v.t <- dplyr::full_join(v.1.1, v.2.1, by= c("Plot", "Seed_t","num.plantas", "Subplot", "Visits", "Plant_Simple", "edge","focal" ,"Seed", "Fruit",  "G_F"))
# en estos datos solo tengo 2 plots, que son en los que aparece PUPA. 

vecinos.pupa <- v.t[,c("Plot","Subplot","Plant_Simple","G_F","Visits","num.plantas", "Fruit","Seed_t", "neigh_inter.plot","neigh_intra.plot",
      "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")]

#MODELOS----
#seeds
#modelo pupa
p1 <-glmer((vecinos.pupa$Seed_t) ~  vecinos.pupa$Visits  + vecinos.pupa$neigh_inter.plot + vecinos.pupa$neigh_intra.plot + 
             vecinos.pupa$neigh_intra.3m + vecinos.pupa$neigh_inter.3m + vecinos.pupa$neigh_inter.1m+ 
             vecinos.pupa$neigh_intra.1m+ vecinos.pupa$neigh_inter.7.5+ vecinos.pupa$neigh_intra.7.5 + (1|Subplot:Plot)+ (1|Plot) , family="poisson",
        data=vecinos.pupa, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(p1)#he probado a hacer el logaritmo de las semillas, me da error porque aparecen valores "non integer"

isSingular(p1, tol = 1e-05)#para ver la singularidad. varianzas. me lo recomienda
rePCA(p1)
r.squaredGLMM(p1)#r2 muy alto. 
simulationOutput.2 <- simulateResiduals(fittedModel = p1, n = 250)
plot(simulationOutput.2)
otro1 <-glmer.nb((vecinos.pupa$Seed) ~  vecinos.pupa$Visits  + vecinos.pupa$neigh_inter.plot + vecinos.pupa$neigh_intra.plot + 
                   vecinos.pupa$neigh_intra.3m + vecinos.pupa$neigh_inter.3m + vecinos.pupa$neigh_inter.1m+ 
                   vecinos.pupa$neigh_intra.1m+ vecinos.pupa$neigh_inter.7.5+ vecinos.pupa$neigh_intra.7.5 + (1|Subplot:Plot)+ (1|Plot) , family="poisson",
         data=vecinos.pupa)
summary(otro1)#no works
simulationOutput.otro <- simulateResiduals(fittedModel = otro, n = 250)

plot(simulationOutput.otro)

options(na.action =  "na.fail")
D3<-dredge(p1)
#write.xlsx(D3, file= "dredge_PUPA.1.csv") #segun esto el mejor modelo por AIC seria el que 
#   tuviese las siguientes variables: vecinos inter e intra 3m, vecinos inter 7.5 cm, vecinos
#   intra 1m  
options(na.action =  "na.omit")

c <-glmer((vecinos.chfu.lema$Seed_t) ~  vecinos.chfu.lema$Visits  + vecinos.chfu.lema$neigh_inter.plot + vecinos.chfu.lema$neigh_intra.plot + 
         + vecinos.chfu.lema$neigh_inter.1m+ vecinos.chfu.lema$neigh_intra.1m+ vecinos.chfu.lema$neigh_inter.7.5+ 
           vecinos.chfu.lema$neigh_intra.7.5 + (1|Subplot:Plot)+ (1|Plot) , family="poisson",
      data=vecinos.chfu.lema, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(c) #este segun lo anterior seria el mejor modelo.
#modelo CHFU+LEMA
c<-glmer((vecinos.chfu.lema$Seed_t) ~  vecinos.chfu.lema$Visits  + vecinos.chfu.lema$neigh_inter.plot + vecinos.chfu.lema$neigh_intra.plot + 
           vecinos.chfu.lema$neigh_intra.3m + vecinos.chfu.lema$neigh_inter.3m + vecinos.chfu.lema$neigh_inter.1m+ 
           vecinos.chfu.lema$neigh_intra.1m+ vecinos.chfu.lema$neigh_inter.7.5+ vecinos.chfu.lema$neigh_intra.7.5 + (1|Subplot:Plot)+ (1|Plot) , family="poisson",
           data=vecinos.chfu.lema, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(c)
simulationOutput.1 <- simulateResiduals(fittedModel = c, n = 250)
plot(simulationOutput.1) #los residuos no se adaptan bien ojo!


#voy a probar por cada sp.focal por separado 
vecinos.chfu <- subset(vecinos.chfu.lema, Plant_Simple == "CHFU")
chufu <-glmer((vecinos.chfu$Seed_t) ~  vecinos.chfu$Visits  + vecinos.chfu$neigh_inter.plot + 
                vecinos.chfu$neigh_intra.plot + vecinos.chfu$neigh_intra.3m + vecinos.chfu$neigh_inter.3m +
                vecinos.chfu$neigh_inter.1m+ vecinos.chfu$neigh_intra.1m+ vecinos.chfu$neigh_inter.7.5+ 
                vecinos.chfu$neigh_intra.7.5 + (1|Subplot:Plot)+ (1|Plot) ,   family="poisson",
          data=vecinos.chfu, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(chufu)
simulationOutput.chufu <- simulateResiduals(fittedModel = chufu, n = 250)
plot(simulationOutput.chufu)
r.squaredGLMM(chufu)#r2 alta, no cuadra con la distribucion de los vecinos

options(na.action =  "na.fail")
D<-dredge(chufu)
#write.xlsx(D, file= "dredge_CHFU.csv") #segun esto el mejor modelo por AIC seria el que 
#   tuviese las siguientes variables: vecinos inter e intra 1m2, vecinos inter 7.5 cm, vecinos intra plot
options(na.action =  "na.omit")

vecinos.lema <- subset(vecinos.chfu.lema, Plant_Simple == "LEMA")
lema <-glmer((vecinos.lema$Seed) ~  vecinos.lema$Visits  + vecinos.lema$neigh_inter.plot + vecinos.lema$neigh_intra.plot + 
                vecinos.lema$neigh_intra.3m + vecinos.lema$neigh_inter.3m + vecinos.lema$neigh_inter.1m+ 
                vecinos.lema$neigh_intra.1m+ vecinos.lema$neigh_inter.7.5+ vecinos.lema$neigh_intra.7.5 + (1|Subplot:Plot)+ (1|Plot) , 
              family="poisson", data=vecinos.lema, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(lema)
simulationOutput.lema <- simulateResiduals(fittedModel = lema, n = 250)
plot(simulationOutput.lema)
r.squaredGLMM(lema)#R2 alta, no cuadra con la distribucion de los residuos

options(na.action =  "na.fail")
D8<-dredge(lema)
#write.xlsx(D8, file= "dredge_lema.csv") #segun esto el mejor modelo por AIC seria el que 
#   tuviese las siguientes variables: vecinos intra 1m y si eso, + inter plot
options(na.action =  "na.omit")


#Intento de reescalar las variables, no funciona
#prueba.rescale<-glmer(c1 ~  c2  + c3 + c4 + 
#                        c5 + c6+ c7+c8+c9+c10 +(1|Subplot:Plot)+ (1|Plot) , family="poisson", data=vecinos.pupa,
 #                     glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
#summary((prueba.rescale))
#c1 <- rescale01(vecinos.pupa$Seed)
#c2 <-rescale01(vecinos.pupa$Visits)
#c3<- rescale01(vecinos.pupa$neigh_inter.plot)
#c4 <-rescale01(vecinos.pupa$neigh_intra.plot)
#c5 <-rescale01(vecinos.pupa$neigh_intra.3m)
#c6 <-rescale01(vecinos.pupa$neigh_inter.3m)
#c7 <- rescale01(vecinos.pupa$neigh_inter.1m)
#c8 <- rescale01(vecinos.pupa$neigh_intra.1m)
#c9 <- rescale01(vecinos.pupa$neigh_inter.7.5)
#c10 <- rescale01(vecinos.pupa$neigh_intra.7.5)

#ahora los mismos análisis pero con VISITAS. 
#visitas ----
v.pupa<-glmer((vecinos.pupa$Visits) ~ vecinos.pupa$neigh_inter.plot + vecinos.pupa$neigh_intra.plot + 
             vecinos.pupa$neigh_intra.3m + vecinos.pupa$neigh_inter.3m + vecinos.pupa$neigh_inter.1m+ 
             vecinos.pupa$neigh_intra.1m+ vecinos.pupa$neigh_inter.7.5+ vecinos.pupa$neigh_intra.7.5 + (1|Subplot:Plot)+ (1|Plot) , family="poisson",
           data=vecinos.pupa, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(v.pupa)
r.squaredGLMM(v.pupa)#r2 muy bajas
isSingular(v.pupa, tol = 1e-05)
SimulationOutput.v.pupa <- simulateResiduals(fittedModel = v.pupa, n = 250)
plot(SimulationOutput.v.pupa)
v.pupa.log<-glmer(log(vecinos.pupa$Visits) ~ vecinos.pupa$neigh_inter.plot + vecinos.pupa$neigh_intra.plot + 
                vecinos.pupa$neigh_intra.3m + vecinos.pupa$neigh_inter.3m + vecinos.pupa$neigh_inter.1m+ 
                vecinos.pupa$neigh_intra.1m+ vecinos.pupa$neigh_inter.7.5+ vecinos.pupa$neigh_intra.7.5 + (1|Subplot:Plot)+ (1|Plot) , family="poisson",
              data=vecinos.pupa, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(v.pupa.log)#no es capaz de calcularme el AIC, aparece inf
r.squaredGLMM(v.pupa.log)#aumenta el r2 al hacer el logaritmo de las visitas, pero el modelo no parece estar bien

options(na.action =  "na.fail")
v1<-dredge(v.pupa)
#write.xlsx(v1, file= "dredge_visitas_pupa.csv") #segun esto el mejor modelo por AIC seria el que 
#   tuviese las siguientes variables: vecinos inter 1m.
options(na.action =  "na.omit")


v.lema<-glmer((vecinos.lema$Visits) ~ vecinos.lema$neigh_inter.plot + vecinos.lema$neigh_intra.plot + 
                vecinos.lema$neigh_intra.3m + vecinos.lema$neigh_inter.3m + vecinos.lema$neigh_inter.1m+ 
                vecinos.lema$neigh_intra.1m+ vecinos.lema$neigh_inter.7.5+ vecinos.lema$neigh_intra.7.5 + (1|Subplot:Plot)+ (1|Plot) , family="poisson",
              data=vecinos.lema, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(v.lema)
r.squaredGLMM(v.lema)#super bajo el r2, apenas explica nada el modelo
SimulationOutput.v.lema <- simulateResiduals(fittedModel = v.lema, n = 250)
plot(SimulationOutput.v.lema)# según esto parece que se ajustaria mas a una normal, pero al cambiar la familia en el glmer me da 
# problemas, lo mismo que si hago un lmer. 

options(na.action =  "na.fail")
v2<-dredge(v.lema)
write.xlsx(v1, file= "dredge_visitas_lema.csv") #segun esto el mejor modelo por AIC seria el que 
#   tuviese las siguientes variables: vecinos inter 1m.y si eso + inter 3m
options(na.action =  "na.omit")


v.chfu<-glmer((vecinos.chfu$Visits) ~ vecinos.chfu$neigh_inter.plot + vecinos.chfu$neigh_intra.plot + 
                vecinos.chfu$neigh_intra.3m + vecinos.chfu$neigh_inter.3m + vecinos.chfu$neigh_inter.1m+ 
                vecinos.chfu$neigh_intra.1m+ vecinos.chfu$neigh_inter.7.5+ vecinos.chfu$neigh_intra.7.5 + (1|Subplot:Plot)+ (1|Plot) , family="poisson",
              data=vecinos.chfu, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(v.chfu)
r.squaredGLMM(v.chfu)# muy mal modelo
SimulationOutput.v.chfu <- simulateResiduals(fittedModel = v.chfu, n = 250)
plot(SimulationOutput.v.chfu)

options(na.action =  "na.fail")
v3<-dredge(v.chfu)
#write.xlsx(v1, file= "dredge_visitas_pupa.csv") #segun esto el mejor modelo por AIC seria el que 
#   tuviese las siguientes variables: vecinos inter 1m.y si eso + inter 3m
options(na.action =  "na.omit")

#voy a probar con lema+chfu juntas, a ver si consigo una r2 mejor, por la adecuacuon del modelo, pero no. 
v.chfu.lema.pupa<-glmer((vecinos.chfu.lema$Visits) ~ vecinos.chfu.lema$neigh_inter.plot + vecinos.chfu.lema$neigh_intra.plot + 
                          vecinos.chfu.lema$neigh_intra.3m + vecinos.chfu.lema$neigh_inter.3m + vecinos.chfu.lema$neigh_inter.1m+ 
                          vecinos.chfu.lema$neigh_intra.1m+ vecinos.chfu.lema$neigh_inter.7.5+ vecinos.chfu.lema$neigh_intra.7.5 + (1|Subplot:Plot)+ (1|Plot) , family="poisson",
              data=vecinos.chfu.lema, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(v.chfu.lema.pupa)
r.squaredGLMM(v.chfu.lema.pupa)#nada, salen unos r2 super bajos. 

#ahora con las variables seleccionadas de los anteriores modelos tengo que crear los path analyses 
#path analysis----

#model <- '
#equation where Fitness is predicted for visits and Small Neigh
#Fitness~visits-smallneigh
#equation where visits is predicted for big neigh - small neigh
#visits~bigneigh-smallneigh
#estimating the variances
#smallneigh~~smallneigh
#bigneigh~~bigneigh
#estimating the residuals variances
#visits~~visits
#fitness~~fitness'
#data <- #aqui meto mi data frame
#fit <- lavaan(model, data= )
#summary

#SEM para lema
#diferentes variables de LEMA: el fitness de lema depende de los vecinos que estan 1m intra (si eso de los vecinos inter plot),
# y las visitas dependen de 1 m inter 
modelo <- "Seed_t ~ Visits + neigh_intra.7.5+neigh_inter.7.5+neigh_intra.1m+neigh_inter.1m
          N.big =~neigh_inter.plot+neigh_intra.plot+neigh_intra.3m+neigh_inter.3m
          N.small=~neigh_intra.1m+neigh_inter.1m+neigh_inter.7.5+neigh_intra.7.5
          Visits ~ neigh_inter.plot+neigh_intra.plot+neigh_intra.3m+neigh_inter.3m+neigh_intra.1m+neigh_inter.1m+neigh_inter.7.5+neigh_intra.7.5
          N.small~~N.big"
#library(lavaan)
fit <- cfa(model =modelo,data= vecinos.lema,check.gradient = FALSE,std.lv=TRUE, orthogonal=TRUE) #me salen mogollon de warnings       
varTable(fit)
summary(fit,fit.measures = TRUE, standardized = TRUE)
modindices(fit, sort = TRUE, maximum.number = 10)
#multicolinearidad

d1<- vecinos.lema[,5:16]
vif(d1)#si el numero es mayor de 10 problemas de colinearidad: tengo bastante colinearidad: Seed_t, inter plot, intra plot, intra 3m
#                       inter 3m.
modelo.l.2 <- "Seed_t ~ Visits + neigh_intra.1m+neigh_inter.1m+neigh_inter.plot
          N.big =~neigh_inter.plot+neigh_intra.plot+neigh_intra.3m+neigh_inter.3m
          N.small=~neigh_intra.1m+neigh_inter.1m+neigh_inter.7.5+neigh_intra.7.5
          Visits ~ neigh_inter.plot+neigh_intra.plot+neigh_intra.1m+neigh_inter.1m
          N.small~~N.big"
fit2 <- cfa(model =modelo.l.2,data= vecinos.lema,check.gradient = FALSE,std.lv=TRUE, orthogonal=TRUE) #me salen mogollon de warnings       
varTable(fit2)
summary(fit2,fit.measures = TRUE, standardized = TRUE)#mejor pero no suficiente
modelo.l.3 <- "Seed_t ~ Visits + neigh_intra.1m+neigh_inter.plot
          N.big =~neigh_inter.plot+neigh_intra.plot+neigh_intra.3m+neigh_inter.3m
          N.small=~neigh_intra.1m+neigh_inter.1m+neigh_inter.7.5+neigh_intra.7.5
          Visits ~ neigh_inter.1m+neigh_inter.3m
          N.small~~N.big"
fit3 <- cfa(model =modelo.l.3,data= vecinos.lema,check.gradient = FALSE,std.lv=TRUE, orthogonal=TRUE) #me salen mogollon de warnings       
varTable(fit3)
summary(fit3,fit.measures = TRUE, standardized = TRUE)
modelo.l.4 <- "Seed_t ~ Visits + neigh_intra.1m+neigh_inter.plot
          N.big =~neigh_inter.plot+neigh_inter.3m
          N.small=~neigh_intra.1m+neigh_inter.1m
          Visits ~ neigh_inter.1m+neigh_inter.3m
          N.small~~N.big"
fit4 <- cfa(model =modelo.l.4,data= vecinos.lema,check.gradient = FALSE,std.lv=TRUE, orthogonal=TRUE) #me salen mogollon de warnings       
varTable(fit4)
summary(fit4,fit.measures = TRUE, standardized = TRUE)#sale mejor el AIC, pero el CFI y resto de indices salen peores (mas bajos de 0.95)
