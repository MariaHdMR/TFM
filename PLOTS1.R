#estos son ya los datos reales 
library(tidyverse)
FV_16 <- subset(FV, Year == 2016) 
FV_19 <- subset(FV, Year == 2019)
abun_16 <- subset (abun_16, year ==2016)
#ver las spp de plantas frente el num de visitas
testB <- FV %>% group_by(Plant_Simple, Group_Floralvisitor) %>% summarise(num.visits = sum(Visits))
pollinator.plot.2016.2019 <- ggplot(testB) + 
    geom_boxplot(aes(x = Plant_Simple, y = num.visits)) +
    ggtitle("Number of visits per Plant")
    # facet_grid(plant~.)+
    NULL
pollinator.plot.2016.2019
pollinator.plot2016 <- ggplot(tabla.completa.16) + 
  geom_boxplot(aes(x = Plant, y = num.visits)) +
  ggtitle("Number of visits per Plant")
# facet_grid(plant~.)+
NULL
pollinator.plot2016
pollinator.plot2019 <- ggplot(tabla.completa.19) + 
  geom_boxplot(aes(x = Plant, y = num.visits)) +
  ggtitle("Number of visits per Plant")
# facet_grid(plant~.)+
NULL
pollinator.plot2019

#Num de pol por grupo y  por año----
#Num de pol en cada grupo por año, IGUALAR LAS ESCALAS
test16 <- FV_16 %>% group_by(Plot, Group_Floralvisitor) %>% summarise (num.pol = n())
boxplot(testC$num.pol~testC$Group_Floralvisitor,xlab= 'Guild', ylab= 'num.pol', main= 'Num de pol en los grupos 16' )
par(mfrow=c(2,1))
test19 <- FV_19 %>% group_by(Plot, Group_Floralvisitor) %>% summarise (num.pol = n())
boxplot(testD$num.pol~testD$Group_Floralvisitor,xlab= 'Guild', ylab= 'num.pol', main= 'Num de pol en los grupos 19' )

#Num pol por plots y año----
# enfrentar los numeros de polinizadores que hay en los plots, IGUALAR LAS ESCALAS
boxplot(testC$num.pol~testC$Plot,xlab='Plots', ylab= 'num.pol', main= 'Num de pol en los plots 16')
boxplot(testD$num.pol~testD$Plot,xlab='Plots', ylab= 'num.pol', main= 'Num de pol en los plots 19')

#tablas completas polinizadores+abundancias----
ab.16 <- abun_16 %>% group_by (Plant, plot, subplot) %>% summarise(num.indv = sum (individuals))

ab.16$Plot <- ab.16$plot
ab.16$Subplot <- ab.16$subplot

ab.19 <- Abun_19 %>% group_by (Sp.Focal, plot, subplot) %>% summarise(num.indv = sum (Plantas))
ab.19$Plot <- ab.19$plot
ab.19$Subplot <- ab.19$subplot
ab.19$Plant<- ab.19$Sp.Focal

test.order.visits.16 <- FV_16 %>% group_by(Plot, Subplot, Visits, Plant, Group_Floralvisitor) %>% summarise(num.visits= sum(Visits))
test.order.visits.19 <- FV_19 %>% group_by(Plot, Subplot, Visits, Plant, Group_Floralvisitor) %>% summarise(num.visits= sum(Visits))
tabla.completa.16 <- dplyr::left_join (test.order.visits.16, ab.16)
tabla.completa.19 <- dplyr::left_join(test.order.visits.19, ab.19 )

tabla.completa.19$num.visits

#relacion entre el numero de abundancias de plantas y el numero de visitas que reciben por años----
visitas.total.16 <- ggplot(tabla.completa.16, aes(x = num.indv, y = num.visits))+
    geom_point()+
    geom_smooth(method = "lm")+
    xlab("Abundancia de plantas")+
    ylab("Numero de visitas de polinizadores")+
    ggtitle ("relacion entre el numero de abundancias de plantas y el numero de visitas que reciben")
    NULL
visitas.total.16
par(mfrow=c(2,1)) # no me hace caso al separar la pantalla
visitas.total.19 <- ggplot(tabla.completa.19, aes(x = num.indv, y = num.visits))+
    geom_point()+
    geom_smooth(method = "lm")+
    xlab("Abundancia de plantas")+
    ylab("Numero de visitas de polinizadores")+
    ggtitle ("relacion entre el numero de abundancias de plantas y el numero de visitas que reciben")
NULL
visitas.total.19

#num de visitas por spp; y num de indv por spp----
boxplot(tabla.completa.16$num.visits~tabla.completa.16$Plant, xlab= 'Species', ylab = 'num.visits',main= "Num de visitas por spp 2016")
boxplot(tabla.completa.19$num.visits~tabla.completa.19$Plant, xlab= 'Species', ylab = 'num.visits',main= "Num de visitas por spp 2019")

boxplot(tabla.completa.16$num.indv~tabla.completa.16$Plant, xlab= 'Species', ylab ='num.indv', main= "Num de indv por spp 2016")
boxplot(tabla.completa.19$num.indv~tabla.completa.19$Plant, xlab= 'Species', ylab ='num.indv', main= "Num de indv por spp 2019")



#dividir por grupos de visitantes florales en un mismo plot ----


visitas.16<- ggplot(tabla.completa.16, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
    geom_point(aes(color = Group_Floralvisitor))+
    geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ggtitle("Todas las spp de plantas en funcion de su abundancia y el num.visits que reciben")
    NULL
visitas.16
visitas.19 <- ggplot(tabla.completa.19, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
    geom_point(aes(color = Group_Floralvisitor))+
    geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
    NULL
visitas.19

#con el mismo script anterior he querido hacer un subset con las spp mas comunes y ver que ocurre
datos16 <- subset(tabla.completa.16, Plant %in% c("LEMA","CHFU","HOMA","MESU","PUPA", "CHMI","MEEL") & Group_Floralvisitor %in% c("Coleoptera", "Diptera", "Hemiptera", "Hymenoptera", "Lepidoptera", "Neuroptera"))
datos19 <- subset(tabla.completa.19, Plant %in% c("LEMA","CHFU","HOMA","MESU","PUPA", "CHMI","MEEL") & Group_Floralvisitor %in% c("Coleoptera", "Diptera", "Hemiptera", "Hymenoptera", "Lepidoptera", "Neuroptera"))

visitas.16.incomp <- ggplot(datos16, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  NULL
visitas.16.incomp


visitas.19.incomp <- ggplot(datos19, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  NULL
visitas.19.incomp

#visitas de pol segun la abundancia por spp de Planta ----
tabla.LEMA.16 <- subset(tabla.completa.16, Plant == 'LEMA'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.LEMA.19 <- subset(tabla.completa.19, Plant == 'LEMA'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.CHFU.16 <- subset(tabla.completa.16, Plant == 'CHFU'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.CHFU.19 <- subset(tabla.completa.19, Plant == 'CHFU'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.PUPA.16 <- subset(tabla.completa.16, Plant == 'PUPA'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.PUPA.19 <- subset(tabla.completa.19, Plant == 'PUPA'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.HOMA.16 <- subset(tabla.completa.16, Plant == 'HOMA'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.HOMA.19 <- subset(tabla.completa.19, Plant == 'HOMA'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.MEEL.16 <- subset(tabla.completa.16, Plant == 'MEEL'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.MEEL.19 <- subset(tabla.completa.19, Plant == 'MEEL'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.CHMI.16 <- subset(tabla.completa.16, Plant == 'CHMI'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.CHMI.19 <- subset(tabla.completa.19, Plant == 'CHMI'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.MEPO.16 <- subset(tabla.completa.16, Plant == 'MEPO' & Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.MEPO.19 <- subset(tabla.completa.19, Plant == 'MEPO' & Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.MESU.16 <- subset(tabla.completa.16, Plant == 'MESU' & Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.MESU.19 <- subset(tabla.completa.19, Plant == 'MESU' & Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.RAPE.16 <- subset(tabla.completa.16, Plant == 'RAPE' & Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
tabla.RAPE.19 <- subset(tabla.completa.19, Plant == 'RAPE' & Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))





head(tabla.LEMA.19)
visitas.LEMA.16 <- ggplot(tabla.LEMA.16, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia LEMA")+
  ggtitle("Relacion de la abundancia de LEMA y las visitas que recibe 2016")+
  NULL
visitas.LEMA.16

visitas.LEMA.19 <- ggplot(tabla.LEMA.19, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia LEMA")+
  ggtitle("Relacion de la abundancia de LEMA y las visitas que recibe 2019")+
  NULL
visitas.LEMA.19

visitas.CHFU.16 <- ggplot(tabla.CHFU.16, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia CHFU")+
  ggtitle("Relacion de la abundancia de CHFU y las visitas que recibe 2016")+
  NULL
visitas.CHFU.16

visitas.CHFU.19 <- ggplot(tabla.CHFU.19, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia CHFU")+
  ggtitle("Relacion de la abundancia de CHFU y las visitas que recibe 2019")+
  NULL
visitas.CHFU.19

#CON meel no sale apenas informacion, no lo voy a usar
visitas.MEEL.16 <- ggplot(tabla.MEEL.16, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia MEEL")+
  ggtitle("Relacion de la abundancia de MEEL y las visitas que recibe 2016")+
  NULL
visitas.MEEL.16
visitas.MEEL.19 <- ggplot(tabla.MEEL.19, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia MEEL")+
  ggtitle("Relacion de la abundancia de MEEL y las visitas que recibe 2019")+
  NULL
visitas.MEEL.19



visitas.PUPA.16 <- ggplot(tabla.PUPA.16, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia PUPA")+
  ggtitle("Relacion de la abundancia de PUPA y las visitas que recibe 2016")+
  NULL
visitas.PUPA.16
visitas.PUPA.19 <- ggplot(tabla.PUPA.19, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia PUPA")+
  ggtitle("Relacion de la abundancia de PUPA y las visitas que recibe 2019")+
  NULL
visitas.PUPA.19

#con HOMA tampoco tienen sentido los graficos, apenas hay abundancia y visitantes.
visitas.HOMA.16 <- ggplot(tabla.HOMA.16, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia HOMA")+
  ggtitle("Relacion de la abundancia de HOMA y las visitas que recibe 2016")+
  NULL
visitas.HOMA.16
visitas.HOMA.19 <- ggplot(tabla.HOMA.19, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia HOMA")+
  ggtitle("Relacion de la abundancia de HOMA y las visitas que recibe 2019")+
  NULL
visitas.HOMA.19
#hacerlo con chmi tampoco tiene sentido
visitas.CHMI.16 <- ggplot(tabla.CHMI.16, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia CHMI")+
  ggtitle("Relacion de la abundancia de CHMI y las visitas que recibe 2016")+
  NULL
visitas.CHMI.16
visitas.CHMI.19 <- ggplot(tabla.CHMI.19, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia CHMI")+
  ggtitle("Relacion de la abundancia de CHMI y las visitas que recibe 2019")+
  NULL
visitas.CHMI.19

visitas.MEPO.16 <- ggplot(tabla.MEPO.16, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia MEPO")+
  ggtitle("Relacion de la abundancia de MEPO y las visitas que recibe 2016")+
  NULL
visitas.MEPO.16
visitas.MEPO.19 <- ggplot(tabla.MEPO.19, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia MEPO")+
  ggtitle("Relacion de la abundancia de MEPO y las visitas que recibe 2019")+
  NULL
visitas.MEPO.19

visitas.MESU.16 <- ggplot(tabla.MESU.16, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia MESU")+
  ggtitle ("Relacion de la abundancia de MESU y las visitas que recibe 2016")+
  NULL
visitas.MESU.16

visitas.MESU.19 <- ggplot(tabla.MESU.19, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia MESU")+
  ggtitle ("Relacion de la abundancia de MESU y las visitas que recibe 2019")+
  NULL
visitas.MESU.19
visitas.RAPE.16 <- ggplot(tabla.RAPE.16, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia RAPE")+
  ggtitle ("Relacion de la abundancia de RAPE y las visitas que recibe 2016")+
  NULL
visitas.RAPE.16
visitas.RAPE.19 <- ggplot(tabla.RAPE.19, aes(x = num.indv, y = num.visits, group = Group_Floralvisitor))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  ylab("num.visits polinizadores")+
  xlab("abundancia RAPE")+
  ggtitle ("Relacion de la abundancia de RAPE y las visitas que recibe 2019")+
  NULL
visitas.RAPE.19

#los unicos plots que parece que tienen sentido son: LEMA; ChFU, PUPA y MESU??


# dividir por especie de planta en el mismo plot----
visitas.planta.16 <- ggplot(tabla.completa.16, aes(x = num.indv, y = num.visits, group = Plant))+
    geom_point(aes(color = Plant))+
    geom_smooth(method = "lm", aes(color = Plant))+
    NULL
visitas.planta.16

visitas.planta.19 <- ggplot(tabla.completa.19, aes(x = num.indv, y = num.visits, group = Plant))+
    geom_point(aes(color = Plant))+
    geom_smooth(method = "lm", aes(color = Plant))+
    NULL
visitas.planta.19



#esto es teniendo en cuenta todos los datos de 2016
visitas.planta.grid.16 <- ggplot(tabla.completa.16, aes(x = num.indv, y = num.visits))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  facet_wrap(Plant~Group_Floralvisitor)+
  NULL
visitas.planta.grid.16

#en el siguiente analisis he usado datos16 en el que se hizo un subset de ciertas spp tanto de plantas como de polinizadores

visitas.planta.incompleta16 <- ggplot(datos16, aes(x = num.indv, y = num.visits))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  facet_wrap(Plant~Group_Floralvisitor)+
  NULL
visitas.planta.incompleta16
visitas.planta.incompleta19 <- ggplot(datos19, aes(x = num.indv, y = num.visits))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  facet_wrap(Plant~Group_Floralvisitor)+
  NULL
visitas.planta.incompleta19 #de este plot parece que se saca poco, no quedan las cosas muy claras.






########################## FENOLOGIA ################################----
FV_16 <- FV_16[which(!is.na(FV_16$Month)),] 
FV_19 <- FV_19[which(!is.na(FV_19$Month)),] 
FV_16$date <- paste(FV_16$Year,"-",FV_16$Month,"-",FV_16$Day,sep="")
FV_19$date <- paste(FV_19$Year,"-",FV_19$Month,"-",FV_19$Day,sep="")
FV_16$week <- strftime(FV_16$date,format = "%V")
FV_19$week <- strftime(FV_19$date,format = "%V")

phenology16 <- ggplot(FV_16, aes(x= week, y = Plant_Simple))+
  geom_boxplot()
NULL
phenology16
phenology19 <- ggplot(FV_19, aes(x= week, y = Plant_Simple))+
  geom_boxplot()
NULL
phenology19


phenology.color.16 <- ggplot(FV_16, aes(x= week, y = Plant_Simple))+
  geom_boxplot(aes(color = week))+
  ggtitle ("Phenology 2016")+
  xlab ("weeks")+
  ylab ("spp_Plants")
NULL
phenology.color.16
phenology.color.19 <- ggplot(FV_19, aes(x= week, y = Plant_Simple))+
  geom_boxplot(aes(color = week))+
  ggtitle ("Phenology 2019")+
  xlab ("weeks")+
  ylab ("spp_Plants")
NULL
phenology.color.19
#de estas tengo que sacar las diferentes etapas!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#estan un poco difusas estas etapas
#pero podrian ser: 
#2016 --> 1) SOAS, SCLA, RAPE, CHFU
      # 2) SPRU, ME, LEMA, CHMI, CETE, BEMA
      # 3)  PUPA, LEMA
#2019 ---> 1) RAPE, CHFU
      #   2) ME, LEMA, HOMA, CHFU, CHMI
      #   3) PUPA

fase1.2016 <- subset(tabla.completa.16, Plant %in% c('SOAS', 'SCLA', 'RAPE', 'CHFU'))
fase2.2016 <- subset(tabla.completa.16, Plant %in% c('SPRU', 'ME', 'LEMA', 'CHMI', 'CETE', 'BEMA'))
fase3.2016 <- subset(tabla.completa.16, Plant %in% c('PUPA', 'LEMA'))

fenologia.fase1.2016 <- ggplot(fase1.2016, aes(x = num.indv, y = num.visits))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  facet_wrap(Plant~Group_Floralvisitor)+
  ggtitle('relacion entre spp de plantas de la fase 1 y los polinizadores 2016')+
  xlab('abundancia de las plantas')+
  ylab('numero de visitas de los pol')+
  NULL
fenologia.fase1.2016

fenologia.fase2.2016 <- ggplot(fase2.2016, aes(x = num.indv, y = num.visits))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  facet_wrap(Plant~Group_Floralvisitor)+
  ggtitle('relacion entre spp de plantas de la fase 2 y los polinizadores 2016')+
  xlab('abundancia de las plantas')+
  ylab('numero de visitas de los pol')+
  NULL
fenologia.fase2.2016

fenologia.fase3.2016 <- ggplot(fase3.2016, aes(x = num.indv, y = num.visits))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  facet_wrap(Plant~Group_Floralvisitor)+
  ggtitle('relacion entre spp de plantas de la fase 2 y los polinizadores 2016')+
  xlab('abundancia de las plantas')+
  ylab('numero de visitas de los pol')+
  NULL
fenologia.fase3.2016


fase1.2019 <- subset(tabla.completa.19, Plant %in% c('RAPE','CHFU'))
fase2.2019 <- subset(tabla.completa.19, Plant %in% c('ME', 'LEMA', 'CHMI', 'CHFU', 'HOMA'))
fase3.2019 <- subset(tabla.completa.19, Plant %in% c('PUPA'))

fenologia.fase1.2019 <- ggplot(fase1.2019, aes(x = num.indv, y = num.visits))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  facet_wrap(Plant~Group_Floralvisitor)+
  ggtitle('relacion entre spp de plantas de la fase 1 y los polinizadores 2019')+
  xlab('abundancia de las plantas')+
  ylab('numero de visitas de los pol')+
  NULL
fenologia.fase1.2019


fenologia.fase2.2019 <- ggplot(fase2.2019, aes(x = num.indv, y = num.visits))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  facet_wrap(Plant~Group_Floralvisitor)+
  ggtitle('relacion entre spp de plantas de la fase 2 y los polinizadores 2019')+
  xlab('abundancia de las plantas')+
  ylab('numero de visitas de los pol')+
  NULL
fenologia.fase2.2019

fenologia.fase3.2019 <- ggplot(fase3.2019, aes(x = num.indv, y = num.visits))+
  geom_point(aes(color = Group_Floralvisitor))+
  geom_smooth(method = "lm", aes(color = Group_Floralvisitor))+
  facet_wrap(Plant~Group_Floralvisitor)+
  ggtitle('relacion entre spp de plantas de la fase 3 y los polinizadores 2019')+
  xlab('abundancia de las plantas')+
  ylab('numero de visitas de los pol')+
  NULL
fenologia.fase3.2019




################################### seed set ----
#with competition, I want to get a graphic with the num.seed vs num.visits by group of pollinators
tabla.compet16 <- subset(competition, year == 2016)
head(tabla.compet16)
test.comp <- tabla.compet16 %>% group_by(plot, subplot, focal ) %>% summarise(num.seed = sum(seed))
head(test.comp)
tabla.comp.visits <- dplyr::left_join(test.comp,tabla.completa.16)
tabla.comp.vists.visitors <- subset(tabla.comp.visits, Group_Floralvisitor %in% c("Coleoptera", "Diptera", "Hymenoptera", "Hemiptera", "Lepidoptera") & focal %in% c("CHFU", "LEMA", "PUPA", "BEMA") )

competition <- ggplot(tabla.comp.vists.visitors, aes(x= num.seed, y = num.visits))+
  geom_point(aes(color=Group_Floralvisitor))+
  geom_smooth(method= 'lm', aes(color= Group_Floralvisitor))+
NULL
competition

#########SALINIDAD ----
Datos_TDR$Salinity <- as.numeric(Datos_TDR$Salinity)
Datos_TDR <- Datos_TDR[which(!is.na(Datos_TDR$Salinity)),] 
sal2019 <- Datos_TDR %>% group_by(Plot, Subplot) %>% summarise(mean.sal= mean(Salinity))
AbunCOMPLETA_2019$Plot <- AbunCOMPLETA_2019$plot
AbunCOMPLETA_2019$Subplot <- AbunCOMPLETA_2019$subplot
A.C_2019 <- AbunCOMPLETA_2019 %>% group_by (Plot, Subplot, Sp.Focal) %>% summarise (num.indv = sum(Plantas)) 
sal.planta.2019.1 <- dplyr::left_join (A.C_2019,sal2019 )

salinity2016 <- subset(salinity2016, year == '2016')
salinity2016$Plot <- salinity2016$plot
salinity2016$Subplot <- salinity2016$subplot
sal2016 <- salinity2016 %>% group_by(Plot, Subplot) %>% summarise(mean.sal= mean(sum_salinity))
abundances <- subset(abundances, year == '2016')
abundances$Plot <- abundances$plot
abundances$Subplot <- abundances$subplot
abundances1 <- abundances %>% group_by(Plot, Subplot, Plant)%>% summarise(num.indv = sum (individuals))
sal.planta.2016 <- dplyr::left_join (abundances1, sal2016)
head(sal.planta.2016)


plot.sal.2016 <- ggplot(sal.planta.2016, aes(x= mean.sal, y = num.indv))+
  geom_point(aes(color=Plant))+
  geom_smooth(method= 'lm', aes(color= Plant))+
  xlab('Salinidad')+
  ylab('Nº de individuos')+
  ggtitle ('Abundancia de spp de Plantas segun la salinidad 2016')
  NULL
plot.sal.2016

plot.sal.2019 <- ggplot(sal.planta.2019.1, aes(x= mean.sal, y = num.indv))+
  geom_point(aes(color= Sp.Focal))+
  geom_smooth(method= 'lm', aes(color= Sp.Focal))+
  xlab('Salinidad')+
  ylab('Nº de individuos')+
  ggtitle ('Abundancia de spp de Plantas segun la salinidad 2019')
NULL
plot.sal.2019
lm(sal.planta.2019.1$mean.sal~sal.planta.2019.1$num.indv)

