#load libraries
library(tidyverse)
library(bipartite)
library(igraph)
library(reshape2)
library(dplyr)
library(lme4)
library(DHARMa)
library(geepack)
#install.packages(geepack)
library(GGally)

#load data
FV <- read.table("data/FV_16_19_modificado_familiayespeciejunto.csv", header=T, sep=";")
FV_19 <- subset(FV, Year == 2019) 
SEED <- read.table("data/SEEDS_CARACOLES_2019.csv", header=T, sep=";")
competencia <- read.table("data/simplex_competencia_SEEDS_2019.csv", header=T, sep=";")
Abun_19 <-read.table("data/Abun_19.csv", header=T, sep=";")
Abun_19$Plot <- Abun_19$plot
Abun_19$Subplot <- Abun_19$subplot
Abun_19$Plant_Simple <- Abun_19$Sp.Focal
ab.19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Plantas))
FV_19$Plot<- as.factor(FV_19$Plot)
ab.19$Plot<- as.factor(ab.19$Plot)
FINAL <- dplyr::left_join(FV_19, ab.19)
a <- FINAL[,c("Plot","Subplot","Plant_Simple","Species","Visits","num.plantas")]
a2 <- subset(a, Plot != "OUT")
head(a2)
a3 <-subset(a2,Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
ntw <- dcast(a3, Plant_Simple ~ Species, fun.aggregate = sum, value.var = "Visits")

head(ntw)
rownames(ntw) <- ntw$Plant_Simple 
ntw <- ntw[,-1]
plotweb(ntw, text.rot = 90,y.lim = c(-1.5, 4),labsize= 0.85)
#ahora voy a sacar la informacion a nivel de nodo de la red, quiero saber la centralidad (betweenness),
#   y cuantos links (degree) por nodo de plantas hay

specieslevel(ntw, index="betweenness", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
specieslevel(ntw, index="degree", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)


#voy a calcular todas las redes por plot, y los indices de betweenes y degree (por plot tambien)

plot1 <- subset(a, Plot == "1")
plot1.1 <-subset(plot1,Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
plot1.1.1 <- dcast(plot1.1, Plant_Simple ~ Species, fun.aggregate = sum, value.var = "Visits")
rownames(plot1.1.1) <- plot1.1.1$Plant_Simple 
plot1.1.1 <- plot1.1.1[,-1]
#plotweb(plot1.1.1, text.rot = 90)

specieslevel(plot1.1.1, index="betweenness", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
specieslevel(plot1.1.1, index="degree", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

#plot2
plot2 <- subset(a, Plot== "2")
plot2.2 <-subset(plot2,Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
plot2.2.2 <- dcast(plot2.2, Plant_Simple ~ Species, fun.aggregate = sum, value.var = "Visits")
rownames(plot2.2.2) <- plot2.2.2$Plant_Simple 
plot2.2.2 <- plot2.2.2[,-1]
#plotweb(plot2.2.2, text.rot = 90)

central <-specieslevel(plot2.2.2, index="betweenness", level="both", logbase=exp(1), low.abun=NULL,
                       high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
                       nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
specieslevel(plot2.2.2, index="degree", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

#plot3
plot3 <- subset(a, Plot== "3")
plot3.3 <-subset(plot3,Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
plot3.3.3 <- dcast(plot3.3, Plant_Simple ~ Species, fun.aggregate = sum, value.var = "Visits")
rownames(plot3.3.3) <- plot3.3.3$Plant_Simple 
plot3.3.3 <- plot3.3.3[,-1]
#plotweb(plot3.3.3, text.rot = 90)

specieslevel(plot3.3.3, index="betweenness", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
specieslevel(plot3.3.3, index="degree", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

#plot4
plot4 <- subset(a, Plot== "4")
plot4.4 <-subset(plot4,Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
plot4.4.4 <- dcast(plot4.4, Plant_Simple ~ Species, fun.aggregate = sum, value.var = "Visits")
rownames(plot4.4.4) <- plot4.4.4$Plant_Simple 
plot4.4.4 <- plot4.4.4[,-1]
#plotweb(plot4.4.4, text.rot = 90)

specieslevel(plot4.4.4, index="betweenness", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
specieslevel(plot4.4.4, index="degree", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

#plot5
plot5 <- subset(a, Plot== "5")
plot5.5 <-subset(plot5,Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
plot5.5.5 <- dcast(plot5.5, Plant_Simple ~ Species, fun.aggregate = sum, value.var = "Visits")
rownames(plot5.5.5) <- plot5.5.5$Plant_Simple 
plot5.5.5 <- plot5.5.5[,-1]
#plotweb(plot5.5.5, text.rot = 90)

specieslevel(plot5.5.5, index="betweenness", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
specieslevel(plot5.5.5, index="degree", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

#ninguna spp central

#plot6
plot6 <- subset(a, Plot== "6")
plot6.6 <-subset(plot6,Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
plot6.6.6 <- dcast(plot6.6, Plant_Simple ~ Species, fun.aggregate = sum, value.var = "Visits")
rownames(plot6.6.6) <- plot6.6.6$Plant_Simple 
plot6.6.6 <- plot6.6.6[,-1]
plotweb(plot6.6.6, text.rot = 90)

specieslevel(plot6.6.6, index="betweenness", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
specieslevel(plot6.6.6, index="degree", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

#plot7
plot7 <- subset(a, Plot== "7")
plot7.7 <-subset(plot7,Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
plot7.7.7 <- dcast(plot7.7, Plant_Simple ~ Species, fun.aggregate = sum, value.var = "Visits")
rownames(plot7.7.7) <- plot7.7.7$Plant_Simple 
plot7.7.7 <- plot7.7.7[,-1]
#plotweb(plot7.7.7, text.rot = 90)


specieslevel(plot7.7.7, index="betweenness", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
specieslevel(plot7.7.7, index="degree", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

#plot8
plot8 <- subset(a, Plot== "8")
plot8.8 <-subset(plot8,Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
plot8.8.8 <- dcast(plot8.8, Plant_Simple ~ Species, fun.aggregate = sum, value.var = "Visits")
rownames(plot8.8.8) <- plot8.8.8$Plant_Simple 
plot8.8.8 <- plot8.8.8[,-1]
plotweb(plot8.8.8, text.rot = 90)


specieslevel(plot8.8.8, index="betweenness", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
specieslevel(plot8.8.8, index="degree", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

#plot9
plot9 <- subset(a, Plot== "9")
plot9.9 <-subset(plot9,Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
plot9.9.9 <- dcast(plot9.9, Plant_Simple ~ Species, fun.aggregate = sum, value.var = "Visits")
rownames(plot9.9.9) <- plot9.9.9$Plant_Simple 
plot9.9.9 <- plot9.9.9[,-1]
plotweb(plot9.9.9, text.rot = 90)

specieslevel(plot9.9.9, index="betweenness", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)
specieslevel(plot9.9.9, index="degree", level="both", logbase=exp(1), low.abun=NULL,
             high.abun=NULL, PDI.normalise=TRUE, PSI.beta=c(1,0), nested.method="NODF",
             nested.normalised=TRUE, nested.weighted=TRUE, empty.web=TRUE)

#con la informacion de estos indices de cada uno de los plots, creo el siguiente dataframe para luego
# integrarlo en el dataframe global. EL SIGUIENTE DATA FRAME SOLO ES DE CHFU
#CHFU
betw_and_degree_CHFU <- data.frame("betweenes"= c(0,0,0,0,0,0,0,0,0), "weighted.betweenes" = c(1,0,0,0,0,0,0.4545455,0,0), "degree"= c(5,2,4,6,8,7,9,16,12), "Plot"= c("1","2", "3", "4","5","6","7","8","9") )

#tengo que sacar el numero de visitas de los polinizadores (nivel spp) por spp de planta
head(a)

#esto que saco debajo es el numero de una especie focal (CHFU) en este caso, y sus vecinos, que coinciden en fenologia
#CHFU
CHFU <- subset(a, Plant_Simple == "CHFU") #aqui ya tengo también el número de visitas que recibe de cada planta por subplot y plot
h2 <- dcast(CHFU, Plot ~ ., fun.aggregate = sum, value.var = "num.plantas")
colnames(h2)[2] <- "abundance_plot"#abundancia de chfu por plot
h3 <- dcast(CHFU, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "num.plantas")#abundancias chfu por subplot
colnames(h3)[3] <- "abundance_subplot.focal" 
others1 <-subset(a, Plant_Simple %in% c("LEMA", "ME", "CHMI"))
h1 <- dcast(others1, Plot ~ ., fun.aggregate = sum, value.var = "num.plantas")
colnames(h1)[2] <- "abundance_plot_inter" #abundancia de vecinos por plot
h4 <- dcast(others1, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "num.plantas")#abundancias vecinos subplot
colnames(h4)[3] <- "abundance_subplot_inter"

#ahora voy a juntar los diferentes dataframe para conseguir tener toda la informacion de chfu solo en 1 dataframe
chfu1 <-left_join(h2,h3, by = c("Plot"))
others_abun <- merge(h1,h4)
CHFU_Focal <-left_join(others_abun,chfu1, by = c("Plot", "Subplot"))
head(CHFU_Focal)
CHFU_Focal[is.na(CHFU_Focal)] <- 0

#seedset de CHFU
CHFU.seedset <- subset(competencia, Plant_Simple == "CHFU")
CHFU.seedset$Plot <- as.numeric(CHFU.seedset$Plot)
CHFU$Plot <- as.numeric(CHFU$Plot)

q <-left_join(CHFU.seedset, CHFU, by= c("Plot", "Subplot", "Plant_Simple"))
q$Plot<-as.numeric(q$Plot)
CHFU_Focal$Plot<-as.numeric(CHFU_Focal$Plot)
CHFU_todo <- left_join(q,CHFU_Focal, by = c("Plot", "Subplot"))

CHFU_todo[is.na(CHFU_todo)] <- 0
betw_and_degree_CHFU$Plot <- as.numeric(betw_and_degree_CHFU$Plot)
all.chfu <- left_join(CHFU_todo,betw_and_degree_CHFU, by = c("Plot"))#dataframe de chfu con toda
#                                                                       la informacion


#ahora que ya tengo el dataframe de chfu voy a correr el modelo. 
hist(log(all.chfu$Seed))

prueba1<-glmer(all.chfu$Seed ~  all.chfu$weighted.betweenes  + all.chfu$degree + all.chfu$Visits + 
                   all.chfu$abundance_plot_inter + all.chfu$abundance_subplot_inter+ (1|Subplot:Plot)+ (1|Plot) , family="poisson", data=all.chfu)
summary(prueba1)#parece ser que es el que mejor se ajusta, auqnue tengo mucha dispersion de los datos 
simulationOutput <- simulateResiduals(fittedModel = prueba1, n = 250)
plot(simulationOutput)
overdisp.glmer(prueba1)
#prueba3<-glmer(all.chfu$Seed ~  all.chfu$weighted.betweenes  + all.chfu$degree + all.chfu$Visits + 
#all.chfu$abundance_plot_inter + all.chfu$abundance_subplot_inter+ (1|Subplot:Plot)+ (1|Plot) , family="quasipoisson", data=all.chfu)
#summary(prueba3)# no works, glmer no admite quasi familias

prueba2<-glmer(all.chfu$Seed ~  all.chfu$weighted.betweenes  + all.chfu$degree + all.chfu$Visits + 
                   all.chfu$abundance_plot_inter + all.chfu$abundance_subplot_inter+ (1|Subplot:Plot)+ (1|Plot) , family="binomial", data=all.chfu)
summary(prueba2)
simulationOutput1 <- simulateResiduals(fittedModel = prueba2, n = 250)
plot(simulationOutput1) #este modelo no es, la prueba 1 se ajusta mejor y no tiene tanta sobredispersion

prueba5<-glm(all.chfu$Seed ~  all.chfu$weighted.betweenes  + all.chfu$degree + all.chfu$Visits + 
                              all.chfu$abundance_plot_inter + all.chfu$abundance_subplot_inter+ (1|Subplot)+ (1|Plot) , family="quasipoisson", data=all.chfu)
#probelms
all.chfu$Subplot <- as.factor(all.chfu$Subplot)

M1<-geeglm(all.chfu$Seed ~all.chfu$weighted.betweenes  + all.chfu$degree + all.chfu$Visits + 
               all.chfu$abundance_plot_inter + all.chfu$abundance_subplot_inter+ (1|Subplot)+ (1|Plot),
           family=quasipoisson,data=all.chfu, scale.fix=T)

prueba6<-glmer((rescale01(all.chfu$Seed))~  (rescale01(all.chfu$weighted.betweenes))  + (rescale01(all.chfu$degree)) + (rescale01(all.chfu$Visits)) + 
                   (rescale01(all.chfu$abundance_plot_inter)) + (rescale01(all.chfu$abundance_subplot_inter))+ (1|Subplot:Plot)+ (1|Plot) , family="poisson", data=all.chfu)
summary(prueba6)