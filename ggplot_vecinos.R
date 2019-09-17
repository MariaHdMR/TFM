library(vegan)
library(tidyverse)
library(reshape)
library(reshape2)
va <- read.table("data/FV_16_19.csv", header=T, sep= ";")
head(va)
vecinos <- read.table("data/neighbour_visits_final.csv", header = T, sep= ";")
va19 <- subset(va, Year== "2019")
pol.9 <- va19 %>% group_by(Plot, Subplot, Group) %>% summarise (num.visitors = sum(Visits))
pol.9 <-pol.9[which(complete.cases(pol.9)),]
pol.9 <- subset(pol.9, Plot != "OUT")
Abun_19 <-read.table("data/Abun_19.csv", header=T, sep=";")
Abun_19$Plot <- Abun_19$plot
Abun_19$Subplot <- Abun_19$subplot
Abun_19$Plant_Simple <- Abun_19$Sp.Focal
ab.19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Plantas))

t <- dcast(ab.19, Plot +Subplot ~ Plant_Simple,fun.aggregate = sum, value.var = "num.plantas")
#CHFU <- (192+131+103+214+334+575)
#beetle +chfu
pol.beetle9 <- subset(pol.9, Group == "Beetle")
b1 <- pol.beetle9 [,c( "num.visitors")]
achfu <- subset(ab.19, Plant_Simple == "CHFU")
plot.chfu <-achfu %>% group_by(Subplot, Plot) %>% summarise (plantas = sum(num.plantas))
chfu.1 <- subset(plot.chfu, Plot=="1")
todo.chfu <- achfu %>% group_by(Plot)%>% summarise(plant = sum(num.plantas))
ch <- achfu [,c( "num.plantas")]
ch <- as.numeric(ch)
achfu$Plot <- as.numeric(achfu$Plot)
pol.beetle9$Plot <- as.numeric(pol.beetle9$Plot)
all <-dplyr::left_join (pol.beetle9,achfu)
v.chfu <- subset(vecinos, Plant_Simple == "CHFU")
#Beetles ----
pol.beetle9 <- subset(pol.9, Group == "Beetle")
pol.beetle9$Plot <- as.numeric(as.character(pol.beetle9$Plot))
pol.beetle.B <- pol.beetle9[,c("Plot", "Subplot", "num.visitors")] #datos de plot, subplot, y visitas de BEETLES
BEETLES <- tidyr::spread(pol.beetle.B,key = Plot, value = num.visitors)
BEETLES[is.na(BEETLES)] <- 0
beetle.plot1 <- BEETLES [,c( "Subplot","1")]
dist.1 <-dist(beetle.plot1 [,c(2)], method= "euclidean", diag =T, upper =T)
#bioenv
r <- bioenv(dist.1~m.bbe1) #no works, me dice que tiene variables diferentes
v.chfu <- subset(vecinos, Plant_Simple == "CHFU")
e <- v.chfu %>% group_by(Plant_Simple, Subplot, Plot)%>% summarise(vecinos = sum(neigh.visits))

v.chfu <- subset(va, Plant_Simple =="CHFU")
v.chfu.19 <- subset(v.chfu, Year== "2019")
solo <- v.chfu.19[,c("Plot", "Subplot", "Plant_Simple","Visits")]
solo.1 <-solo[which(complete.cases(solo)),]
solo.1.1 <- subset(solo.1, Plot=="1")
v.chfu <- subset(vecinos, Plant_Simple=="CHFU")
v.chfu.1 <- subset(v.chfu, Plot== "1")
visitas.chfu.1 <- v.chfu.1 %>% group_by(Plant_Simple, Subplot)%>% summarise(vecinos = sum(neigh.visits))
todo.chfu.subset <- solo.1 %>% group_by(Plant_Simple)%>% summarise(n.visits = sum(Visits))

#####
glm(beetle.plot1$`1` ~(solo.1.1$Visits+ visitas.chfu.1$vecinos+todo.chfu.subset$n.visits))


##################
#primero, plot 1. lo primero que voy a hacer es sacar la visitas de beetles en el plot 1. 
pol.beetle9 <- subset(pol.9, Group == "Beetle")
Bt.1 <- subset(pol.beetle9, Plot== "1") #data
#ahora tengo que sacar los datos de visitas a chfu a nivel de  1 subplot, 7 vecinos y los 36 subplots
v.chfu <- subset(vecinos, Plant_Simple == "CHFU")
be.chfu <- subset(v.chfu,Group == "Beetles")
be.chfu.1 <- subset(be.chfu, Plot== "1")


