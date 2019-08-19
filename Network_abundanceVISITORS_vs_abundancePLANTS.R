#load libraries
library(tidyverse)
library(bipartite)
library(igraph)
library(reshape2)

#load data
V_a_16_19 <-read.table("data/V_a_16_19.csv", header=T, sep=";")
FV_19 <- subset(V_a_16_19, Year == 2019) 
Abun_19 <-read.table("data/Abun_19.csv", header=T, sep=";")
Abun_19$Plot <- Abun_19$plot
Abun_19$Subplot <- Abun_19$subplot
Abun_19$Plant_Simple <- Abun_19$Sp.Focal
ab.19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Plantas))
ALL <- dplyr::left_join(FV_19, ab.19)

ALL1 <- subset(ALL,Plant_Simple %in% c("LEMA","CHFU","RAPE","ME", "HOMA","PUPA", "CHMI") & Group %in% c("Beetles", "Flies", "Butterflies","Bees"))
ALL2 <- ALL1[,c("Plot","Subplot","Plant_Simple","Group","Abundances","num.plantas")]
ALL3 <- ALL2[which(complete.cases(ALL2)),]
ALL3 <- subset(ALL3, Plot != "OUT")


red.gene <- ALL3[,c("Plant_Simple","Group","Abundances")]
red.general <- red.gene[which(complete.cases(red.gene)),]
red.total <- red.general %>% group_by(Group,Plant_Simple) %>% summarise (total.ab.pol = sum(Abundances))
red.col <- tidyr::spread(red.total,key = Group, value = total.ab.pol)
red.col[is.na(red.col)] <- 0
nombres <- list(red.col$Plant_Simple, names(red.col[,2:length(red.col)]))
red.matrix <- as.matrix(red.col[,2:length(red.col)], dimnames = nombres)
rownames(red.matrix) <- red.col$Plant_Simple

red.igraph <- graph_from_incidence_matrix(red.matrix, weighted = TRUE)
node.size.df <- FINAL3 %>% group_by(Plant_Simple) %>% summarise(size = sum(num.plantas))
plant.size <- node.size.df$size*2
names(plant.size) <- node.size.df$Plant_Simple
#visitors.size <- rep(1000,8)
visitor.size <- FINAL3 %>% group_by(Group) %>% summarise (size.pol = sum(Abundances))

#Abundancia_de_visitors <- dcast(FINAL3, Plant_Simple ~ Group, fun.aggregate = sum, value.var = "Abundances")
visitors.size <- c(74,815,35,540)*35 # este vector lo he sacado de sumar todas las visitas de Bee, todas las de Beetle...de la matriz anterior
names(visitors.size) <- nombres[[2]]
node.size <- c(plant.size,visitors.size)


plot(red.igraph, vertex.color=(c("tomato","steelblue")[V(red.igraph)$type+1]),
     # vertex.label=NA,
     # vertex.size=2*igraph::degree(ppIg),
     vertex.size=node.size/900,
     edge.width=(edge_attr(red.igraph)$weight)/45, 
     edge.color="gray8", 
     rescale = T,
     frame= F,
     #margin= c(0,0,0,0),
     asp =2/5,
     edge.label.cex = 40,
     vertex.label.dist= 0,	
     vertex.label.cex= 1.01,
     vertex.label.color= "gray8",
     #edge.curved=0.3,
     layout=layout_as_bipartite) 
     #main=" Relation between the abundance of floral visitors and plants")
title("Relation between the abundance of floral visitors and plants", line= -3)

#otra manera  <- NACHO

#plot.network()

head(ALL3)
ntw.1 <- dcast(ALL3, Plant_Simple ~ Group, fun.aggregate = sum, value.var = "Abundances")
head(ntw.1)
rownames(ntw.1) <- ntw.1$Plant_Simple 
ntw.1 <- ntw.1[,-1]
plotweb(ntw.1)
# You can also calculate easely it's modularity, for example
computeModules(ntw.1)
metaComputeModules(ntw.1, N=5)
m.1 <- computeModules(ntw.1, method = "DormannStrauss")
m.1@likelihood #very low modularity... #with guilds is low, too.#M: with clean data: 0.268074
visweb(ntw.1)
plotModuleWeb(m.1)
networklevel(ntw.1) #Plants have a moderatelly high niche overlap: niche.overlap.LL: 0.6757449. M: now is 0.4885. 

#this can be the first descriptive result. plants share a lot of pollinators. BUT NA's and taxonomy may need further cleaning.


