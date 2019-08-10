#load libraries
library(tidyverse)
library(bipartite)
library(igraph)
library(reshape2)

#load data
FV_16_19 <-read.table("data/FV_16_19.csv", header=T, sep=";")
FV_19 <- subset(FV_16_19, Year == 2019) 
Abun_19 <-read.table("data/Abun_19.csv", header=T, sep=";")
Abun_19$Plot <- Abun_19$plot
Abun_19$Subplot <- Abun_19$subplot
Abun_19$Plant_Simple <- Abun_19$Sp.Focal
ab.19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Plantas))
FINAL <- dplyr::left_join(FV.19, ab.19)

FINAL1 <- subset(FINAL,Plant_Simple %in% c("LEMA","CHFU","RAPE","ME", "HOMA","PUPA", "CHMI") & Group %in% c("Beetle", "Fly", "Butterfly","Bee"))
FINAL2 <- FINAL1[,c("Plot","Subplot","Plant_Simple","Group","num.visits","num.plantas")]
FINAL3 <- FINAL2[which(complete.cases(FINAL2)),]
FINAL3 <- subset(FINAL3, Plot != "OUT")
red.general <- FINAL3[,c("Plant_Simple","Group","num.visits")]
red.general <- red.general[which(complete.cases(red.general)),]
red.total <- red.general %>% group_by(Group,Plant_Simple) %>% summarise (total.visitas = sum(num.visits))
red.col <- tidyr::spread(red.total,key = Group, value = total.visitas)
red.col[is.na(red.col)] <- 0
nombres <- list(red.col$Plant_Simple, names(red.col[,2:length(red.col)]))
red.matrix <- as.matrix(red.col[,2:length(red.col)], dimnames = nombres)
rownames(red.matrix) <- red.col$Plant_Simple

red.igraph <- graph_from_incidence_matrix(red.matrix, weighted = TRUE)
node.size.df <- FINAL3 %>% group_by(Plant_Simple) %>% summarise(size = sum(num.plantas))
plant.size <- node.size.df$size*7
names(plant.size) <- node.size.df$Plant_Simple
#visitors.size <- rep(1000,8)
Abundancia_de_visitors <- dcast(FINAL3, Plant_Simple ~ Group, fun.aggregate = sum, value.var = "num.visits")
visitors.size <- c(88,450,41,662)*25 #este vector lo he sacado de sumar todas las visitas de Bee, todas las de Beetle...de la matriz anterior
names(visitors.size) <- nombres[[2]]
node.size <- c(plant.size,visitors.size)


plot(red.igraph, vertex.color=(c("tomato","steelblue")[V(red.igraph)$type+1]),
     # vertex.label=NA,
     # vertex.size=2*igraph::degree(ppIg),
     vertex.size=node.size/500,
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
     layout=layout_as_bipartite, main=" Relacion entre los visitantes florales y las plantas")


#otra manera  <- NACHO

#plot.network()

head(FINAL3)
ntw <- dcast(FINAL3, Plant_Simple ~ Group, fun.aggregate = sum, value.var = "num.visits")
head(ntw)
rownames(ntw) <- ntw$Plant_Simple 
ntw <- ntw[,-1]
plotweb(ntw)
# You can also calculate easely it's modularity, for example
computeModules(ntw)
metaComputeModules(ntw, N=5)
m <- computeModules(ntw, method = "DormannStrauss")
m@likelihood #very low modularity... #with guilds is low, too.#M: with clean data: 0.273014
visweb(ntw)
plotModuleWeb(m)
networklevel(ntw) #Plants have a moderatelly high niche overlap: niche.overlap.LL: 0.6757449. M: now is 0.4706 is the clean data. 

#this can be the first descriptive result. plants share a lot of pollinators. BUT NA's and taxonomy may need further cleaning.


