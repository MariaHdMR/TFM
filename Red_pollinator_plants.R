#red de las visitas de los polinizadores frente las abundancias de plantas --> data original de visitors =FINAL
library(tidyverse)
library(bipartite)
library(igraph)

FV_16_19 <-read.table("FV_16_19.csv", header=T, sep=";")
FV_19 <- subset(FV_16_19, Year == 2019) 
Abun_19 <-read.table("Abun_19.csv", header=T, sep=";")
Abun_19$Plot <- Abun_19$plot
Abun_19$Subplot <- Abun_19$subplot
Abun_19$Plant_Simple <- Abun_19$Sp.Focal
ab.19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Plantas))
FINAL <- dplyr::left_join(FV.19, ab.19)
FINAL <- FINAL[which(complete.cases(FINAL)),]

FINAL1 <- subset(FINAL,Plant_Simple %in% c("LEMA","CHFU","RAPE","ME", "HOMA","PUPA", "CHMI") & Group %in% c("Beetle", "Fly", "Butterfly","Bee"))
red.general <- FINAL1[,c("Plant_Simple","Group","num.visits")]
red.general <- red.general[which(complete.cases(red.general)),]
red.total <- red.general %>% group_by(Group,Plant_Simple) %>% summarise (total.visitas = sum(num.visits))
red.col <- tidyr::spread(red.total,key = Group, value = total.visitas)
#red.col[is.na(red.col)] <- 0
nombres <- list(red.col$Plant_Simple, names(red.col[,2:length(red.col)]))
red.matrix <- as.matrix(red.col[,2:length(red.col)], dimnames = nombres)
rownames(red.matrix) <- red.col$Plant_Simple

red.igraph <- graph_from_incidence_matrix(red.matrix, weighted = TRUE)
node.size.df <- FINAL %>% group_by(Plant_Simple) %>% summarise(size = sum(num.plantas))
plant.size <- node.size.df$size
names(plant.size) <- node.size.df$Plant_Simple
#visitors.size <- rep(1000,8)
Abundancia_de_visitors <- dcast(FINAL1, Plant_Simple ~ Group, fun.aggregate = sum, value.var = "num.visits")
visitors.size <- c(88,450,41,662)*12 #este vector lo he sacado de sumar todas las visitas de Bee, todas las de Beetle...de la matriz anterior
names(visitors.size) <- nombres[[2]]
node.size <- c(plant.size,visitors.size)



plot(red.igraph, vertex.color=(c("tomato","steelblue")[V(red.igraph)$type+1]),
     # vertex.label=NA,
     # vertex.size=2*igraph::degree(ppIg),
     vertex.size=node.size/200,
     edge.width=(edge_attr(red.igraph)$weight)/30, 
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
     layout=layout_as_bipartite, main="Visitantes florales y  las plantas visitadas")

