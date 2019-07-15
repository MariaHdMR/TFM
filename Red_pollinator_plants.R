#red de las visitas de los polinizadores frente las abundancias de plantas --> data original de visitors =FINAL
library(tidyverse)
library(bipartite)
library(igraph)

FINAL1 <- subset(FINAL,Plant_Simple %in% c("LEMA","CHFU","RAPE","ME", "HOMA","PUPA", "CHMI") & Group %in% c("Beetle", "Fly", "Butterfly","Bee"))
red.general <- FINAL1[,c("Plant_Simple","Group","num.visits")]
red.general <- red.general[which(complete.cases(red.general)),]
red.total <- red.general %>% group_by(Group,Plant_Simple) %>% summarise (total.visitas = sum(num.visits))
red.col <- tidyr::spread(red.total,key = Group, value = total.visitas)
red.col[is.na(red.col)] <- 0
nombres <- list(red.col$Plant_Simple, names(red.col[,2:length(red.col)]))
red.matrix <- as.matrix(red.col[,2:length(red.col)], dimnames = nombres)
rownames(red.matrix) <- red.col$Plant_Simple
library(tidyverse)
red.igraph <- graph_from_incidence_matrix(red.matrix, weighted = TRUE)
node.size.df <- FINAL %>% group_by(Plant_Simple) %>% summarise(size = sum(num.plantas))
plant.size <- node.size.df$size
names(plant.size) <- node.size.df$Plant_Simple
visitors.size <- rep(1000,8)
names(visitors.size) <- nombres[[2]]
node.size <- c(plant.size,visitors.size)



plot(red.igraph, vertex.color=(c("tomato","steelblue")[V(red.igraph)$type+1]),
     # vertex.label=NA,
     # vertex.size=2*igraph::degree(ppIg),
     vertex.size=node.size/300,
     edge.width=(edge_attr(red.igraph)$weight)/50, 
     edge.color="gray8", 
     rescale = T,
     frame= F,
     margin= c(0,0,0,0),
     asp =2/5,
     edge.label.cex = 40,
     vertex.label.dist= 0,	
     vertex.label.cex= 1.01,
     vertex.label.color= "gray8",
     #edge.curved=0.3,
     layout=layout_as_bipartite, main="Visitantes florales y  las plantas visitadas")
