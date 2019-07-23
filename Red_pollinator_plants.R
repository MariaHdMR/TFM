#red de las visitas de los polinizadores frente las abundancias de plantas --> data original de visitors =FINAL

#load packages
library(tidyverse)
library(bipartite)
library(igraph)

#load data
FINAL <- read.csv("data/FV_16_19.csv", sep = ";")
head(FINAL)

#I would use only 2019 for your TFM, makes things easier to explain.
FINAL1 <- subset(FINAL,Plant_Simple %in% c("LEMA","CHFU","RAPE","ME", 
                                           "HOMA","PUPA", "CHMI") & 
                   Group %in% c("Beetle", "Fly", "Butterfly","Bee") &
                   Year == 2019)
red.general <- FINAL1[,c("Plant_Simple","Group","Visits")]
red.general <- red.general[which(complete.cases(red.general)),]
red.total <- red.general %>% group_by(Group,Plant_Simple) %>% summarise (total.visitas = sum(Visits))
red.col <- tidyr::spread(red.total,key = Group, value = total.visitas)
red.col[is.na(red.col)] <- 0
nombres <- list(red.col$Plant_Simple, names(red.col[,2:length(red.col)]))
red.matrix <- as.matrix(red.col[,2:length(red.col)], dimnames = nombres)
rownames(red.matrix) <- red.col$Plant_Simple
red.igraph <- graph_from_incidence_matrix(red.matrix, weighted = TRUE)
node.size.df <- FINAL %>% group_by(Plant_Simple) %>% summarise(size = sum(num.plantas))
plant.size <- node.size.df$size
names(plant.size) <- node.size.df$Plant_Simple
visitors.size <- rep(1000,8)
names(visitors.size) <- nombres[[2]]
node.size <- c(plant.size,visitors.size)

#I can't run this code, but I am not sure I am using the right data.

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

#Alternativelly, you can also plot using bipartite
library(bipartite)
library(reshape2)
head(FINAL1)
ntw <- dcast(FINAL1, Plant_Simple ~ Species, fun.aggregate = sum, value.var = "Visits")
head(ntw)
rownames(ntw) <- ntw$Plant_Simple 
ntw <- ntw[,-1]
plotweb(ntw)
# You can also calculate easely it's modularity, for example
m <- computeModules(ntw, method = "DormannStrauss")
m@likelihood #very low modularity...
visweb(ntw)
networklevel(ntw) #Plants have a moderatelly high niche overlap: niche.overlap.LL: 0.6757449 

#this can be the first descriptive result. plants share a lot of pollinators. BUT NA's and taxonomy may need further cleaning.

