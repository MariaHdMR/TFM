#red de las visitas de los polinizadores frente las abundancias de plantas 

#load packages
library(tidyverse)
library(bipartite)
library(igraph)
library(reshape2)

#load data
FV_16_19 <-read.table("FV_16_19.csv", header=T, sep=";")
FV_19 <- subset(FV_16_19, Year == 2019) 
Abun_19 <-read.table("Abun_19.csv", header=T, sep=";")
#Abun_19$Plot <- Abun_19$plot
#Abun_19$Subplot <- Abun_19$subplot
#Abun_19$Plant_Simple <- Abun_19$Sp.Focal
#The code above brakes the code. I think it's a matter of which csv you use.
head(Abun_19)
ab.19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Abundances))

str(FV_19)
FV_19$Plot #REMOVE OUT FIRST!!! 
FV_19_2 <- subset(FV_19, Plot != "OUT")
FV_19_2$Plot <- as.numeric(as.character(FV_19_2$Plot))
FINAL <- dplyr::left_join(FV_19_2, ab.19)
head(FINAL)
#FINAL <- FINAL[which(complete.cases(FINAL)),] #removes everything, as Sex is mostly NA!
head(FINAL)


#I would use only 2019 for your TFM, makes things easier to explain.
FINAL1 <- subset(FINAL,Plant_Simple %in% c("LEMA","CHFU","RAPE","ME", 
                                           "HOMA","PUPA", "CHMI") & 
                  Group %in% c("Beetle", "Fly", "Butterfly","Bee") &
                   Year == 2019)
#M: no sé por qué pero cambia la red al meter FINAL y FINAL1, con FINAL hay más abundancia de LEMA y CHFU ¿? 


#Analysis
head(FINAL)
red.general <- FINAL[,c("Plant_Simple","Group","Visits")]
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
#visitors.size <- rep(1000,8)
Abundancia_de_visitors <- dcast(FINAL, Plant_Simple ~ Group, fun.aggregate = sum, value.var = "Visits")
visitors.size <- c(88,450,41,662)*12 #este vector lo he sacado de sumar todas las visitas de Bee, todas las de Beetle...de la matriz anterior
names(visitors.size) <- nombres[[2]] #me da aquí un erro en los nombres, pero sigue saliendo
#IB: tienes más nombres que numeros (6 nombres, 4 numeros).
node.size <- c(plant.size,visitors.size)

node.size[is.na(node.size)] <- 1 #IB: manually editing this, because there are NA's

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
     layout=layout_as_bipartite, main="Interactions between visitors and plants")

#otra manera de hacerlo <- NACHO

#plot.network()

head(FINAL1)
ntw <- dcast(FINAL1, Plant_Simple ~ Group, fun.aggregate = sum, value.var = "Visits")
head(ntw)
rownames(ntw) <- ntw$Plant_Simple 
ntw <- ntw[,-1]
plotweb(ntw)
# You can also calculate easely it's modularity, for example
#computeModules()
#metaComputeModules(moduleObject, N=5)
m <- computeModules(ntw, method = "DormannStrauss")
m@likelihood #very low modularity... #with guilds is low, too.
visweb(ntw)
plotModuleWeb(m)
networklevel(ntw) #Plants have a moderatelly high niche overlap: niche.overlap.LL: 0.6757449 

#this can be the first descriptive result. plants share a lot of pollinators. BUT NA's and taxonomy may need further cleaning.


