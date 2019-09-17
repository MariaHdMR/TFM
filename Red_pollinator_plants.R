#load libraries
library(tidyverse)
library(bipartite)
library(igraph)
library(reshape2)

#load data
FV_16_19 <-read.table("data/FV_16_19.csv", header=T, sep=";")
FV_19 <- subset(FV_16_19, Year == 2019) 
vi <- FV_19 %>% group_by(Plot, Subplot, Plant_Simple, Group) %>% summarise (num.visits = sum(Visits))
vi$Plot <-as.numeric(vi$Plot)
Abun_19 <-read.table("data/Abun_19.csv", header=T, sep=";")
Abun_19$Plot <- Abun_19$plot
Abun_19$Subplot <- Abun_19$subplot
Abun_19$Plant_Simple <- Abun_19$Sp.Focal
ab.19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Plantas))

FINAL <- dplyr::left_join(vi, ab.19)

F.1 <- subset(FINAL,Plant_Simple %in% c("LEMA","CHFU","RAPE","ME", "HOMA","PUPA", "CHMI") & Group %in% c("Beetle", "Fly", "Butterfly","Bee"))
F.2 <- F.1[,c("Plot","Subplot","Plant_Simple","Group","num.visits","num.plantas")]
F.3 <- F.2[which(complete.cases(F.2)),]
F.3 <- subset(F.3, Plot != "OUT")
red_general <- F.3[,c("Plant_Simple","Group","num.visits")]
red_general <- red_general[which(complete.cases(red_general)),]
red_total <- red_general %>% group_by(Group,Plant_Simple) %>% summarise (total.visitas = sum(num.visits))
red_col <- tidyr::spread(red_total,key = Group, value = total.visitas)
red_col[is.na(red_col)] <- 0
nombres.1 <- list(red_col$Plant_Simple, names(red_col[,2:length(red_col)]))
red_matrix <- as.matrix(red_col[,2:length(red_col)], dimnames = nombres.1)
rownames(red_matrix) <- red_col$Plant_Simple

red_igraph <- graph_from_incidence_matrix(red_matrix, weighted = TRUE)
node.size.df <- F.3 %>% group_by(Plant_Simple) %>% summarise(size = sum(num.plantas))
plant.size <- node.size.df$size*7
names(plant.size) <- node.size.df$Plant_Simple
#visitors.size <- rep(1000,8)

Abun.poliniz <- F.3 %>% group_by(Plant_Simple, Group) %>% summarise (abundancia = n())
Abundancia.d.visitors <- dcast(Abun.poliniz, Plant_Simple ~ Group, fun.aggregate = sum, value.var = "abundancia")
visitors.size.1 <- c(62,205,22,335)*100
#Abundancia.d.visitors <- dcast(F.3, Plant_Simple ~ Group, fun.aggregate = sum, value.var = "num.visits") #esto me contabilizaba las visitas, no abundancias
#visitors.size.1 <- c(88,450,41,662)*50 #este vector lo he sacado de sumar todas las visitas de Bee, todas las de Beetle...de la matriz anterior
names(visitors.size.1) <- nombres.1[[2]]
node.size.1 <- c(plant.size,visitors.size.1)


plot(red_igraph, vertex.color=(c("tomato","steelblue")[V(red_igraph)$type+1]),
     # vertex.label=NA,
     # vertex.size=2*igraph::degree(ppIg),
     vertex.size=node.size.1/850,
     edge.width=(edge_attr(red_igraph)$weight)/45, 
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
     layout=layout_as_bipartite, main=" Relation between floral visitors and plants")


#otra manera  <- NACHO

#plot.network()

head(F.3)
ntw <- dcast(F.3, Plant_Simple ~ Group, fun.aggregate = sum, value.var = "num.visits")
head(ntw)
rownames(ntw) <- ntw$Plant_Simple 
ntw <- ntw[,-1]
plotweb(ntw)
# You can also calculate easely it's modularity, for example
computeModules(ntw)
metaComputeModules(ntw, N=5)
m <- computeModules(ntw, method = "DormannStrauss")
m@likelihood #very low modularity... #with guilds is low, too.#M: with clean data: 0.272507
visweb(ntw)
plotModuleWeb(m)
networklevel(ntw) #Plants have a moderatelly high niche overlap: niche.overlap.LL: 0.6757449. M: now is 0.4711 with the clean data. 

#this can be the first descriptive result. plants share a lot of pollinators. BUT NA's and taxonomy may need further cleaning.


