# code for 2020 spatial analysis
#This analysises are to know if the spatial scale or the distribution of the species (plant and floral visitors) have an impacto on the plant fitness. We
#have 3 main questions: 1) Are the plans, floral visitors and fitness distributed equally accros plots? 2) Are the plants and the floral visitors affected by
# their neighbors (direct effects)? 3) Know the indirect and direct interactions at different levels that affect plant fitness
#to answer this question we follow 3 main analysis: 1) Moran's I
#libraries and packages----
library(ggthemes) #to the ggplots
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(ape)
library(spdep)#-> for Moran's I
library(reshape2)
library(vegan)
library(nlme)
library(lme4)
library(DHARMa)
library(scales)
library(semPlot)
library(predictmeans) #plot the diagnosis for the lme
library(ncf) #spline.correlog
library(semPlot)# to make the SEM path
library(MuMIn)
library(multcomp)
library(emmeans)
library(sjPlot)
library(sjmisc)
library(nlme) #for the lmecontrol
library(sjmisc)
library(corrgram)#SEM
library(sem)#SEM
library(lavaan)#SEM
library(tidyr)

#load data and data transformations----

#neighbors data, there is not the plot 4
start.plants <- read.table("data/focal_neighbours.2020_start.csv", header=T, sep=";")
head(start.plants)
end.plants <- read.table("data/focal_neighbours.2020_2nphenology.csv", header=T, sep=";")
head(end.plants)


#data of the coordenates of the plots
distances <- read.csv("data/caracolesplotposition.csv", sep = ";") 
head(distances)


distances <- distances[seq(2,72,2),]
distances2 <- rbind(distances, distances, distances,
                    distances, distances, distances,
                    distances, distances, distances) 
distances2$plot <- c(rep(1,36),rep(2,36),rep(3,36),rep(4,36),rep(5,36),
                     rep(6,36),rep(7,36),rep(8,36),rep(9,36))
tesaurus <- data.frame(plot = c(1:9),
                       add.x = c(0, 33, 45, 21, 23, 20, 91, 39, 40),
                       add.y = c(0, 26, 28, 91, 7, 2, 133, -19, 16))
tesaurus$cumulative_x <- cumsum(tesaurus$add.x)
tesaurus$cumulative_y <- cumsum(tesaurus$add.y)
dis <- merge(distances2, tesaurus[,c(1,4,5)])
head(dis)
dis <- subset(dis, plot != 4)
dis$x_coor2 <- dis$x_coor + dis$cumulative_x
dis$y_coor2 <- dis$y_coor + dis$cumulative_y
head(dis)
disfinal <- dis[,c("plot", "position","x_coor2", "y_coor2")]
disfinal$Plot <- disfinal$plot
disfinal$Subplot <- disfinal$position
disfinal <- disfinal[,c("Plot", "Subplot","x_coor2", "y_coor2")]
disfinal$plot <- disfinal$Plot
disfinal$subplot <- disfinal$Subplot
distancias.matriz <-as.matrix(dist(cbind(disfinal$x_coor2, disfinal$y_coor2)))
#now, I make the inverse matrix and the diagonal equal to 0
plots.dists.inv <- 1/distancias.matriz
diag(plots.dists.inv) <- 0
#plots.dists.inv[1:5,1:5]

# preparacion de datos para analizar por los 8 vecinos mas cercanos --> w5. Asi vemos si se parecen mas 
# entre los vecinos que al resto en la I de Moran
disfinal$x_coor2 <- as.numeric(disfinal$x_coor2)
disfinal$y_coor2 <- as.numeric(disfinal$y_coor2)
w5 <- knn2nb(knearneigh(coordinates(disfinal[,3:4]), k=8))


#pollinators, abundances and competition data
FV <- read.table("data/Data_2020/raw_Pollinators_2020_1.csv", header=T, sep=";") #pollinators 2020
head(FV)
FV3 <- FV %>% group_by(Day, Month, Year, Plot, Subplot,Group,ID_Simple, Plant) %>% summarise (visits = sum(Visits))%>%
  ungroup()
FV3$ID <- FV3$ID_Simple
head(FV3)

Ab <-read.table("data/Data_2020/Abundances_2020.csv", header=T, sep=";")#plants abundance 2020
head(Ab)
Ab <- subset(Ab, plot != 4)
#there is an error in the Abundance data base. It appears for the same subplot and plot 2 measures of abundance for the same species
#the next row is to fix this error
Ab$unique_id <- paste(Ab$plot, Ab$subplot,Ab$species, sep="_")
Ab <- Ab %>% distinct(unique_id, .keep_all = TRUE)

comp <- competencia <- read.table("data/Data_2020/competition_caracoles2020.csv", header=T, sep=";")#competition 2020
head(comp)
comp <- subset(comp, plot != 4)

fitness <- read.table("data/Data_2020/Fitness_2020.csv", header=T, sep=";")
head(fitness)
fitness <- subset(fitness, Plot != 4)

alfonsodata<-  read.table("data/Data_2020/2020_data_models_phenol_overlap_Seeds_per_fruit.csv", header=T, sep=",")
head(alfonsodata)
alfonsodata<- alfonsodata[,c("Plot","Subplot","Plant","Seeds_per_fruit","Fruit", "visits_GF", "ID")]
alfonsodata$seed <- alfonsodata$Seeds_per_fruit
alfonsodata$fruit <- alfonsodata$Fruit
alfonsodata$visits <- alfonsodata$visits_GF
alfonsodata <- subset(alfonsodata, Plot != 4)

FV3.2 <- FV3[,c("Day", "Month", "Year", "Plant", "ID", "Group")]
head(FV3.2)

todo.1 <- left_join(alfonsodata, FV3.2,  by= c("ID", "Plant"))
todo.1 <- distinct(todo.1)
todo.1.1 <- todo.1 %>% group_by(Day, Month, Year, Plot, Subplot, Group, Plant, seed, fruit) %>% summarise (vis = sum(visits))%>%
  ungroup() 

fitness <- fitness[,c("Plot","Subplot","Plant","Seeds.Fruit","Mean.Seeds.Fruit")]

#select the floral visitors that i want
head(todo.1.1)
todo1 <- subset(todo.1.1, Plot != "OUT")
todo4 <- subset(todo1, Subplot != "OUT")
todo4 <- subset(todo4, Plot != 4)

todo4$vis <- as.numeric(todo4$vis) #necesary?
todo4$seed <- as.numeric(todo4$seed)
todo4$fruit <- as.numeric(todo4$fruit)

todo4$seed[is.na(todo4$seed)] <- 0
todo4$fruit[is.na(todo4$fruit)] <- 0
todo4$Plant <- as.factor(todo4$Plant)
todo4$plot <- todo4$Plot
todo4$subplot <- todo4$Subplot

Ab$Plant <- Ab$species

todo.numplants <- left_join(todo4, Ab, by= c("plot", "subplot", "Plant"))
final <- todo.numplants[,c("Day", "Month", "Year","plot","subplot","Group",  "vis","Plant","individuals", "seed","fruit")]
final$subplot <- as.factor(final$subplot)
final <- subset(final, plot != "OUT")
final <- subset(final, subplot != "OUT")

final$visits <- final$vis
final$individuals <- as.numeric(final$individuals)

#minimum has to be 1 individual of plants, so I change the 0 to 1, the same has to be with the flowers...
final$individuals[is.na(final$individuals)] <- 1

final$visits[final$visits== 0] <- 0.01 #in order to not have 0 to do the log I change the 0 values to 0,01
final$visitas_indv <- final$visits/ final$individuals #Aqui es donde no me cuadra el calculo.----
#Visitas por flor o visitas totales, no?-----
final$visitas_indv_hora <- (final$visitas_indv*60)/30#this is to have the numer of visits per individuals per hour.
final$visitas_indv_hora2 <- log(final$visitas_indv_hora ) #the distribution of the visits is check doing the log.
hist(final$visitas_indv_hora2)
hist(log(final$visits)) #zero inflated...----

#NEIGHBORS Data transformation
start.plants <- subset(start.plants,edge %in% c("FALSE"))#esto es para quitar aquellas plantas que estan en los bordes. 
#                           Esto hace que no tenga las columnas A y F, ni las finasl 1 y 6
start.plants <- subset(start.plants, focal %in% c("SOAS","CHFU","LEMA","CHMI", "SCLA"))

end.plants <- subset(end.plants,edge %in% c("FALSE"))
end.plants <- subset(end.plants, focal %in% c("BEMA","CETE","MESU","PUPA", "SPRU"))
neighbors <- rbind(start.plants, end.plants) #all the neighbors together, now i have to join to the final dataset
neighbors_1 <- neighbors[-which(duplicated(neighbors)), ] 
neighbors_1$Plant <- neighbors_1$focal
neighbors_1$neigh_inter <- as.numeric(neighbors_1$neigh_inter)
neighbors_1$neigh_intra <- as.numeric(neighbors_1$neigh_intra)
neighbors_1$unique_id <- paste(neighbors_1$plot, neighbors_1$subplot,neighbors_1$Plant,neighbors_1$distance, sep="_")
neighbors1 <- neighbors_1 %>% distinct(unique_id, .keep_all = TRUE)

final$unique_id <- paste(final$plot, final$subplot,final$Plant, sep="_")
final$subplot <- as.factor(final$subplot)
final$Plant <- as.factor(final$Plant)

neighbors <- neighbors_1[,c("plot","subplot","Plant","distance", "neigh_intra","neigh_inter")] 


neighbors.7.5 <- subset(neighbors, distance %in% c("d1")) 
neighbors.7.5$distance7.5 <- neighbors.7.5$distance
neighbors.7.5$neigh_intra.7.5 <- neighbors.7.5$neigh_intra
neighbors.7.5$neigh_inter.7.5<- neighbors.7.5$neigh_inter

neighbors.1m <- subset(neighbors, distance %in% c("d2")) 
neighbors.1m$distances.1m <- neighbors.1m$distance
neighbors.1m$neigh_intra.1m <- neighbors.1m$neigh_intra
neighbors.1m$neigh_inter.1m<- neighbors.1m$neigh_inter
neighbors.3m <- subset(neighbors, distance %in% c("d3")) 
neighbors.3m$distances.3m <- neighbors.3m$distance
neighbors.3m$neigh_intra.3m <- neighbors.3m$neigh_intra
neighbors.3m$neigh_inter.3m<- neighbors.3m$neigh_inter
neighbors.plot <- subset(neighbors, distance %in% c("d4")) 
neighbors.plot$distances.plot <- neighbors.plot$distance
neighbors.plot$neigh_intra.plot <- neighbors.plot$neigh_intra
neighbors.plot$neigh_inter.plot <- neighbors.plot$neigh_inter

a <- dplyr::full_join(neighbors.7.5, neighbors.1m, by= c("plot", "subplot",  "Plant"))

b <- dplyr::full_join(neighbors.3m, neighbors.plot, by= c("plot", "subplot",  "Plant"))

ab<- dplyr::full_join(a, b, by= c("plot", "subplot",  "Plant"))
head(ab) 
vecinos<- ab[,c("plot","subplot","Plant", "neigh_inter.plot","neigh_intra.plot",
                "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 
vecinos <- unique(vecinos)
vecinos$unique_id <- paste(vecinos$plot, vecinos$subplot,vecinos$Plant, sep="_")
vecinos <- vecinos %>% distinct(unique_id, .keep_all = TRUE)
#final$unique_id <- paste(final$plot, final$subplot,final$Plant, sep="_")
final.data <- dplyr::full_join(final, vecinos , by=c("plot", "subplot", "Plant")) #veinos + base de datos pol+abund

none.visitor <- na.omit(final.data) #only neighbors in the places that i have a pollinator
none.visitor<- none.visitor[,c("plot","subplot","Plant","Group","visits", "individuals", "seed", "fruit", "visitas_indv", "visitas_indv_hora", "neigh_inter.plot","neigh_intra.plot",
                               "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 


data3 <- dplyr::right_join(none.visitor, disfinal, by=c("plot", "subplot"))#ahora junto la base de datos con la de las coordenadas
#this data have all the neigbors together


#data4 <- data3 %>% group_by(plot, subplot, Plant, Group, individuals, seed, fruit,visitas_indv_hora, x_coor2, y_coor2,
 #                         neigh_inter.plot,neigh_intra.plot,neigh_intra.3m ,neigh_inter.3m, neigh_inter.1m ,neigh_intra.1m ,
  #                        neigh_inter.7.5,neigh_intra.7.5) %>% summarise (visits.total = sum(visits))%>%
  #ungroup()
data <- na.omit(data3)

#data <- data[,c("plot","subplot","Plant","Group","visits.total", "individuals", "seed", "fruit",  "neigh_inter.plot","neigh_intra.plot",
 #               "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5", "x_coor2", "y_coor2")] 
head(data)
data$unique_id <- paste(data$plot, data$subplot,data$Plant,data$Group, sep="_")
data.1 <- data %>% distinct(unique_id, .keep_all = TRUE)

data.spread.visitors <- spread(data.1, Group, visits, fill = 0, convert = FALSE,
                               drop = TRUE, sep = NULL) #solo 222 entradas


neigbors.intra.inter <- tidyr::gather(data.spread.visitors,'neigh_inter.plot', 'neigh_intra.plot', 'neigh_intra.3m', 'neigh_inter.3m', 'neigh_inter.1m', 
                                      'neigh_intra.1m', 'neigh_inter.7.5', 'neigh_intra.7.5', key = "distance", value = "n_neighbors" )
neigbors.intra.inter$distance[neigbors.intra.inter$distance == "neigh_inter.plot"] <- "a"
neigbors.intra.inter$distance[neigbors.intra.inter$distance == "neigh_intra.plot"] <- "b"
neigbors.intra.inter$distance[neigbors.intra.inter$distance == "neigh_inter.3m"] <- "c"
neigbors.intra.inter$distance[neigbors.intra.inter$distance == "neigh_intra.3m"] <- "d"
neigbors.intra.inter$distance[neigbors.intra.inter$distance == "neigh_inter.1m"] <- "e"
neigbors.intra.inter$distance[neigbors.intra.inter$distance == "neigh_intra.1m"] <- "f"
neigbors.intra.inter$distance[neigbors.intra.inter$distance == "neigh_inter.7.5"] <- "g"
neigbors.intra.inter$distance[neigbors.intra.inter$distance == "neigh_intra.7.5"] <- "h"
neigbors.intra.inter$distance <- as.factor(neigbors.intra.inter$distance)
neigbors.intra.inter$n_neighbors_total <- neigbors.intra.inter$n_neighbors 
neigbors.intra.inter$distance_total <- neigbors.intra.inter$distance
inter.neigh <- subset(neigbors.intra.inter, distance_total%in% c("a","c","e","g"))
inter.neigh$n_neighbors_inter <- inter.neigh$n_neighbors_total
intra.neigh <- subset(neigbors.intra.inter, distance_total%in% c("b","d","f","h"))
intra.neigh$distance_total <- as.character(intra.neigh$distance_total)
intra.neigh$n_neighbors_intra <- intra.neigh$n_neighbors_total
intra.neigh$distance[intra.neigh$distance_total == "b"] <- 'a' #PLOT
intra.neigh$distance[intra.neigh$distance_total == "d"] <- "c"#3M
intra.neigh$distance[intra.neigh$distance_total == "f"] <- "e"#1M
intra.neigh$distance[intra.neigh$distance_total == "h"] <- "g"#7.5cm
intra <- intra.neigh[,c("n_neighbors_intra" )] 
neigbors.intra.inter.split<- cbind(inter.neigh, intra)# in this data i have the neighbors separate with the level plot, 3m, 1m and 7.5 cm
head(neigbors.intra.inter.split)



#######flowers


flores<-  read.table("data/Data_2020/Flowers_Abundance_2020.csv", header=T, sep=";")
flores <- na.omit(flores)
head(flores)
flores$date <- paste(flores$Year,"-",flores$Month,"-", flores$Day,sep="")
flores$week <- strftime(flores$date,format = "%V")
flores$week <- strftime(flores$date,format = "%V")
flores.juntas <- tidyr::gather(flores, key = "Plant", value = "flowers", 6:23)
flores.juntas$plot <- flores.juntas$Plot
flores.juntas$subplot <- flores.juntas$Subplot
flores.juntas <- subset(flores.juntas, subplot != "OUT")


#nueva base de datos

fl.vs <- full_join(final, flores.juntas, by= c("Day", "Month", "Year", "plot", "subplot", "Plant") )#casi completa, me falta añadir vecinos y coordenadas

fl.vs.coor <- full_join(fl.vs, disfinal, by= c("plot", "subplot"))#ahora me toca añadir los vecinos


together <- full_join(fl.vs.coor, vecinos, by= c("plot", "subplot", "Plant"))


together1 <- together[,c("Day", "Month","plot","subplot","Group",  "vis","Plant","individuals", "seed","fruit", "visits", "visitas_indv_hora", "flowers",
            "x_coor2", "y_coor2", "neigh_inter.plot","neigh_intra.plot", "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m", 
            "neigh_inter.7.5", "neigh_intra.7.5")]

together2 <- together1[!is.na(together1$neigh_inter.plot),]#esto lo he hecho para eliminar los NAs correspondientes a los vecinos. Elimino
#     estos NAs, porque los datos que se me quedan son los que he podido calcular los vecinos, recuerda, has quitado las ultimas lineas y filas
#     de los plots. 

sum(is.na(together2$flowers))
#Ahora tengo que arreglar la columna de flores, ya que hay NAs que son 0, pero también hay NAs que son NAs


df1 <- together2
df2 <- together2
df3 <- together2
filas.sin.na <- 1073:1



str(df1)
filas.sin.na <- 1:1051 #selecciono las filas que quiero convertir los Nas en 0
posiciones <- which(is.na(df3[filas.sin.na,13])) #aqui me solecciona solo las lineas que son NAs
df3$flowers[posiciones] <- 0 #ahora sustituyo esas lineas que sn Nas en 0. #arreglado


df3 <- as.data.frame(df3)
df5 <- df3
head(as.data.frame(df3))
filas.sin.na3 <- 1271:1517
posiciones4 <- which(is.na(df5[filas.sin.na3,13])) #no entiendo por qué pero no se mecambian los datos.
df5$flowers[filas.sin.na3[posiciones4]] <- 0
df5$flowers[1271:1517]

#df5, si corrijo esos datos de flores, sería la base de datos completa. 

final$log.seed <-  log(final$seed)
hist(final$seed)
ggplot() + geom_histogram(aes(x=log(final$seed))) + scale_x_continuous(breaks=seq(0,15,0.5))#to see the seeds distribution

#Phenology -----
#I First I have to see the pehnology of the 2020 to calculate the right neighbors. 
FV$date <- paste(FV$Year,"-",FV$Month,"-",FV$Day,sep="")
FV$week <- strftime(FV$date,format = "%V")
FV$week <- strftime(FV$date,format = "%V")
phenology.color <- ggplot(FV, aes(x= week, y = Plant))+
  geom_point(aes(color = week))+
  ggtitle ("Phenology 2020")+
  xlab ("weeks")+
  ylab ("spp_Plants")+
  NULL
phenology.color #This grpah is to know in which week a visit was register in a plant. This graph show that (mostly for the quarentin) there are
# 2 phenologies: the ones that started first: CHFU, LEMA, SOAS, CHMI and SCLA, and the second one that are: LEMA, CHMI, CHFU, BEMA, PUPA, SOAS, SCLA,
#MESU and CETE. For CHMI occurs and special thing, and it is that the chmi visits apper later, but in the competition that species has been measured in the 
#first part of the season, so for that reason it is included in the first group. For calculating the neighbors for the first group, we will only count the 
#abundances of the species that have been measured in competition at the same time that the ones that belong to the first flowering group (at the group 1,
#we have to add the abundances of HOMA), and for the second group, we will consider the abundances of all the species except for SUSP, SASO and RAPE and
#COSQ. FOr the two first we dont have the number of seed per fruit, and for the second last, they appear so later, so there is not overlap.  

#Now I'm going to calculate the neighbors inter e intra at different scales following the David Neighbors script. At the end, there would be
#2 dataframes, one for the first species, and another for the last species. 




################################################ 1.Moran's I####################################################

##### ##############################>Pollinators I de Moran#######################################
#beetles 
beetle <- subset(final, Group == "Beetle")
beetle$plot <- as.numeric(as.character(beetle$plot))
beetle.v <- beetle[,c("plot", "subplot","visitas_indv_hora")] #datos de plot, subplot, y visitas de BEETLES
beetle.v <-beetle.v %>% group_by(plot, subplot) %>% summarise (visits = sum(visitas_indv_hora))
beetle.v <- left_join(disfinal, beetle.v, by= c("plot", "subplot"))
beetle.v$visits[is.na(beetle.v$visits)] <- 0
head(beetle.v)

#grafico de la correlacion espacial
bet.corr <- spline.correlog(x=beetle.v$x_coor2, y=beetle.v$y_coor2,
                            z=beetle.v$visits, resamp=100, quiet=TRUE) 
plot(bet.corr, main= " Spatial Autocorrelation beetles across plots")

#test de moran. Aqui quiero obtener El estadistico de Moran y p.value 
moran.test(beetle.v$visits,mat2listw(plots.dists.inv)) # I= 1.085506e-01
moran.test(beetle.v$visits, nb2listw(w5)) # vecinos, I= 0.17
moran.plot(beetle.v$visits,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation Beetles across plots")


moran.mc(beetle.v$visits, mat2listw(plots.dists.inv), nsim=99) #in internet it is said that The Montercalo
# has more accurate results then the moran.test function. 


#beetles (Nacho plays with data)
beetle <- subset(final, Group == "Beetle")
beetle$plot <- as.numeric(as.character(beetle$plot))
head(beetle)
beetle.v <- beetle[,c("plot", "subplot","visits")] #datos de plot, subplot, y visitas de BEETLES
beetle.v <-beetle.v %>% group_by(plot, subplot) %>% summarise (visits = sum(visits))
beetle.v <- left_join(disfinal, beetle.v, by= c("plot", "subplot"))
beetle.v$visits[is.na(beetle.v$visits)] <- 0
head(beetle.v)
#grafico de la correlacion espacial
bet.corr <- spline.correlog(x=beetle.v$x_coor2, y=beetle.v$y_coor2,
                            z=beetle.v$visits, resamp=100, quiet=TRUE) 
plot(bet.corr, main= " Spatial Autocorrelation beetles across plots")
#A MI ME GUSTARIA QUE LA SPLIE FUERA MÁS SMOOTH, AHORA FLUCTUA MUCHO... OSCAR, SABES COMO TOCAR ESE PARAMETRO?----
#test de moran. Aqui quiero obtener El estadistico de Moran y p.value 
moran.test(beetle.v$visits,mat2listw(plots.dists.inv)) # I= 0.33 (in plot = large distances)
moran.test(beetle.v$visits, nb2listw(w5)) # vecinos, I= 0.44 (in plot = short distances)
moran.plot(beetle.v$visits,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation Beetles across plots")
#Yo voto usar observaciones, por que la pregunta es sobre si los escarabajos estan clustered o no.
#Alternativamente, podemos usar visitation rate (POR FLOR) que es otra pregunta.----

#flies
flies <- subset(final, Group == "Fly")
flies$plot <- as.numeric(as.character(flies$plot))
flies.v <- flies[,c("plot", "subplot","visitas_indv_hora")] 
flies.v <-flies.v %>% group_by(plot, subplot) %>% summarise (visits = sum(visitas_indv_hora))
flies.v <- left_join(disfinal,flies.v, by= c("plot", "subplot"))
flies.v$visits[is.na(flies.v$visits)] <- 0

flies.corr <- spline.correlog(x=flies.v$x_coor2, y=flies.v$y_coor2,
                              z=flies.v$visits, resamp=100, quiet=TRUE)
plot(flies.corr, main= "Spatial autocorrelation flies across plots")

moran.test(flies.v$visits,mat2listw(plots.dists.inv)) # I= 0.04
moran.test(flies.v$visits, nb2listw(w5)) # vecinos, I= 0.056
moran.plot(flies.v$visits,mat2listw(plots.dists.inv), main= "Spatial autocorrelation flies across plots")

#butterflies
but <- subset(final, Group == "Butterfly")
but$plot <- as.numeric(as.character(but$plot))
but.v <- but[,c("plot", "subplot","visitas_indv_hora")] #datos de plot, subplot, y visitas 
but.v <-but.v %>% group_by(plot, subplot) %>% summarise (visits = sum(visitas_indv_hora))
but.v <- left_join(disfinal,but.v , by= c("plot", "subplot"))
but.v$visits[is.na(but.v$visits)] <- 0

but.corr <- spline.correlog(x=but.v$x_coor2, y=but.v$y_coor2,
                            z=but.v$visits, resamp=100, quiet=TRUE)
plot(but.corr, main= "Spatial autocorrelation butterflies across plots")

moran.test(but.v$visits,mat2listw(plots.dists.inv)) # I= 9.403899e-03, almost homogenius
moran.test(but.v$visits, nb2listw(w5)) # vecinos, I= 0.0009210841
moran.plot(but.v$visits,mat2listw(plots.dists.inv),  main= "Spatial autocorrelation butterflies across plots")

#bees
bee <- subset(final, Group == "Bee")
bee$plot <- as.numeric(as.character(bee$plot))
bee.v <- bee[,c("plot", "subplot","visitas_indv_hora")] #datos de plot, subplot, y visitas 
bee.v <-bee.v %>% group_by(plot, subplot) %>% summarise (visits = sum(visitas_indv_hora))
bee.v <- left_join(disfinal, bee.v, by= c("plot", "subplot"))
bee.v$visits[is.na(bee.v$visits)] <- 0

bee.corr <- spline.correlog(x=bee.v$x_coor2, y=bee.v$y_coor2,
                            z=bee.v$visits, resamp=100, quiet=TRUE)
plot(bee.corr, main= "Spatial autocorrelation bees across plots")

moran.test(bee.v$visits,mat2listw(plots.dists.inv))# I = 0.02
moran.test(bee.v$visits, nb2listw(w5))# I= 0.02
moran.plot(bee.v$visits,mat2listw(plots.dists.inv), main= "Spatial autocorrelation bees across plots")
#polinizadores general 
junto <- final[,c("plot", "subplot","visitas_indv_hora")]
junto <- junto %>% group_by(plot, subplot) %>% summarise (visit = sum(visitas_indv_hora))
junto$plot <- as.numeric(junto$plot)
junto <- left_join(disfinal, junto, by= c("plot", "subplot"))
junto <- subset(junto, subplot != "OUT")
junto$visit[is.na(junto$visit)] <- 0

total.corr <- spline.correlog(x=junto$x_coor2, y=junto$y_coor2,
                              z=junto$visit, resamp=100, quiet=TRUE)
plot(total.corr, main= "Spatial Autocorrelation visitors across plots")

moran.test(junto$visit,mat2listw(plots.dists.inv)) # I= 0.077
moran.test(junto$visit, nb2listw(w5)) # vecinos, I= 0.099
moran.plot(junto$visit,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation pollinators across plots")

par(mfrow=c(2,3))
plot(bet.corr, main= " Beetles distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(flies.corr, main= "Flies distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(but.corr, main= "Butterflies distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(bee.corr, main= "Bees distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(total.corr, main= "Floral visitors distribution across plots",xlab="Distance (m)", ylab="Correlation")
par(mfrow=c(1,1))
       
############################################ >Plants I de Moran########################################
plantas <- final[,c("plot", "subplot", "Plant", "individuals", "fruit", "seed")]
plantas <- plantas[-which(duplicated(plantas)), ] #It appears in the data set a lot of duplicated rows, because the previus data set was with the pollinators
#                                                   so we have to eliminate the duplicates, and I did it with this step
#CHFU
CHFU <- subset(plantas, Plant == "CHFU") #ESTO USA INDIVIDUOS POR m2? O INTRAS? M: Individuos por m2---
CHFU$individuals <- as.numeric(CHFU$individuals)
CHFU.p <- CHFU %>% group_by(plot, subplot) %>% summarise (num.planta = sum(individuals))
CHFU.p <- left_join(disfinal, CHFU.p,by= c("plot", "subplot"))
CHFU.p$num.planta[is.na(CHFU.p$num.planta)] <- 0
head(CHFU)

#grafico de la correlacion espacial
chfu.corr <- spline.correlog(x=CHFU.p$x_coor2, y=CHFU.p$y_coor2,
                             z=CHFU.p$num.planta, resamp=100, quiet=TRUE)
plot(chfu.corr, main= " Spatial Autocorrelation CHFU across plots")

moran.test(CHFU.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.38 #No cuadra con el grafico! Por que no es negativo?
moran.test(CHFU.p$num.planta, nb2listw(w5)) # vecinos, I= 0.48 #Cuadra con el grafico
moran.plot(CHFU.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHFU across plots")
#LEMA
LEMA <- subset(plantas, Plant == "LEMA")  
LEMA$individuals <- as.numeric(LEMA$individuals)
LEMA.1 <- LEMA %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
LEMA.2 <- left_join(disfinal, LEMA.1, by= c("plot", "subplot"))

LEMA.2$num.planta[is.na(LEMA.2$num.planta)] <- 0

lema.corr <- spline.correlog(x=LEMA.2$x_coor2, y=LEMA.2$y_coor2,
                             z=LEMA.2$num.planta, resamp=100, quiet=TRUE)
plot(lema.corr, main= " Spatial Autocorrelation LEMA across plots")

moran.test(LEMA.2$num.planta,mat2listw(plots.dists.inv)) # I= 0.58 #Idem...
moran.test(LEMA.2$num.planta, nb2listw(w5)) # vecinos, I= 0.73 
moran.plot(LEMA.2$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation LEMA across plots")
#PUPA 
PUPA <- subset(plantas, Plant == "PUPA")

PUPA.1 <- PUPA %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
PUPA.p1 <- left_join(disfinal, PUPA.1, by= c("plot", "subplot"))
PUPA.p1$num.planta[is.na(PUPA.p1$num.planta)] <- 0

pupa.corr <- spline.correlog(x=PUPA.p1$x_coor2, y=PUPA.p1$y_coor2,
                             z=PUPA.p1$num.planta, resamp=100, quiet=TRUE)
plot(pupa.corr, main= " Spatial Autocorrelation PUPA across plots")

moran.test(PUPA.p1$num.planta,mat2listw(plots.dists.inv)) # I= 0.34
moran.test(PUPA.p1$num.planta, nb2listw(w5)) # vecinos, I= 0.49
moran.plot(PUPA.p1$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation PUPA across plots")
#MESU
MESU <- subset(plantas, Plant == "MESU") 
MESU$individuals <- as.numeric(MESU$individuals)
MESU.p <- MESU %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
MESU.p <- left_join(disfinal, MESU.p,  by= c("plot", "subplot"))
MESU.p$num.planta[is.na(MESU.p$num.planta)] <- 0

me.corr <- spline.correlog(x=MESU.p$x_coor2, y=MESU.p$y_coor2,
                           z=MESU.p$num.planta, resamp=100, quiet=TRUE)
plot(me.corr, main= " Spatial Autocorrelation MESU across plots")

moran.test(MESU.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.37
moran.test(MESU.p$num.planta, nb2listw(w5)) # vecinos, I= 0.49
moran.plot(MESU.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation MESU across plots")
#CHMI 
CHMI <- subset(plantas, Plant == "CHMI") # solo 15 entradas
CHMI$individuals <- as.numeric(CHMI$individuals)
CHMI.p <- CHMI %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
CHMI.p <- left_join(disfinal, CHMI.p,  by= c("plot", "subplot"))
CHMI.p$num.planta[is.na(CHMI.p$num.planta)] <- 0

chmi.corr <- spline.correlog(x=CHMI.p$x_coor2, y=CHMI.p$y_coor2,
                           z=CHMI.p$num.planta, resamp=100, quiet=TRUE)
plot(chmi.corr, main= " Spatial Autocorrelation CHMI across plots")

moran.test(CHMI.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.07
moran.test(CHMI.p$num.planta, nb2listw(w5)) # vecinos, I= 0.13
moran.plot(CHMI.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHMI across plots")


#CETE 
CETE <- subset(plantas, Plant == "CETE") 
CETE$individuals <- as.numeric(CETE$individuals)
CETE.p <- CETE %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
CETE.p <- left_join(disfinal, CETE.p, by= c("plot", "subplot"))
CETE.p$num.planta[is.na(CETE.p$num.planta)] <- 0

cete.corr <- spline.correlog(x=CETE.p$x_coor2, y=CETE.p$y_coor2,
                           z=CETE.p$num.planta, resamp=100, quiet=TRUE)
plot(cete.corr, main= " Spatial Autocorrelation CETE across plots")

moran.test(CETE.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.27
moran.test(CETE.p$num.planta, nb2listw(w5)) # vecinos, I= 0.45
moran.plot(CETE.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CETE across plots")
#BEMA 
BEMA <- subset(plantas, Plant == "BEMA") 
BEMA$individuals <- as.numeric(BEMA$individuals)
BEMA.p <- BEMA %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
BEMA.p <- left_join(disfinal, BEMA.p, by= c("plot", "subplot"))
BEMA.p$num.planta[is.na(BEMA.p$num.planta)] <- 0

bema.corr <- spline.correlog(x=BEMA.p$x_coor2, y=BEMA.p$y_coor2,
                           z=BEMA.p$num.planta, resamp=100, quiet=TRUE)
plot(bema.corr, main= " Spatial Autocorrelation BEMA across plots")

moran.test(BEMA.p$num.planta,mat2listw(plots.dists.inv)) # I=  0.32
moran.test(BEMA.p$num.planta, nb2listw(w5)) # vecinos, 0.45
moran.plot(BEMA.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation BEMA across plots")

#SCLA 
SCLA <- subset(plantas, Plant == "SCLA") 
SCLA$individuals <- as.numeric(SCLA$individuals)
SCLA.p <- SCLA %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
SCLA.p <- left_join(disfinal, SCLA.p, by= c("plot", "subplot"))
SCLA.p$num.planta[is.na(SCLA.p$num.planta)] <- 0

scla.corr <- spline.correlog(x=SCLA.p$x_coor2, y=SCLA.p$y_coor2,
                           z=SCLA.p$num.planta, resamp=100, quiet=TRUE)
plot(scla.corr, main= " Spatial Autocorrelation SCLA across plots")

moran.test(SCLA.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.32
moran.test(SCLA.p$num.planta, nb2listw(w5)) # vecinos,  0.51
moran.plot(SCLA.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SCLA across plots")
#Soas
SOAS <- subset(plantas, Plant == "SOAS")
SOAS$individuals <- as.numeric(SOAS$individuals)
SOAS.p <- SOAS %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
SOAS.p <- left_join(disfinal, SOAS.p, by= c("plot", "subplot"))
SOAS.p$num.planta[is.na(SOAS.p$num.planta)] <- 0

soas.corr <- spline.correlog(x=SOAS.p$x_coor2, y=SOAS.p$y_coor2,
                             z=SOAS.p$num.planta, resamp=100, quiet=TRUE)
plot(soas.corr, main= " Spatial Autocorrelation SOAS across plots")

moran.test(SOAS.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.49
moran.test(SOAS.p$num.planta, nb2listw(w5)) # vecinos,  0.68
moran.plot(SOAS.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SOAS across plots")

#SPRU
SPRU <- subset(plantas, Plant == "SPRU") 
SPRU$individuals <- as.numeric(SPRU$individuals)
SPRU.p <- SPRU %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
SPRU.p <- left_join(disfinal, SPRU.p, by= c("plot", "subplot"))
SPRU.p$num.planta[is.na(SPRU.p$num.planta)] <- 0

spru.corr <- spline.correlog(x=SPRU.p$x_coor2, y=SPRU.p$y_coor2,
                             z=SPRU.p$num.planta, resamp=100, quiet=TRUE)
plot(spru.corr, main= " Spatial Autocorrelation SPRU across plots")

moran.test(SPRU.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.27
moran.test(SPRU.p$num.planta, nb2listw(w5)) # vecinos,  0.45
moran.plot(SPRU.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SPRU across plots")


#plantas juntas 
juntas.plantas <- plantas[,c("plot", "subplot","individuals")]
juntas.plantas$individuals <- as.numeric(juntas.plantas$individuals)
num.plant <- juntas.plantas %>% group_by(plot, subplot) %>% summarise (num.planta = sum(individuals))
num.plant$plot <- as.numeric(num.plant$plot)
num.plant <- left_join(disfinal, num.plant, by= c("plot", "subplot"))
num.plant$num.planta[is.na(num.plant$num.planta)] <- 0

total.corr.pl <- spline.correlog(x=num.plant$x_coor2, y=num.plant$y_coor2,
                                 z=num.plant$num.planta, resamp=100, quiet=TRUE)
plot(total.corr.pl, main= "Spatial Autocorrelation plants across plots")

moran.test(num.plant$num.planta,mat2listw(plots.dists.inv)) # I= 0.58
moran.test(num.plant$num.planta, nb2listw(w5)) # vecinos, I= 0.72
moran.plot(num.plant$num.planta,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation plants across plots")
par(mfrow=c(4,3))
plot(chfu.corr, main= " CHFU distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(lema.corr, main= " LEMA distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(pupa.corr, main= " PUPA distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(me.corr, main= " MESU distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(cete.corr, main= " CETE distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(bema.corr, main= " BEMA distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(scla.corr, main= " SCLA distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(spru.corr, main= " SPRU distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(soas.corr, main= " SOAS distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(total.corr.pl, main= "Plants distribution across plots",xlab="Distance (m)", ylab="Correlation")
par(mfrow=c(1,1))

##########################################>Fitness I de Moran###################################################

#CHFU 
CHFU$seed <- as.numeric(CHFU$seed)
CHFU.fit <- CHFU %>% group_by(plot, subplot) %>% summarise (seeds = sum(seed))
CHFU.fit <- left_join(disfinal, CHFU.fit,by= c("plot", "subplot"))
CHFU.fit$seeds[is.na(CHFU.fit$seeds)] <- 0

chfu.corr.s <- spline.correlog(x=CHFU.fit$x_coor2, y=CHFU.fit$y_coor2,
                               z=CHFU.fit$seeds, resamp=100, quiet=TRUE)
plot(chfu.corr.s, main= " Spatial Autocorrelation CHFU fitness across plots")

moran.test(CHFU.fit$seeds,mat2listw(plots.dists.inv)) # I= 4.433781e-02 
moran.test(CHFU.fit$seeds, nb2listw(w5)) # vecinos, I= 0.069
moran.plot(CHFU.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHFU fitness across plots")
#LEMA
LEMA$seed <- as.numeric(LEMA$seed)
LEMA.fit <- LEMA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
LEMA.fit <- left_join(disfinal, LEMA.fit, by= c("plot", "subplot"))
LEMA.fit$seeds[is.na(LEMA.fit$seeds)] <- 0

lema.corr.s <- spline.correlog(x=LEMA.fit$x_coor2, y=LEMA.fit$y_coor2,
                               z=LEMA.fit$seeds, resamp=100, quiet=TRUE)
plot(lema.corr.s, main= " Spatial Autocorrelation LEMA fitness across plots")

moran.test(LEMA.fit$seeds,mat2listw(plots.dists.inv)) # I= -3.679386e-03
moran.test(LEMA.fit$seeds, nb2listw(w5)) # vecinos, I= -3.484321e-03
moran.plot(LEMA.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation LEMA fitness across plots")

#PUPA 
PUPA$seed <- as.numeric(PUPA$seed)
PUPA.fit <- PUPA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
PUPA.fit <- left_join(disfinal, PUPA.fit, by= c("plot", "subplot"))
PUPA.fit$seeds <- PUPA.fit$seeds
PUPA.fit$seeds [is.na(PUPA.fit$seeds )] <- 0

pupa.corr.s <- spline.correlog(x=PUPA.fit$x_coor2, y=PUPA.fit$y_coor2,
                               z=PUPA.fit$seeds, resamp=100, quiet=TRUE)
plot(pupa.corr.s, main= " Spatial Autocorrelation PUPA fitness across plots")

moran.test(PUPA.fit$seeds,mat2listw(plots.dists.inv)) # -2.173268e-03 
moran.test(PUPA.fit$seeds, nb2listw(w5)) # vecinos, I=  -2.177700e-03
moran.plot(PUPA.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation PUPA across plots")
#MESU
MESU$seed <- as.numeric(MESU$seed)
MESU.fit <- MESU %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
MESU.fit <- left_join(disfinal, MESU.fit, by= c("plot", "subplot"))
MESU.fit$seeds[is.na(MESU.fit$seeds)] <- 0 
mesu.corr.s <- spline.correlog(x=MESU.fit$x_coor2, y=MESU.fit$y_coor2,
                               z=MESU.fit$seeds, resamp=100, quiet=TRUE)
plot(mesu.corr.s, main= " Spatial Autocorrelation MESU fitness across plots")

moran.test(MESU.fit$seeds,mat2listw(plots.dists.inv)) # 0.19
moran.test(MESU.fit$seeds, nb2listw(w5)) # vecinos, I= 0.23
moran.plot(MESU.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation MESU across plots")

#CETE 
CETE.fit <- CETE %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
CETE.fit <- left_join(disfinal, CETE.fit, by= c("plot", "subplot"))
CETE.fit$seeds <- CETE.fit$seeds
CETE.fit$seeds [is.na(CETE.fit$seeds )] <- 0

cete.corr.s <- spline.correlog(x=CETE.fit$x_coor2, y=CETE.fit$y_coor2,
                               z=CETE.fit$seeds, resamp=100, quiet=TRUE)
plot(cete.corr.s, main= " Spatial Autocorrelation CETE fitness across plots")

moran.test(CETE.fit$seeds,mat2listw(plots.dists.inv)) # 0.066
moran.test(CETE.fit$seeds, nb2listw(w5)) # vecinos, I= 0.08
moran.plot(CETE.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CETE across plots")

#SPRU 
SPRU.fit <- SPRU %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
SPRU.fit <- left_join(disfinal, SPRU.fit, by= c("plot", "subplot"))
SPRU.fit$seeds <- SPRU.fit$seeds
SPRU.fit$seeds [is.na(SPRU.fit$seeds )] <- 0

spru.corr.s <- spline.correlog(x=SPRU.fit$x_coor2, y=SPRU.fit$y_coor2,
                               z=SPRU.fit$seeds, resamp=100, quiet=TRUE)
plot(spru.corr.s, main= " Spatial Autocorrelation SPRU fitness across plots")

moran.test(SPRU.fit$seeds,mat2listw(plots.dists.inv)) # 0.36
moran.test(SPRU.fit$seeds, nb2listw(w5)) # vecinos, I= 0.49
moran.plot(SPRU.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SPRU across plots")

#SOAS 
SOAS.fit <- SOAS %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
SOAS.fit <- left_join(disfinal, SOAS.fit, by= c("plot", "subplot"))
SOAS.fit$seeds <- SOAS.fit$seeds
SOAS.fit$seeds [is.na(SOAS.fit$seeds )] <- 0

soas.corr.s <- spline.correlog(x=SOAS.fit$x_coor2, y=SOAS.fit$y_coor2,
                               z=SOAS.fit$seeds, resamp=100, quiet=TRUE)
plot(soas.corr.s, main= " Spatial Autocorrelation SOAS fitness across plots")

moran.test(SOAS.fit$seeds,mat2listw(plots.dists.inv)) #  -4.163243e-03 
moran.test(SOAS.fit$seeds, nb2listw(w5)) # vecinos, I=  -4.355401e-03
moran.plot(SOAS.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SOAS across plots")


#BEMA
BEMA.fit <- BEMA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
BEMA.fit <- left_join(disfinal, BEMA.fit, by= c("plot", "subplot"))
BEMA.fit$seeds <- BEMA.fit$seeds
BEMA.fit$seeds [is.na(BEMA.fit$seeds )] <- 0

bema.corr.s <- spline.correlog(x=BEMA.fit$x_coor2, y=BEMA.fit$y_coor2,
                               z=BEMA.fit$seeds, resamp=100, quiet=TRUE)
plot(bema.corr.s, main= " Spatial Autocorrelation BEMA fitness across plots")

moran.test(BEMA.fit$seeds,mat2listw(plots.dists.inv)) #  0.29
moran.test(BEMA.fit$seeds, nb2listw(w5)) # vecinos, I=  0.36
moran.plot(BEMA.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation BEMA across plots")


#SCLA
SCLA.fit <- SCLA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
SCLA.fit <- left_join(disfinal, SCLA.fit, by= c("plot", "subplot"))
SCLA.fit$seeds <- SCLA.fit$seeds
SCLA.fit$seeds [is.na(SCLA.fit$seeds )] <- 0

scla.corr.s <- spline.correlog(x=SCLA.fit$x_coor2, y=SCLA.fit$y_coor2,
                               z=SCLA.fit$seeds, resamp=100, quiet=TRUE)
plot(scla.corr.s, main= " Spatial Autocorrelation SCLA fitness across plots")

moran.test(SCLA.fit$seeds,mat2listw(plots.dists.inv)) # 0.08
moran.test(SCLA.fit$seeds, nb2listw(w5)) # vecinos, I= 0.12
moran.plot(SCLA.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SCLA across plots")

#CHMI
CHMI.fit <- CHMI %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
CHMI.fit <- left_join(disfinal, CHMI.fit, by= c("plot", "subplot"))
CHMI.fit$seeds <- CHMI.fit$seeds
CHMI.fit$seeds [is.na(CHMI.fit$seeds )] <- 0

chmi.corr.s <- spline.correlog(x=CHMI.fit$x_coor2, y=CHMI.fit$y_coor2,
                               z=CHMI.fit$seeds, resamp=100, quiet=TRUE)
plot(chmi.corr.s, main= " Spatial Autocorrelation CHMI fitness across plots")

moran.test(CHMI.fit$seeds,mat2listw(plots.dists.inv)) # 6.541291e-02
moran.test(CHMI.fit$seeds, nb2listw(w5)) # vecinos, I= 0.14
moran.plot(CHMI.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHMI across plots")

#global fitness

juntas.plantas.fit <- plantas[,c("plot", "subplot","seed")]
juntas.plantas.fit$seed <- as.numeric(juntas.plantas.fit$seed)
juntas.plantas.fit <- juntas.plantas.fit %>% group_by(plot, subplot) %>% summarise (seeds = sum(seed))
juntas.plantas.fit$plot <- as.numeric(juntas.plantas.fit$plot)
juntas.plantas.fit <- left_join(disfinal, juntas.plantas.fit, by= c("plot", "subplot"))
juntas.plantas.fit$seeds[is.na(juntas.plantas.fit$seeds)] <- 0

total.corr.pl.s <- spline.correlog(x=juntas.plantas.fit$x_coor2, y=juntas.plantas.fit$y_coor2,
                                   z=juntas.plantas.fit$seeds, resamp=100, quiet=TRUE)
plot(total.corr.pl.s, main= "Spatial Autocorrelation plant fitness across plots")

moran.test(juntas.plantas.fit$seeds,mat2listw(plots.dists.inv)) # I= 3.991074e-03
moran.test(juntas.plantas.fit$seeds, nb2listw(w5)) # vecinos, I=  -0.0089702967
moran.plot(juntas.plantas.fit$seeds,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation plants fitness across plots")

par(mfrow=c(3,3))
plot(chfu.corr.s, main= " CHFU fitness distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(lema.corr.s, main= " LEMA fitness distributionfitness across plots",xlab="Distance (m)", ylab="Correlation")
plot(pupa.corr.s, main= " PUPA fitness distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(mesu.corr.s, main= " MESU fitness distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(scla.corr.s, main= " SCLA fitness distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(soas.corr.s, main= " SOAS fitness distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(spru.corr.s, main= " SPRU fitness distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(chmi.corr.s, main= " Spatial Autocorrelation CHMI fitness across plots",xlab="Distance (m)", ylab="Correlation")
plot(total.corr.pl.s, main= "Plant fitness distribution across plots",xlab="Distance (m)", ylab="Correlation")
par(mfrow=c(1,1))


##################################################3. GLM#######################################################################
#control for the lme
lCtr <- lmeControl(maxIter = 5000, msMaxIter = 5000, tolerance = 1e-9, niterEM = 250, msMaxEval = 200)#this lmecontrol is
#                   going to be used in all the lme models for all the species
#>CHFU
CHFU.vis <- subset(data.spread.visitors, Plant == "CHFU")
CHFU.vis$seed <-  as.numeric(CHFU.vis$seed)#There are 0 seeds, I'm going to change them to 1, to can do the log
CHFU.vis$seed[CHFU.vis$seed== 0]<- 0.01 #to check the numer of seeds that are 0
CHFU.vis$log.seed <- log(CHFU.vis$seed) #I just tried the model of CHFU seeds without the log, and the resiuals are worst than the log, also the 
#                                               Q-Q plot

CHFU.vis$subplot <- as.factor(CHFU.vis$subplot)
#fit random structure
m1<- lme(log.seed ~ 1, data= CHFU.vis,random = ~1 |plot, control=lCtr,
         method = "ML")
m2 <- lme(log.seed ~ 1, data= CHFU.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")

m3 <- lme(log.seed ~ 1, data= CHFU.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")

m4 <- lme(log.seed ~ 1, data= CHFU.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(m1, m2, m3, m4) #best model is m1, without coordenates
#IS THIS CORRECT? CAN YOU USE AIC (in models with method = ML) FOR THIS? I am asking as I don't know.----

options(na.action = "na.fail")

#final model for CHFU
head(as.data.frame(CHFU.vis))
m.prueba.chufu.3 <- lme(log.seed ~ Beetle+Fly+ Bee +neigh_inter.1m*neigh_intra.1m, 
                        data= CHFU.vis, random = ~1 |plot, control=lCtr,
                         method = "ML")
summary(m.prueba.chufu.3) #por que solo 37 obs?
#por que solo inter's a 1m? Pense que comparabamos a diferentes escalas, o a 7.5 sino...
#visits are per individual, which may be biasing our estimates.
#QUESTION why drege the model, and not just presenting the full model?
m.prueba_sec.chufu.2 <- dredge(m.prueba.chufu.3, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(m.prueba_sec.chufu.2, "rank.call"))
fmList.prueba.chufu.2 <- get.models(m.prueba_sec.chufu.2, 1:6) 
summary(model.avg(fmList.prueba.chufu.2))# neigh_intra.1m. beetle and fly almost sig negatively
r.squaredGLMM(m.prueba.chufu.3)

residplot(m.prueba.chufu.3)


CHFU.vis$fittedvalues <- fitted(m.prueba.chufu.3)
g.chfu <- ggplot(CHFU.vis, aes(x = neigh_intra.1m))+
  geom_point(aes(y=log.seed))+
  geom_smooth(method = "lm",aes(y=fittedvalues))+
  ggtitle("CHFU fitness with neighbors intra at 1m")+
  ylab("Number of seeds (log)")+
  xlab("Number of neighbors intra at 1m")+
  theme_light()
g.chfu #in this graph i just add the fitted values of the model


#ggsave(filename = paste("Graphics/2020/CHFU.fitness.pdf",sep=""),
 #   plot = g.chfu,width = 9,height = 3,dpi = 600)


#>LEMA
LEMA.vis <- subset(data.spread.visitors, Plant == "LEMA") 
LEMA.vis$seed <-  as.numeric(LEMA.vis$seed) #There are 0 seeds, I'm going to change them to 1, to can do the log
LEMA.vis$log.seed <- log(LEMA.vis$seed)

#random structure
l1<- lme(log.seed ~ 1, data= LEMA.vis,random = ~1 |plot, control=lCtr,
         method = "ML")
l2 <- lme(log.seed ~ 1, data= LEMA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML") 
l3<- lme(log.seed ~ 1, data= LEMA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
l4 <- lme(log.seed ~ 1, data= LEMA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(l1, l2, l3, l4) #best model is l2

options(na.action = "na.fail")

m.prueba.lema.2 <- lme(log.seed ~ Beetle+Fly+ Bee +neigh_inter.1m+neigh_intra.1m, data= LEMA.vis, random = ~1 |plot, control=lCtr,
                       corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")# This model includes the log.seed
#                                                               because, with only the seeds, the model have an error of convergence. 
summary(m.prueba.lema.2)
m.prueba_sec.lema.2 <- dredge(m.prueba.lema.2, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(m.prueba_sec.lema.2, "rank.call"))
fmList.prueba.lema.2 <- get.models(m.prueba_sec.lema.2, 1:4) 
summary(model.avg(fmList.prueba.lema.2))# neigh_inter*

r.squaredGLMM(m.prueba.lema.2)
residplot(m.prueba.lema.2)
LEMA.vis$fittedvalues <- fitted(m.prueba.lema.2)
b <- ggplot(LEMA.vis, aes(x = neigh_inter.1m))+
    geom_point(aes(y= log.seed))+
    geom_smooth(method = "lm",aes(y=fittedvalues))+
    ylab("Number of seeds (log)")+
    xlab("Number of neighbors inter at 1m")+
    ggtitle("LEMA fitness with neighbors inter at 1m")+
  theme_light()

b #outlayer!

#ggsave(filename = paste("Graphics/2020/LEMA.fitness.pdf",sep=""),
 #      plot = b,width = 9,height = 3,dpi = 600)

#>PUPA
PUPA.vis <- subset(data.spread.visitors, Plant == "PUPA") 
PUPA.vis$seed <-  as.numeric(PUPA.vis$seed) 
PUPA.vis$log.seed <- PUPA.vis$seed[PUPA.vis$seed == 0] <- 1 
PUPA.vis$log.seed <- log(PUPA.vis$seed)

#random structure
p1<- lme(seed ~ 1, data= PUPA.vis,random = ~1 |plot, control=lCtr,
         method = "ML")
p2 <- lme(seed ~ 1, data= PUPA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML") 
p3<- lme(seed ~ 1, data= PUPA.vis, random = ~1 |plot, control=lCtr,
         corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
p4 <- lme(seed ~ 1, data= PUPA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(p1, p2, p3, p4) #best model is p1

options(na.action = "na.fail")

#I have two models because I'm not sure of which model is best seeing the residuals. The log model or without log?
m.prueba.pupa.2 <- lme(log.seed ~ Beetle+Fly+ Bee+Butterfly +neigh_inter.1m+neigh_intra.1m, data= PUPA.vis, random = ~1 |plot, control=lCtr,
                       method = "ML")
#m.prueba.pupa.6 <- lme(seed ~ Beetle+Fly+ Bee+Butterfly +neigh_inter.1m+neigh_intra.1m, data= PUPA.vis, random = ~1 |plot, control=lCtr,
 #                      method = "ML")
plot(m.prueba.pupa.2) #looks good, use log.
#plot(m.prueba.pupa.6) #Not good.

m.prueba_sec.pupa.2 <- dredge(m.prueba.pupa.2, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(m.prueba_sec.pupa.2, "rank.call"))
fmList.prueba.pupa.2 <- get.models(m.prueba_sec.pupa.2, 1:4) 
summary(model.avg(fmList.prueba.pupa.2))# maybe neigh intra 1m
r.squaredGLMM(m.prueba.pupa.2)
residplot(m.prueba.pupa.2) # I have doubts with which of them have the best distribution of residuals. I thin that with the log 
#                               it is better
#residplot(m.prueba.pupa.6) #clearly, this is not good, the trend is too pronounced.



PUPA.vis$fittedvalues <- fitted(m.prueba.pupa.2)
b.p <- ggplot(PUPA.vis, aes(x = neigh_inter.1m))+
  geom_point(aes(y= log.seed))+
  geom_smooth(method = "lm",aes(y=fittedvalues))+
  ylab("Number of seeds (log)")+
  xlab("Number of neighbors intra at 1m")+
  ggtitle("PUPA fitness with neighbors intra at 1m")+
  theme_light()
b.p


#ggsave(filename = paste("Graphics/2020/PUPA.fitness.pdf",sep=""),
 #      plot = b.p,width = 9,height = 3,dpi = 600)
#>MESU
MESU.vis <- subset(data.spread.visitors, Plant == "MESU") #only 1 row
#>cete
CETE.vis <- subset(data.spread.visitors, Plant == "CETE") #only 6 row
#>spru
spru.vis <- subset(data.spread.visitors, Plant == "SPRU") # 0 row
#>SOAS
soas.vis <- subset(data.spread.visitors, Plant == "SOAS") #0 row
#>scla
scla.vis <- subset(data.spread.visitors, Plant == "SCLA") #0 row
#>bema
BEMA.vis <- subset(data.spread.visitors, Plant == "BEMA") #only 3 row
#>chmi
chmi.vis <- subset(data.spread.visitors, Plant == "CHMI") #only 1 row


#> visits lme----

head(data.spread.visitors)
#I'm going to transform the number of visits per group into the log. 
data.spread.visitors$log.fly<- log(data.spread.visitors$Fly)
data.spread.visitors$log.beetle<- log(data.spread.visitors$Beetle)
data.spread.visitors$log.butterfly<- log(data.spread.visitors$Butterfly)
data.spread.visitors$log.bee<- log(data.spread.visitors$Bee)


vis.all <- data %>% group_by(plot, subplot, visits.total, Group, x_coor2, y_coor2, neigh_inter.plot, neigh_intra.plot,
                           neigh_intra.3m, neigh_inter.3m, neigh_inter.1m, neigh_intra.1m,
                           neigh_inter.7.5, neigh_intra.7.5) %>% summarise (visits = sum(visits.total))%>%
    ungroup()

#Beetle visits
Bet.vis <- subset(vis.all, Group== "Beetle")
#In Bet.vis I have some coordenates that are not unique, for that I change in 0.1 the y_coord in other to have different coordenates and the model could 
#           converge. The next lines in the code are for changing these y coordenates.
#YOU CAN DO THIS BY USING:
#Bet.vis$x_coor2 <-  jitter(Bet.vis$x_coor2)

Bet.vis[37, 6]<-  57.6
Bet.vis[58, 6]<-  289.1
Bet.vis[71, 6]<-  274.6
Bet.vis[74, 6]<-  271.6
Bet.vis[76, 6]<-  273.1
Bet.vis[78, 6]<-  274.6
Bet.vis[80, 6]<-  270.1
Bet.vis[84, 6]<-  274.6
Bet.vis[85, 6]<-  274.7
Bet.vis[87, 6]<-  271.6
Bet.vis[91, 6]<-  286.1
Bet.vis[93, 6]<-  287.6
Bet.vis[95, 6]<-  289.1
Bet.vis[97, 6]<-  286.1
Bet.vis[99, 6]<-  287.6
Bet.vis[100, 6]<-  287.7
Bet.vis[102, 6]<-  289.1
Bet.vis[105, 6]<-  286.1
Bet.vis[110, 6]<-  287.6
Bet.vis[112, 6]<-  289.1
Bet.vis[114, 6]<-  290.6

#random structure
c.vis.bet.1<- lme(visits ~ 1, data= Bet.vis,random = ~1 |plot, control=lCtr,
                method = "ML")
c2.vis.bet.1 <- lme(visits ~ 1, data= Bet.vis, random = ~1 |plot, control=lCtr,
                  corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
c3.vis.bet.1<- lme(visits ~ 1, data= Bet.vis, random = ~1 |plot, control=lCtr,
                 corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
c4.vis.bet.1 <- lme(visits ~ 1, data= Bet.vis, random = ~1 |plot, control=lCtr,
                  corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(c.vis.bet.1, c2.vis.bet.1, c3.vis.bet.1, c4.vis.bet.1)#best model 1, I check it with the log(visits) and also the best model is the first one.

k <- lme(log(visits) ~ neigh_inter.1m+ neigh_intra.1m, data= Bet.vis,random = ~1 |plot, control=lCtr,
    method = "ML")
#ok, ya veo, las visitas a una planta (luego discutimos como están calculadas) dependen de inter y intra
#POR TANTO; NO TENDRIA QUE IR ESPECIE en Random?----
#MIRA MI COMENT EN GDOCS SOBRE LA REDUNDANCIA DE ESTE MODELO
residplot(k) #i just tried also the model without the log, and the residuals and the q-q plot have a worst distribution
m.prueba_sec.k <- dredge(k, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(m.prueba_sec.k, "rank.call"))
fmList.prueba.k <- get.models(m.prueba_sec.k, 1:4) 
summary(model.avg(fmList.prueba.k))#neigh intra 1m

Bet.vis$fittedvalues <- fitted(k)
k.bet <- ggplot(Bet.vis, aes(x = neigh_intra.1m))+
    geom_point(aes(y= log(visits)))+
    geom_smooth(method = "lm",aes(y=fittedvalues))+
    ggtitle("Beetle visits with neighbors intra at 1m")+
  ylab("Number of visits (log)")+
  xlab("Number of neighbors intra at 1m")+
  theme_light()
k.bet


#ggsave(filename = paste("Graphics/2020/Beetle.neigh.pdf",sep=""),
 #      plot = k.bet,width = 9,height = 3,dpi = 600)

#flies visits
fly.vis <- subset(vis.all, Group== "Fly") #Here I have also same coordenates for different rows. I need to change them. I'm 
#                                           going to change the y coordenates in 0.1 as well.

fly.vis[2, 6]<-  5.1
fly.vis[6, 6]<-  3.6
fly.vis[9, 6]<-  28.1
fly.vis[10, 6]<-  28.2
fly.vis[12, 6]<-  29.6
fly.vis[20, 6]<-  29.6
fly.vis[28, 6]<-  56.1
fly.vis[37, 6]<-  292.1
fly.vis[39, 6]<-  270.1
fly.vis[47, 6]<-  270.1
fly.vis[52, 6]<-  271.6
fly.vis[53, 6]<-  271.7
fly.vis[55, 6]<-  274.6
fly.vis[61, 6]<-  286.1
fly.vis[65, 6]<-  286.1

#random structure
c.vis.fly.1<- lme(visits ~ 1, data= fly.vis,random = ~1 |plot, 
                  method = "ML")
c2.vis.fly.1 <- lme(visits ~ 1, data= fly.vis, random = ~1 |plot, 
                    corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
c3.vis.fly.1<- lme(visits ~ 1, data= fly.vis, random = ~1 |plot, control=lCtr,
                   corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
c4.vis.fly.1 <- lme(visits ~ 1, data= fly.vis, random = ~1 |plot, 
                    corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
                    #the control of the model is the one that do the model to not work in that case. 
AIC(c.vis.fly.1, c2.vis.fly.1, c3.vis.fly.1, c4.vis.fly.1)# the first one. I also check it with the log in visits and the best 
#                                                           model is the same.

k1 <- lme(log(visits) ~ neigh_inter.1m+ neigh_intra.1m, data= fly.vis,random = ~1 |plot, control=lCtr,
           method = "ML") 
residplot(k1)#The residuals and the Q-Q plot are better with the log in the model
m.prueba_sec.k1 <- dredge(k1, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(m.prueba_sec.k1, "rank.call"))
fmList.prueba.k1 <- get.models(m.prueba_sec.k1, 1:4) 
summary(model.avg(fmList.prueba.k1))#neigh intra 1m


fly.vis$fittedvalues <- fitted(k1)
k.fly <- ggplot(fly.vis, aes(x = neigh_intra.1m))+
    geom_point(aes(y= log(visits)))+
    geom_smooth(method = "lm",aes(y=fittedvalues))+
    ggtitle("Fly visits with neighbors intra at 1m")+
  ylab("Number of visits (log)")+
  xlab("Number of neighbors intra at 1m")+
  theme_light()
k.fly


#ggsave(filename = paste("Graphics/2020/Fly.neigh.pdf",sep=""),
 #      plot = k.fly,width = 9,height = 3,dpi = 600)

##butterflies
but.vis <- subset(vis.all, Group== "Butterfly") #solo 3 entradas
#bees
bee.vis <- subset(vis.all, Group== "Bee")#I have rows with the same coordenates. I'll fix it in the next lines

bee.vis[4, 6]<-  28.1
bee.vis[6, 6]<-  32.6
bee.vis[8, 6]<-  28.1
bee.vis[11, 6]<-  28.1
bee.vis[16, 6]<-  60.6
bee.vis[28, 6]<-  292.1
bee.vis[32, 6]<-  273.1
bee.vis[43, 6]<-  290.6

#random structure
c.vis.bee.1<- lme(visits ~ 1, data= bee.vis,random = ~1 |plot, control=lCtr,
                  method = "ML")
c2.vis.bee.1 <- lme(visits ~ 1, data= bee.vis, random = ~1 |plot, control=lCtr,
                    corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
c3.vis.bee.1<- lme(visits ~ 1, data= bee.vis, random = ~1 |plot, control=lCtr,
                   corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
c4.vis.bee.1 <- lme(visits ~ 1, data= bee.vis, random = ~1 |plot, control=lCtr,
                    corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")

AIC(c.vis.bee.1, c2.vis.bee.1, c3.vis.bee.1, c4.vis.bee.1)#the 1st model is the best. I also try with the log in the visits, same
#                                                                   results

k2.bee <- lme(log(visits) ~ neigh_inter.1m+ neigh_intra.1m, data= bee.vis,random = ~1 |plot, control=lCtr,
           method = "ML")
residplot(k2.bee) #the resiudals are better with the log. 

m.prueba_sec.k2.bee <- dredge(k2.bee, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(m.prueba_sec.k2.bee, "rank.call"))
fmList.prueba.k2.bee <- get.models(m.prueba_sec.k2.bee, 1:4) 
summary(model.avg(fmList.prueba.k2.bee))#neigh intra 1m***


bee.vis$fittedvalues <- fitted(k2.bee)
k.bee <- ggplot(bee.vis, aes(x = neigh_intra.1m))+
    geom_point(aes(y= log(visits)))+
    geom_smooth(method = "lm",aes(y=fittedvalues))+
    ggtitle("Bee visits with neighbors intra at 1m")+
  ylab("Number of visits (log)")+
  xlab("Number of neighbors intra at 1m")+
  theme_light()
k.bee
#ggsave(filename = paste("Graphics/2020/Bee.neigh.pdf",sep=""),
 #      plot = k.bee,width = 9,height = 3,dpi = 600)

#SEM----
#>CHFU.
#First I'm going to do the SEM that have the Neighbors intra and inter separate (2 columns) at Plot, 3m,1m and 7.5cm
#The group argument in the model is the distance level: plot (letter a), 3m (letter c), 1m (letter e), 7.5cm (letter g). 
#reminder: neigbors.intra.inter.split is the dataframe with in distance_total have the levels of plot, 3m,2m, 7.5cm
# and the dataframe neigbors.intra.inter is the datafreme that in distance have: plot_inter, plot_intra, 3m_intra, 3m_inter
# 1m_inter, 1m_intra, 7.5_inter, 7.5_intra.

modelo.chfu <- ' #This model is for the dataframe neigbors.intra.inter.split
#regresions
#Plant_fitness = ~ seed 
seed ~ Fly
seed ~ Beetle
seed ~ n_neighbors_inter
seed ~ n_neighbors_intra
Fly ~  n_neighbors_intra
Fly ~ n_neighbors_inter
Beetle ~  n_neighbors_intra
Beetle ~ n_neighbors_inter
#intercept
'
modelo1.chfu <- ' #This model is for the dataframe neigbors.intra.inter
#regresions
#Plant_fitness = ~ seed 
seed ~ Fly
seed ~ Beetle
seed ~ n_neighbors
Fly ~  n_neighbors
Beetle ~ n_neighbors
#intercept
'
#with the nighbors splited in Plot, 3m, 1m, and 7.5
CHFU.sem <- subset(neigbors.intra.inter.split, Plant== "CHFU")
CHFU.sem1 <- CHFU.sem[,c( "seed", "fruit", "x_coor2", "y_coor2", "Bee","Beetle",
                 "Fly", "distance_total", "n_neighbors_inter" , "n_neighbors_intra")] #there are no visits of 
#                                        butterflies, so I just need to eliminate it to can do the corregram
CHFU.sem1$seed <- rescale(CHFU.sem1$seed) 
CHFU.sem1$n_neighbors_inter <- rescale(CHFU.sem1$n_neighbors_inter) 
CHFU.sem1$n_neighbors_intra <- rescale(CHFU.sem1$n_neighbors_intra) 
corrgram(CHFU.sem1, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
          diag.panel=panel.density, pch=16, lty=1, main="CHFU correlations") #to check the correlation between the variables

multigroup.1 <- sem(modelo.chfu, CHFU.sem1, group = "distance_total") 
summary(multigroup.1, standardize=T)
varTable(multigroup.1)
fitMeasures(multigroup.1, c("cfi","rmsea","srmr", "pvalue"))
print(modindices(multigroup.1))

multigroup2.constrained <- sem(modelo.chfu, CHFU.sem1, group = "distance_total", group.equal = c("intercepts", "regressions"))
summary(multigroup2.constrained)

#I have to compare both models to see which is the best one.
anova(multigroup.1, multigroup2.constrained)#better the NO contrained
par(mfrow=c(1,1))
semPaths(multigroup.1) #aqui tengo que plotear el modelo que mejor me ha salido en el paso anterior

#Now I will try the same model but with all the levels of neighbors together, I mean, plot_intra, plot_inter...
CHFU.sem.t <- subset(neigbors.intra.inter, Plant== "CHFU")
CHFU.sem1.t <- CHFU.sem.t[,c( "seed", "fruit", "x_coor2", "y_coor2", "Bee","Beetle",
                          "Fly", "distance", "n_neighbors")]
CHFU.sem1.t$seed <- rescale(CHFU.sem1.t$seed) 
CHFU.sem1.t$n_neighbors <- rescale(CHFU.sem1.t$n_neighbors) 
corrgram(CHFU.sem1.t, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="CHFU correlations")
multigroup.1.t <- sem(modelo1.chfu, CHFU.sem1.t, group = "distance") 
summary(multigroup.1.t, standardize=T)
varTable(multigroup.1.t)
fitMeasures(multigroup.1.t, c("cfi","rmsea","srmr", "pvalue"))
print(modindices(multigroup.1.t))

multigroup2.constrained.t <- sem(modelo1.chfu, CHFU.sem1.t, group = "distance", group.equal = c("intercepts", "regressions"))
summary(multigroup2.constrained.t)
anova(multigroup.1.t, multigroup2.constrained.t)#better the NO constrained
par(mfrow=c(1,1))
semPaths(multigroup.1.t)

#> LEMA 

modelo.lema<- ' #This model is for the dataframe neigbors.intra.inter.split
#regresions
#Plant_fitness = ~ seed 
seed ~ Bee
seed ~ Beetle
seed ~ n_neighbors_inter
seed ~ n_neighbors_intra
Bee ~  n_neighbors_intra
Bee ~ n_neighbors_inter
Beetle ~  n_neighbors_intra
Beetle ~ n_neighbors_inter
#intercept
'
modelo1.lema <- ' #This model is for the dataframe neigbors.intra.inter
#regresions
#Plant_fitness = ~ seed 
seed ~ Bee
seed ~ Beetle
seed ~ n_neighbors
Bee ~  n_neighbors
Beetle ~ n_neighbors
#intercept
'

LEMA.sem <- subset(neigbors.intra.inter.split, Plant== "LEMA")
LEMA.sem <- LEMA.sem[,c("seed", "fruit", "x_coor2", "y_coor2", "Bee","Beetle",
                          "distance_total", "n_neighbors_inter" , "n_neighbors_intra")]# I only include the predictors
#                                       that appear in the lme model
LEMA.sem$seed <- rescale(LEMA.sem$seed) 
LEMA.sem$n_neighbors_inter <- rescale(LEMA.sem$n_neighbors_inter) 
LEMA.sem$n_neighbors_intra <- rescale(LEMA.sem$n_neighbors_intra) 
corrgram(LEMA.sem, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="LEMA correlations") #to check the correlation between the variables

multigroup.1.lema <- sem(modelo.lema, LEMA.sem, group = "distance_total") 
summary(multigroup.1.lema, standardize=T)
varTable(multigroup.1.lema)
fitMeasures(multigroup.1.lema, c("cfi","rmsea","srmr", "pvalue"))
print(modindices(multigroup.1.lema))

multigroup2.constrained.lema <- sem(modelo.lema, LEMA.sem, group = "distance_total", group.equal = c("intercepts", "regressions"))
summary(multigroup2.constrained.lema)
anova(multigroup.1.lema, multigroup2.constrained.lema)#better the constrained
par(mfrow=c(1,1))
semPaths(multigroup2.constrained.lema)


#Now I will try the same model but with all the levels of neighbors together, I mean, plot_intra, plot_inter...
LEMA.sem.t <- subset(neigbors.intra.inter, Plant== "LEMA")
LEMA.sem.t <- LEMA.sem.t[,c( "seed", "fruit", "x_coor2", "y_coor2", "Bee","Beetle",
                             "distance", "n_neighbors")]# I only include the predictors
#                                       that appear in the lme model
LEMA.sem.t$seed <- rescale(LEMA.sem.t$seed) 
LEMA.sem.t$n_neighbors<- rescale(LEMA.sem.t$n_neighbors) 
corrgram(LEMA.sem.t, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="LEMA correlations") #to check the correlation between the variables

multigroup.1.lema.t <- sem(modelo1.lema, LEMA.sem.t, group = "distance") 
summary(multigroup.1.lema.t, standardize=T)
varTable(multigroup.1.lema.t)
fitMeasures(multigroup.1.lema.t, c("cfi","rmsea","srmr", "pvalue"))
print(modindices(multigroup.1.lema.t))

multigroup2.constrained.lema.t <- sem(modelo1.lema, LEMA.sem.t, group = "distance", group.equal = c("intercepts", "regressions"))
summary(multigroup2.constrained.lema.t)
anova(multigroup.1.lema.t, multigroup2.constrained.lema.t)#multigroup constrained
par(mfrow=c(1,1))
semPaths(multigroup2.constrained.lema.t)

#PUPA 

modelo.pupa<- ' #This model is for the dataframe neigbors.intra.inter.split
#regresions
#Plant_fitness = ~ seed 
seed ~ Fly
seed ~ Beetle
seed ~ n_neighbors_inter
seed ~ n_neighbors_intra
Fly ~ n_neighbors_intra
Fly ~ n_neighbors_inter
Beetle ~ n_neighbors_intra
Beetle ~ n_neighbors_inter
#intercept
'
modelo1.pupa <- ' #This model is for the dataframe neigbors.intra.inter
#regresions
#Plant_fitness = ~ seed 
seed ~ Fly
seed ~ Beetle
seed ~ n_neighbors
Fly ~  n_neighbors
Beetle ~ n_neighbors
#intercept
'
PUPA.sem <- subset(neigbors.intra.inter.split, Plant== "PUPA")
PUPA.sem <- PUPA.sem[,c( "seed", "fruit", "x_coor2", "y_coor2", "Fly","Beetle",
                         "distance_total", "n_neighbors_inter" , "n_neighbors_intra")]# I only include the predictors
#                                       that appear in the lme model
PUPA.sem$seed <- rescale(PUPA.sem$seed) 
PUPA.sem$n_neighbors_inter <- rescale(PUPA.sem$n_neighbors_inter) 
PUPA.sem$n_neighbors_intra <- rescale(PUPA.sem$n_neighbors_intra) 
corrgram(PUPA.sem, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="PUPA correlations") #to check the correlation between the variables

multigroup.1.pupa <- sem(modelo.pupa, PUPA.sem, group = "distance_total") 
summary(multigroup.1.pupa, standardize=T)
varTable(multigroup.1.pupa)
fitMeasures(multigroup.1.pupa, c("cfi","rmsea","srmr", "pvalue"))
print(modindices(multigroup.1.pupa))

multigroup2.constrained.pupa <- sem(modelo.pupa, PUPA.sem, group = "distance_total", group.equal = c("intercepts", "regressions"))
summary(multigroup2.constrained.pupa)
anova(multigroup.1.pupa, multigroup2.constrained.pupa)#better the NO constrained
par(mfrow=c(1,1))
semPaths(multigroup.1.pupa)


#Now I will try the same model but with all the levels of neighbors together, I mean, plot_intra, plot_inter...
PUPA.sem.t <- subset(neigbors.intra.inter, Plant== "PUPA")
PUPA.sem.t <- PUPA.sem.t[,c( "seed", "fruit", "x_coor2", "y_coor2", "Fly","Beetle",
                             "distance", "n_neighbors")]# I only include the predictors
#                                       that appear in the lme model
PUPA.sem.t$seed <- rescale(PUPA.sem.t$seed) 
PUPA.sem.t$n_neighbors<- rescale(PUPA.sem.t$n_neighbors) 
corrgram(PUPA.sem.t, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="PUPA correlations") #to check the correlation between the variables

multigroup.1.pupa.t <- sem(modelo1.pupa, PUPA.sem.t, group = "distance") 
summary(multigroup.1.pupa.t, standardize=T)
varTable(multigroup.1.pupa.t)
fitMeasures(multigroup.1.pupa.t, c("cfi","rmsea","srmr", "pvalue"))#good pvalue
print(modindices(multigroup.1.pupa.t))

multigroup2.constrained.pupa.t <- sem(modelo1.pupa, PUPA.sem.t, group = "distance", group.equal = c("intercepts", "regressions"))
summary(multigroup2.constrained.pupa.t)
anova(multigroup.1.pupa.t, multigroup2.constrained.pupa.t)#Best model no constrained
par(mfrow=c(1,1))
semPaths(multigroup.1.pupa.t)

