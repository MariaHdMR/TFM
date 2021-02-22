#Load the data for the BES analyses, make the transformations and get the global dataframe to can work with. 
#Caculate the neigbors. David Garcia-Callejas code ----
#We decide to only have one period of phenology. I used the phenology in order to calculate the neigbors. For calculating the neighbors I used the 
#code created for David Garcia-Callejas call CaracolesNeighbors. 

# neighbours of each focal at different distances:
# d1- 7.5 cm, using competition data
# d2- 1m^2, subplot data, using abundances data
# d3- 8 most nearest subplots, using abundances data
# d4- whole plot

# NOTE: subplots on the edges of the plot
# do not have 8 neighbouring subplots
# perhaps it's better to remove them:
# A1,B1,C1,D1,E1,F1
# A1,A2,A3,A4,A5,A6
# A6,B6,C6,D6,E6,F6
# F1,F2,F3,F4,F5,F6

# load packages for calculating the neighbors
library(tidyverse)
library(dplyr)  
library(spdep) #this is for calculating the neighbors for Moran's I
library(corrgram) # to do the corrgram

#source("R/CaracolesNeighbours.R")
source("Analisis_BES/CaracolesNeighbours.R")
# read data
comp <- read.csv2("Analisis_BES/data/competition_caracoles2020.csv",stringsAsFactors = FALSE)
comp<- subset(comp, plot != 4)#This year I don't have for some species the abundance of the plot 4, for that reason I remove it for all the dataframes
str(comp)
abund <- read.csv2("Analisis_BES/data/Abundances_2020.csv",stringsAsFactors = FALSE)
abund <- dplyr::arrange(abund,year,month,day,plot,subplot,species)
abund$unique_id <- paste(abund$plot, abund$subplot, abund$species, sep="_")
abund <- abund %>% distinct(unique_id,.keep_all = TRUE)
abund2 <- abund %>% group_by(year, plot, subplot,species, unique_id) %>% summarise (indv = sum(individuals))%>%
    ungroup()
abund2<- subset(abund2, plot != 4)
abund.1 <- abund2[,c("year","plot", "subplot","species", "indv")]

# convert abundances to wide format
abund.wide <- abund.1 %>% spread(key = species,value = indv
                                 ,fill = 0)
abund.wide <- na.omit(abund.wide)

# set of species that are sampled in both datasets
all.sp <- sort(intersect(unique(comp$focal),unique(abund$species)))

# create results dataframe
neigh <- data.frame(comp[,c("year","plot","subplot","focal")])

# neighbours of the same species
neigh$d1.intra <- 0
neigh$d2.intra <- 0
neigh$d3.intra <- 0
neigh$d4.intra <- 0
# neighbours of different species
neigh$d1.inter <- 0
neigh$d2.inter <- 0
neigh$d3.inter <- 0
neigh$d4.inter <- 0

# go through each individual
for(i.focal in 1:nrow(comp)){
    # identity of this focal
    my.sp <- comp$focal[i.focal]
    other.sp <- all.sp[which(all.sp != my.sp)]
    
    # neighbours at 7.5cm
    neigh$d1.intra[i.focal] <- comp[i.focal,which(names(comp) == my.sp)]
    neigh$d1.inter[i.focal] <- sum(comp[i.focal,which(names(comp) %in% other.sp)])
    if(length(which(abund.wide$plot == neigh$plot[i.focal])) == 0) next
    if (length(which(abund.wide$subplot == neigh$subplot[i.focal])) == 0) next
    if (length(which(abund.wide$year == neigh$year[i.focal])) == 0) next
    
    # neighbours in subplot using abundances
    my.subplot <- which(abund.wide$year == neigh$year[i.focal] & 
                            abund.wide$plot == neigh$plot[i.focal] &
                            abund.wide$subplot == neigh$subplot[i.focal])
    if (length(my.subplot) == 0) next
    neigh$d2.intra[i.focal] <- sum(abund.wide[my.subplot,which(names(abund.wide) == my.sp)])
    neigh$d2.inter[i.focal] <- sum(abund.wide[my.subplot,which(names(abund.wide) %in% other.sp)])
    
    # neighbours in 8 subplots
    # if subplot is on a side/corner, there are less neighbours
    my.neigh_ID <- CaracolesNeighbours(comp$subplot[i.focal],"M")
    # 
    
    my.neigh <- which(abund.wide$year == neigh$year[i.focal] & 
                          abund.wide$plot == neigh$plot[i.focal] &
                          abund.wide$subplot %in% my.neigh_ID)
    neigh$d3.intra[i.focal] <- sum(abund.wide[my.neigh,which(names(abund.wide) == my.sp)])
    neigh$d3.inter[i.focal] <- sum(abund.wide[my.neigh,which(names(abund.wide) %in% other.sp)])
    
    # neighbours in plot
    
    my.plot <- which(abund.wide$year == neigh$year[i.focal] & 
                         abund.wide$plot == neigh$plot[i.focal])
    
    neigh$d4.intra[i.focal] <- sum(abund.wide[my.plot,which(names(abund.wide) == my.sp)])
    neigh$d4.inter[i.focal] <- sum(abund.wide[my.plot,which(names(abund.wide) %in% other.sp)])
}

# convert to long format
# it's easier to do it separately 
# for intra and inter neighbours
neigh.long.intra <- tidyr::gather(neigh[,c("year","plot",
                                    "subplot","focal",
                                    "d1.intra","d2.intra",
                                    "d3.intra","d4.intra")],
                           key = "distance",
                           value = "neigh_intra",
                           d1.intra,d2.intra,d3.intra,d4.intra)
neigh.long.intra$distance <- sub(".intra","",neigh.long.intra$distance)

neigh.long.inter <- tidyr::gather(neigh[,c("year","plot",
                                    "subplot","focal",
                                    "d1.inter","d2.inter",
                                    "d3.inter","d4.inter")],
                           key = "distance",
                           value = "neigh_inter",
                           d1.inter,d2.inter,d3.inter,d4.inter)
neigh.long.inter$distance <- sub(".inter","",neigh.long.inter$distance)

# join again intra and inter
neigh.long <- dplyr::left_join(neigh.long.intra,neigh.long.inter)

# add a column to know if this subplot is an edge subplot
edges <- c("A1","B1","C1","D1","E1","F1",
           "A2","A3","A4","A5","A6",
           "B6","C6","D6","E6","F6",
           "F2","F3","F4","F5")
neigh.long$edge <- neigh.long$subplot %in% edges

# small test
tt <- neigh.long %>% 
    group_by(subplot) %>% 
    mutate(sum_intra = sum(neigh_intra),sum_inter = sum(neigh_inter))

ggplot(tt,aes(x = subplot,y = sum_intra)) + geom_point(aes(color = edge)) 

#write.csv2(neigh.long,file = "C:/Users/Cisco/Documents/TFM/Analisis_BES/data/focal_neighbours.2020_2nphenology.csv",row.names = FALSE)
#This dataframe is the focal_neighbors.2020_2ndphenology.csv , where I have all the neighbors. 

#load data and transforming----
plants.neigh <- read.table("Analisis_BES/data/focal_neighbours.2020_2nphenology.csv", header=T, sep=";")
head(plants.neigh)


#data of the coordenates of the plots
distances <- read.csv("Analisis_BES/data/caracolesplotposition.csv", sep = ";") 
head(distances) #In this data I have only the coordenates for one plot. Now I have to calculate for the rest plots

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

# preparacion de datos para analizar por los 8 vecinos mas cercanos --> w5. Asi vemos si se parecen mas 
# entre los vecinos que al resto en la I de Moran
disfinal$x_coor2 <- as.numeric(disfinal$x_coor2)
disfinal$y_coor2 <- as.numeric(disfinal$y_coor2)
w5 <- knn2nb(knearneigh(coordinates(disfinal[,3:4]), k=8))

#write.csv2(disfinal,file = "C:/Users/Cisco/Documents/TFM/Analisis_BES/data/distances.csv",row.names = FALSE)

#pollinators, abundances and competition data
FV <- read.table("Analisis_BES/data/raw_Pollinators_2020_1.csv", header=T, sep=";") #pollinators 2020
head(FV)
FV <- FV %>% group_by(Day, Month, Year, Plot, Subplot,Group,ID_Simple, Plant) %>% summarise (visits = sum(Visits))%>%
    ungroup()
FV$ID <- FV$ID_Simple
head(FV)

#In the alfonso data I have the fitness (number of seeds per one fruit) and the pollinators
alfonsodata<-  read.table("Analisis_BES/data/2020_NN_NEW_data_models_phenol_overlap.csv", header=T, sep=",")
head(alfonsodata)#This dataframe do not have the total seeds per individual. I need to calculate it, more down
alfonsodata<- alfonsodata[,c("Plot","Subplot","Plant","Seeds_GF","Fruit_GF", "visits_GF", "ID")]

alfonsodata$seed <- alfonsodata$Seeds_GF
alfonsodata$fruit <- alfonsodata$Fruit_GF
alfonsodata$visits <- alfonsodata$visits_GF
alfonsodata <- subset(alfonsodata, Plot != 4)

FV <- FV[,c("Day", "Month", "Year", "Plant", "ID_Simple", "Group")]
head(FV)# reminder, my ID-SImple is the ID of the data of Alfonso

FV$ID <- FV$ID_Simple
#Remove points from ID names
FV$ID <- sub("\\.", "", FV$ID)

# filter tabanidae
FV <- FV %>% filter(ID != "Tabanidae")

FV.al <- left_join(alfonsodata, FV,  by= c("ID", "Plant"))
FV.al <- distinct(FV.al)
FV.al<- FV.al %>% group_by(Day, Month, Year, Plot, Subplot, Group, Plant, seed, fruit) %>% summarise (vis = sum(visits))%>%
    ungroup() 


#select the plots and subplots that I want
head(FV.al)
FV.al <- FV.al %>% filter(!is.na(Plant),Plant!="0",Subplot!="OUT",Plant!="Ground")
FV.al <- subset(FV.al, Plot != "OUT")
FV.al <- subset(FV.al, Plot != 4)

FV.al$vis <- as.numeric(FV.al$vis) #necesary?
FV.al$seed <- as.numeric(FV.al$seed)
FV.al$fruit <- as.numeric(FV.al$fruit)

FV.al$seed[is.na(FV.al$seed)] <- 0
FV.al$fruit[is.na(FV.al$fruit)] <- 0
FV.al$Plant <- as.factor(FV.al$Plant)
FV.al$plot <- FV.al$Plot
FV.al$subplot <- FV.al$Subplot

abund$Plant <- abund$species

final <- left_join(FV.al, abund, by= c("plot", "subplot", "Plant"))
final <- final[,c("Day", "Month", "Year","plot","subplot","Group",  "vis","Plant","individuals", "seed","fruit")]
final$subplot <- as.factor(final$subplot)
final <- subset(final, plot != "OUT")
final <- subset(final, subplot != "OUT")

final$visits <- final$vis
final$individuals <- as.numeric(final$individuals)
final$unique_id <- paste(final$plot, final$subplot,final$Plant, sep="_")
final$subplot <- as.factor(final$subplot)
final$Plant <- as.factor(final$Plant)
#minimum has to be 1 individual of plants, so I change the 0 to 1
final$individuals[is.na(final$individuals)] <- 1

final$visits[final$visits== 0] <- 0.01 #in order to not have 0 to do the log I change the 0 values to 0,01
hist(log(final$visits)) #zero inflated...

#NEIGHBORS Data transformation

plants.neigh <- subset(plants.neigh,edge %in% c("FALSE"))
plants.neigh <- subset(plants.neigh, focal %in% c("SOAS","CHFU","LEMA","CHMI", "SCLA","BEMA","CETE","MESU","PUPA", "SPRU"))
plants.neigh1 <- plants.neigh[-which(duplicated(plants.neigh)), ] 
plants.neigh1$Plant <- plants.neigh1$focal
plants.neigh1$neigh_inter <- as.numeric(plants.neigh1$neigh_inter)
plants.neigh1$neigh_intra <- as.numeric(plants.neigh1$neigh_intra)
plants.neigh1$unique_id <- paste(plants.neigh1$plot, plants.neigh1$subplot,plants.neigh1$Plant,plants.neigh1$distance, sep="_")
neighbors <- plants.neigh1 %>% distinct(unique_id, .keep_all = TRUE)

#vecinos
neighbors <- neighbors[,c("plot","subplot","Plant","distance", "neigh_intra","neigh_inter")] 

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

#######flowers
flores<-  read.table("Analisis_BES/data/Flowers_Abundance_2020.csv", header=T, sep=";")
flores <- na.omit(flores)
head(flores)
flores.juntas <- tidyr::gather(flores, key = "Plant", value = "flowers", 6:23)
flores.juntas$plot <- flores.juntas$Plot
flores.juntas$subplot <- flores.juntas$Subplot
flores.juntas <- subset(flores.juntas, subplot != "OUT")

#nueva base de datos

flores.pol.fitness <- full_join(final, flores.juntas, by= c("Day", "Month", "Year", "plot", "subplot", "Plant") )#casi completa, me falta añadir vecinos y coordenadas
flores.pol.fitness_coord <- full_join(flores.pol.fitness, disfinal, by= c("plot", "subplot"))#ahora me toca añadir los vecinos
flores.pol.fitness_coord_vecinos <- full_join(flores.pol.fitness_coord, vecinos, by= c("plot", "subplot", "Plant"))

data <- flores.pol.fitness_coord_vecinos[,c("Day", "Month","plot","subplot","Group",  "vis","Plant","individuals", "seed","fruit", "visits", "flowers",
                         "x_coor2", "y_coor2", "neigh_inter.plot","neigh_intra.plot", "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m", 
                         "neigh_inter.7.5", "neigh_intra.7.5")]

data<- data[!is.na(data$neigh_inter.plot),]#esto lo he hecho para eliminar los NAs correspondientes a los vecinos. Elimino
#     estos NAs, porque los datos que se me quedan son los que he podido calcular los vecinos, recuerda, has quitado las ultimas lineas y filas
#     de los plots. 

sum(is.na(data$flowers))
#Ahora tengo que arreglar la columna de flores, ya que hay NAs que son 0, pero también hay NAs que son NAs. Las fechas 22 y 23 del mes 4 son NAs

df1 <- data
df1$flowers2 <- ifelse(!df1$Day %in% c(22,23) & !(df1$Month == 4) & is.na(df1$flowers), 0, df1$flowers) 

df1$visits.flower <- (df1$visits/df1$flowers2)#There are -Inf. Should I write 1 flower in the places that we have visits? 
df1$seed.indv <- (df1$seed)*(df1$fruit)
data <- df1 #Base de datos final
#write.csv2(data,file = "C:/Users/Cisco/Documents/TFM/Analisis_BES/data/Final_global_data.csv",row.names = FALSE)


data$unique_id <- paste(data$plot, data$subplot,data$Plant,data$Group, sep="_")
data <- data %>% distinct(unique_id, .keep_all = TRUE)
data.spread.visitors <- spread(data, Group, visits, fill = 0, convert = FALSE,
                              drop = TRUE, sep = NULL) 

#I want to see the correlations between the scale 7.5cm and 1m
scales <- data.spread.visitors[,c("neigh_inter.1m", "neigh_intra.1m", "neigh_inter.7.5", "neigh_intra.7.5")]
corrgram(scales, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.density, pch=16, lty=1, main="Correlations between 1m and 7.5cm")

#now I want to calculate the neighbors intra and inter separately.
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
neigbors.intra.inter.split<- cbind(inter.neigh, intra)# in this data I have the neighbors separate with the level plot, 3m, 1m and 7.5 cm
head(neigbors.intra.inter.split)


#write.csv2(neigbors.intra.inter.split,file = "C:/Users/Cisco/Documents/TFM/Analisis_BES/data/Data_neighbors.intra.inter.split.check1.csv",row.names = FALSE)


#Final dataframes ----
#write.csv2(data,file = "Analisis_BES/data/Final_global_data.check1.csv",row.names = FALSE) 
#       Here I have all the neighbors together

#write.csv2(neigbors.intra.inter.split,file = "Analisis_BES/data/Data_neighbors.intra.inter.split.check1.csv",row.names = FALSE)
#       Here I have the neighbors separate in intra and inter

#write.csv2(disfinal,file = "Analisis_BES/data/distances.csv",row.names = FALSE)
#       dataframe with the distances across plots and subplots

