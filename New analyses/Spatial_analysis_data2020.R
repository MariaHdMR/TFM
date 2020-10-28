# code for 2020 spatial analysis
#libraries
library(tidyverse)
library(ggplot2)
library(spdep) #neighbors
library(tidyverse)
library(ape)
library(ncf)
#install.packages("spdep") -> for Moran's I
library(spdep)
library(reshape2)
library(vegan)
library(nlme)
library(lme4)
library(DHARMa)
library(scales)
#data
FV <- read.table("C:/Users/Cisco/Documents/TFM/data/Data_2020/raw_Pollinators_2020_1.csv", header=T, sep=";") #pollinators 2020
FV3 <- FV %>% group_by(Plot, Subplot,Group,ID, Plant) %>% summarise (visits = sum(Visits))%>%
    ungroup()
Ab <-read.table("C:/Users/Cisco/Documents/TFM/data/Data_2020/Abundances_2020.csv", header=T, sep=";")#plants abundance 2020
Ab <- subset(Ab, Plot != 4)
comp <- competencia <- read.table("C:/Users/Cisco/Documents/TFM/data/Data_2020/competition_caracoles2020.csv", header=T, sep=";")#competition 2020
comp <- subset(comp, Plot != 4)
fitness <- read.table("C:/Users/Cisco/Documents/TFM/data/Data_2020/Fitness_2020.csv", header=T, sep=";")
fitness <- subset(fitness, Plot != 4)
alfonsodata<-  read.table("C:/Users/Cisco/Documents/TFM/data/Data_2020/2020_NN_data_models_phenol_overlap.csv", header=T, sep=",")
alfonsodata<- alfonsodata[,c("Plot","Subplot","Plant","Seeds_GF","Fruit_GF", "visits_GF", "ID")]
alfonsodata$seed <- alfonsodata$Seeds_GF
alfonsodata$fruit <- alfonsodata$Fruit_GF
alfonsodata$Visits <- alfonsodata$visits_GF

todo <- full_join(FV3, alfonsodata, by= c("Plot", "Subplot", "Plant", "ID"))

fitness <- fitness[,c("Plot","Subplot","Plant","Seeds.Fruit","Mean.Seeds.Fruit")]

#select the floral visitors that i want
todo1 <- subset(todo, Group %in% c("Bee","Beetle","Butterfly","Fly"))
todo1 <- subset(todo1, Plot != "OUT")
todo1 <- subset(todo1, Subplot != "OUT")
todo1 <- subset(todo1, Plot != 4)
todo2 <- na.omit(todo1)
todo2 <- subset(todo1, Group %in% c("Bee","Beetle","Butterfly","Fly"))
todo2 <- todo2[,c("Plot","Subplot","Group", "ID", "Plant","visits", "seed","fruit")]


#esto siguiente es para corregir dos entradas que hay erroneas en semillas
#todo2[145, 6] <- 7
#todo2[275, 6] <- 2
#todo2[23,7] <- 0
#todo2[23,8] <- 0
#todo2[24,7] <- 0
#todo2[23,8] <- 0
#todo2[60,7] <- 754
#todo2[60,8] <- 13
#todo2[82,7] <- 60
#todo2[82,8] <- 1
#todo2[83,7] <- 60
#todo2[83,8] <- 1
#todo2[86,7] <- 0
#todo2[86,8] <- 0
#todo2[114,7] <- 0
#todo2[114,8] <- 0
#todo2[121,7] <- 172
#todo2[121,8] <- 43
#???mirar de aqui para arriba que no sean los datos de fitness, es decir, 1 fruto con tantas semillas
#todo2[127,7] <- 0
#todo2[127,8] <- 0
#todo2[128,7] <- 0
#todo2[128,8] <- 0
#todo2[130,7] <- 0
#todo2[130,8] <- 0

todo4 <- todo2 %>% group_by(Plot, Subplot, Group, Plant, seed, fruit) %>% summarise (vis = sum(visits))
#todo2[105,7] <- 207
#todo2[105,8] <- 9
#todo2[164,7] <- 0
#todo2[164,8] <- 0
#todo2[165,7] <- 0
#todo2[165,8] <- 0
#todo2[169,7] <- 0
#todo2[169,8] <- 0
#todo2[171,7] <- 0
#todo2[171,8] <- 0
#todo2[172,7] <- 0
#todo2[172,8] <- 0
#todo2[173,7] <- 0
#todo2[173,8] <- 0
#todo2[182,7] <- 0
#todo2[182,8] <- 0
#todo2[183,7] <- 0
#todo2[183,8] <- 0
#todo2[190,7] <- 0
#todo2[190,8] <- 0
todo2$seed[is.na(todo2$seed)] <- 0
todo2$fruit[is.na(todo2$fruit)] <- 0
todo2$Plant <- as.factor(todo2$Plant)
todo6 <- subset(todo2, Plant %in% c("BEMA","CETE","CHFU","CHMI", "LEMA", "MESU", "PUPA", "SCLA", "SOAS", "SPRU"))
todo6 <- na.omit(todo6)
#todo 6 serian los polinizadores + el fitness 
Ab$Plant <- Ab$species
todo6$plot <- todo6$Plot
todo6$subplot <- todo6$Subplot
todo.numplants <- left_join(todo6, Ab, by= c("plot", "subplot", "Plant"))
final <- todo.numplants[,c("plot","subplot","Group",  "visits","Plant","individuals", "seed","fruit")]
final$unique_id <- paste(final$plot, final$subplot,final$Plant,final$Group, sep="_")
final <- final %>% group_by(plot, subplot, Plant, Group, seed, fruit, individuals, unique_id) %>% summarise (num.visits = sum(visits))%>%
  group_by(unique_id)%>%
  distinct(unique_id, .keep_all=TRUE)%>%
  ungroup()

final$visits <- final$num.visits

final$visits <-as.numeric(final$visits)
final$individuals <- as.numeric(final$individuals)
#minimum has to be 1 individual of plants
final$individuals[is.na(final$individuals)] <- 1


final$visitas_indv <- final$visits/ final$individuals
final$visitas_indv_hora <- (final$visitas_indv*60)/30
#la base de datos de "final" ya tiene los polinizadores,las visitas/hora/indv, las plantas, y 
#                       las abundancias de las plantas
hist(final$visitas_indv_hora)
hist(log(final$visitas_indv_hora))

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


##########################################DISTANCES PLOTS#############################################
distances <- read.csv("C:/Users/Cisco/Documents/TFM/data/caracolesplotposition.csv", sep = ";") #aqui solamente esta la informacion de 1 
# plot, se midieron las distancias entre los plots y se añadieron
distances <- distances[seq(2,72,2),]
distances2 <- rbind(distances, distances, distances,
                    distances, distances, distances,
                    distances, distances, distances) #For publication, this need to be properly measured from Klm.
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

###############################################LOAD Neighbors data##################################
#neighbors data, there is not the plot 4
start.plants <- read.table("C:/Users/Cisco/Documents/TFM/data/focal_neighbours.2020_start.csv", header=T, sep=";")
end.plants <- read.table("C:/Users/Cisco/Documents/TFM/data/focal_neighbours.2020_2nphenology.csv", header=T, sep=";")

################################################ 1.Moran's I####################################################

##### ##############################>Pollinators#######################################
#beetles 
beetle <- subset(final, Group == "Beetle")
beetle$plot <- as.numeric(as.character(beetle$plot))
beetle.v <- beetle[,c("plot", "subplot","visitas_indv_hora")] #datos de plot, subplot, y visitas de BEETLES
beetle.v <-beetle.v %>% group_by(plot, subplot) %>% summarise (visits = sum(visitas_indv_hora))
beetle.v <- left_join(disfinal, beetle.v, by= c("plot", "subplot"))
beetle.v$visits[is.na(beetle.v$visits)] <- 0

#grafico de la correlacion espacial
bet.corr <- spline.correlog(x=beetle.v$x_coor2, y=beetle.v$y_coor2,
                            z=beetle.v$visits, resamp=100, quiet=TRUE)
plot(bet.corr, main= " Spatial Autocorrelation beetles across plots")

#test de moran. Aquí quiero obtener El estadistico de Moran y p.value 
moran.test(beetle.v$visits,mat2listw(plots.dists.inv)) # I= 0.156
moran.test(beetle.v$visits, nb2listw(w5)) # vecinos, I= 0.27
moran.plot(beetle.v$visits,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation Beetles across plots")

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

moran.test(flies.v$visits,mat2listw(plots.dists.inv)) # I= 0.08
moran.test(flies.v$visits, nb2listw(w5)) # vecinos, I= 0.12
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

moran.test(but.v$visits,mat2listw(plots.dists.inv)) # I= 6.334408e-03, almost homogenius
moran.test(but.v$visits, nb2listw(w5)) # vecinos, I= 0.0143189814
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

moran.test(bee.v$visits,mat2listw(plots.dists.inv))# I = 0.01
moran.test(bee.v$visits, nb2listw(w5))# I= -0.01 , homogeneus!
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

moran.test(junto$visit,mat2listw(plots.dists.inv)) # I= 0.16
moran.test(junto$visit, nb2listw(w5)) # vecinos, I= 0.197
moran.plot(junto$visit,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation pollinators across plots")

par(mfrow=c(2,3))
plot(bet.corr, main= " Spatial Autocorrelation beetles across plots")
plot(flies.corr, main= "Spatial autocorrelation flies across plots")
plot(but.corr, main= "Spatial autocorrelation butterflies across plots")
plot(bee.corr, main= "Spatial autocorrelation bees across plots")
plot(total.corr, main= "Spatial Autocorrelation visitors across plots")
par(mfrow=c(1,1))
############################################ >Plants########################################
plantas <- final[,c("plot", "subplot", "Plant", "individuals", "fruit", "seed")]
plantas <- plantas[-which(duplicated(plantas)), ] #It appears in the data set a lot of duplicated rows, because the previus data set was with the pollinators
#                                                   so we have to eliminate the duplicates, and I did it with this step
#CHFU
CHFU <- subset(plantas, Plant == "CHFU")
CHFU$individuals <- as.numeric(CHFU$individuals)
CHFU.p <- CHFU %>% group_by(plot, subplot) %>% summarise (num.planta = sum(individuals))
CHFU.p <- left_join(disfinal, CHFU.p,by= c("plot", "subplot"))
CHFU.p$num.planta[is.na(CHFU.p$num.planta)] <- 0

#grafico de la correlacion espacial
chfu.corr <- spline.correlog(x=CHFU.p$x_coor2, y=CHFU.p$y_coor2,
                             z=CHFU.p$num.planta, resamp=100, quiet=TRUE)
plot(chfu.corr, main= " Spatial Autocorrelation CHFU across plots")

moran.test(CHFU.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.34
moran.test(CHFU.p$num.planta, nb2listw(w5)) # vecinos, I= 0.46
moran.plot(CHFU.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHFU across plots")
#LEMA
LEMA <- subset(plantas, Plant == "LEMA") #check the data 
LEMA$individuals <- as.numeric(LEMA$individuals)
LEMA.1 <- LEMA %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
LEMA.2 <- left_join(disfinal, LEMA.1, by= c("plot", "subplot"))

LEMA.2$num.planta[is.na(LEMA.2$num.planta)] <- 0

lema.corr <- spline.correlog(x=LEMA.2$x_coor2, y=LEMA.2$y_coor2,
                             z=LEMA.2$num.planta, resamp=100, quiet=TRUE)
plot(lema.corr, main= " Spatial Autocorrelation LEMA across plots")

moran.test(LEMA.2$num.planta,mat2listw(plots.dists.inv)) # I= 0.54
moran.test(LEMA.2$num.planta, nb2listw(w5)) # vecinos, I= 0.67
moran.plot(LEMA.2$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation LEMA across plots")
#PUPA 
PUPA <- subset(plantas, Plant == "PUPA")
PUPA$individuals <- as.numeric(PUPA$individulas)
PUPA.p <- PUPA %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
PUPA.p <- left_join(disfinal, PUPA.p, by= c("plot", "subplot"))
PUPA.p$num.planta[is.na(PUPA.p$num.planta)] <- 0

pupa.corr <- spline.correlog(x=PUPA.p$x_coor2, y=PUPA.p$y_coor2,
                             z=PUPA.p$num.planta, resamp=100, quiet=TRUE)
plot(pupa.corr, main= " Spatial Autocorrelation PUPA across plots")

moran.test(PUPA.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.21
moran.test(PUPA.p$num.planta, nb2listw(w5)) # vecinos, I= 0.37
moran.plot(PUPA.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation PUPA across plots")
#MESU
MESU <- subset(plantas, Plant == "MESU") #solo 6 entradas
MESU$individuals <- as.numeric(MESU$individuals)
MESU.p <- MESU %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
MESU.p <- left_join(disfinal, MESU.p, by= c("plot", "subplot"))
MESU.p$num.planta[is.na(MESU.p$num.planta)] <- 0

me.corr <- spline.correlog(x=MESU.p$x_coor2, y=MESU.p$y_coor2,
                           z=MESU.p$num.planta, resamp=100, quiet=TRUE)
plot(me.corr, main= " Spatial Autocorrelation MESU across plots")

moran.test(MESU.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.03
moran.test(MESU.p$num.planta, nb2listw(w5)) # vecinos, I= 0.055
moran.plot(MESU.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation MESU across plots")
#CHMI 
CHMI <- subset(plantas, Plant == "CHMI") # solo 4 entradas
#CETE 
CETE <- subset(plantas, Plant == "CETE") # solo 17 entradas
CETE$individuals <- as.numeric(CETE$individuals)
CETE.p <- CETE %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
CETE.p <- left_join(disfinal, CETE.p, by= c("plot", "subplot"))
CETE.p$num.planta[is.na(CETE.p$num.planta)] <- 0

cete.corr <- spline.correlog(x=CETE.p$x_coor2, y=CETE.p$y_coor2,
                           z=CETE.p$num.planta, resamp=100, quiet=TRUE)
plot(cete.corr, main= " Spatial Autocorrelation CETE across plots")

moran.test(CETE.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.066
moran.test(CETE.p$num.planta, nb2listw(w5)) # vecinos, I= 0.066
moran.plot(CETE.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CETE across plots")
#BEMA 
BEMA <- subset(plantas, Plant == "BEMA") # solo 6 entradas
BEMA$individuals <- as.numeric(BEMA$individuals)
BEMA.p <- BEMA %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
BEMA.p <- left_join(disfinal, BEMA.p, by= c("plot", "subplot"))
BEMA.p$num.planta[is.na(BEMA.p$num.planta)] <- 0

bema.corr <- spline.correlog(x=BEMA.p$x_coor2, y=BEMA.p$y_coor2,
                           z=BEMA.p$num.planta, resamp=100, quiet=TRUE)
plot(bema.corr, main= " Spatial Autocorrelation BEMA across plots")

moran.test(BEMA.p$num.planta,mat2listw(plots.dists.inv)) # I=  -2.244244e-03 
moran.test(BEMA.p$num.planta, nb2listw(w5)) # vecinos, -0.0181305948
moran.plot(BEMA.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation BEMA across plots")
#
#SCLA 
SCLA <- subset(plantas, Plant == "SCLA") # solo 6 entradas
SCLA$individuals <- as.numeric(SCLA$individuals)
SCLA.p <- SCLA %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
SCLA.p <- left_join(disfinal, SCLA.p, by= c("plot", "subplot"))
SCLA.p$num.planta[is.na(SCLA.p$num.planta)] <- 0

scla.corr <- spline.correlog(x=SCLA.p$x_coor2, y=SCLA.p$y_coor2,
                           z=SCLA.p$num.planta, resamp=100, quiet=TRUE)
plot(scla.corr, main= " Spatial Autocorrelation SCLA across plots")

moran.test(SCLA.p$num.planta,mat2listw(plots.dists.inv)) # I=   -8.679902e-04
moran.test(SCLA.p$num.planta, nb2listw(w5)) # vecinos,   -0.006042405
moran.plot(SCLA.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SCLA across plots")
#Soas
SOAS <- subset(plantas, Plant == "SOAS")#only 2 entries
#SPRU
SPRU <- subset(plantas, Plant == "SPRU") #only 2 entries
#plantas juntas ----
juntas.plantas <- plantas[,c("plot", "subplot","individuals")]
juntas.plantas$individuals <- as.numeric(juntas.plantas$individuals)
num.plant <- juntas.plantas %>% group_by(plot, subplot) %>% summarise (num.planta = sum(individuals))
num.plant$plot <- as.numeric(num.plant$plot)
num.plant <- left_join(disfinal, num.plant, by= c("plot", "subplot"))
num.plant$num.planta[is.na(num.plant$num.planta)] <- 0

total.corr.pl <- spline.correlog(x=num.plant$x_coor2, y=num.plant$y_coor2,
                                 z=num.plant$num.planta, resamp=100, quiet=TRUE)
plot(total.corr.pl, main= "Spatial Autocorrelation plants across plots")

moran.test(num.plant$num.planta,mat2listw(plots.dists.inv)) # I= 0.49
moran.test(num.plant$num.planta, nb2listw(w5)) # vecinos, I= 0.599
moran.plot(num.plant$num.planta,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation plants across plots")
par(mfrow=c(4,2))
plot(chfu.corr, main= " Spatial Autocorrelation CHFU across plots")
plot(lema.corr, main= " Spatial Autocorrelation LEMA across plots")
plot(pupa.corr, main= " Spatial Autocorrelation PUPA across plots")
plot(me.corr, main= " Spatial Autocorrelation MESU across plots")
plot(cete.corr, main= " Spatial Autocorrelation CETE across plots")
plot(bema.corr, main= " Spatial Autocorrelation BEMA across plots")
plot(scla.corr, main= " Spatial Autocorrelation SCLA across plots")
plot(total.corr.pl, main= "Spatial Autocorrelation plants across plots")
par(mfrow=c(1,1))

##########################################>Fitness###################################################

#CHFU 
CHFU.fit <- CHFU %>% group_by(plot, subplot) %>% summarise (seeds = sum(seed))
CHFU.fit <- left_join(disfinal, CHFU.fit,by= c("plot", "subplot"))
CHFU.fit$seeds[is.na(CHFU.fit$seeds)] <- 0

chfu.corr.s <- spline.correlog(x=CHFU.fit$x_coor2, y=CHFU.fit$y_coor2,
                               z=CHFU.fit$seeds, resamp=100, quiet=TRUE)
plot(chfu.corr.s, main= " Spatial Autocorrelation CHFU fitness across plots")

moran.test(CHFU.fit$seeds,mat2listw(plots.dists.inv)) # I= 0.05
moran.test(CHFU.fit$seeds, nb2listw(w5)) # vecinos, I= 0.0779
moran.plot(CHFU.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHFU fitness across plots")
#LEMA
LEMA.fit <- LEMA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
LEMA.fit <- left_join(disfinal, LEMA.fit, by= c("plot", "subplot"))
LEMA.fit$seeds[is.na(LEMA.fit$seeds)] <- 0

lema.corr.s <- spline.correlog(x=LEMA.fit$x_coor2, y=LEMA.fit$y_coor2,
                               z=LEMA.fit$seeds, resamp=100, quiet=TRUE)
plot(lema.corr.s, main= " Spatial Autocorrelation LEMA fitness across plots")

moran.test(LEMA.fit$seeds,mat2listw(plots.dists.inv)) # I= 0.353
moran.test(LEMA.fit$seeds, nb2listw(w5)) # vecinos, I= 0.409
moran.plot(LEMA.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation LEMA fitness across plots")

#PUPA 
PUPA.fit <- PUPA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
PUPA.fit <- left_join(disfinal, PUPA.fit, by= c("plot", "subplot"))
PUPA.fit$seeds <- PUPA.fit$seeds
PUPA.fit$seeds [is.na(PUPA.fit$seeds )] <- 0

pupa.corr.s <- spline.correlog(x=PUPA.fit$x_coor2, y=PUPA.fit$y_coor2,
                               z=PUPA.fit$seeds, resamp=100, quiet=TRUE)
plot(pupa.corr.s, main= " Spatial Autocorrelation PUPA fitness across plots")

moran.test(PUPA.fit$seeds,mat2listw(plots.dists.inv)) # 0.054
moran.test(PUPA.fit$seeds, nb2listw(w5)) # vecinos, I= 0.0479
moran.plot(PUPA.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation PUPA across plots")
#MESU
MESU.fit <- MESU %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
MESU.fit <- left_join(disfinal, MESU.fit, by= c("plot", "subplot"))
MESU.fit$seeds[is.na(MESU.fit$seeds)] <- 0 #check it ----

#All the seeds of MESU are equal to 0!! 
#CETE 
CETE.fit <- CETE %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
CETE.fit <- left_join(disfinal, CETE.fit, by= c("plot", "subplot"))
CETE.fit$seeds <- CETE.fit$seeds
CETE.fit$seeds [is.na(CETE.fit$seeds )] <- 0

cete.corr.s <- spline.correlog(x=CETE.fit$x_coor2, y=CETE.fit$y_coor2,
                               z=CETE.fit$seeds, resamp=100, quiet=TRUE)
plot(cete.corr.s, main= " Spatial Autocorrelation CETE fitness across plots")

moran.test(CETE.fit$seeds,mat2listw(plots.dists.inv)) # 0.07
moran.test(CETE.fit$seeds, nb2listw(w5)) # vecinos, I= 0.088
moran.plot(CETE.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CETE across plots")

#NEIGHBORS ----
start.plants <- subset(start.plants,edge %in% c("FALSE"))
start.plants <- subset(start.plants, focal %in% c("SOAS","CHFU","LEMA","CHMI",  "SCLA"))
end.plants <- subset(end.plants,edge %in% c("FALSE"))
end.plants <- subset(end.plants, focal %in% c("BEMA","CETE","MESU","PUPA", "SPRU"))
neighbors <- rbind(start.plants, end.plants) #all the neighbors together, now i have to join to the final dataset
neighbors$Plant <- neighbors$focal
neighbors$neigh_inter <- as.numeric(neighbors$neigh_inter)
neighbors$neigh_intra <- as.numeric(neighbors$neigh_intra)
neighbors$unique_id <- paste(neighbors$plot, neighbors$subplot,neighbors$Plant, sep="_")
final$unique_id <- paste(final$plot, final$subplot,final$Plant, sep="_")
final$subplot <- as.factor(final$subplot)
final$Plant <- as.factor(final$Plant)

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
final$unique_id <- paste(final$plot, final$subplot,final$Plant, sep="_")
final.data <- dplyr::full_join(vecinos, final, by=c("plot", "subplot", "Plant"))
                                 
none.visitor <- na.omit(final.data) #only neighbors in the places that i have a pollinator
none.visitor<- none.visitor[,c("plot","subplot","Plant","Group","visits", "individuals", "seed", "fruit", "visitas_indv", "visitas_indv_hora", "neigh_inter.plot","neigh_intra.plot",
                  "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 


x <- dplyr::full_join(none.visitor, disfinal, by=c("plot", "subplot"))
data <- x #this data have all the neigbors together


neigbors.intra.inter <- tidyr::gather(data,'neigh_inter.plot', 'neigh_intra.plot', 'neigh_intra.3m', 'neigh_inter.3m', 'neigh_inter.1m', 
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
intra.neigh$n_neighbors_intra <- intra.neigh$n_neighbors_total
intra.neigh$distance[intra.neigh$distance == "b"] <- "a" #PLOT
intra.neigh$distance[intra.neigh$distance == "d"] <- "c"#3M
intra.neigh$distance[intra.neigh$distance == "f"] <- "e"#1M
intra.neigh$distance[intra.neigh$distance == "h"] <- "g"#7.5cm
intra <- intra.neigh[,c("n_neighbors_intra" )] 
neigbors.intra.inter<- cbind(inter.neigh, intra)
neigbors.intra.inter <- na.omit(neigbors.intra.inter)
##################################################3. GLM#######################################################################

###HERE IS ONE OPTION FOLLOWING NLME.

#FIRST FIT THE RANDOM STRUCTURE. #we are gonna work with to types of spatial autcorrelation types exponential and poisson. 
# Do first example with CHFU (Maria you do the rest)

library(MuMIn)
library(multcomp)
library(emmeans)
library(sjPlot)
library(sjmisc)
#>CHFU----
CHFU.vis <- subset(data, Plant == "CHFU")
#different models
lCtr <- lmeControl(maxIter = 5000, msMaxIter = 5000, tolerance = 1e-9, niterEM = 250, msMaxEval = 200)
CHFU.vis$seed <-  as.numeric(CHFU.vis$seed) #There are 0 seeds, I'm going to change them to 1, to can do the log
CHFU.vis$log.seed <- log(CHFU.vis$seed)
CHFU.vis <- replace(CHFU.vis, CHFU.vis == -Inf, 0)#hasta este punto tengo las semillas corregidas por el log, y los -inf 
#                                                     convertidos en 0

CHFU.vis$subplot <- as.factor(CHFU.vis$subplot)
CHFU.vis$unique_id <- paste(CHFU.vis$plot, CHFU.vis$subplot , sep="_")
CHHFU.prueba <- CHFU.vis %>% distinct(unique_id, .keep_all = TRUE)


m1<- lme(log.seed ~ 1, data= CHHFU.prueba,random = ~1 |plot, control=lCtr,
         method = "ML")
m2 <- lme(log.seed ~ 1, data= CHHFU.prueba, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")

m3 <- lme(log.seed ~ 1, data= CHFU.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")

m4 <- lme(log.seed ~ 1, data= CHFU.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(m1, m2, m3, m4) #best model is m2

options(na.action = "na.fail")

m.prueba.chufu.2 <- lme(log.seed ~ visitas_indv_hora+neigh_inter.1m*neigh_intra.1m, data= CHFU.vis, random = ~1 |plot, control=lCtr,
                        corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")

m.prueba_sec.chufu.2 <- dredge(m.prueba.chufu.2, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(m.prueba_sec.chufu.2, "rank.call"))
fmList.prueba.chufu.2 <- get.models(m.prueba_sec.chufu.2, 1:4) 
summary(model.avg(fmList.prueba.chufu.2))#neigh inter 1m
plot(fitted(m.prueba.chufu.2), resid(m.prueba.chufu.2, type = "pearson"), main= "chfu fitness")
abline(0,0, col="red")
r.squaredGLMM(m.prueba.chufu.2)

#>LEMA----
LEMA.vis <- subset(data, Plant == "LEMA")
LEMA.vis$seed <-  as.numeric(LEMA.vis$seed) #There are 0 seeds, I'm going to change them to 1, to can do the log
LEMA.vis$log.seed <- log(LEMA.vis$seed)
LEMA.vis <- replace(LEMA.vis, LEMA.vis == -Inf, 0)
l1<- lme(log.seed ~ 1, data= LEMA.vis,random = ~1 |plot, control=lCtr,
         method = "ML")
l2 <- lme(log.seed ~ 1, data= LEMA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML") #no works, same problem
#with spatial correlation


