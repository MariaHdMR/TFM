#This code is to answer the question if the plants, floral visitors and plant fitness have spatial autocorrelation or not. It is a descriptive
#analysis to see how they are distributed accros plots. For answering that, we are going to make a Moran's I, to see the correlation. 
library(tidyverse)
library(spdep)#-> for Moran's I
library(ncf) #spline.correlog
####load data
data <- read_csv2("Analisis_BES/data/Final_global_data.csv")
data$visits <- as.numeric(data$visits) #por que es esto necesario?
data <- as.data.frame(data) #Maria, por que usas tidyverse para cargar datos, si luego los pasa a base r? 
head(data)
distances <- read_csv2("Analisis_BES/data/distances.csv")
distances <- as.data.frame(distances)
distances$x_coor2 <- as.numeric(distances$x_coor2)
distances$y_coor2 <- as.numeric(distances$y_coor2)
distancias.matriz <-as.matrix(dist(cbind(distances$x_coor2, distances$y_coor2)))
#now, I make the inverse matrix and the diagonal equal to 0
plots.dists.inv <- 1/distancias.matriz
diag(plots.dists.inv) <- 0
w5 <- knn2nb(knearneigh(coordinates(distances[,3:4]), k=8))

##### ##############################Pollinators I de Moran#######################################
#Here I used the number of visits (count) per one Group of pollinators per subplot and per plot (regardless the plant species that they visited)
#Visits (count)----
#>beetles 
beetle <- subset(data, Group == "Beetle")
beetle$plot <- as.numeric(as.character(beetle$plot)) #esto me preocupa un poco por que indica que los datos no son homogeneos...#M:pero beetle ya tiene
# plot como numeric, lo hice para que no me surgieran problemas, por qué pueden ser no homogeneos?
beetle.v <- beetle[,c("plot", "subplot","visits")] #datos de plot, subplot, y visitas de BEETLES
beetle.v <-beetle.v %>% group_by(plot, subplot) %>% summarise (visits = sum(visits))
beetle.v <- left_join(distances, beetle.v, by= c("plot", "subplot"))
beetle.v$visits[is.na(beetle.v$visits)] <- 0
head(beetle.v)

#grafico de la correlacion espacial
bet.corr <- spline.correlog(x=beetle.v$x_coor2, y=beetle.v$y_coor2,
                            z=beetle.v$visits, resamp=100, quiet=TRUE) 
plot(bet.corr, main= " Spatial Autocorrelation beetles across plots",
     ylim= c(-0.5:0.5))

#test de moran. Aqui quiero obtener El estadistico de Moran y p.value 
moran.test(beetle.v$visits,mat2listw(plots.dists.inv)) # I= 0.19
moran.test(beetle.v$visits, nb2listw(w5)) # vecinos, I= 0.2655
moran.plot(beetle.v$visits,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation Beetles across plots")

moran.mc(beetle.v$visits, mat2listw(plots.dists.inv), nsim=99) #p.value= 0.01 #in internet it is said that The Montercalo
# has more accurate results then the moran.test function. 
#IB: GREAT, beetles tend to be slightly clustered, specially at short distances.

#>flies
flies <- subset(data, Group == "Fly")
flies$plot <- as.numeric(as.character(flies$plot))
flies.v <- flies[,c("plot", "subplot","visits")] 
flies.v <-flies.v %>% group_by(plot, subplot) %>% summarise (visits = sum(visits))
flies.v <- left_join(distances,flies.v, by= c("plot", "subplot"))
flies.v$visits[is.na(flies.v$visits)] <- 0

flies.corr <- spline.correlog(x=flies.v$x_coor2, y=flies.v$y_coor2,
                              z=flies.v$visits, resamp=100, quiet=TRUE)
plot(flies.corr, main= "Spatial autocorrelation flies across plots",ylim= c(-0.5:0.5))

moran.test(flies.v$visits,mat2listw(plots.dists.inv)) # I= 0.0698
moran.test(flies.v$visits, nb2listw(w5)) # vecinos, I= 0.11
moran.plot(flies.v$visits,mat2listw(plots.dists.inv), main= "Spatial autocorrelation flies across plots")
moran.mc(flies.v$visits, mat2listw(plots.dists.inv), nsim=99) #p-value= 0.01
#IB: Flies show a very small spatial autocorrelation. 

#butterflies
but <- subset(data, Group == "Butterfly")
but$plot <- as.numeric(as.character(but$plot))
but.v <- but[,c("plot", "subplot","visits")] #datos de plot, subplot, y visitas 
but.v <-but.v %>% group_by(plot, subplot) %>% summarise (visits = sum(visits))
but.v <- left_join(distances,but.v , by= c("plot", "subplot"))
but.v$visits[is.na(but.v$visits)] <- 0

but.corr <- spline.correlog(x=but.v$x_coor2, y=but.v$y_coor2,
                            z=but.v$visits, resamp=100, quiet=TRUE)
plot(but.corr, main= "Spatial autocorrelation butterflies across plots",ylim= c(-0.5:0.5))

moran.test(but.v$visits,mat2listw(plots.dists.inv)) # I= -1.032727e-02, homogenius
moran.test(but.v$visits, nb2listw(w5)) # vecinos, I=  -0.0125584112
moran.plot(but.v$visits,mat2listw(plots.dists.inv),  main= "Spatial autocorrelation butterflies across plots")
moran.mc(but.v$visits, mat2listw(plots.dists.inv), nsim=99) #p-value= 0.98
#IB: Buterflies do not show spatial autocorrelation. This agrees with its high mobility.

#bees
bee <- subset(data, Group == "Bee")
bee$plot <- as.numeric(as.character(bee$plot))
bee.v <- bee[,c("plot", "subplot","visits")] #datos de plot, subplot, y visitas 
bee.v <-bee.v %>% group_by(plot, subplot) %>% summarise (visits = sum(visits))
bee.v <- left_join(distances, bee.v, by= c("plot", "subplot"))
bee.v$visits[is.na(bee.v$visits)] <- 0

bee.corr <- spline.correlog(x=bee.v$x_coor2, y=bee.v$y_coor2,
                            z=bee.v$visits, resamp=100, quiet=TRUE)
plot(bee.corr, main= "Spatial autocorrelation bees across plots", ylim= c(-0.5:0.5))

moran.test(bee.v$visits,mat2listw(plots.dists.inv))# I = 0.03588
moran.test(bee.v$visits, nb2listw(w5))# I= 0.0617
moran.plot(bee.v$visits,mat2listw(plots.dists.inv), main= "Spatial autocorrelation bees across plots")
moran.mc(bee.v$visits, mat2listw(plots.dists.inv), nsim=99) #p-value= 0.01
#IB: Bees also show a really weak spatial autocorrelation. It fits the mobility expectations 
#beetles < flies < bees < buterflies.

#polinizadores general 
junto <- data[,c("Group", "plot", "subplot","visits")]
junto <- junto[!is.na(junto$Group),] #I want all the Groups (Flies, beetles, butterflies and bees) but no the NAs
junto <- junto %>% group_by(plot, subplot) %>% summarise (visit = sum(visits))
junto <- left_join(distances, junto, by= c("plot", "subplot"))
junto <- subset(junto, subplot != "OUT")
junto$visit[is.na(junto$visit)] <- 0

total.corr <- spline.correlog(x=junto$x_coor2, y=junto$y_coor2,
                              z=junto$visit, resamp=100, quiet=TRUE)
plot(total.corr, main= "Spatial Autocorrelation visitors across plots", ylim= c(-0.5:0.5))

moran.test(junto$visit,mat2listw(plots.dists.inv)) # I= 0.1840
moran.test(junto$visit, nb2listw(w5)) # vecinos, I= 0.2493
moran.plot(junto$visit,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation pollinators across plots")
moran.mc(junto$visit, mat2listw(plots.dists.inv), nsim=99) #p-value= 0.01
#IB: Overall, there is a moderate spatial pattern. I am not sure we need to present this in the paper. 

par(mfrow=c(2,2))
plot(bet.corr, main= " Beetles distribution",xlab="Distance (m)", ylab="Correlation", ylim= c(-0.5:0.5))
plot(flies.corr, main= "Flies distribution",xlab="Distance (m)", ylab="Correlation", ylim= c(-0.5:0.5))
plot(but.corr, main= "Butterflies distribution",xlab="Distance (m)", ylab="Correlation", ylim= c(-0.5:0.5))
plot(bee.corr, main= "Bees distribution",xlab="Distance (m)", ylab="Correlation", ylim= c(-0.5:0.5))
plot(total.corr, main= "Floral visitors distribution",xlab="Distance (m)", ylab="Correlation", ylim= c(-0.5:0.5))
par(mfrow=c(1,1))




#now, I'll try with the number of visits per Group of floral visitors / number of flowers of the  Plant species that they visited per subplot
#                       and per plot
#IB: I don't think this is necessary for the paper.
#visits/flowers----

#>beetles flowers 

beetle.fl <- beetle[,c("plot", "subplot","visits.flower")] 
beetle.fl$visits.flower[beetle.fl$visits.flower== Inf] <- 'NA'
beetle.fl$visits.flower <-as.numeric(beetle.fl$visits.flower) 
beetle.fl <-na.omit(beetle.fl)
beetle.fl <-beetle.fl %>% group_by(plot, subplot) %>% summarise (visits.fl = sum(visits.flower))%>%
    ungroup()

beetle.fl <- left_join(distances, beetle.fl, by= c("plot", "subplot"))
head(beetle.fl)
beetle.fl$visits.fl[is.na(beetle.fl$visits.fl)] <- 0
head(beetle.fl)

#grafico de la correlacion espacial
bet.corr.fl <- spline.correlog(x=beetle.fl$x_coor2, y=beetle.fl$y_coor2,
                            z=beetle.fl$visits.fl, resamp=100, quiet=TRUE) 
plot(bet.corr.fl, main= " Spatial Autocorrelation beetles per flowers across plots")

#test de moran. Aqui quiero obtener El estadistico de Moran y p.value 
moran.test(beetle.fl$visits.fl,mat2listw(plots.dists.inv)) # I= 0.07
moran.test(beetle.fl$visits.fl, nb2listw(w5)) # vecinos, I= 0.107
moran.plot(beetle.fl$visits.fl,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation Beetles per flowers across plots")

moran.mc(beetle.fl$visits.fl, mat2listw(plots.dists.inv), nsim=99) #p.value= 0.01 

#>flies flowers

flies.fl <- flies[,c("plot", "subplot","visits.flower")] 
flies.fl$visits.flower[flies.fl$visits.flower== Inf] <- 'NA'
flies.fl$visits.flower <-as.numeric(flies.fl$visits.flower)
flies.fl <-na.omit(flies.fl)
flies.fl <-flies.fl %>% group_by(plot, subplot) %>% summarise (visits.fl = sum(visits.flower))%>%
    ungroup()

flies.fl <- left_join(distances, flies.fl, by= c("plot", "subplot"))
head(flies.fl)
flies.fl$visits.fl[is.na(flies.fl$visits.fl)] <- 0
head(flies.fl)

#grafico de la correlacion espacial
fly.corr.fl <- spline.correlog(x=flies.fl$x_coor2, y=flies.fl$y_coor2,
                               z=flies.fl$visits.fl, resamp=100, quiet=TRUE) 
plot(fly.corr.fl, main= " Spatial Autocorrelation flies per flowers across plots")

#test de moran. Aqui quiero obtener El estadistico de Moran y p.value 
moran.test(flies.fl$visits.fl,mat2listw(plots.dists.inv)) # I= 0.043
moran.test(flies.fl$visits.fl, nb2listw(w5)) # vecinos, I= 0.068
moran.plot(flies.fl$visits.fl,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation flies per flowers across plots")
moran.mc(flies.fl$visits.fl, mat2listw(plots.dists.inv), nsim=99) #p.value= 0.01 

#>butterflies flowers

but.fl <- but[,c("plot", "subplot","visits.flower")] #only 3 rows
but.fl$visits.flower[but.fl$visits.flower== Inf] <- 'NA'
but.fl$visits.flower <-as.numeric(but.fl$visits.flower)
but.fl <-na.omit(but.fl)
but.fl <-but.fl %>% group_by(plot, subplot) %>% summarise (visits.fl = sum(visits.flower))%>%
    ungroup()

but.fl <- left_join(distances, but.fl, by= c("plot", "subplot"))
head(but.fl)
but.fl$visits.fl[is.na(but.fl$visits.fl)] <- 0
head(but.fl)

#grafico de la correlacion espacial
but.corr.fl <- spline.correlog(x=but.fl$x_coor2, y=but.fl$y_coor2,
                               z=but.fl$visits.fl, resamp=100, quiet=TRUE) 
plot(but.corr.fl, main= " Spatial Autocorrelation butterflies per flowers across plots")

#test de moran. Aqui quiero obtener El estadistico de Moran y p.value 
moran.test(but.fl$visits.fl,mat2listw(plots.dists.inv)) # I= -5.863297e-03
moran.test(but.fl$visits.fl, nb2listw(w5)) # vecinos, I= -0.0073344496
moran.plot(but.fl$visits.fl,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation butterflies per flowers across plots")
moran.mc(but.fl$visits.fl, mat2listw(plots.dists.inv), nsim=99) #p.value= 0.83 

#>bee flowers

bee.fl <- bee[,c("plot", "subplot","visits.flower")] 
bee.fl$visits.flower[bee.fl$visits.flower== Inf] <- 'NA'
bee.fl$visits.flower <-as.numeric(bee.fl$visits.flower)
bee.fl <-na.omit(bee.fl)
bee.fl <-bee.fl %>% group_by(plot, subplot) %>% summarise (visits.fl = sum(visits.flower))%>%
    ungroup()

bee.fl <- left_join(distances, bee.fl, by= c("plot", "subplot"))
head(bee.fl)
bee.fl$visits.fl[is.na(bee.fl$visits.fl)] <- 0
head(bee.fl)

#grafico de la correlacion espacial
bee.corr.fl <- spline.correlog(x=bee.fl$x_coor2, y=bee.fl$y_coor2,
                               z=bee.fl$visits.fl, resamp=100, quiet=TRUE) 
plot(bee.corr.fl, main= " Spatial Autocorrelation bees per flowers across plots")

#test de moran. Aqui quiero obtener El estadistico de Moran y p.value 
moran.test(bee.fl$visits.fl,mat2listw(plots.dists.inv)) # I= 0.0259
moran.test(bee.fl$visits.fl, nb2listw(w5)) # vecinos, I= 0.0156
moran.plot(bee.fl$visits.fl,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation bees per flowers across plots")
moran.mc(bee.fl$visits.fl, mat2listw(plots.dists.inv), nsim=99) #p.value= 0.01 


#polinizadores general 
junto.fl <- data[,c("Group", "plot", "subplot","visits.flower")]
junto.fl <- junto.fl[!is.na(junto.fl$Group),] #I want all the Groups (Flies, beetles, butterflies and bees) but no the NAs
junto.fl$visits.flower[junto.fl$visits.flower== Inf] <- 'NA'
junto.fl$visits.flower <-as.numeric(junto.fl$visits.flower)
junto.fl <-na.omit(junto.fl)
junto.fl <- junto.fl %>% group_by(plot, subplot) %>% summarise (visit.fl = sum(visits.flower))
junto.fl <- left_join(distances, junto.fl, by= c("plot", "subplot"))
junto.fl <- subset(junto.fl, subplot != "OUT")
junto.fl$visit.fl[is.na(junto.fl$visit.fl)] <- 0

total.corr.fl <- spline.correlog(x=junto.fl$x_coor2, y=junto.fl$y_coor2,
                              z=junto.fl$visit.fl, resamp=100, quiet=TRUE)
plot(total.corr.fl, main= "Spatial Autocorrelation visitors per flowers across plots")

moran.test(junto.fl$visit.fl,mat2listw(plots.dists.inv)) # I= 0.07
moran.test(junto.fl$visit.fl, nb2listw(w5)) # vecinos, I= 0.11
moran.plot(junto.fl$visit.fl,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation pollinators per flowers across plots")
moran.mc(junto.fl$visit.fl, mat2listw(plots.dists.inv), nsim=99) #p-value= 0.01

par(mfrow=c(2,3))
plot(bet.corr.fl, main= " Visits of Beetles per flower distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(fly.corr.fl, main= "Visits of Flies per flower distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(but.corr.fl, main= "Visits of Butterflies per flowers distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(bee.corr.fl, main= "Visits of Bees per flowers distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(total.corr.fl, main= "Visits of Floral visitors per flowers distribution across plots",xlab="Distance (m)", ylab="Correlation")
par(mfrow=c(1,1))
#IB: I will not show this one to keep the story simple.

############################################ Plants Individuals########################################
#Here I'm going to use the number of individuals of each species per subplot and per plot
plantas <- data[,c("plot", "subplot", "Plant", "individuals", "fruit", "seed", "seed.indv")]
plantas <- plantas[-which(duplicated(plantas)), ] #It appears in the data set a lot of duplicated rows, because the previus data set was with the pollinators
#                                                   so we have to eliminate the duplicates, and I did it with this step
#CHFU
CHFU <- subset(plantas, Plant == "CHFU")
CHFU <- na.omit(CHFU)
CHFU$individuals <- as.numeric(CHFU$individuals)
CHFU.p <- CHFU %>% group_by(plot, subplot) %>% summarise (num.planta = sum(individuals))
CHFU.p <- left_join(distances, CHFU.p,by= c("plot", "subplot"))
CHFU.p$num.planta[is.na(CHFU.p$num.planta)] <- 0

#grafico de la correlacion espacial
chfu.corr <- spline.correlog(x=CHFU.p$x_coor2, y=CHFU.p$y_coor2,
                             z=CHFU.p$num.planta, resamp=100, quiet=TRUE)
plot(chfu.corr, main=expression(paste("Spatial autocorrelation ",italic("Chamaemelum fuscatum"))), ylim= c(-0.5:0.5))


moran.test(CHFU.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.20
moran.test(CHFU.p$num.planta, nb2listw(w5)) # vecinos, I= 0.3198
moran.plot(CHFU.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHFU across plots")
moran.mc(CHFU.p$num.planta, mat2listw(plots.dists.inv), nsim=99) #p-value= 0.01

#LEMA
LEMA <- subset(plantas, Plant == "LEMA")  
LEMA <- na.omit(LEMA)
LEMA$individuals <- as.numeric(LEMA$individuals)
LEMA.1 <- LEMA %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
LEMA.2 <- left_join(distances, LEMA.1, by= c("plot", "subplot"))
LEMA.2$num.planta[is.na(LEMA.2$num.planta)] <- 0

lema.corr <- spline.correlog(x=LEMA.2$x_coor2, y=LEMA.2$y_coor2,
                             z=LEMA.2$num.planta, resamp=100, quiet=TRUE)
plot(lema.corr, main= " Spatial Autocorrelation LEMA across plots")

plot(lema.corr, main=expression(paste("Spatial autocorrelation ",italic("Leontodon maroccanus"))), ylim= c(-0.5:0.5))

moran.test(LEMA.2$num.planta,mat2listw(plots.dists.inv)) # I= 0.24
moran.test(LEMA.2$num.planta, nb2listw(w5)) # vecinos, I= 0.39
moran.plot(LEMA.2$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation LEMA across plots")
moran.mc(LEMA.2$num.planta, mat2listw(plots.dists.inv), nsim=99) #p-value= 0.01

#PUPA 
PUPA <- subset(plantas, Plant == "PUPA")
PUPA <- na.omit(PUPA)
PUPA.1 <- PUPA %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
PUPA.p1 <- left_join(distances, PUPA.1, by= c("plot", "subplot"))
PUPA.p1$num.planta[is.na(PUPA.p1$num.planta)] <- 0

pupa.corr <- spline.correlog(x=PUPA.p1$x_coor2, y=PUPA.p1$y_coor2,
                             z=PUPA.p1$num.planta, resamp=100, quiet=TRUE)
plot(pupa.corr, main= " Spatial Autocorrelation PUPA across plots")

plot(pupa.corr, main=expression(paste("Spatial autocorrelation ",italic("Pulicaria paludosa"))), ylim= c(-0.5:0.5))


moran.test(PUPA.p1$num.planta,mat2listw(plots.dists.inv)) # I= 0.2128
moran.test(PUPA.p1$num.planta, nb2listw(w5)) # vecinos, I= 0.288
moran.plot(PUPA.p1$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation PUPA across plots")
moran.mc(PUPA.p1$num.planta, mat2listw(plots.dists.inv), nsim=99) #p-value= 0.01

#MESU
MESU <- subset(plantas, Plant == "MESU") 
MESU <- na.omit(MESU)
MESU$individuals <- as.numeric(MESU$individuals)
MESU.p <- MESU %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
MESU.p <- left_join(distances, MESU.p,  by= c("plot", "subplot"))
MESU.p$num.planta[is.na(MESU.p$num.planta)] <- 0

me.corr <- spline.correlog(x=MESU.p$x_coor2, y=MESU.p$y_coor2,
                           z=MESU.p$num.planta, resamp=100, quiet=TRUE)
plot(me.corr, main= " Spatial Autocorrelation MESU across plots")

plot(me.corr, main=expression(paste("Spatial autocorrelation ",italic("Melillotus sulcatus"))), ylim= c(-0.5:0.5))

moran.test(MESU.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.153
moran.test(MESU.p$num.planta, nb2listw(w5)) # vecinos, I= 0.20
moran.plot(MESU.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation MESU across plots")
moran.mc(MESU.p$num.planta, mat2listw(plots.dists.inv), nsim=99) #p-value= 0.01


#CHMI 
CHMI <- subset(plantas, Plant == "CHMI") # solo 15 entradas
CHMI <- na.omit(CHMI)
CHMI$individuals <- as.numeric(CHMI$individuals)
CHMI.p <- CHMI %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
CHMI.p <- left_join(distances, CHMI.p,  by= c("plot", "subplot"))
CHMI.p$num.planta[is.na(CHMI.p$num.planta)] <- 0

chmi.corr <- spline.correlog(x=CHMI.p$x_coor2, y=CHMI.p$y_coor2,
                             z=CHMI.p$num.planta, resamp=100, quiet=TRUE)
plot(chmi.corr, main= " Spatial Autocorrelation CHMI across plots")

plot(chmi.corr, main=expression(paste("Spatial autocorrelation ",italic("Chamaemelum mixtum"))), ylim= c(-0.5:0.5))

moran.test(CHMI.p$num.planta,mat2listw(plots.dists.inv)) # I= 2.779055e-02 
moran.test(CHMI.p$num.planta, nb2listw(w5)) # vecinos, I=  0.0566546763 
moran.plot(CHMI.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHMI across plots")
moran.mc(CHMI.p$num.planta, mat2listw(plots.dists.inv), nsim=99) #p-value= 0.01

#CETE 
CETE <- subset(plantas, Plant == "CETE") 
CETE <- na.omit(CETE)
CETE$individuals <- as.numeric(CETE$individuals)
CETE.p <- CETE %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
CETE.p <- left_join(distances, CETE.p, by= c("plot", "subplot"))
CETE.p$num.planta[is.na(CETE.p$num.planta)] <- 0

cete.corr <- spline.correlog(x=CETE.p$x_coor2, y=CETE.p$y_coor2,
                             z=CETE.p$num.planta, resamp=100, quiet=TRUE)
plot(cete.corr, main= " Spatial Autocorrelation CETE across plots")

plot(cete.corr, main=expression(paste("Spatial autocorrelation ",italic("Centaurium tenuiflorum"))), ylim= c(-0.5:0.5))

moran.test(CETE.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.1435
moran.test(CETE.p$num.planta, nb2listw(w5)) # vecinos, I= 0.2557
moran.plot(CETE.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CETE across plots")
moran.mc(CETE.p$num.planta, mat2listw(plots.dists.inv), nsim=99) #p-value= 0.01

#BEMA 
BEMA <- subset(plantas, Plant == "BEMA") 
BEMA <-na.omit(BEMA)
BEMA$individuals <- as.numeric(BEMA$individuals)
BEMA.p <- BEMA %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
BEMA.p <- left_join(distances, BEMA.p, by= c("plot", "subplot"))
BEMA.p$num.planta[is.na(BEMA.p$num.planta)] <- 0

bema.corr <- spline.correlog(x=BEMA.p$x_coor2, y=BEMA.p$y_coor2,
                             z=BEMA.p$num.planta, resamp=100, quiet=TRUE)
plot(bema.corr, main= " Spatial Autocorrelation BEMA across plots")

plot(bema.corr, main=expression(paste("Spatial autocorrelation ",italic("Beta macrocarpa"))), ylim= c(-0.5:0.5))

moran.test(BEMA.p$num.planta,mat2listw(plots.dists.inv)) # I=  0.1452
moran.test(BEMA.p$num.planta, nb2listw(w5)) # vecinos, 0.277
moran.plot(BEMA.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation BEMA across plots")
moran.mc(BEMA.p$num.planta, mat2listw(plots.dists.inv), nsim=99) #p-value= 0.01

#SCLA 
SCLA <- subset(plantas, Plant == "SCLA") 
SCLA <- na.omit(SCLA)
SCLA$individuals <- as.numeric(SCLA$individuals)
SCLA.p <- SCLA %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
SCLA.p <- left_join(distances, SCLA.p, by= c("plot", "subplot"))
SCLA.p$num.planta[is.na(SCLA.p$num.planta)] <- 0

scla.corr <- spline.correlog(x=SCLA.p$x_coor2, y=SCLA.p$y_coor2,
                             z=SCLA.p$num.planta, resamp=100, quiet=TRUE)
plot(scla.corr, main= " Spatial Autocorrelation SCLA across plots")

plot(scla.corr, main=expression(paste("Spatial autocorrelation ",italic("Scorzonera laciniata"))), ylim= c(-0.5:0.5))

moran.test(SCLA.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.119
moran.test(SCLA.p$num.planta, nb2listw(w5)) # vecinos,  0.19
moran.plot(SCLA.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SCLA across plots")
moran.mc(SCLA.p$num.planta, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#Soas
SOAS <- subset(plantas, Plant == "SOAS")
SOAS <-na.omit(SOAS)
SOAS$individuals <- as.numeric(SOAS$individuals)
SOAS.p <- SOAS %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
SOAS.p <- left_join(distances, SOAS.p, by= c("plot", "subplot"))
SOAS.p$num.planta[is.na(SOAS.p$num.planta)] <- 0

soas.corr <- spline.correlog(x=SOAS.p$x_coor2, y=SOAS.p$y_coor2,
                             z=SOAS.p$num.planta, resamp=100, quiet=TRUE)
plot(soas.corr, main= " Spatial Autocorrelation SOAS across plots")

plot(soas.corr, main=expression(paste("Spatial autocorrelation ",italic("Sonchus asper"))), ylim= c(-0.5:0.5))

moran.test(SOAS.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.2331
moran.test(SOAS.p$num.planta, nb2listw(w5)) # vecinos,  0.3411
moran.plot(SOAS.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SOAS across plots")
moran.mc(SOAS.p$num.planta, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#SPRU
SPRU <- subset(plantas, Plant == "SPRU") 
SPRU <- na.omit(SPRU)
SPRU$individuals <- as.numeric(SPRU$individuals)
SPRU.p <- SPRU %>% group_by(plot, subplot, Plant) %>% summarise (num.planta = sum(individuals))
SPRU.p <- left_join(distances, SPRU.p, by= c("plot", "subplot"))
SPRU.p$num.planta[is.na(SPRU.p$num.planta)] <- 0

spru.corr <- spline.correlog(x=SPRU.p$x_coor2, y=SPRU.p$y_coor2,
                             z=SPRU.p$num.planta, resamp=100, quiet=TRUE)
plot(spru.corr, main= " Spatial Autocorrelation SPRU across plots")
plot(spru.corr, main=expression(paste("Spatial autocorrelation ",italic("Spergularia rubra"))), ylim= c(-0.5:0.5))

moran.test(SPRU.p$num.planta,mat2listw(plots.dists.inv)) # I= 0.1086
moran.test(SPRU.p$num.planta, nb2listw(w5)) # vecinos,  0.09
moran.plot(SPRU.p$num.planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SPRU across plots")
moran.mc(SPRU.p$num.planta, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#plantas juntas 
juntas.plantas <- plantas[,c("plot", "subplot","individuals")]
juntas.plantas <-na.omit(juntas.plantas)
juntas.plantas$individuals <- as.numeric(juntas.plantas$individuals)
num.plant <- juntas.plantas %>% group_by(plot, subplot) %>% summarise (num.planta = sum(individuals))
num.plant$plot <- as.numeric(num.plant$plot)
num.plant <- left_join(distances, num.plant, by= c("plot", "subplot"))
num.plant$num.planta[is.na(num.plant$num.planta)] <- 0

total.corr.pl <- spline.correlog(x=num.plant$x_coor2, y=num.plant$y_coor2,
                                 z=num.plant$num.planta, resamp=100, quiet=TRUE)
plot(total.corr.pl, main= "Spatial Autocorrelation plants across plots")


moran.test(num.plant$num.planta,mat2listw(plots.dists.inv)) # I= 0.21
moran.test(num.plant$num.planta, nb2listw(w5)) # vecinos, I= 0.3247
moran.plot(num.plant$num.planta,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation plants across plots")
moran.mc(num.plant$num.planta, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

par(mfrow=c(3,1))
plot(chfu.corr, main=expression(paste("Distribution",italic(" Chamaemelum fuscatum"))), ylim= c(-0.5:0.5),xlab="Distance (m)", ylab="Correlation")
plot(lema.corr, main=expression(paste("Distribution",italic(" Leontodon maroccanus"))), ylim= c(-0.5:0.5),xlab="Distance (m)", ylab="Correlation")
plot(pupa.corr, main=expression(paste("Distribution",italic(" Pulicaria paludosa"))), ylim= c(-0.5:0.5),xlab="Distance (m)", ylab="Correlation")

par(mfrow=c(1,1))
#IB: Great, remember to highlight plants are more clustered than pollinators.

#####Fitness###----
#First the fitness is the number of seeds per ONE fruit----

#CHFU 

CHFU.fit <- CHFU %>% group_by(plot, subplot) %>% summarise (seeds = sum(seed))
CHFU.fit <- left_join(distances, CHFU.fit,by= c("plot", "subplot"))
CHFU.fit$seeds[is.na(CHFU.fit$seeds)] <- 0

chfu.corr.s <- spline.correlog(x=CHFU.fit$x_coor2, y=CHFU.fit$y_coor2,
                               z=CHFU.fit$seeds, resamp=100, quiet=TRUE)
plot(chfu.corr.s, main= " Spatial Autocorrelation CHFU fitness (seeds) across plots")

moran.test(CHFU.fit$seeds,mat2listw(plots.dists.inv)) # I= 0.1032
moran.test(CHFU.fit$seeds, nb2listw(w5)) # vecinos, I= 0.1921
moran.plot(CHFU.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHFU fitness (seeds) across plots")
moran.mc(CHFU.fit$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01
#IB: The plot stills show a very weak pattern. 

#LEMA

LEMA.fit <- LEMA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
LEMA.fit <- left_join(distances, LEMA.fit, by= c("plot", "subplot"))
LEMA.fit$seeds[is.na(LEMA.fit$seeds)] <- 0

lema.corr.s <- spline.correlog(x=LEMA.fit$x_coor2, y=LEMA.fit$y_coor2,
                               z=LEMA.fit$seeds, resamp=100, quiet=TRUE)
plot(lema.corr.s, main= " Spatial Autocorrelation LEMA fitness (seeds) across plots")

moran.test(LEMA.fit$seeds,mat2listw(plots.dists.inv)) # I= 0.1065
moran.test(LEMA.fit$seeds, nb2listw(w5)) # vecinos, I= 0.2095
moran.plot(LEMA.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation LEMA fitness (seeds) across plots")
moran.mc(LEMA.fit$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01
#IB: Same here

#PUPA 

PUPA.fit <- PUPA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
PUPA.fit <- left_join(distances, PUPA.fit, by= c("plot", "subplot"))
PUPA.fit$seeds [is.na(PUPA.fit$seeds )] <- 0

pupa.corr.s <- spline.correlog(x=PUPA.fit$x_coor2, y=PUPA.fit$y_coor2,
                               z=PUPA.fit$seeds, resamp=100, quiet=TRUE)
plot(pupa.corr.s, main= " Spatial Autocorrelation PUPA fitness (seeds) across plots")

moran.test(PUPA.fit$seeds,mat2listw(plots.dists.inv)) # 0.058
moran.test(PUPA.fit$seeds, nb2listw(w5)) # vecinos, I=  0.1031
moran.plot(PUPA.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation PUPA fitness (seeds) across plots")
moran.mc(PUPA.fit$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#MESU

MESU.fit <- MESU %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
MESU.fit <- left_join(distances, MESU.fit, by= c("plot", "subplot"))
MESU.fit$seeds[is.na(MESU.fit$seeds)] <- 0 
mesu.corr.s <- spline.correlog(x=MESU.fit$x_coor2, y=MESU.fit$y_coor2,
                               z=MESU.fit$seeds, resamp=100, quiet=TRUE)
plot(mesu.corr.s, main= " Spatial Autocorrelation MESU fitness (seeds) across plots")

moran.test(MESU.fit$seeds,mat2listw(plots.dists.inv)) # 0.27
moran.test(MESU.fit$seeds, nb2listw(w5)) # vecinos, I= 0.3413
moran.plot(MESU.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation MESU fitness (seeds) across plots")
moran.mc(MESU.fit$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#CETE 
CETE.fit <- CETE %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
CETE.fit <- left_join(distances, CETE.fit, by= c("plot", "subplot"))
CETE.fit$seeds <- CETE.fit$seeds
CETE.fit$seeds [is.na(CETE.fit$seeds )] <- 0

cete.corr.s <- spline.correlog(x=CETE.fit$x_coor2, y=CETE.fit$y_coor2,
                               z=CETE.fit$seeds, resamp=100, quiet=TRUE)
plot(cete.corr.s, main= " Spatial Autocorrelation CETE fitness (seeds) across plots")

moran.test(CETE.fit$seeds,mat2listw(plots.dists.inv)) # 0.118
moran.test(CETE.fit$seeds, nb2listw(w5)) # vecinos, I= 0.1894
moran.plot(CETE.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CETE fitness (seeds) across plots")
moran.mc(CETE.fit$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#SPRU 
SPRU.fit <- SPRU %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
SPRU.fit <- left_join(distances, SPRU.fit, by= c("plot", "subplot"))
SPRU.fit$seeds <- SPRU.fit$seeds
SPRU.fit$seeds [is.na(SPRU.fit$seeds )] <- 0

spru.corr.s <- spline.correlog(x=SPRU.fit$x_coor2, y=SPRU.fit$y_coor2,
                               z=SPRU.fit$seeds, resamp=100, quiet=TRUE)
plot(spru.corr.s, main= " Spatial Autocorrelation SPRU fitness (seeds) across plots")

moran.test(SPRU.fit$seeds,mat2listw(plots.dists.inv)) # 0.2177
moran.test(SPRU.fit$seeds, nb2listw(w5)) # vecinos, I= 0.2635
moran.plot(SPRU.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SPRU fitness (seeds) across plots")
moran.mc(SPRU.fit$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#SOAS 
SOAS.fit <- SOAS %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
SOAS.fit <- left_join(distances, SOAS.fit, by= c("plot", "subplot"))
SOAS.fit$seeds <- SOAS.fit$seeds
SOAS.fit$seeds [is.na(SOAS.fit$seeds )] <- 0

soas.corr.s <- spline.correlog(x=SOAS.fit$x_coor2, y=SOAS.fit$y_coor2,
                               z=SOAS.fit$seeds, resamp=100, quiet=TRUE)
plot(soas.corr.s, main= " Spatial Autocorrelation SOAS fitness (seeds) across plots")

moran.test(SOAS.fit$seeds,mat2listw(plots.dists.inv)) #  0.1463
moran.test(SOAS.fit$seeds, nb2listw(w5)) # vecinos, I=  0.20
moran.plot(SOAS.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SOAS fitness (seeds) across plots")
moran.mc(SOAS.fit$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#BEMA
BEMA.fit <- BEMA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
BEMA.fit <- left_join(distances, BEMA.fit, by= c("plot", "subplot"))
BEMA.fit$seeds <- BEMA.fit$seeds
BEMA.fit$seeds [is.na(BEMA.fit$seeds )] <- 0

bema.corr.s <- spline.correlog(x=BEMA.fit$x_coor2, y=BEMA.fit$y_coor2,
                               z=BEMA.fit$seeds, resamp=100, quiet=TRUE)
plot(bema.corr.s, main= " Spatial Autocorrelation BEMA fitness (seeds) across plots")

moran.test(BEMA.fit$seeds,mat2listw(plots.dists.inv)) #  0.11
moran.test(BEMA.fit$seeds, nb2listw(w5)) # vecinos, I=  0.2363
moran.plot(BEMA.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation BEMA fitness (seeds) across plots")
moran.mc(BEMA.fit$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#SCLA
SCLA.fit <- SCLA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
SCLA.fit <- left_join(distances, SCLA.fit, by= c("plot", "subplot"))
SCLA.fit$seeds <- SCLA.fit$seeds
SCLA.fit$seeds [is.na(SCLA.fit$seeds )] <- 0

scla.corr.s <- spline.correlog(x=SCLA.fit$x_coor2, y=SCLA.fit$y_coor2,
                               z=SCLA.fit$seeds, resamp=100, quiet=TRUE)
plot(scla.corr.s, main= " Spatial Autocorrelation SCLA fitness (seeds) across plots")

moran.test(SCLA.fit$seeds,mat2listw(plots.dists.inv)) # 0.1643
moran.test(SCLA.fit$seeds, nb2listw(w5)) # vecinos, I= 0.2720
moran.plot(SCLA.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SCLA fitness (seeds) across plots")
moran.mc(SCLA.fit$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#CHMI
CHMI.fit <- CHMI %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed))
CHMI.fit <- left_join(distances, CHMI.fit, by= c("plot", "subplot"))
CHMI.fit$seeds [is.na(CHMI.fit$seeds )] <- 0

chmi.corr.s <- spline.correlog(x=CHMI.fit$x_coor2, y=CHMI.fit$y_coor2,
                               z=CHMI.fit$seeds, resamp=100, quiet=TRUE) 
plot(chmi.corr.s, main= " Spatial Autocorrelation CHMI fitness (seeds) across plots")

moran.test(CHMI.fit$seeds,mat2listw(plots.dists.inv)) # 0.0556
moran.test(CHMI.fit$seeds, nb2listw(w5)) # vecinos, I= 0.1087
moran.plot(CHMI.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHMI fitness (seeds) across plots")
moran.mc(CHMI.fit$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#global fitness

juntas.plantas.fit <- plantas[,c("plot", "subplot","seed")]
juntas.plantas.fit <-na.omit(juntas.plantas.fit)
juntas.plantas.fit <- juntas.plantas.fit %>% group_by(plot, subplot) %>% summarise (seeds = sum(seed))
juntas.plantas.fit <- left_join(distances, juntas.plantas.fit, by= c("plot", "subplot"))
juntas.plantas.fit$seeds[is.na(juntas.plantas.fit$seeds)] <- 0

total.corr.pl.s <- spline.correlog(x=juntas.plantas.fit$x_coor2, y=juntas.plantas.fit$y_coor2,
                                   z=juntas.plantas.fit$seeds, resamp=100, quiet=TRUE)
plot(total.corr.pl.s, main= "Spatial Autocorrelation plant fitness (seeds) across plots")

moran.test(juntas.plantas.fit$seeds,mat2listw(plots.dists.inv)) # I= 0.1510
moran.test(juntas.plantas.fit$seeds, nb2listw(w5)) # vecinos, I= 0.2596
moran.plot(juntas.plantas.fit$seeds,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation plants fitness (seeds) across plots")
moran.mc(juntas.plantas.fit$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01


par(mfrow=c(2,2))
plot(chfu.corr.s, main= " CHFU fitness (seeds) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(lema.corr.s, main= " LEMA fitness (seeds) distributionfitness across plots",xlab="Distance (m)", ylab="Correlation")
plot(pupa.corr.s, main= " PUPA fitness (seeds) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(mesu.corr.s, main= " MESU fitness (seeds) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(scla.corr.s, main= " SCLA fitness (seeds) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(soas.corr.s, main= " SOAS fitness (seeds) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(spru.corr.s, main= " SPRU fitness (seeds) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(chmi.corr.s, main= " CHMI fitness (seeds) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(cete.corr.s, main= " CETE fitness (seeds) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(bema.corr.s, main= " BEMA fitness (seeds) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(total.corr.pl.s, main= "Plant fitness (seeds) distribution across plots",xlab="Distance (m)", ylab="Correlation")
par(mfrow=c(1,1))

par(mfrow=c(3,1))
plot(chfu.corr.s, main=expression(paste("Distribution",italic(" Chamaemelum fuscatum"), " fitness")), ylim= c(-0.5:0.5),xlab="Distance (m)", ylab="Correlation")
plot(lema.corr.s, main=expression(paste("Distribution",italic(" Leontodon maroccanus")," fitness")), ylim= c(-0.5:0.5),xlab="Distance (m)", ylab="Correlation")
plot(pupa.corr.s, main=expression(paste("Distribution",italic(" Pulicaria paludosa"), " fitness")), ylim= c(-0.5:0.5),xlab="Distance (m)", ylab="Correlation")
par(mfrow=c(1,1))

#IB: I think we can skip this one too for the presentation. 
#In any case, I would only explore the three species analyzed in detail for fitness

#Now with the data of fitness in fruits = (seeds/one fruit)*fruits----

#CHFU 

CHFU.fit.fr <- CHFU %>% group_by(plot, subplot) %>% summarise (seeds = sum(seed.indv))
CHFU.fit.fr <- left_join(distances, CHFU.fit.fr,by= c("plot", "subplot"))
CHFU.fit.fr$seeds[is.na(CHFU.fit.fr$seeds)] <- 0

chfu.corr.s.fr <- spline.correlog(x=CHFU.fit.fr$x_coor2, y=CHFU.fit.fr$y_coor2,
                               z=CHFU.fit.fr$seeds, resamp=100, quiet=TRUE)
plot(chfu.corr.s.fr, main= " Spatial Autocorrelation CHFU fitness (fruits) across plots")

moran.test(CHFU.fit.fr$seeds,mat2listw(plots.dists.inv)) # I= 0.0569
moran.test(CHFU.fit.fr$seeds, nb2listw(w5)) # vecinos, I= 0.1648
moran.plot(CHFU.fit.fr$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHFU fitness (fruits) across plots")
moran.mc(CHFU.fit.fr$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01
#IB: really small.

#LEMA

LEMA.fit.fr <- LEMA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed.indv))
LEMA.fit.fr <- left_join(distances, LEMA.fit.fr, by= c("plot", "subplot"))
LEMA.fit.fr$seeds[is.na(LEMA.fit.fr$seeds)] <- 0

lema.corr.s.fr <- spline.correlog(x=LEMA.fit.fr$x_coor2, y=LEMA.fit.fr$y_coor2,
                               z=LEMA.fit.fr$seeds, resamp=100, quiet=TRUE)
plot(lema.corr.s.fr, main= " Spatial Autocorrelation LEMA fitness (fruits) across plots")

moran.test(LEMA.fit.fr$seeds,mat2listw(plots.dists.inv)) # I= 0.1577
moran.test(LEMA.fit.fr$seeds, nb2listw(w5)) # vecinos, I= 0.1773
moran.plot(LEMA.fit.fr$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation LEMA fitness (fruits) across plots")
moran.mc(LEMA.fit.fr$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01
#A bit stronger... 

#PUPA 

PUPA.fit.fr <- PUPA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed.indv))
PUPA.fit.fr <- left_join(distances, PUPA.fit.fr, by= c("plot", "subplot"))
PUPA.fit.fr$seeds [is.na(PUPA.fit.fr$seeds )] <- 0

pupa.corr.s.fr <- spline.correlog(x=PUPA.fit.fr$x_coor2, y=PUPA.fit.fr$y_coor2,
                               z=PUPA.fit.fr$seeds, resamp=100, quiet=TRUE)
plot(pupa.corr.s.fr, main= " Spatial Autocorrelation PUPA fitness (fruits) across plots")

moran.test(PUPA.fit.fr$seeds,mat2listw(plots.dists.inv)) # 3.433128e-02
moran.test(PUPA.fit.fr$seeds, nb2listw(w5)) # vecinos, I=   0.0482428762 
moran.plot(PUPA.fit.fr$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation PUPA fitness (fruits) across plots")
moran.mc(PUPA.fit.fr$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#MESU

MESU.fit.fr <- MESU %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed.indv))
MESU.fit.fr <- left_join(distances, MESU.fit.fr, by= c("plot", "subplot"))
MESU.fit.fr$seeds[is.na(MESU.fit.fr$seeds)] <- 0 
mesu.corr.s.fr <- spline.correlog(x=MESU.fit.fr$x_coor2, y=MESU.fit.fr$y_coor2,
                               z=MESU.fit.fr$seeds, resamp=100, quiet=TRUE)
plot(mesu.corr.s.fr, main= " Spatial Autocorrelation MESU fitness (fruits) across plots")

moran.test(MESU.fit.fr$seeds,mat2listw(plots.dists.inv)) # 0.1588
moran.test(MESU.fit.fr$seeds, nb2listw(w5)) # vecinos, I= 0.1638
moran.plot(MESU.fit.fr$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation MESU fitness (fruits) across plots")
moran.mc(MESU.fit.fr$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01


#CETE 
CETE.fit.fr <- CETE %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed.indv))
CETE.fit.fr <- left_join(distances, CETE.fit.fr, by= c("plot", "subplot"))
CETE.fit.fr$seeds <- CETE.fit.fr$seeds
CETE.fit.fr$seeds [is.na(CETE.fit.fr$seeds )] <- 0

cete.corr.s.fr <- spline.correlog(x=CETE.fit.fr$x_coor2, y=CETE.fit.fr$y_coor2,
                               z=CETE.fit.fr$seeds, resamp=100, quiet=TRUE)
plot(cete.corr.s.fr, main= " Spatial Autocorrelation CETE fitness (fruits) across plots")

moran.test(CETE.fit.fr$seeds,mat2listw(plots.dists.inv)) # 0.1231
moran.test(CETE.fit.fr$seeds, nb2listw(w5)) # vecinos, I= 0.1945
moran.plot(CETE.fit.fr$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CETE fitness (fruits) across plots")
moran.mc(CETE.fit.fr$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#SPRU 
SPRU.fit.fr <- SPRU %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed.indv))
SPRU.fit.fr <- left_join(distances, SPRU.fit.fr, by= c("plot", "subplot"))
SPRU.fit.fr$seeds [is.na(SPRU.fit.fr$seeds )] <- 0

spru.corr.s.fr <- spline.correlog(x=SPRU.fit.fr$x_coor2, y=SPRU.fit.fr$y_coor2,
                               z=SPRU.fit.fr$seeds, resamp=100, quiet=TRUE)
plot(spru.corr.s.fr, main= " Spatial Autocorrelation SPRU fitness (fruits) across plots")

moran.test(SPRU.fit.fr$seeds,mat2listw(plots.dists.inv)) # 0.1534
moran.test(SPRU.fit.fr$seeds, nb2listw(w5)) # vecinos, I= 0.1667
moran.plot(SPRU.fit.fr$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SPRU fitness (fruits) across plots")
moran.mc(SPRU.fit.fr$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#SOAS 
SOAS.fit.fr <- SOAS %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed.indv))
SOAS.fit.fr <- left_join(distances, SOAS.fit.fr, by= c("plot", "subplot"))
SOAS.fit.fr$seeds [is.na(SOAS.fit.fr$seeds )] <- 0

soas.corr.s.fr <- spline.correlog(x=SOAS.fit.fr$x_coor2, y=SOAS.fit.fr$y_coor2,
                               z=SOAS.fit.fr$seeds, resamp=100, quiet=TRUE)
plot(soas.corr.s.fr, main= " Spatial Autocorrelation SOAS fitness (fruits) across plots")

moran.test(SOAS.fit.fr$seeds,mat2listw(plots.dists.inv)) #  0.1355
moran.test(SOAS.fit.fr$seeds, nb2listw(w5)) # vecinos, I=  0.2090
moran.plot(SOAS.fit.fr$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SOAS fitness (fruits) across plots")
moran.mc(SOAS.fit.fr$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#BEMA
BEMA.fit.fr <- BEMA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed.indv))
BEMA.fit.fr <- left_join(distances, BEMA.fit.fr, by= c("plot", "subplot"))
BEMA.fit.fr$seeds [is.na(BEMA.fit.fr$seeds )] <- 0

bema.corr.s.fr <- spline.correlog(x=BEMA.fit.fr$x_coor2, y=BEMA.fit.fr$y_coor2,
                               z=BEMA.fit.fr$seeds, resamp=100, quiet=TRUE)
plot(bema.corr.s.fr, main= " Spatial Autocorrelation BEMA fitness (fruits) across plots")

moran.test(BEMA.fit.fr$seeds,mat2listw(plots.dists.inv)) #  0.11
moran.test(BEMA.fit.fr$seeds, nb2listw(w5)) # vecinos, I=  0.17
moran.plot(BEMA.fit.fr$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation BEMA fitness (fruits) across plots")
moran.mc(BEMA.fit.fr$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#SCLA
SCLA.fit.fr <- SCLA %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed.indv))
SCLA.fit.fr <- left_join(distances, SCLA.fit.fr, by= c("plot", "subplot"))
SCLA.fit.fr$seeds [is.na(SCLA.fit.fr$seeds )] <- 0

scla.corr.s.fr <- spline.correlog(x=SCLA.fit.fr$x_coor2, y=SCLA.fit.fr$y_coor2,
                               z=SCLA.fit.fr$seeds, resamp=100, quiet=TRUE)
plot(scla.corr.s.fr, main= " Spatial Autocorrelation SCLA fitness (fruits) across plots")

moran.test(SCLA.fit.fr$seeds,mat2listw(plots.dists.inv)) # 0.14
moran.test(SCLA.fit.fr$seeds, nb2listw(w5)) # vecinos, I= 0.2391
moran.plot(SCLA.fit.fr$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation SCLA fitness (fruits) across plots")
moran.mc(SCLA.fit.fr$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01

#CHMI
CHMI.fit.fr <- CHMI %>% group_by(plot, subplot, Plant) %>% summarise (seeds = sum(seed.indv))
CHMI.fit.fr <- left_join(distances, CHMI.fit.fr, by= c("plot", "subplot"))
CHMI.fit.fr$seeds [is.na(CHMI.fit.fr$seeds )] <- 0

chmi.corr.s.fr <- spline.correlog(x=CHMI.fit.fr$x_coor2, y=CHMI.fit.fr$y_coor2,
                               z=CHMI.fit.fr$seeds, resamp=100, quiet=TRUE) 
plot(chmi.corr.s.fr, main= " Spatial Autocorrelation CHMI fitness (fruits) across plots")

moran.test(CHMI.fit.fr$seeds,mat2listw(plots.dists.inv)) # 1.136502e-02
moran.test(CHMI.fit.fr$seeds, nb2listw(w5)) # vecinos, I= 0.0181689317 , p-value= 0.1455
moran.plot(CHMI.fit.fr$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHMI fitness (fruits) across plots")
moran.mc(CHMI.fit.fr$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.06

#global fitness

juntas.plantas.fit.fr <- plantas[,c("plot", "subplot","seed.indv")]
juntas.plantas.fit.fr <-na.omit(juntas.plantas.fit.fr)
juntas.plantas.fit.fr <- juntas.plantas.fit.fr %>% group_by(plot, subplot) %>% summarise (seeds = sum(seed.indv))
juntas.plantas.fit.fr <- left_join(distances, juntas.plantas.fit.fr, by= c("plot", "subplot"))
juntas.plantas.fit.fr$seeds[is.na(juntas.plantas.fit.fr$seeds)] <- 0

total.corr.pl.s.fr <- spline.correlog(x=juntas.plantas.fit.fr$x_coor2, y=juntas.plantas.fit.fr$y_coor2,
                                   z=juntas.plantas.fit.fr$seeds, resamp=100, quiet=TRUE)
plot(total.corr.pl.s.fr, main= "Spatial Autocorrelation plant fitness (fruits) across plots")

moran.test(juntas.plantas.fit.fr$seeds,mat2listw(plots.dists.inv)) # I= 0.1463
moran.test(juntas.plantas.fit.fr$seeds, nb2listw(w5)) # vecinos, I= 0.2188
moran.plot(juntas.plantas.fit.fr$seeds,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation plants fitness (fruits) across plots")
moran.mc(juntas.plantas.fit.fr$seeds, mat2listw(plots.dists.inv), nsim=99) #p-value=0.01


par(mfrow=c(4,3))
plot(chfu.corr.s.fr, main= " CHFU fitness (fruits) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(lema.corr.s.fr, main= " LEMA fitness (fruits) distributionfitness across plots",xlab="Distance (m)", ylab="Correlation")
plot(pupa.corr.s.fr, main= " PUPA fitness (fruits) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(mesu.corr.s.fr, main= " MESU fitness (fruits) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(scla.corr.s.fr, main= " SCLA fitness (fruits) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(soas.corr.s.fr, main= " SOAS fitness (fruits) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(spru.corr.s.fr, main= " SPRU fitness (fruits) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(chmi.corr.s.fr, main= " CHMI fitness (fruits) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(cete.corr.s.fr, main= " CETE fitness (fruits) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(bema.corr.s.fr, main= " BEMA fitness (fruits) distribution across plots",xlab="Distance (m)", ylab="Correlation")
plot(total.corr.pl.s.fr, main= "Plant fitness (fruits) distribution across plots",xlab="Distance (m)", ylab="Correlation")
par(mfrow=c(1,1))
#IB: Same here, I would show only the results for the three species studied in detail in the sems.
