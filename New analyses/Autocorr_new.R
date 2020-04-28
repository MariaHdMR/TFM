library(tidyverse)
library(ape)
library(ncf)
#install.packages("spdep") -> for Moran's I
library(spdep)
library(reshape2)
library(vegan)

#I de Moran. Valores entre -1 y 1. Si es positivo esta positivamente correlacionado, y si es negativo no hay
#   correlacion. Lugares más proximos se parecen mas(+ correlacion) que a los lugares lejanos. 

#cargar datos y limpiar

va <- read.table("data/Metadata_Pollinators_2019_2016_bueno.csv", header=T, sep=";")
head(va)
plantas <- read.table("data/Abun_19.csv", header=T, sep= ";")
va19 <- subset(va, Year== "2019")
pol <- va19 %>% group_by(Plot, Subplot, Group) %>% summarise (num.visitors = sum(Visits))
pol <-pol[which(complete.cases(pol)),]
pol <- subset(pol, Plot != "OUT")
pol <- subset(pol, Subplot != "OUT")

#voy a ver si mis datos son normales -> no lo son. 
hist(pol$num.visitors)

#ahora creo los datos de coordenadas across plots
distances <- read.csv("data/caracolesplotposition.csv", sep = ";")
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
dis$x_coor2 <- dis$x_coor + dis$cumulative_x
dis$y_coor2 <- dis$y_coor + dis$cumulative_y
head(dis)
disfinal <- dis[,c("plot", "position","x_coor2", "y_coor2")]
disfinal$Plot <- disfinal$plot
disfinal$Subplot <- disfinal$position
disfinal1 <- disfinal[,c("Plot", "Subplot","x_coor2", "y_coor2")]
distancias.matriz <-as.matrix(dist(cbind(disfinal1$x_coor2, disfinal1$y_coor2)))
    #aqui hago la matriz inversa y la diagonal con 0
plots.dists.inv <- 1/distancias.matriz
diag(plots.dists.inv) <- 0
plots.dists.inv[1:5,1:5]
#
#analisis por grupos
#beetles ----
pol.beetle <- subset(pol, Group == "Beetle")
pol.beetle$Plot <- as.numeric(as.character(pol.beetle$Plot))
pol.beetle.B <- pol.beetle[,c("Plot", "Subplot","num.visitors")] #datos de plot, subplot, y visitas de BEETLES
beetle <- full_join(pol.beetle.B, disfinal1, by= c("Plot", "Subplot"))
beetle$num.visitors[is.na(beetle$num.visitors)] <- 0

        #este es el grafico de la correlacion espacial
bet.corr <- spline.correlog(x=beetle$x_coor2, y=beetle$y_coor2,
                           z=beetle$num.visitors, resamp=100, quiet=TRUE)
plot(bet.corr, main= " Spatial Autocorrelation Beetles across plots")
    #test de moran. Aquí quiero obtener El estadistico de Moran y p.value 

moran.test(beetle$num.visitors,mat2listw(plots.dists.inv)) # I= 0.3497
moran.plot(beetle$num.visitors,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation Beetles across plots")

#flies----
flies <- subset(pol, Group == "Fly")
flies$Plot <- as.numeric(as.character(flies$Plot))
flies.B <- flies[,c("Plot", "Subplot","num.visitors")] #datos de plot, subplot, y visitas 
flies.t <- full_join(flies.B, disfinal1, by= c("Plot", "Subplot"))
flies.t$num.visitors[is.na(flies.t$num.visitors)] <- 0

flies.corr <- spline.correlog(x=flies.t$x_coor2, y=flies.t$y_coor2,
                           z=flies.t$num.visitors, resamp=100, quiet=TRUE)
plot(flies.corr, main= "Spatial autocorrelation flies across plots")

moran.test(flies.t$num.visitors,mat2listw(plots.dists.inv)) # I= 0.3447
moran.plot(beetle$num.visitors,mat2listw(plots.dists.inv), main= "Spatial autocorrelation flies across plots")

#butterflies----
but <- subset(pol, Group == "Butterfly")
but$Plot <- as.numeric(as.character(but$Plot))
but.B <- but[,c("Plot", "Subplot","num.visitors")] #datos de plot, subplot, y visitas 
but.t <- full_join(but.B, disfinal1, by= c("Plot", "Subplot"))
but.t$num.visitors[is.na(but.t$num.visitors)] <- 0

but.corr <- spline.correlog(x=but.t$x_coor2, y=but.t$y_coor2,
                                 z=but.t$num.visitors, resamp=100, quiet=TRUE)
plot(but.corr, main= "Spatial autocorrelation butterflies across plots")

moran.test(but.t$num.visitors,mat2listw(plots.dists.inv)) # I= 0.2989
moran.plot(but.t$num.visitors,mat2listw(plots.dists.inv),  main= "Spatial autocorrelation butterflies across plots")

#bees----

bee <- subset(pol, Group == "Bee")
bee$Plot <- as.numeric(as.character(bee$Plot))
bee.B <- bee[,c("Plot", "Subplot","num.visitors")] #datos de plot, subplot, y visitas 
bee.t <- full_join(bee.B, disfinal1, by= c("Plot", "Subplot"))
bee.t$num.visitors[is.na(bee.t$num.visitors)] <- 0

bee.corr <- spline.correlog(x=bee.t$x_coor2, y=bee.t$y_coor2,
                               z=bee.t$num.visitors, resamp=100, quiet=TRUE)
plot(bee.corr, main= "Spatial autocorrelation bee across plots")

moran.test(bee.t$num.visitors,mat2listw(plots.dists.inv))# I = 0.39
moran.plot(bee.t$num.visitors,mat2listw(plots.dists.inv), main= "Spatial autocorrelation bee across plots")


############################ problems 
#me gustaria probar esto, que es coger los 8 vecinos de un dato y ver cuanto se parecen estos 8 vecinos a mi
#   dato, pero me da error. Seria por comprobar los analisis anteriores. 
disfinal2$x_coor2 <- as.numeric(disfinal2$x_coor2)
disfinal2$y_coor2 <- as.numeric(disfinal2$y_coor2)
w5 <- knn2nb(knearneigh(disfinal2, k=8))
moran.test(bird$nSpecies, nb2listw(w2))
###########################

#polinizadores general ----
junto <- pol[,c("Plot", "Subplot","num.visitors")]
polinizadores <- junto %>% group_by(Plot, Subplot) %>% summarise (visit = sum(num.visitors))
polinizadores$Plot <- as.numeric(polinizadores$Plot)
final <- full_join(polinizadores, disfinal1, by= c("Plot", "Subplot"))
final.1 <- subset(final, Subplot != "OUT")
final.1$visit[is.na(final.1$visit)] <- 0

total.corr <- spline.correlog(x=final.1$x_coor2, y=final.1$y_coor2,
                                z=final.1$visit, resamp=100, quiet=TRUE)
plot(total.corr, main= "Spatial Autocorrelation pollinators across plots")

moran.test(final.1$visit,mat2listw(plots.dists.inv)) # I= 0.23 de manera genral hay menos 
#                                                       agregación que por grupos

moran.plot(final.1$visit,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation pollinators across plots")
