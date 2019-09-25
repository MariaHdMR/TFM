#test for diferences in interaction diversity:

library(reshape2)
library(devtools)
#install_github("ibartomeus/betalink", ref = "new_features") #just once
library(betalink)
library(vegan)
library(ade4)
library(gstat)
library(lattice)
library(aqfig)


va <- read.table("data/FV_16_19.csv", header=T, sep= ";")
head(va)

#build a list of matrices of plants per visits for each plot
levels(va$Plant_Simple)
ntw <- list()
for (i in 1:9){
  temp <- subset(va, Plot == i & Year == 2019 & Plant_Simple %in% c("LEMA", "CHFU",
                                                             "PUPA", "ME"))
  temp <- droplevels(temp)
  comm <- dcast(temp, Plant_Simple ~ Group, fun.aggregate = sum, value.var = "Visits")
  rownames(comm) <- comm$Plant_Simple
  ntw[[i]] <- comm[,-1, drop = FALSE]
}

betalink.dist

W <- ntw
partitionOS <- matrix(NA, 9,9)
for (i in c(1:(length(W) - 1))) {
  for (j in c((i + 1):(length(W)))) {
    partitionOS[j,i] <- betalink(W[[i]], W[[j]])$OS
  }
}

partitionWN <- matrix(NA, 9,9)
for (i in c(1:(length(W) - 1))) {
  for (j in c((i + 1):(length(W)))) {
    partitionWN[j,i] <- betalink(W[[i]], W[[j]])$WN
  }
}

#MARIA, para sacar la beta de plantas: $L, polinizadores $U.
partitionL <- matrix(NA, 9,9)
for (i in c(1:(length(W) - 1))) {
  for (j in c((i + 1):(length(W)))) {
    partitionL[j,i] <- betalink(W[[i]], W[[j]])$L
  }
}

partitionU <- matrix(NA, 9,9)
for (i in c(1:(length(W) - 1))) {
  for (j in c((i + 1):(length(W)))) {
    partitionU[j,i] <- betalink(W[[i]], W[[j]])$U
  }
}

#Add distance among plots.

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


#dis is at subplot level.

#tapply(dis$x_coor2, INDEX = dis$plot, FUN = mean)
#tapply(dis$y_coor, INDEX = dis$plot, FUN = mean)

dis_plot <- tesaurus[,c(1,4,5)]

#distance

spatial <- vegdist(dis_plot[,-1], method = "euclidean", diag = FALSE)

#Relate link betadiversity to distance
partitionWN <- as.dist(partitionWN)
str(partitionWN)
str(spatial)
mantel.rtest(partitionWN, spatial, nrepet = 9999)
# Hay una gran estructuracion de la interacciones

partitionOS <- as.dist(partitionOS)
mantel.rtest(partitionOS, spatial, nrepet = 9999) #creo que esto era las interacciones comunes
#Pero debido a especies (BetaOS muy)

partitionL <- as.dist(partitionL)#plantas
mantel.rtest(partitionL, spatial, nrepet = 9999)

partitionU <- as.dist(partitionU)#polinizadores
mantel.rtest(partitionU, spatial, nrepet = 9999)

#plot
partitionU.1<-as.matrix(partitionU)
Bichos.df <-as.data.frame(partitionU.1)

#Not tested becasue I dont have XQuartz
partitionWN1 <- as.matrix(partitionWN)
geo_data3 <- as.geodata(partitionWN1, coords.col = 7:8, data.col = 3)# problemas ----
#vis3 <- likfit(geo_data3, ini = c(1,0.5), fix.nugget = T)
#pred.grid3 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
#kc_ph_1133 <- krige.conv(geo_data3, loc = pred.grid3, krige = krige.control(obj.m = vis3))
#image(kc_ph_1133, loc = pred.grid3, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Beetles abundance distribution in plot 3")
#vertical.image.legend(col=rainbow(15),zlim=c(min(kc_ph_1133$predict),max(kc_ph_1133$predict))) #este es el unico con valores significativos, el resto se aproximan a la significacion
#plantas
#partitionWN1 <- as.matrix(partitionWN)
partitionL1 <- as.matrix(partitionL)
geo_data4 <- as.geodata(partitionL1, coords.col = 7:8, data.col = 3)
vis4 <- likfit(geo_data4, ini = c(1,0.5), fix.nugget = T) ## problem ----
#pred.grid4 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
#kc_ph_1134 <- krige.conv(geo_data4, loc = pred.grid4, krige = krige.control(obj.m = vis4))
#image(kc_ph_1134, loc = pred.grid4, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Beetles abundance distribution in plot 3")
#vertical.image.legend(col=rainbow(15),zlim=c(min(kc_ph_1134$predict),max(kc_ph_1134$predict))) #este es el unico con valores significativos, el resto se aproximan a la significacion

#visitors
partitionU1 <- as.matrix(partitionU)
geo_dataU <- as.geodata(partitionU1, coords.col = spatial, data.col = c(1:9,1:9))
visU <- likfit(geo_dataU, ini = c(1,0.5), fix.nugget = T)#problem ----

#pred.gridU <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
#kc_ph_113U <- krige.conv(geo_dataU, loc = pred.gridU, krige = krige.control(obj.m = visU))
#image(kc_ph_113U, loc = pred.gridU, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Beetles abundance distribution in plot 3")
#vertical.image.legend(col=rainbow(15),zlim=c(min(kc_ph_113U$predict),max(kc_ph_113U$predict)))

#subplot level
#build a list of matrices of plants per visits for each plot
levels(va$Plant_Simple)
ntw.1 <- list()
for (i in 1:9){
  temp.1 <- subset(va, Plot == i & Year == 2019 & Plant_Simple %in% c("LEMA", "CHFU",
                                                                    "PUPA", "ME"))
  temp.1 <- droplevels(temp.1)
  comm.1 <- dcast(temp.1, Plant_Simple ~ Group, fun.aggregate = sum, value.var = "Visits")
  rownames(comm.1) <- comm$Plant_Simple
  ntw.1[[i]] <- comm.1[,-1, drop = FALSE]
}

betalink.dist

W <- ntw
partitionOS <- matrix(NA, 9,9)
for (i in c(1:(length(W) - 1))) {
  for (j in c((i + 1):(length(W)))) {
    partitionOS[j,i] <- betalink(W[[i]], W[[j]])$OS
  }
}

partitionWN <- matrix(NA, 9,9)
for (i in c(1:(length(W) - 1))) {
  for (j in c((i + 1):(length(W)))) {
    partitionWN[j,i] <- betalink(W[[i]], W[[j]])$WN
  }
}

#MARIA, para sacar la beta de plantas: $L, polinizadores $U.
partitionL <- matrix(NA, 9,9)
for (i in c(1:(length(W) - 1))) {
  for (j in c((i + 1):(length(W)))) {
    partitionL[j,i] <- betalink(W[[i]], W[[j]])$L
  }
}

partitionU <- matrix(NA, 9,9)
for (i in c(1:(length(W) - 1))) {
  for (j in c((i + 1):(length(W)))) {
    partitionU[j,i] <- betalink(W[[i]], W[[j]])$U
  }
}




#dis
espacio <- dist(dis [,c(8,9)], method= "euclidean", diag =T, upper =T)
mantel.rtest(partitionWN, espacio, nrepet = 9999)


#prueba: otro metodo para obtener los data frames de los visitantes across plots 
#voy a sacar todas las visitas
#pol.9
pol.9$plot <- pol.9$Plot
pol.9$plot <- as.numeric(pol.9$plot)
bichos <- dplyr::left_join(dist2,pol.9)
bichos[is.na(bichos)] <- 0
Vgeo_data <- as.geodata(bichos[1:778,], coords.col = 4:5, data.col = 10)
Vvis <- likfit(Vgeo_data, ini = c(1,0.5), fix.nugget = T)#aqui hay un problema----
#Vpred.grid <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
#kc_V<- krige.conv(t.butgeo_data3, loc = Vpred.grid, krige = krige.control(obj.m = Vvis)) 
#image(kc_V, loc = Vpred.grid, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Distribution of Visitor abundances across plots")
#vertical.image.legend(col=rainbow(15),zlim=c(min(kc_V$predict),max(kc_V$predict)))




#landscape level ----
# BEETLES: primero tengo que obtener una matriz de distancias de todos los beetles a nivel de plot, tengo que unirle
#los datos llamados dis. Calcular B-diversidad, y luego hacer Bioenv.
va <- read.table("data/FV_16_19.csv", header=T, sep= ";")
va19 <- subset(va, Year== "2019")
pol.9 <- va19 %>% group_by(Plot, Subplot, Group, Species) %>% summarise (num.visitors = sum(Visits))
#pol.9 <-pol.9[which(complete.cases(pol.9)),]
pol.9 <- subset(pol.9, Plot != "OUT")

dist2<-dis[,c( "plot","cell", "position", "x_coor2", "y_coor2")]#subplots
dist2$Subplot <- dist2$position
dist_plot <- dis[,c("plot","cell","position", "cumulative_x", "cumulative_y")]#plots
#beetles
pol.beetle9 <- subset(pol.9, Group == "Beetle")
beetles.total <- pol.beetle9 %>% group_by(Plot, Subplot, Species) %>% summarise (visits = sum(num.visitors))
columnsbet.total <- dcast(beetles.total, Plot+ Subplot ~ Species, fun.aggregate = sum, value.var = "visits")
columnsbet.total$plot <- columnsbet.total$Plot
columnsbet.total$plot <- as.numeric(columnsbet.total$plot)
sitios.be.t <- dplyr::left_join (dist2,columnsbet.total)
sitios.be.t[is.na(sitios.be.t)] <- 0

d.beetle.total <-dist(sitios.be.t [,c(8:16)], method= "euclidean", diag =T, upper =T)

m.beetle.total <- vegdist(d.beetle.total, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
sites.beet <-dist(sitios.be.t [,c(4,5)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.beetle.total, sites.beet,nrepet = 9999)
#mean(m.beetle.total)
#sd(m.beetle.total)

#plot
beetles.plot <- pol.beetle9 %>% group_by(Plot,Species) %>% summarise (visits = sum(num.visitors))
columnsbet.plot <- dcast(beetles.plot, Plot ~ Species, fun.aggregate = sum, value.var = "visits")
columnsbet.plot$plot <- columnsbet.plot$Plot
columnsbet.plot$plot <- as.numeric(columnsbet.plot$plot)
sitios.be.t.1 <- dplyr::left_join (dis_plot,columnsbet.plot)
sitios.be.t.1[is.na(sitios.be.t.1)] <- 0

d.beetle.total.1 <-dist(sitios.be.t.1 [,c(5:13)], method= "euclidean", diag =T, upper =T)

m.beetle.total.1 <- vegdist(d.beetle.total.1, method="morisita", binary=FALSE, diag= T, upper=T,
                          na.rm = FALSE) 
sites.beet.1 <-dist(sitios.be.t.1 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.beetle.total.1, sites.beet.1,nrepet = 9999)


#fly
pol.fly9 <- subset(pol.9, Group == "Fly")
fly.total <- pol.fly9 %>% group_by(Plot, Subplot, Species) %>% summarise (visits = sum(num.visitors))
columnsfly.total <- dcast(fly.total, Plot+ Subplot ~ Species, fun.aggregate = sum, value.var = "visits")
columnsfly.total$plot <- columnsfly.total$Plot
columnsfly.total$plot <- as.numeric(columnsfly.total$plot)
sitios.fly.t <- dplyr::left_join (dist2,columnsfly.total)
sitios.fly.t[is.na(sitios.fly.t)] <- 0
d.fly.total <-dist(sitios.fly.t [,c(8:25)], method= "euclidean", diag =T, upper =T)

m.fly.total <- vegdist(d.fly.total, method="morisita", binary=FALSE, diag= T, upper=T,
                          na.rm = FALSE) 
sites.flyt <-dist(sitios.fly.t [,c(4,5)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.fly.total, sites.flyt,nrepet = 9999)



#plot
fly.plot <- pol.fly9 %>% group_by(Plot, Species) %>% summarise (visits = sum(num.visitors))
columnsfly.plot <- dcast(fly.plot, Plot ~ Species, fun.aggregate = sum, value.var = "visits")
columnsfly.plot$plot <- columnsfly.plot$Plot
columnsfly.plot$plot <- as.numeric(columnsfly.plot$plot)
sitios.fly.t.1 <- dplyr::left_join (dis_plot,columnsfly.plot)
sitios.fly.t.1[is.na(sitios.fly.t.1)] <- 0
d.fly.total.1 <-dist(sitios.fly.t.1 [,c(5:22)], method= "euclidean", diag =T, upper =T)

m.fly.total.1 <- vegdist(d.fly.total.1, method="morisita", binary=FALSE, diag= T, upper=T,
                       na.rm = FALSE) 
sites.flyt.1 <-dist(sitios.fly.t.1 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.fly.total.1, sites.flyt.1,nrepet = 9999)


#bee
pol.bees9 <- subset(pol.9, Group == "Bee")
Bees.total <- pol.bees9 %>% group_by(Plot, Subplot, Species) %>% summarise (visits = sum(num.visitors))
columnsbees.total <- dcast(Bees.total, Plot+ Subplot ~ Species, fun.aggregate = sum, value.var = "visits")
columnsbees.total$plot <- columnsbees.total$Plot
columnsbees.total$plot <- as.numeric(columnsbees.total$plot)
sitios.bees.t <- dplyr::left_join (dist2,columnsbees.total)
sitios.bees.t[is.na(sitios.bees.t)] <- 0
d.bees.total <-dist(sitios.bees.t [,c(8:12)], method= "euclidean", diag =T, upper =T)
m.bees.total <- vegdist(d.bees.total, method="morisita", binary=FALSE, diag= T, upper=T,
                       na.rm = FALSE) 
sites.beest <-dist(sitios.bees.t [,c(4,5)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bees.total, sites.beest,nrepet = 9999)

#plot

Bees.plot <- pol.bees9 %>% group_by(Plot,Species) %>% summarise (visits = sum(num.visitors))
columnsbees.plot <- dcast(Bees.plot, Plot ~ Species, fun.aggregate = sum, value.var = "visits")
columnsbees.plot$plot <- columnsbees.plot$Plot
columnsbees.plot$plot <- as.numeric(columnsbees.plot$plot)
sitios.bees.t.1 <- dplyr::left_join (dis_plot,columnsbees.plot)
sitios.bees.t.1[is.na(sitios.bees.t.1)] <- 0


d.bees.total.1 <-dist(sitios.bees.t.1 [,c(5:9)], method= "euclidean", diag =T, upper =T)

m.bees.total.1 <- vegdist(d.bees.total.1, method="morisita", binary=FALSE, diag= T, upper=T,
                        na.rm = FALSE) 
sites.beest.1 <-dist(sitios.bees.t.1 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bees.total.1, sites.beest.1,nrepet = 9999)



#butterfly
pol.but9 <- subset(pol.9, Group == "Butterfly")
But.total <- pol.but9 %>% group_by(Plot, Subplot, Species) %>% summarise (visits = sum(num.visitors))
columnsbut.total <- dcast(But.total, Plot+ Subplot ~ Species, fun.aggregate = sum, value.var = "visits")
columnsbut.total$plot <- columnsbut.total$Plot
columnsbut.total$plot <- as.numeric(columnsbut.total$plot)
sitios.but.t <- dplyr::left_join (dist2,columnsbut.total)
sitios.but.t[is.na(sitios.but.t)] <- 0

d.but.total <-dist(sitios.but.t [,c(8:11)], method= "euclidean", diag =T, upper =T)

m.but.total <- vegdist(d.but.total, method="morisita", binary=FALSE, diag= T, upper=T,
                        na.rm = FALSE) 
sites.butt <-dist(sitios.but.t [,c(4,5)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.but.total, sites.butt,nrepet = 9999)

But.total$plot <- But.total$Plot
But.total$plot <- as.numeric(But.total$plot)
prueba <- dplyr::left_join(dist2,But.total)
prueba[is.na(prueba)] <- 0

t.butgeo_data3 <- as.geodata(prueba[1:330,], coords.col = 4:5, data.col = 9)
t.butvis3 <- likfit(t.butgeo_data3, ini = c(1,0.5), fix.nugget = T)#aqui hay un problema----
#t.butpred.grid3 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
#kc_but_3<- krige.conv(t.butgeo_data3, loc = butpred.gri32, krige = krige.control(obj.m = t.butvis3)) 
#image(kc_but_3, loc = butpred.grid3, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Flies abundance distribution in plot 7")
#vertical.image.legend(col=rainbow(15),zlim=c(min(kc_but_3$predict),max(kc_but_3$predict)))

#plot

But.plot <- pol.but9 %>% group_by(Plot, Species) %>% summarise (visits = sum(num.visitors))
columnsbut.plot <- dcast(But.plot, Plot ~ Species, fun.aggregate = sum, value.var = "visits")
columnsbut.plot$plot <- columnsbut.plot$Plot
columnsbut.plot$plot <- as.numeric(columnsbut.plot$plot)
sitios.but.t.1 <- dplyr::left_join (dis_plot,columnsbut.plot)
sitios.but.t.1[is.na(sitios.but.t.1)] <- 0

d.but.plot <-dist(sitios.but.t.1 [,c(5:8)], method= "euclidean", diag =T, upper =T)

m.but.plot <- vegdist(d.but.plot, method="morisita", binary=FALSE, diag= T, upper=T,
                       na.rm = FALSE) 
sites.butt.1 <-dist(sitios.but.t.1 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.but.plot, sites.butt.1,nrepet = 9999)
#ahora lo uqe voy a hacer es la b-diversidad total por guild (es decir sin spp) en binario con el metodo betadiver
#betadiver 
library(tidyverse)
library(reshape2)
#primero una matriz de todos los coleopteros 
#beetles.total.h <- pol.9 %>% group_by(Plot, Species) %>% summarise (visits = sum(num.visitors))
c.1 <- dcast(pol.9, Plot+Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
c.1$plot <- c.1$Plot
c.1$plot <- as.numeric(c.1$plot)
#t.c1.1 <- subset(c.1, plot== "1")

sitios.be.t.h <- dplyr::left_join (dist2,c.1)
sitios.be.t.h[is.na(sitios.be.t.h)] <- 0

rowSums(sitios.be.t.h [,c(8:40)])
d.beetle.total.1 <-dist(sitios.be.t.h [,c(8:40)], method= "euclidean", diag =T, upper =T)

#m.beetle.total.h <-
 #betadiver(sipoo) 
m.todo.plot1 <- betadiver(sitios.be.t.h[,c(8:40)], method=15) 
sites.beet.h <-dist(sitios.be.t.h [,c(4,5)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.todo.plot1 , sites.beet.h,nrepet = 9999)
as.numeric(m.todo.plot1)
#######buenoooo----
c.1.1 <- dcast(pol.9, Plot +Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
c.1.1$plot <- c.1.1$Plot
c.1.1$plot <- as.numeric(c.1.1$plot)
t.c1.1.2<- subset(c.1.1, plot== "1")
uno <- subset(dist2, plot =="1")
sitios.be.t.h.2 <- dplyr::left_join (uno,t.c1.1.2)
sitios.be.t.h.2[is.na(sitios.be.t.h.2)] <- 0

d.beetle.total.1 <-dist(sitios.be.t.h.2 [,c(8:40)], method= "euclidean", diag =T, upper =T)

temp <- sitios.be.t.h.2[which(rowSums(sitios.be.t.h.2 [,c(8:40)]) != 0),8:40]
rowSums(temp)#bueno, esto es sin 0!!!
de <-sitios.be.t.h.2[which(rowSums(sitios.be.t.h.2 [,c(8:40)]) != 0),4:5]

m.todo. <- betadiver(temp [,c(1:30)], method=15) 

#colSums(sitios.be.t.h.2[,c(8:40)])
sites.beet.h.2 <-dist(de [,c(1,2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.todo. , sites.beet.h.2,nrepet = 9999)
#as.numeric(m.todo.plot1)
####################
plot2.t<- subset(c.1.1, plot== "2")
dos <- subset(dist2, plot =="2")
plot2.sitios <- dplyr::left_join (dos,plot2.t)
plot2.sitios[is.na(plot2.sitios)] <- 0

# <-dist(plot2.sitios [,c(8:40)], method= "euclidean", diag =T, upper =T)

temp2 <- plot2.sitios[which(rowSums(plot2.sitios [,c(8:40)]) != 0),8:40]

rowSums(plot2.sitios [,c(8:40)])

de.2 <-plot2.sitios[which(rowSums(plot2.sitios [,c(8:40)]) != 0),4:5]

m.todo.2 <- betadiver(temp2 [,c(1:30)], method=15) 


#colSums(sitios.be.t.h.2[,c(8:40)])
sites.2 <-dist(de.2 [,c(1,2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.todo.2 , sites.2,nrepet = 9999)
