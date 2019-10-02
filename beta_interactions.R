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
library(tidyverse)


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




